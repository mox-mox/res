library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity MEM_CTL_DUMMY is
	--{{{
	port(
		rst               : in   std_logic;
-- Command Path -------------------------------------------------------------------------------------------------------
		p1_cmd_clk        : in  std_logic;                     -- User clock for the command FIFO
		p1_cmd_instr      : in  std_logic_vector( 2 downto 0); -- Current instruction. 000: Wrtie, 001: Read, 010: Write w. precharge, 011: ...
		p1_cmd_addr       : in  std_logic_vector(29 downto 0); -- Byte start address for current transaction.
		p1_cmd_bl         : in  std_logic_vector( 5 downto 0); -- Busrst length-1, eg. 0 indicates a burst of one word
		p1_cmd_en         : in  std_logic;                     -- Write enable for the command FIFO: 0: Diabled, 1: Enabled
		p1_cmd_empty      : out std_logic;                     -- Command FIFO empty bit: 0: Not empty, 1: Empty
		p1_cmd_error      : out std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_cmd_full       : out std_logic;                     -- Command FIFO full bit: 0: Not full, 1: Full
-- Write Datapath -----------------------------------------------------------------------------------------------------
		p1_wr_clk         : in  std_logic;                     -- Clock for the write data FIFO
		p1_wr_data        : in  std_logic_vector(31 downto 0); -- Data to be stored in the FIFO and be written to the DDR2-DRAM.
		p1_wr_mask        : in  std_logic_vector( 3 downto 0); -- Mask write data. A high bit means corresponding byte is not written to the RAM.
		p1_wr_en          : in  std_logic;                     -- Write enable for the write data FIFO
		p1_wr_count       : out std_logic_vector( 6 downto 0); -- Write data FIFO fill level: 0: empty. Note longer latency than p1_wr_empty!
		p1_wr_empty       : out std_logic;                     -- Write data FIFO empty bit: 0: Not empty, 1: Empty
		p1_wr_error       : out std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_wr_full        : out std_logic;                     -- Write data FIFO full bit: 0: Not full, 1: Full
		p1_wr_underrun    : out std_logic;                     -- Underrun flag. 0: All ok, 1: Underrun. Last valid data is written repeatedly.
-- Read Datapath ------------------------------------------------------------------------------------------------------
		p1_rd_clk         : in  std_logic;                     -- Clock for the read data FIFO
		p1_rd_en          : in  std_logic;                     -- Read enable bit for the read data FIFO: 0: Diabled, 1: Enabled
		p1_rd_data        : out std_logic_vector(31 downto 0); -- Data read from the RAM
		p1_rd_full        : out std_logic;                     -- Read data FIFO full bit: 0: All ok, 1: Full. Data will be discarded.
		p1_rd_empty       : out std_logic;                     -- Read data FIFO empty bit: 0: Not empty, 1: Empty. Cannot read data from FIFO.
		p1_rd_count       : out std_logic_vector( 6 downto 0); -- Read data FIFO fill level: 0: empty. Note longer latency than p1_rd_full!
		p1_rd_overflow    : out std_logic;                     -- Overflow flag: 0: All ok, 1: Data was lost because the FIFO overflowed.
		p1_rd_error       : out std_logic);                    -- Error bit. Need to reset the MCB to resolve. }}}
end MEM_CTL_DUMMY;

architecture normal of MEM_CTL_DUMMY is

	--{{{ Wiring logic

	signal cmd_empty                   : std_logic;
	signal p1_cmd_instr_bl_addr_concat : std_logic_vector(38 downto 0);
	signal cmd_readen                  : std_logic;
	signal cmd_instr_bl_addr_concat    : std_logic_vector(38 downto 0);
	alias  cmd_instr                   is cmd_instr_bl_addr_concat(38 downto 36);
	alias  cmd_bl                      is cmd_instr_bl_addr_concat(35 downto 30);
	alias  cmd_addr                    is cmd_instr_bl_addr_concat(29 downto 0);

	signal wr_empty                    : std_logic;
	signal p1_wr_mask_data_concat      : std_logic_vector(35 downto 0);
	signal wr_readen                   : std_logic;
	signal wr_mask_data_concat         : std_logic_vector(35 downto 0);
	alias  wr_mask                     is wr_mask_data_concat(35 downto 32);
	alias  wr_data                     is wr_mask_data_concat(31 downto  0);

	signal rd_full                     : std_logic;
	signal rd_data                     : std_logic_vector(31 downto 0);
	signal rd_writeen                  : std_logic;


	--cmd_readen  <= '0';
	--wr_readen   <= '0';
	--rd_writeen  <= '0';
	--rd_data     <= '0';





	--}}}

	--{{{ Components

	--{{{
    component CMD_FIFO
        Generic (
            constant DATA_WIDTH : positive := 38;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(38 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(38 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	--{{{
    component WR_FIFO
        Generic (
            constant DATA_WIDTH : positive := 35;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(35 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(35 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	--{{{
    component RD_FIFO
        Generic (
            constant DATA_WIDTH : positive := 32;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(31 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(31 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	--for fifo_cmd   : CMD_FIFO    use entity work.FWFT_FIFO(Behavioral);
	--for fifo_wr    : WR_FIFO     use entity work.FWFT_FIFO(Behavioral);
	for fifo_rd    : RD_FIFO     use entity work.FWFT_FIFO(Behavioral);
	--}}}

	--{{{ The Dummy RAM

	type ram_type is array (4194304 downto 0) of std_logic_vector (31 downto 0);
	shared variable RAM : ram_type;
	--}}}

	signal random_delay : natural;

	--{{{
	procedure write_ram (mask : std_logic_vector(3 downto 0); addr : natural; data : std_logic_vector(31 downto 0)) is
	begin
		for i in 0 to 3 loop
			if mask(i) = '1' then
				RAM(addr)(((i+1)*8)-1 downto (i*8)) := data(((i+1)*8)-1 downto (i*8));
			end if;
		end loop;
	end;
	--}}}

begin
	p1_cmd_error   <= '0';
	p1_wr_error    <= '0';
	p1_rd_error    <= '0';
	p1_wr_count    <= "1010101";
	p1_rd_count    <= "1010101";
	p1_wr_underrun <= '0';
	p1_rd_overflow <= '0';

	--{{{ Port Maps

	--{{{
	fifo_cmd : CMD_FIFO port map (
		CLK     => p1_cmd_clk,
		DataIn  => p1_cmd_instr_bl_addr_concat,
		WriteEn => p1_cmd_en,
		ReadEn  => cmd_readen,
		DataOut => cmd_instr_bl_addr_concat,
		Empty   => cmd_empty,
		Full    => p1_cmd_full,
		RST     => rst
	);
	p1_cmd_instr_bl_addr_concat <= p1_cmd_instr & p1_cmd_bl & p1_cmd_addr;
	p1_cmd_empty <= cmd_empty;
	--}}}

	--{{{
	fifo_wr : wr_FIFO port map (
		CLK     => p1_wr_clk,
		DataIn  => p1_wr_mask_data_concat,
		WriteEn => p1_wr_en,
		ReadEn  => wr_readen,
		DataOut => wr_mask_data_concat,
		Empty   => wr_empty,
		Full    => p1_wr_full,
		RST     => rst
	);
	p1_wr_mask_data_concat <= p1_wr_mask & p1_wr_data;
	p1_wr_empty <= wr_empty;
	--}}}

	--{{{
	fifo_rd : RD_FIFO port map (
		CLK     => p1_rd_clk,
		DataIn  => rd_data,
		WriteEn => rd_writeen,
		ReadEn  => p1_rd_en,
		DataOut => p1_rd_data,
		Empty   => p1_rd_empty,
		Full    => rd_full,
		RST     => rst
	);
	p1_rd_full <= rd_full;
	--}}}
	--}}}

	--{{{
	generate_random_delay : process(p1_cmd_clk)
		variable seed1  : positive := 1;
		variable seed2  : positive := 1;
		variable helper : real;
	begin
		if(rising_edge(p1_cmd_clk)) then
			uniform (seed1,seed2,helper);
			random_delay <= integer(helper * real(8));
		end if;
	end process;
	--}}}

	perform_work : process(p1_cmd_clk)
		variable delay_counter : natural;
		variable burst_length  : natural;
		variable read          : std_logic;
		variable addr          : natural;


	begin
		if(rising_edge(p1_cmd_clk)) then
			if(rst = '1') then
				delay_counter := random_delay;
				cmd_readen  <= '0';
				wr_readen   <= '0';
				rd_writeen  <= '0';
				rd_data     <= (others => '0');
			else
				--{{{
				if delay_counter = 1 and cmd_empty = '0' then
					cmd_readen     <= '1';
					wr_readen   <= '0';
					rd_writeen  <= '0';
					rd_data     <= (others => '0');
					delay_counter  := delay_counter - 1;
					if cmd_instr="001" or cmd_instr="011" then
						read       := '1';
					elsif cmd_instr="000" or cmd_instr="010" then
						read       := '0';
					else
						assert true report "Invalid cmd." severity failure;
					end if;
					burst_length   := to_integer(unsigned(cmd_bl))+1;
					addr           := to_integer(unsigned(cmd_addr));
				--}}}

				--{{{
				elsif delay_counter = 0 then
					if burst_length = 0 then
						delay_counter := random_delay;
						cmd_readen  <= '0';
						wr_readen   <= '0';
						rd_writeen  <= '0';
						rd_data     <= (others => '0');
					else
						if read = '1' then
							assert rd_full = '0'  report "Read data overflow." severity failure;
							cmd_readen  <= '0';
							wr_readen   <= '0';
							rd_writeen  <= '1';
							rd_data     <= RAM(addr);
						else -- write
							assert wr_empty = '0'  report "Insufficient write data." severity failure;
							write_ram(wr_mask, addr, wr_data);
							cmd_readen  <= '0';
							wr_readen   <= '1';
							rd_writeen  <= '0';
							rd_data     <= (others => '0');
						end if;
						addr := addr+1;
					end if;
				--}}}

				else -- Emulate the delay of the real controller, just wait.
					delay_counter := delay_counter - 1;
					cmd_readen  <= '0';
					wr_readen   <= '0';
					rd_writeen  <= '0';
					rd_data     <= (others => '0');
				end if;


			end if;
		end if;
	end process;





end normal;

