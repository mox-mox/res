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
		p1_wr_mask        : in  std_logic_vector( 3 downto 0); -- Mask write data. A high bit means corresponding byte is not written to the DDR2-DRAM.
		p1_wr_en          : in  std_logic;                     -- Write enable for the write data FIFO
		p1_wr_count       : out std_logic_vector( 6 downto 0); -- Write data FIFO fill level: 0: empty. Note longer latency than p1_wr_empty!
		p1_wr_empty       : out std_logic;                     -- Write data FIFO empty bit: 0: Not empty, 1: Empty
		p1_wr_error       : out std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_wr_full        : out std_logic;                     -- Write data FIFO full bit: 0: Not full, 1: Full
		p1_wr_underrun    : out std_logic;                     -- Underrun flag. 0: All ok, 1: Underrun. Last valid data is written repeatedly.
-- Read Datapath ------------------------------------------------------------------------------------------------------
		p1_rd_clk         : in  std_logic;                     -- Clock for the read data FIFO
		p1_rd_en          : in  std_logic;                     -- Read enable bit for the read data FIFO: 0: Diabled, 1: Enabled
		p1_rd_data        : out std_logic_vector(31 downto 0); -- Data read from the DDR2-DRAM
		p1_rd_full        : out std_logic;                     -- Read data FIFO full bit: 0: All ok, 1: Full. Data will be discarded.
		p1_rd_empty       : out std_logic;                     -- Read data FIFO empty bit: 0: Not empty, 1: Empty. Cannot read data from FIFO.
		p1_rd_count       : out std_logic_vector( 6 downto 0); -- Read data FIFO fill level: 0: empty. Note longer latency than p1_rd_full!
		p1_rd_overflow    : out std_logic;                     -- Overflow flag: 0: All ok, 1: Data was lost because the FIFO overflowed.
		p1_rd_error       : out std_logic);                    -- Error bit. Need to reset the MCB to resolve. }}}
end MEM_CTL_DUMMY;

architecture normal of MEM_CTL_DUMMY is

	--{{{ Wiring logic

	constant wire_delay : time := 1 ns;
	signal p1_cmd_instr_bl_addr_concat : std_logic_vector(38 downto 0);
	signal cmd_readen                  : std_logic;
	signal cmd_instr_bl_addr_concat    : std_logic_vector(38 downto 0);
	alias  cmd_instr                   is cmd_instr_bl_addr_concat(38 downto 36);
	alias  cmd_bl                      is cmd_instr_bl_addr_concat(35 downto 30);
	alias  cmd_addr                    is cmd_instr_bl_addr_concat(29 downto 0);
	constant cmd_write_normal          : std_logic_vector( 2 downto 0) := "000";
	constant cmd_write_precharge       : std_logic_vector( 2 downto 0) := "010";
	constant cmd_read_normal           : std_logic_vector( 2 downto 0) := "001";
	constant cmd_read_precharge        : std_logic_vector( 2 downto 0) := "011";
	--constant cmd_refresh               : std_logic_vector( 2 downto 0) := "100";

	signal p1_wr_mask_data_concat      : std_logic_vector(35 downto 0);
	signal wr_readen                   : std_logic;
	signal wr_mask_data_concat         : std_logic_vector(35 downto 0);
	alias  wr_mask                     is wr_mask_data_concat(35 downto 32);
	alias  wr_data                     is wr_mask_data_concat(31 downto  0);

	signal rd_data                     : std_logic_vector(31 downto 0);
	signal rd_writeen                  : std_logic;

	signal p1_cmd_empty_sig      : std_logic;
	signal p1_cmd_error_sig      : std_logic;
	signal p1_cmd_full_sig       : std_logic;

	signal p1_wr_count_sig       : std_logic_vector( 6 downto 0);
	signal p1_wr_empty_sig       : std_logic;
	signal p1_wr_error_sig       : std_logic;
	signal p1_wr_full_sig        : std_logic;
	signal p1_wr_underrun_sig    : std_logic;

	signal p1_rd_data_sig        : std_logic_vector(31 downto 0);
	signal p1_rd_full_sig        : std_logic;
	signal p1_rd_empty_sig       : std_logic;
	signal p1_rd_count_sig       : std_logic_vector( 6 downto 0);
	signal p1_rd_overflow_sig    : std_logic;
	signal p1_rd_error_sig       : std_logic;


	--}}}

	--{{{ Components

	--{{{
    component CMD_FIFO
        Generic (
            constant DATA_WIDTH : positive := 39;
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
            constant DATA_WIDTH : positive := 36;
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

	for fifo_cmd   : CMD_FIFO    use entity work.FWFT_FIFO(Behavioral);
	for fifo_wr    : WR_FIFO     use entity work.FWFT_FIFO(Behavioral);
	for fifo_rd    : RD_FIFO     use entity work.FWFT_FIFO(Behavioral);
	--}}}

	--{{{ The Dummy DRAM

		-- Use either the full sized 16 MB DRAM as a shared variable...
	--type ram_type is array (0 to 4194304) of std_logic_vector (31 downto 0);
	--shared variable DRAM : ram_type := (x"AAAAAAAA", x"BBBBBBBB", x"CCCCCCCC", x"DDDDDDDD", x"EEEEEEEE", x"FFFFFFFF", x"AFAFAFAF", x"BFBFBFBF", x"CFCFCFCF", x"DFDFDFDF", x"EFEFEFEF", x"FFFFFFFF", others => x"00000000");

		-- ... or define it as signal, enabling viewing it in gtkwave, but reduce the size.
	type ram_type is array (0 to 1024) of std_logic_vector (31 downto 0);
	signal DRAM : ram_type := (x"AAAAAAAA", x"BBBBBBBB", x"CCCCCCCC", x"DDDDDDDD", x"EEEEEEEE", x"FFFFFFFF", x"AFAFAFAF", x"BFBFBFBF", x"CFCFCFCF", x"DFDFDFDF", x"EFEFEFEF", x"FFFFFFFF", others => x"00000000");
	--}}}

	--{{{ State machine states

	type mem_ctl_dummy_state_type is (idle, read, write);
	signal current_state,        next_state        : mem_ctl_dummy_state_type := idle;
	signal current_delay_count,  next_delay_count  : natural                  := 0;
	signal current_burst_length, next_burst_length : natural                  := 0;
	signal current_addr,         next_addr         : natural                  := 0;

	signal random_delay : natural;
	--}}}

begin

	--{{{ Wire the internal signals to the outputs with wire_delays

	--{{{ Constant values not simulated

	p1_cmd_error_sig   <= '0';
	p1_wr_error_sig    <= '0';
	p1_rd_error_sig    <= '0';
	p1_wr_count_sig    <= "1010101";
	p1_rd_count_sig    <= "1010101";
	p1_wr_underrun_sig <= '0';
	p1_rd_overflow_sig <= '0';
	--}}}

	p1_cmd_empty   <= p1_cmd_empty_sig   after wire_delay;
	p1_cmd_error   <= p1_cmd_error_sig   after wire_delay;
	p1_cmd_full    <= p1_cmd_full_sig    after wire_delay;

	p1_wr_count    <= p1_wr_count_sig    after wire_delay;
	p1_wr_empty    <= p1_wr_empty_sig    after wire_delay;
	p1_wr_error    <= p1_wr_error_sig    after wire_delay;
	p1_wr_full     <= p1_wr_full_sig     after wire_delay;
	p1_wr_underrun <= p1_wr_underrun_sig after wire_delay;

	p1_rd_data     <= p1_rd_data_sig     after wire_delay;
	p1_rd_full     <= p1_rd_full_sig     after wire_delay;
	p1_rd_empty    <= p1_rd_empty_sig    after wire_delay;
	p1_rd_count    <= p1_rd_count_sig    after wire_delay;
	p1_rd_overflow <= p1_rd_overflow_sig after wire_delay;
	p1_rd_error    <= p1_rd_error_sig    after wire_delay;
	--}}}

	--{{{ Port Maps

	--{{{
	fifo_cmd : CMD_FIFO port map (
		CLK     => p1_cmd_clk,
		DataIn  => p1_cmd_instr_bl_addr_concat,
		WriteEn => p1_cmd_en,
		ReadEn  => cmd_readen,
		DataOut => cmd_instr_bl_addr_concat,
		Empty   => p1_cmd_empty_sig,
		Full    => p1_cmd_full_sig,
		RST     => rst
	);
	--assert p1_cmd_instr(2)='0' report "The cache controller issued a refresh command" severity failure;
	p1_cmd_instr_bl_addr_concat <= p1_cmd_instr & p1_cmd_bl & p1_cmd_addr;
	--}}}

	--{{{
	fifo_wr : wr_FIFO port map (
		CLK     => p1_wr_clk,
		DataIn  => p1_wr_mask_data_concat,
		WriteEn => p1_wr_en,
		ReadEn  => wr_readen,
		DataOut => wr_mask_data_concat,
		Empty   => p1_wr_empty_sig,
		Full    => p1_wr_full_sig,
		RST     => rst
	);
	p1_wr_mask_data_concat <= p1_wr_mask & p1_wr_data;
	--}}}

	--{{{
	fifo_rd : RD_FIFO port map (
		CLK     => p1_rd_clk,
		DataIn  => rd_data,
		WriteEn => rd_writeen,
		ReadEn  => p1_rd_en,
		DataOut => p1_rd_data_sig,
		Empty   => p1_rd_empty_sig,
		Full    => p1_rd_full_sig,
		RST     => rst
	);
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
			random_delay <= integer(helper * real(3));
		end if;
	end process;
	--}}}

	--{{{ Do the actual work

	--{{{
	generate_next_state : process (current_delay_count, p1_cmd_empty_sig, cmd_instr) --TODO
	begin
		next_state        <= current_state        after wire_delay; -- default assignement
		next_delay_count  <= current_delay_count  after wire_delay;
		next_burst_length <= current_burst_length after wire_delay;
		next_addr         <= current_addr         after wire_delay;
		case current_state is
			when idle =>
				if current_delay_count = 0 then
					if p1_cmd_empty_sig = '0' then -- If there are commands waiting
						next_burst_length <= to_integer(unsigned(cmd_bl))+1; -- TODO: Is the +1 really needed?
						next_addr         <= to_integer(unsigned(cmd_addr));
						if cmd_instr=cmd_read_normal or cmd_instr=cmd_read_precharge then
							next_state <= read after wire_delay;
						else --cmd_instr=cmd_write_normal or cmd_instr=cmd_write_precharge then
							next_state <= write after wire_delay;
						end if; -- Refresh command is excluded via assertion.
					else
						next_state       <= idle         after wire_delay;
						next_delay_count <= random_delay after wire_delay;
					end if;
				else
					next_state        <= idle                    after wire_delay;
					next_delay_count  <= current_delay_count - 1 after wire_delay;
				end if;
			when read =>
				if current_burst_length = 0 then -- When the current command is done
					next_state        <= idle                     after wire_delay;
					next_delay_count  <= random_delay             after wire_delay;
				else
					next_state        <= read                     after wire_delay;
					next_burst_length <= current_burst_length - 1 after wire_delay;
					next_addr         <= current_addr + 1         after wire_delay;
				end if;

			when write =>
				if current_burst_length = 0 then -- When the current command is done
					next_state        <= idle                     after wire_delay;
					next_delay_count  <= random_delay             after wire_delay;
				else
					next_state        <= write                    after wire_delay;
					next_burst_length <= current_burst_length - 1 after wire_delay;
					next_addr         <= current_addr + 1         after wire_delay;
				end if;
		end case;
	end process;
	--}}}

	--{{{
	adopt_next_state : process (p1_cmd_clk)
	begin
		if(rising_edge(p1_cmd_clk)) then
			if rst = '1' then
				current_state         <= idle;
				current_delay_count   <= random_delay;
				current_burst_length  <= 0;
				current_addr          <= 0;
			else
				current_state        <= next_state;
				current_delay_count  <= next_delay_count;
				current_burst_length <= next_burst_length;
				current_addr         <= next_addr;
			end if;
		end if;
	end process;
	--}}}

	--{{{ Drive FIFOs

	cmd_readen <= '0'               after wire_delay when rst = '1' else
				  '1'               after wire_delay when current_state=idle and current_delay_count=0 and p1_cmd_empty_sig='0' else
				  '0'               after wire_delay;

	wr_readen  <= '0'               after wire_delay when rst = '1' else
	              '1'               after wire_delay when current_state=write else
	              '0'               after wire_delay;

	rd_writeen <= '0'               after wire_delay when rst = '1' else
	              '1'               after wire_delay when current_state=read else
	              '0'               after wire_delay;

	rd_data    <= (others => '-')   after wire_delay when rst = '1' else
	              DRAM(current_addr) after wire_delay when current_state=read else
	              (others => '-')   after wire_delay;
	--}}}

	--{{{
	write_ram : process(p1_cmd_clk)
	begin
		if(rising_edge(p1_cmd_clk)) then
			if(rst='0' and current_state=write) then
				for i in 0 to 3 loop
					if wr_mask(i) = '1' then
						DRAM(current_addr)(((i+1)*8)-1 downto (i*8)) <= wr_data(((i+1)*8)-1 downto (i*8));
					end if;
				end loop;
			end if;
		end if;
	end process;
	--}}}
	--}}}

end normal;

