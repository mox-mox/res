library ieee;
use ieee.std_logic_1164.all;
use work.read_fsm_pkg.all;
use work.write_fsm_pkg.all;
use ieee.numeric_std.all;


entity AHBL2SDRAM is
	port (
-- AHB-LITE Interface {{{

-- Global signals ---------------------------------------------------------------------------------------------------------------
		HCLK              : in    std_logic;                     -- Bus clock
		HRESETn           : in    std_logic;                     -- Reset
-- AHB Slave inputs ---------------------------------------------------------------------------------------------------
		HSEL              : in    std_logic;                     -- Slave select
		HADDR             : in    std_logic_vector(31 downto 0); -- Slave address
		HWRITE            : in    std_logic;                     -- Diretion: 0: Master read, 1: Master write
		HSIZE             : in    std_logic_vector( 2 downto 0); -- Transfer Word size: 000: Byte, 001: Halfword, 010: Word, others: undefined
		-- HBURST         : in    std_logic_vector( 2 downto 0)  -- NOT IMPLEMENTED
		-- HPROT          : in    std_logic_vector( 3 downto 0)  -- NOT IMPLEMENTED, Could be used to create a seperated cache for instructions and data.
		HTRANS            : in    std_logic_vector( 1 downto 0); -- Transaction status: 00: IDLE, 01: BUSY, 10: NON-SEQUENTIAL, 11: SEQUENTIAL
		-- HMASTLOCK      : in    std_logic;                     -- NOT IMPLEMENTED
		HREADY            : in    std_logic;                     -- Master's ready signal: 0: Busy, 1: Ready for next transaction
		HWDATA            : in    std_logic_vector(31 downto 0); -- Incomming Data from master
-- AHB Slave outputs --------------------------------------------------------------------------------------------------
		HREADYOUT         : out   std_logic;                     -- Slave's ready signal: 0: Busy, 1: Ready
		HRESP             : out   std_logic;                     -- Transfer response: 0: Okay, 1: Error. Needs one additional wait state with HREADYout low.
		HRDATA            : out   std_logic_vector(31 downto 0); -- Outgoing Data to master }}}

-- Memory Controller Interface {{{

-- Command Path -------------------------------------------------------------------------------------------------------
		p1_cmd_clk        : out   std_logic;                     -- User clock for the command FIFO
		p1_cmd_instr      : out   std_logic_vector( 2 downto 0); -- Current instruction. 000: Wrtie, 001: Read, 010: Read w. precharge, 011: ...
		p1_cmd_addr       : out   std_logic_vector(29 downto 0); -- Byte start address for current transaction.
		p1_cmd_bl         : out   std_logic_vector( 5 downto 0); -- Busrst length-1, eg. 0 indicates a burst of one word
		p1_cmd_en         : out   std_logic;                     -- Write enable for the command FIFO: 0: Diabled, 1: Enabled
		p1_cmd_empty      : in    std_logic;                     -- Command FIFO empty bit: 0: Not empty, 1: Empty
		p1_cmd_error      : in    std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_cmd_full       : in    std_logic;                     -- Command FIFO full bit: 0: Not full, 1: Full
-- Write Datapath -----------------------------------------------------------------------------------------------------
		p1_wr_clk         : out   std_logic;                     -- Clock for the write data FIFO
		p1_wr_data        : out   std_logic_vector(31 downto 0); -- Data to be stored in the FIFO and be written to the DDR2-DRAM.
		p1_wr_mask        : out   std_logic_vector( 3 downto 0); -- Mask write data. A high bit means corresponding byte is not written to the DDR2-DRAM.
		p1_wr_en          : out   std_logic;                     -- Write enable for the write data FIFO
		p1_wr_count       : in    std_logic_vector( 6 downto 0); -- Write data FIFO fill level: 0: empty. Note longer latency than p1_wr_empty!
		p1_wr_empty       : in    std_logic;                     -- Write data FIFO empty bit: 0: Not empty, 1: Empty
		p1_wr_error       : in    std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_wr_full        : in    std_logic;                     -- Write data FIFO full bit: 0: Not full, 1: Full
		p1_wr_underrun    : in    std_logic;                     -- Underrun flag. 0: All ok, 1: Underrun. Last valid data is written repeatedly.
-- Read Datapath ------------------------------------------------------------------------------------------------------
		p1_rd_clk         : out   std_logic;                     -- Clock for the read data FIFO
		p1_rd_en          : out   std_logic;                     -- Read enable bit for the read data FIFO: 0: Diabled, 1: Enabled
		p1_rd_data        : in    std_logic_vector(31 downto 0); -- Data read from the DDR2-DRAM
		p1_rd_full        : in    std_logic;                     -- Read data FIFO full bit: 0: All ok, 1: Full. Data will be discarded.
		p1_rd_empty       : in    std_logic;                     -- Read data FIFO empty bit: 0: Not empty, 1: Empty. Cannot read data from FIFO.
		p1_rd_count       : in    std_logic_vector( 6 downto 0); -- Read data FIFO fill level: 0: empty. Note longer latency than p1_rd_full!
		p1_rd_overflow    : in    std_logic;                     -- Overflow flag: 0: All ok, 1: Data was lost because the FIFO overflowed.
		p1_rd_error       : in    std_logic;                     -- Error bit. Need to reset the MCB to resolve. }}}

-- Miscellaneous non-bus signals {{{
		DCLK              : in    std_logic;                      -- Double speed internal clock used to speed up the internal logic.
		mem_calib_done    : in    std_logic;                      -- Clock used to speed up the internal logic. MUST BE SYNCHRONISED WITH HCLK!!
		HIT_COUNT         : out   std_logic_vector(31 downto 0);  -- The number of accesses that resulted in a cache hit since the last reset.
		MISS_COUNT        : out   std_logic_vector(31 downto 0);  -- The number of accesses that resulted in a cache miss since the last reset.
		INVALIDATE_LOW    : inout std_logic_vector(31 downto 0);  -- Writing this and INVALIDATE_HIGH to a value except 0xffffffff marks all lines in that...
		INVALIDATE_HIGH   : inout std_logic_vector(31 downto 0)   -- ...range as invalid. !!! SUCCESSIVE WRITES MUST WAIT UNTIL THE CACHE CONTROLLER HAS...
		);                                                        -- ...WRITTEN BOTH REGISTERS TO 0xffffffff AGAIN !!! }}} TODO: Implement this.
end AHBL2SDRAM;


--{{{
architecture cache of AHBL2SDRAM is

	--{{{ Address Format:

	-- 31       23           1l      4   1  0
	-- |00000000|XXXXXXXXXXXX|XXXXXXX|XXX|XX|
	-- |00000000|TAG         |INDEX  |WS |BS|
	-- |8       |12          |7      |3  |2 |

	alias HADDR_NULLED      is HADDR(31 downto 24);
	alias HADDR_TAG         is HADDR(23 downto 12);
	alias HADDR_IDX         is HADDR(11 downto  5);
	alias HADDR_WS          is HADDR( 4 downto  2);
	alias HADDR_BS          is HADDR( 1 downto  0);
	--}}}

	--{{{ DRAM constants

	constant DRAM_CMD_WRITE                     : std_logic_vector( 2 downto 0) := "000";
	constant DRAM_CMD_READ                      : std_logic_vector( 2 downto 0) := "001";
	--}}}

	--{{{ Tag SRAM:

	signal tag_sram_a_en       :  std_logic;
	signal tag_sram_a_idx      :  std_logic_vector( 9 downto 0);
	signal tag_sram_a_do       :  std_logic_vector(15 downto 0);
	alias  tag_sram_a_do_tag   is tag_sram_a_do(   11 downto 0);
	alias  tag_sram_a_do_valid is tag_sram_a_do(            12);

	signal tag_sram_b_en       :  std_logic;
	signal tag_sram_b_idx      :  std_logic_vector( 9 downto 0);
	signal tag_sram_b_di       :  std_logic_vector(15 downto 0);

	component TAG_SRAM is
		port (clk  : in std_logic;
		   -- Port A
			  en_A   : in std_logic;
			  we_A   : in std_logic;
			  addr_A : in std_logic_vector( 9 downto 0);
			  di_A   : in std_logic_vector(15 downto 0);
			  do_A   : out std_logic_vector(15 downto 0);

		   -- Port B
			  en_B   : in std_logic;
			  we_B   : in std_logic;
			  addr_B : in std_logic_vector( 9 downto 0);
			  di_B   : in std_logic_vector(15 downto 0);
			  do_B   : out std_logic_vector(15 downto 0)
		  );
	end component TAG_SRAM;
	--}}}

	--{{{ Data SRAM:

	signal data_sram_a_en         : std_logic;
	signal data_sram_a_mask    : std_logic_vector( 3 downto 0);
	signal data_sram_a_idx        : std_logic_vector( 9 downto 0);
	signal data_sram_a_di         : std_logic_vector(31 downto 0);

	signal data_sram_b_en         : std_logic;
	signal data_sram_b_we         : std_logic;
	signal data_sram_b_idx        : std_logic_vector( 9 downto 0);
	signal data_sram_b_di         : std_logic_vector(31 downto 0);
	signal data_sram_b_do         : std_logic_vector(31 downto 0);

	component DATA_SRAM is
		port (clk       : in  std_logic;
		  -- Port A
			  en_A      : in  std_logic;
			  we_A      : in  std_logic;
			  addr_A    : in  std_logic_vector( 9 downto 0);
			  di_A      : in  std_logic_vector(31 downto 0);
			  wr_mask_A : in  std_logic_vector( 3 downto 0);
			  do_A      : out std_logic_vector(31 downto 0);

		  -- Port B
			  en_B      : in  std_logic;
			  we_B      : in  std_logic;
			  addr_B    : in  std_logic_vector( 9 downto 0);
			  di_B      : in  std_logic_vector(31 downto 0);
			  wr_mask_B : in  std_logic_vector( 3 downto 0);
			  do_B      : out std_logic_vector(31 downto 0)
		  );
	end component DATA_SRAM;
	--}}}

	--{{{ Address and data save registers

	signal SAVE0_HADDR   : std_logic_vector(31 downto  0) := (others => '-');
	alias  Save0_HADDR_TAG         is HADDR(23 downto 12);
	alias  Save0_HADDR_IDX         is HADDR(11 downto  5);
	alias  Save0_HADDR_WS          is HADDR( 4 downto  2);
	alias  Save0_HADDR_BS          is HADDR( 1 downto  0);
	signal SAVE0_HSIZE   : std_logic_vector( 2 downto  0) := (others => '-');
	signal SAVE0_HWRITE  : std_logic                      := '-';
	signal SAVE1_HWRITE  : std_logic                      := '-';
	--}}}

	--{{{ Common signals
	signal hit           : std_logic;
	signal HCLK_PHASE    : std_logic := '0';
	signal HREADYOUT_sig : std_logic := '1';

	--}}}

	--{{{ Write FSM

	signal write_request             :  std_logic;
	signal write_dram_busy           :  std_logic;
	signal write_current_state       :  write_fsm_state_type;

	signal write_SAVE1_HADDR         :   std_logic_vector(31 downto  0) := (others => '-');
	alias  write_SAVE1_HADDR_BS      is write_SAVE1_HADDR( 1 downto  0);
	signal write_SAVE1_HWDATA        :   std_logic_vector(31 downto  0) := (others => '-');
	signal write_SAVE1_HSIZE         :   std_logic_vector( 2 downto  0) := (others => '-');
	signal write_busy                :   std_logic;

	component WRITE_FSM is
		port (
				 DCLK      : in  std_logic; -- 2xHCLK
				 RES_n     : in  std_logic; -- HRESETn

		-- The input variables to the state machine
				 REQUEST   : in  std_logic; -- HWRITE && HREADY && ( HSEL or HSEL & HPROT for non-unified cache )
				 HIT       : in  std_logic; -- The cache hit or miss information
				 DRAM_BUSY : in  std_logic; -- p1_cmd_full || p1_rd_empty
				 HCLK      : in  std_logic; -- HCLK

		-- The state register
				 state     : out write_fsm_state_type
			 );
	end component WRITE_FSM;
	--}}}

	--{{{ Read FSM

	signal read_request              :  std_logic;
	signal read_ws_zero              :  std_logic;
	signal read_current_state        :  read_fsm_state_type;

	signal read_SAVE1_HADDR          :  std_logic_vector(31 downto  0) := (others => '-');
	alias  read_SAVE1_HADDR_TAG      is read_SAVE1_HADDR(23 downto 12);
	alias  read_SAVE1_HADDR_WS       is read_SAVE1_HADDR( 4 downto  2);
	alias  read_SAVE1_HADDR_IDX      is read_SAVE1_HADDR(11 downto  5);
	alias  read_SAVE1_HADDR_BS       is read_SAVE1_HADDR( 1 downto  0);
	signal read_SAVE1_HSIZE          :  std_logic_vector( 2 downto  0) := (others => '-');
	signal read_keep_dram_data       :  std_logic_vector(31 downto  0) := (others => '-');
	signal read_busy                 :  std_logic;

	component READ_FSM is
	port (
		DCLK              : in  std_logic;                      -- 2xHCLK
		RES_n             : in  std_logic;                      -- HRESETn

		-- The input signals to the state machine
		REQUEST           : in  std_logic;                      -- !HWRITE && HREADY && ( HSEL or HSEL & HPROT for non-unified cache )
		HIT               : in  std_logic;                      -- The cache hit or miss information
		DRAM_BUSY         : in  std_logic;                      -- p1_cmd_full
		DRAM_EMPTY        : in  std_logic;                      -- p1_rd_empty
		WS_ZERO           : in  std_logic;                      -- Whether the requested word was the first in cache line
		HCLK              : in  std_logic;

		-- The state register
		state             : out read_fsm_state_type
         );
	end component READ_FSM;
	--}}}

	--{{{ Hit and Miss Registers for statistics These will probably need to be moved to the module that contains the register file.

	--signal hit_counter  : unsigned (31 downto 0) := (others => '0');
	--signal miss_counter : unsigned (31 downto 0) := (others => '0');
	signal hit_counter  : std_logic_vector(31 downto 0) := (others => '0');
	signal miss_counter : std_logic_vector(31 downto 0) := (others => '0');
	---}}}

	--{{{ Helper functions

	--{{{
	function hsize_2_real_size (HSIZE : std_logic_vector(2 downto 0); res_n : std_logic) return natural is
	variable real_size : integer range 4 downto 0;
	begin
		--for i in 0 to HSIZE'LENGTH-1 loop
		--	report "HSIZE("&integer'image(i)&") value is" & std_logic'image(HSIZE(i));
		--end loop;
		if HSIZE = "000" then
			real_size := 1;
		elsif HSIZE = "001" then
			real_size := 2;
		elsif HSIZE = "010" then
			real_size := 4;
		else
			if res_n = '1' then -- do not enforce encoding when in reset
				for i in 0 to HSIZE'LENGTH-1 loop
					report "HSIZE("&integer'image(i)&") value is" & std_logic'image(HSIZE(i));
				end loop;
				assert false report "Invalid HSIZE" severity error;
			end if;
			real_size := 0; -- block all reads for bigger sizes
		end if;
		return real_size;
	end;
	--}}}

	--{{{
	function write_mask (BS : std_logic_vector(1 downto 0); HSIZE : std_logic_vector(2 downto 0); res_n : std_logic) return std_logic_vector is
	variable result: std_logic_vector(3 downto 0) := "1111";
	variable real_size : natural := hsize_2_real_size(HSIZE, res_n);
	begin
		if(res_n = '1') then
			for i in 0 to 3 loop
				if ( i >= to_integer(unsigned(BS)) and i < to_integer(unsigned(BS) + real_size)) then
					result(i) := '0';
				end if;
			end loop;
		end if;
		return result;
	end;
	--}}}
	--}}}

begin

	--{{{ Port Maps

--	ts : tag_sram port map ( clk => DCLK,
--			en_A => tag_sram_a_en, we_A => '0',           addr_A => tag_sram_a_idx, di_A => (others => '-'), do_A => tag_sram_a_do, -- Port A
--			en_B => '1', we_B => '1', addr_B => "0000000010", di_B => x"aa55",   do_B => open           -- Port B
--		);

	ts : tag_sram port map ( clk => DCLK,
			en_A => tag_sram_a_en, we_A => '0',           addr_A => tag_sram_a_idx, di_A => (others => '-'), do_A => tag_sram_a_do, -- Port A
			en_B => tag_sram_b_en, we_B => tag_sram_b_en, addr_B => tag_sram_b_idx, di_B => tag_sram_b_di,   do_B => open           -- Port B
		);

	ds : DATA_SRAM port map (clk => DCLK,
			en_A => data_sram_a_en, we_A      => data_sram_a_en,   addr_A => data_sram_a_idx, -- Port A
			di_A => data_sram_a_di, wr_mask_A => data_sram_a_mask, do_A   => open,
			en_B => data_sram_b_en, we_B      => data_sram_b_we,   addr_B => data_sram_b_idx, -- Port B
			di_B => data_sram_b_di, wr_mask_B => "0000",           do_B   => data_sram_b_do
		);

	w_fsm:  WRITE_FSM port map (dclk => DCLK, res_n => HRESETn,
			request => write_request, hit => hit, dram_busy => write_dram_busy, HCLK => HCLK_PHASE, -- The input variables to the state machine
			state   => write_current_state                                                    -- The state register
			);

	r_fsm : read_fsm port map(dclk => DCLK, res_n => HRESETn,
			request    => read_request, hit     => hit,          dram_busy => p1_cmd_full, -- The input variables to the state machine
			dram_empty => p1_rd_empty,  ws_zero => read_ws_zero, HCLK      => HCLK_PHASE,
			state => read_current_state -- The state register
		);
	--}}}

	--{{{ Common signals

	hit_count      <= hit_counter;
	miss_count     <= miss_counter;
	p1_cmd_clk     <= DCLK;
	p1_wr_clk      <= DCLK;
	p1_rd_clk      <= DCLK;
	HRESP          <= '0'; -- By design there are no errors introduced by this module :) TODO: Treat the Error bit from the DRAM controller
	tag_sram_a_idx <= (others => '0') when HRESETn = '0' else
					 "000" & HADDR_IDX; -- We could increase the size of the cache without changing the TAGSRAM...
	--{{{
	tag_sram_a_en  <= '0'  when (HSEL = '0' or HREADY = '0') else -- Don't read without a request
	                  '1'  when (read_current_state=idl_rdt or write_current_state=idl_rdt) else
	                  '0' ;

	--}}}
	--{{{
	hit            <= '1'  when ((tag_sram_a_do_tag = save0_haddr_tag) and tag_sram_a_do_valid = '1') else
	                  '0' ; -- TODO: Verfify if this is correct.
	--}}}
	--{{{
	HREADYOUT_sig  <= '1'              when (HRESETn = '0')                                                                          else
	                  '0'              when (mem_calib_done = '0' and HSEL = '1')                   else
	                  --'0'              when (mem_calib_done = '0' and (read_request = '1' or write_request = '1'))                   else
	                  --'0'              when ((read_request = '1' and read_busy = '1') or (write_request = '1' and write_busy = '1')) else
	                  '0'              when ((HWRITE='0' and read_busy = '1') or (HWRITE='1' and write_busy = '1')) else
	                  hit              when (read_current_state=cmp_dlv)                                                             else
	                  '0'              when (read_current_state=req0 or read_current_state=req1)                                     else
	                  not p1_rd_empty  when (read_current_state=rd0)                                                                 else
	                  '1'              when (read_current_state=rd1_keep)                                                            else
	                  '1'              when (write_current_state=cmp_sto)                                                            else
	                  '1'             ; -- Signal readiness on reset and all conditions where the cache is not not ready.
	HREADYOUT      <= HREADYOUT_sig;
	--}}}
	--{{{
	HRDATA         <= data_sram_b_do      after 2 ns when (read_current_state=cmp_dlv) else
	                  p1_rd_data          after 2 ns when (read_current_state=rd0)     else
	                  read_keep_dram_data after 2 ns when (read_current_state=rd1_keep) else
	                  (others => '-')     after 2 ns;
	--}}}
	--{{{
	p1_cmd_instr   <= DRAM_CMD_READ    when ((read_current_state=cmp_dlv or read_current_state=req0 or read_current_state=req1) and hit='0') else
	                  DRAM_CMD_WRITE   when (write_current_state=cmp_sto or write_current_state=wait_sto)                                    else
	                  (others => '-') ;
	--}}}
	--{{{
	p1_cmd_addr    <= SAVE0_HADDR(31 downto 2)                                       when (read_current_state=cmp_dlv and hit='0') else
	                  read_SAVE1_HADDR(31 downto 2)                                  when (read_current_state=req0)                else
	                  read_SAVE1_HADDR(31 downto 5)&"000"                            when (read_current_state=req1)                else
	                  SAVE0_HADDR(31 downto 2)                                       when (write_current_state=cmp_sto)            else
	                  write_SAVE1_HADDR(31 downto 2)                                 when (write_current_state=wait_sto)           else
	                  (others => '-');
	--}}}
	--{{{
	p1_cmd_bl      <= "000" & std_logic_vector(7 - unsigned(     SAVE0_HADDR( 4 downto 2)))  when (read_current_state=cmp_dlv and hit='0') else
	                  "000" & std_logic_vector(7 - unsigned(read_SAVE1_HADDR( 4 downto 2)))  when (read_current_state=req0)                else
	                  "000" & std_logic_vector(unsigned(read_SAVE1_HADDR( 4 downto 2)) - 1)  when (read_current_state=req1)                else
	                  (others => '0')                                                        when (write_current_state=cmp_sto)            else
	                  (others => '0')                                                        when (write_current_state=wait_sto)           else
	                  (others => '-')                                                       ;
	--}}}
	--{{{
	p1_cmd_en      <= '0'  when (p1_cmd_full = '1') else
	                  '1'  when (read_current_state=cmp_dlv and hit='0')          or
	                                      (read_current_state=req0)                         or
	                                      (read_current_state=req1)                         or
	                                      (write_current_state=cmp_sto  and p1_wr_full = '0') or
	                                      (write_current_state=wait_sto and p1_wr_full = '0') else
	                  '0' ;
	--}}}
	--{{{
	p1_wr_data     <= HWDATA              when (write_current_state=cmp_sto) else
	                  write_SAVE1_HWDATA  when (write_current_state=wait_sto) else
	                  (others => '-')    ;
	--}}}
	--{{{
	p1_wr_mask     <= write_mask(HADDR_BS, HSIZE, HRESETn) when (write_current_state=cmp_sto) else
	                  write_mask(write_SAVE1_HADDR_BS, write_SAVE1_HSIZE, HRESETn)  when (write_current_state=wait_sto) else
	                  (others => '-')  ;
	--}}}
	--{{{
	p1_wr_en       <= '1'  when ((write_current_state=cmp_sto or write_current_state=wait_sto) and p1_wr_full = '0') else
	                  '0' ;
	--}}}
	--{{{
	p1_rd_en       <= '0'  when (p1_rd_empty = '1') else
	                  '1'  when ((read_current_state=rd0) or (read_current_state=rd1_keep) or (read_current_state=rd1) or
	                                       (read_current_state=rd2) or (read_current_state=rd3) or (read_current_state=rd4) or
	                                       (read_current_state=rd5) or (read_current_state=rd6) or (read_current_state=rd7)) else
	                  '0' ;
	--}}}
	--{{{
	latch_bus : process(HCLK) -- Using HCLK is intended here!
	begin
		if(rising_edge(HCLK)) then
			if(HRESETn = '0') then
				SAVE0_HADDR  <= (others => '0');
				SAVE0_HSIZE  <= (others => '0');
				SAVE0_HWRITE <= '-';
				SAVE1_HWRITE <= '-';
			else
				if ( HSEL = '1' and HREADY = '1' ) then
					SAVE0_HADDR  <= HADDR;
					SAVE0_HSIZE  <= HSIZE;
				end if;
				if ( HREADY = '1' ) then
					SAVE0_HWRITE <= HWRITE;
					SAVE1_HWRITE <= SAVE0_HWRITE;
				end if;
			end if;
		end if;
	end process latch_bus;
	--}}}

	--{{{
	-- toggle flip-flop to route HCLK into the state-machines
	process
	begin
		wait until rising_edge(DCLK);
		HCLK_PHASE <= not HCLK_PHASE;
	end process;
	--}}}

	--}}}

	--{{{ Write FSM signals

	latch_write_reqest : process (HCLK)
	begin
		if(rising_edge(HCLK)) then
			write_request    <= HWRITE and HREADY and HSEL ;
		end if;
	end process;
	write_dram_busy  <= p1_cmd_full or p1_rd_empty ;
	--{{{
	write_busy       <= '0'  when (write_current_state=idl_rdt or write_current_state=cmp_sto) else
	                    '1' ;
	--}}}
	data_sram_a_en   <= '1'  when (write_current_state=cmp_sto) else '0' ;
	data_sram_a_mask <= write_mask(HADDR_BS, HSIZE, HRESETn)  when (write_current_state=cmp_sto) else (others => '-') ;
	data_sram_a_idx  <= "000" & SAVE0_HADDR_IDX  when (write_current_state=cmp_sto) else (others => '-') ;
	data_sram_a_di   <= HWDATA  when (write_current_state=cmp_sto) else (others => '-') ;
	--{{{
	write_propagate : process(DCLK)
	begin
		if(rising_edge(DCLK)) then
			if(HRESETn = '0') then
				write_SAVE1_HADDR  <= ( others => '0' );
				write_SAVE1_HSIZE  <= ( others => '0' );
				write_SAVE1_HWDATA <= ( others => '0' );
		elsif ( write_current_state = cmp_sto ) then
				write_SAVE1_HADDR  <= SAVE0_HADDR;
				write_SAVE1_HSIZE  <= SAVE0_HSIZE;
				write_SAVE1_HWDATA <= HWDATA;
			end if;
		end if;
	end process write_propagate;
	--}}}
	--}}}

	--{{{ Read FSM signals

	latch_read_reqest : process (HCLK)
	begin
		if(rising_edge(HCLK)) then
			read_request     <= HSEL and HREADY and not HWRITE ;
		end if;
	end process;
	--{{{
	read_ws_zero     <= '1'  when (read_current_state=cmp_dlv and SAVE0_HADDR_WS = "000") else
	                    '1'  when (read_current_state=req0 and read_SAVE1_HADDR_WS = "000") else
	                    '0' after 10 ns;
	--}}}
	--{{{
	read_busy        <= '0'  when (read_current_state=idl_rdt or read_current_state=cmp_dlv or read_current_state=req0 or read_current_state=req1 or read_current_state=rd0 or read_current_state=rd1_keep) else
	                    '1' ;
	--}}}
	--{{{
	tag_sram_b_en    <= '1'  when (read_current_state=rd1_keep) else
	                    '0' ;
	--}}}
	--{{{
	tag_sram_b_idx   <= "000" & read_SAVE1_HADDR_IDX       when (read_current_state=rd1_keep) else
	                    (others => '-')                    ;
	--}}}
	--{{{
	tag_sram_b_di    <= "111" & '1' & read_SAVE1_HADDR_TAG  when (read_current_state=rd1_keep) else
	                    (others => '-') after 1 ns;
	--}}}
	--{{{
	data_sram_b_en   <= '1'  when (read_current_state=cmp_dlv) or
	                              (p1_rd_empty = '0' and read_current_state=rd0) or
	                              (p1_rd_empty = '0' and read_current_state=rd1_keep) or
	                              (p1_rd_empty = '0' and read_current_state=rd1) or
	                              (p1_rd_empty = '0' and read_current_state=rd2) or
	                              (p1_rd_empty = '0' and read_current_state=rd3) or
	                              (p1_rd_empty = '0' and read_current_state=rd4) or
	                              (p1_rd_empty = '0' and read_current_state=rd5) or
	                              (p1_rd_empty = '0' and read_current_state=rd6) or
	                              (p1_rd_empty = '0' and read_current_state=rd7) else
	                    '0' ;
	--}}}
	--{{{
	data_sram_b_we   <= '1'  when (p1_rd_empty = '0' and read_current_state=rd0) or
	                              (p1_rd_empty = '0' and read_current_state=rd1_keep) or
	                              (p1_rd_empty = '0' and read_current_state=rd1) or
	                              (p1_rd_empty = '0' and read_current_state=rd2) or
	                              (p1_rd_empty = '0' and read_current_state=rd3) or
	                              (p1_rd_empty = '0' and read_current_state=rd4) or
	                              (p1_rd_empty = '0' and read_current_state=rd5) or
	                              (p1_rd_empty = '0' and read_current_state=rd6) or
	                              (p1_rd_empty = '0' and read_current_state=rd7) else
	                    '0' ;
	--}}}
	--{{{
	data_sram_b_idx  <= HADDR_IDX            & HADDR_WS                                           when (read_current_state=cmp_dlv)  else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+0)  when (read_current_state=rd0)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+1)  when (read_current_state=rd1_keep) else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+1)  when (read_current_state=rd1)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+2)  when (read_current_state=rd2)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+3)  when (read_current_state=rd3)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+4)  when (read_current_state=rd4)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+5)  when (read_current_state=rd5)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+6)  when (read_current_state=rd6)      else
	                    read_SAVE1_HADDR_IDX & std_logic_vector(unsigned(read_SAVE1_HADDR_WS)+7)  when (read_current_state=rd7)      else
	                    (others => '-') ;
	--}}}
	--{{{
	data_sram_b_di   <= p1_rd_data  when (read_current_state=rd0 or read_current_state=rd1_keep or read_current_state=rd1 or
	                                                read_current_state=rd2 or read_current_state=rd3      or read_current_state=rd4 or
	                                                read_current_state=rd5 or read_current_state=rd6      or read_current_state=rd7) else
	                     (others => '-') ;
	--}}}
	--{{{
	read_propagate : process(DCLK)
	begin
		if(rising_edge(DCLK)) then
			if(HRESETn = '0') then
				read_SAVE1_HADDR <= (others => '0');
				read_SAVE1_HSIZE <= (others => '0');
			else
				if ( read_current_state = cmp_dlv ) then
					read_SAVE1_HADDR <= SAVE0_HADDR;
					read_SAVE1_HSIZE <= SAVE0_HSIZE;
				else
					read_SAVE1_HADDR <= read_SAVE1_HADDR;
					read_SAVE1_HSIZE <= read_SAVE1_HSIZE;
				end if;

				if ( read_current_state = rd0) then
					read_keep_dram_data <= p1_rd_data;
				end if;
			end if;
		end if;
	end process read_propagate;
	--}}}
	--}}}

	--{{{ hit counters
	process(DCLK)
	begin
		if ( HRESETn = '0' ) then
			hit_counter  <= (others => '0');
			miss_counter <= (others => '0');
		elsif ( read_current_state=cmp_dlv or write_current_state=cmp_sto ) then
			if ( hit = '1' ) then
				hit_counter <= std_logic_vector(unsigned(hit_counter) + "1");
			else
				miss_counter <= std_logic_vector(unsigned(miss_counter) + "1");
			end if;
		end if;
	end process;
	--}}}

end cache;
--}}}
