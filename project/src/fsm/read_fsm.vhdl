library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.read_fsm_pkg.all;

entity READ_FSM is
	port (
		DCLK               : in  std_logic;                      -- 2xHCLK
		RES_n             : in  std_logic;                      -- HRESETn

		-- The input signals to the state machine
		REQUEST           : in  std_logic;                      -- !HWRITE && HREADY && ( HSEL or HSEL & HPROT for non-unified cache )
		HIT               : in  std_logic;                      -- The cache hit or miss information
		DRAM_BUSY         : in  std_logic;                      -- pX_cmd_full
	    DRAM_EMPTY        : in  std_logic;                      -- pX_rd_empty
		WS_ZERO           : in  std_logic;                      -- Whether the requested word was the first in cache line
		HCLK              : in  std_logic;

		-- The state register
		state             : out read_fsm_state_type
         );
end READ_FSM;

architecture syn of READ_FSM is
	signal current_state, next_state : read_fsm_state_type := idl_rdt;
	constant next_state_delay        : time                := 2 ns;
begin
	state <= current_state;
	--{{{
	calculate_next_state: process(current_state, REQUEST, HIT, DRAM_BUSY, DRAM_EMPTY, WS_ZERO, HCLK)
	begin
		next_state        <= current_state        after next_state_delay; -- default assignement

		case current_state is
			when idl_rdt =>
				if( REQUEST = '1' and HCLK = '1' ) then
					next_state        <= cmp_dlv after next_state_delay;
				end if;

			when cmp_dlv =>
				if ( HIT = '1' ) then
					--if ( REQUEST = '0' ) then
						next_state <= idl_rdt after next_state_delay;
					--else -- REQUEST = '1'
					--	next_state <= cmp_dlv after next_state_delay;
					--end if;
				else -- HIT = '0'
					if ( DRAM_BUSY = '1' ) then
						next_state <= req0 after next_state_delay;
					else -- DRAM_BUSY = '0'
						if ( WS_ZERO = '0' ) then
							next_state <= req1 after next_state_delay;
						else -- WS_ZERO = '1'
							next_state <= rd0 after next_state_delay;
						end if;
					end if;
				end if;

			when req0 =>
				if ( DRAM_BUSY = '1' ) then
					next_state <= req0 after next_state_delay;
				else -- DRAM_BUSY = '0'
					if ( WS_ZERO = '0' ) then
						next_state <= req0 after next_state_delay;
					else -- WS_ZERO = '1'
						next_state <= rd0 after next_state_delay;
					end if;
				end if;

			when req1 =>
				if( DRAM_BUSY = '1' ) then
					next_state <= req1 after next_state_delay;
				else
					next_state <= rd0 after next_state_delay;
				end if;

			--{{{ rdN -> rd(N+1) if !DRAM_EMPTY

			when rd0 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd0 after next_state_delay;
				else
					next_state       <= rd1_keep after next_state_delay;
				end if;

			when rd1_keep =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd1 after next_state_delay;
				else
					next_state       <= rd2 after next_state_delay;
				end if;

			when rd1 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd1 after next_state_delay;
				else
					next_state       <= rd2 after next_state_delay;
				end if;

			when rd2 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd2 after next_state_delay;
				else
					next_state       <= rd3 after next_state_delay;
				end if;

			when rd3 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd3 after next_state_delay;
				else
					next_state       <= rd4 after next_state_delay;
				end if;

			when rd4 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd4 after next_state_delay;
				else
					next_state       <= rd5 after next_state_delay;
				end if;

			when rd5 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd5 after next_state_delay;
				else
					next_state       <= rd6 after next_state_delay;
				end if;

			when rd6 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd6 after next_state_delay;
				else
					next_state       <= rd7 after next_state_delay;
				end if;
			--}}}
			when rd7 =>
				if( DRAM_EMPTY = '1' ) then
					next_state <= rd7 after next_state_delay;
				else
					if ( HCLK = '1' ) then -- First phase of HCLK
	 					next_state <= sync after next_state_delay;
					else -- HCLK = '0' -- Second phase of HCLK
						if( REQUEST = '0' ) then
							next_state <= idl_rdt after next_state_delay;
						else
							next_state <= cmp_dlv after next_state_delay;
						end if;
					end if;
				end if;

			when sync =>
				if ( REQUEST = '0' ) then
					next_state <= idl_rdt after next_state_delay;
				else -- REQUEST = '1'
					next_state <= cmp_dlv after next_state_delay;
				end if;


			when others =>
				-- shouldn't happen
				assert true report "Read FSM has encountered an invalid state" severity failure;
				next_state <= idl_rdt after next_state_delay;

		end case;

	end process;
	--}}}

	--{{{
	adopt_next_state: process(DCLK)
	begin
		if(rising_edge(DCLK)) then
			if( RES_n = '0' ) then
				current_state        <= idl_rdt;
			else
				current_state        <= next_state;
			end if;
		end if;
	end process;
	--}}}
end syn;
