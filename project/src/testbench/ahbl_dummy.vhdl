library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity AHBL_DUMMY is
	port(
		ENDSIM            : out    boolean;                       -- Stop the simulation when patterns run out
-- Global signals ---------------------------------------------------------------------------------------------------------------
		HCLK              : in     std_logic;                     -- Bus clock
		HRESETn           : out    std_logic;                     -- Reset
-- AHB Slave inputs ---------------------------------------------------------------------------------------------------
		HSEL              : out    std_logic;                     -- Slave select
		HADDR             : out    std_logic_vector(31 downto 0); -- Slave address
		HWRITE            : out    std_logic;                     -- Diretion: 0: Master read, 1: Master write
		HSIZE             : out    std_logic_vector( 2 downto 0); -- Transfer Word size: 000: Byte, 001: Halfword, 010: Word, others: undefined
		-- HBURST         : out    std_logic_vector( 2 downto 0)  -- NOT IMPLEMENTED
		-- HPROT          : out    std_logic_vector( 3 downto 0)  -- NOT IMPLEMENTED, Could be used to create a seperated cache for instructions and data.
		HTRANS            : out    std_logic_vector( 1 downto 0); -- Transaction status: 00: IDLE, 01: BUSY, 10: NON-SEQUENTIAL, 11: SEQUENTIAL
		-- HMASTLOCK      : out    std_logic;                     -- NOT IMPLEMENTED
		HREADY            : out    std_logic;                     -- Master's ready signal: 0: Busy, 1: Ready for next transaction
		HWDATA            : out    std_logic_vector(31 downto 0); -- Incomming Data from master
-- AHB Slave outputs --------------------------------------------------------------------------------------------------
		HREADYOUT         : in    std_logic;                     -- Slave's ready signal: 0: Busy, 1: Ready
		HRESP             : in    std_logic;                     -- Transfer response: 0: Okay, 1: Error. Needs one additional wait state with HREADYout low.
		HRDATA            : in    std_logic_vector(31 downto 0)  -- Outgoing Data to master }}}
		);
end AHBL_DUMMY;






--{{{
architecture read_sequence of AHBL_DUMMY is

	constant HTRANS_idle     : std_logic_vector( 1 downto 0) := "00";
	constant HTRANS_nonseq   : std_logic_vector( 1 downto 0) := "10";
	constant zeros           : std_logic_vector( 7 downto 0) := "00000000";
	constant HSIZE1          : std_logic_vector( 2 downto 0) := "000";
	constant HSIZE2          : std_logic_vector( 2 downto 0) := "001";
	constant HSIZE4          : std_logic_vector( 2 downto 0) := "010";



	--constant HTRANS_busy     : std_logic_vector( 1 downto 0)  := "01";
	--constant HTRANS_seq      : std_logic_vector( 1 downto 0)  := "11";

	--{{{ Logic wiring

	constant wire_delay      : time := 17 ns;
	signal   reset_sig       : boolean                        := true;
	signal   HSEL_sig        : std_logic                      := '0';
	signal   HADDR_sig       : std_logic_vector(31 downto 0)  := (others => '-');
	signal   HWRITE_sig      : std_logic                      := '-';
	signal   HSIZE_sig       : std_logic_vector( 2 downto 0)  := (others => '-');
	signal   HTRANS_sig      : std_logic_vector( 1 downto 0)  := (others => '-');
	signal   HWDATA_sig      : std_logic_vector(31 downto 0)  := (others => '-');
	--}}}

	--{{{ addr and data conversion functions

	--{{{
	function hex_to_unsigned_32bit(data: string) return unsigned is
	variable sum: unsigned(31 downto 0) := (others => '0');
	variable tmp: integer;
	begin
		for string_pos in data'range loop
			sum := resize(sum * 16, 32);
			case data(string_pos) is
				when '0'     => tmp := 0;
				when '1'     => tmp := 1;
				when '2'     => tmp := 2;
				when '3'     => tmp := 3;
				when '4'     => tmp := 4;
				when '5'     => tmp := 5;
				when '6'     => tmp := 6;
				when '7'     => tmp := 7;
				when '8'     => tmp := 8;
				when '9'     => tmp := 9;
				when 'a'|'A' => tmp := 10;
				when 'b'|'B' => tmp := 11;
				when 'c'|'C' => tmp := 12;
				when 'd'|'D' => tmp := 13;
				when 'e'|'E' => tmp := 14;
				when 'f'|'F' => tmp := 15;
				when others => report("hex_to_unsigned_32bit: Illegal digit: " & data(string_pos)) severity failure;
			end case;
			sum := sum + tmp;
		end loop;
		return sum;
	end hex_to_unsigned_32bit;
	--}}}

	function to_addr (addr : string) return std_logic_vector is
	begin
		return std_logic_vector(hex_to_unsigned_32bit(addr));
	end to_addr;

	function to_data (data : string) return std_logic_vector is
	begin
		return std_logic_vector(hex_to_unsigned_32bit(data));
	end to_data;
	--}}}

	--{{{ Sequence

	type read_write is (read, write);

	type ahbl_command_type is record
		delay      : natural;
		rw         : read_write;
		addr       : std_logic_vector(31 downto 0);
		data       : std_logic_vector(31 downto 0);           -- For writes, the datum to be written, for reads the datum expected.
		size       : positive range 1 to 4;                   -- The number of byts to read or write
	end record;

	type bus_access_array is array (natural range <>) of ahbl_command_type;
	constant bus_sequence : bus_access_array := (
		(0, write, to_addr("ffffffff"), to_data("00000000"), 1),  -- dummy line
		(0, read,  to_addr("00000004"), to_data("00000000"), 4),
		(0, read,  to_addr("00000004"), to_data("00000000"), 1),
		(0, read,  to_addr("00000005"), to_data("00000000"), 1),
		(0, read,  to_addr("ffffffff"), to_data("00000000"), 1)); -- dummy line
	signal delay_counter : natural := 0;
	--}}}



	type ahbl_dummy_state is (idle, write, write_stall, read, read_stall);
	signal current_state,       next_state        : ahbl_dummy_state := idle;
	signal current_delay_count, next_delay_count  : natural          := 0;
	signal current_index,       next_index        : natural          := 0;


begin
	reset_sig  <= true, false after 12 ns;
	HRESETn    <= '0' when reset_sig else '1';

	HSEL       <= '0' when reset_sig else
				  '1' after wire_delay when bus_sequence(current_index+1).addr(31 downto 24) = zeros and current_delay_count=0 else -- TODO: not sure if good
	              '0' after wire_delay;
	HADDR       <= (others => '-') when reset_sig else HADDR_sig after wire_delay;
	HWRITE      <= '-' when reset_sig else
				   '1' after wire_delay when bus_sequence(current_index+1).rw=write and current_delay_count=0 else
				   '0' after wire_delay when bus_sequence(current_index+1).rw=read  and current_delay_count=0 else
				   '-' after wire_delay;

	HSIZE       <= (others => '-') when reset_sig else
				   HSIZE1 after wire_delay when bus_sequence(current_index+1).size=1 and current_delay_count=0 else
				   HSIZE2 after wire_delay when bus_sequence(current_index+1).size=2 and current_delay_count=0 else
				   HSIZE4 after wire_delay when bus_sequence(current_index+1).size=4 and current_delay_count=0 else
				   (others => '-') after wire_delay;

	HTRANS      <= HTRANS_idle when reset_sig else
				   HTRANS_nonseq after wire_delay when current_delay_count=0 else
				   HTRANS_idle   after wire_delay;

	HREADY     <= '0' when reset_sig else HREADYOUT after wire_delay;
	HWDATA     <= (others => '-') when reset_sig else
				  bus_sequence(current_index).data after wire_delay when current_delay_count=0 and (current_state=read or current_state=read_stall) else
				  (others => '-') after wire_delay;

	--{{{
	calculate_next_state : process (current_delay_count) --TODO
	begin
		next_state        <= current_state        after wire_delay; -- default assignement
		next_delay_count  <= current_delay_count  after wire_delay;
		next_index        <= current_index        after wire_delay;
		case current_state is
			when idle =>
				if current_delay_count = 0 then
					next_index <= current_index + 1 after wire_delay;
					if bus_sequence(current_index).rw=read then
						next_state <= read after wire_delay;
					else
						next_state <= write after wire_delay;
					end if;
				else
					next_delay_count <= current_delay_count - 1;
				end if;
			when write =>
				if hreadyout = '0' then
					next_state <= write_stall after wire_delay;
				else
					if bus_sequence(current_index + 1).delay = 0 then
						next_index <= current_index + 1 after wire_delay;
						if bus_sequence(current_index).rw=read then
							next_state <= read after wire_delay;
						else
							next_state <= write after wire_delay;
						end if;
					else
						next_state <= idle;
						next_delay_count <= bus_sequence(current_index + 1).delay after wire_delay;
					end if;
				end if;
			when write_stall =>
				if hreadyout = '0' then
					next_state <= write_stall after wire_delay;
				else
					if bus_sequence(current_index + 1).delay = 0 then
						next_index <= current_index + 1 after wire_delay;
						if bus_sequence(current_index).rw=read then
							next_state <= read after wire_delay;
						else
							next_state <= write after wire_delay;
						end if;
					else
						next_state <= idle;
						next_delay_count <= bus_sequence(current_index + 1).delay after wire_delay;
					end if;
				end if;

			when read =>
				if hreadyout = '0' then
					next_state <= read_stall after wire_delay;
				else
					if bus_sequence(current_index + 1).delay = 0 then
						next_index <= current_index + 1 after wire_delay;
						if bus_sequence(current_index).rw=read then
							next_state <= read after wire_delay;
						else
							next_state <= write after wire_delay;
						end if;
					else
						next_state <= idle;
						next_delay_count <= bus_sequence(current_index + 1).delay after wire_delay;
					end if;
				end if;
			when read_stall =>
				if hreadyout = '0' then
					next_state <= read_stall after wire_delay;
				else
					if bus_sequence(current_index + 1).delay = 0 then
						next_index <= current_index + 1 after wire_delay;
						if bus_sequence(current_index).rw=read then
							next_state <= read after wire_delay;
						else
							next_state <= write after wire_delay;
						end if;
					else
						next_state <= idle;
						next_delay_count <= bus_sequence(current_index + 1).delay after wire_delay;
					end if;
				end if;
		end case;
	end process;
	--}}}

	--{{{
	adopt_next_state : process (HCLK)
	begin
		if(rising_edge(HCLK)) then
			if  reset_sig then
				current_state         <= idle;
				current_delay_count   <= 0;
				current_index         <= 0;
			else
				current_state        <= next_state;
				current_delay_count  <= next_delay_count;
				current_index         <= next_index;
			end if;
		end if;
	end process;
	--}}}





end read_sequence;
--}}}














--
----{{{
--architecture read_sequence of AHBL_DUMMY is
--	signal reset : boolean                               := true;
--
--	signal HADDR_sig : std_logic_vector(31 downto 0)     := (others => '0');
--	signal HWDATA_sig : std_logic_vector(31 downto 0)    := (others => '0');
--	signal HWRITE_sig : std_logic                        := '0';
--	signal HSIZE_sig : std_logic_vector( 2 downto 0)     := (others => '0');
--	signal HREADY_sig : std_logic                        := '0';
--	signal index         : natural                       := 1;
--	constant delay : time := 17 ns;
--
--begin
--
--	reset       <= true, false after 12 ns;
--	HRESETn     <= '0' when reset else '1';
--	HSEL        <= '0' when reset else '1' after delay when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else '0' after delay;
--	HTRANS      <= (others => 'X') when reset else "00" after delay when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else "10" after delay;
--	HADDR       <= (others => 'X') when reset else HADDR_sig  after delay;
--	HREADY      <= HREADY_sig after delay;
--
--	HWDATA      <= (others => 'X') when reset else HWDATA_sig after delay;
--	HWRITE      <= 'X'             when reset else HWRITE_sig after delay;
--	HSIZE       <= (others => 'X') when reset else HSIZE_sig  after delay;
--
--	set_hready : process(HCLK)
--	begin
--		if(rising_edge(HCLK)) then
--			if reset then
--				HREADY_sig <= '0';
--			else
--				HREADY_sig <= HREADYOUT;
--			end if;
--		end if;
--	end process;
--
--
--	--{{{
--	drive_bus : process(HCLK)
--		type bus_access_type is record
--			write : boolean;                                 -- 1: write, 0: read
--			addr  : unsigned(31 downto 0);                   -- Where to read or write
--			size  : positive range 1 to 4;                    -- The number of byts to read or write
--			data  : unsigned(31 downto 0);                   -- For writes, the datum to be written, for reads the datum expected.
--			delay : natural;                                 -- How long to wait before sending this request over the bus.
--		end record;
--		--  The patterns to apply.
--		type bus_access_array is array (natural range <>) of bus_access_type;
--		constant patterns : bus_access_array := ( --{{{
--			(false, to_unsigned(16#7fffffff#, 32), 1, to_unsigned(16#00000000#, 32), 0), -- dummy line
--			(false, to_unsigned(16#00000004#, 32), 4, to_unsigned(16#00000000#, 32), 0),
--			(false, to_unsigned(16#00000004#, 32), 1, to_unsigned(16#00000000#, 32), 0),
--			(false, to_unsigned(16#00000005#, 32), 1, to_unsigned(16#00000000#, 32), 0),
--			(false, to_unsigned(16#7f111111#, 32), 1, to_unsigned(16#00000000#, 32), 0),
--			(false, to_unsigned(16#00222222#, 32), 1, to_unsigned(16#00000000#, 32), 0),
--			(false, to_unsigned(16#00333333#, 32), 1, to_unsigned(16#00000000#, 32), 0),
--			(true,  to_unsigned(16#7fffffff#, 32), 1, to_unsigned(16#00000000#, 32), 0)); --}}}
--		variable delay_counter : natural := 0;
--	begin
--		if(rising_edge(HCLK)) then
--			if reset then
--				HADDR_sig  <= (others => '0');
--				HWDATA_sig <= (others => '0');
--				HWRITE_sig <= '0';
--				HSIZE_sig  <= (others => '0');
--				delay_counter := patterns(0).delay;
--			else
--				if delay_counter = 0 then
--					if(HREADYOUT = '1') then
--						HADDR_sig <= std_logic_vector(patterns(index).addr);
--						delay_counter := patterns(index).delay;
--						if(patterns(index).write = true) then
--							HWDATA_sig <= std_logic_vector(patterns(index).data);
--							HWRITE_sig <= '1';
--						else
--							HWDATA_sig <= (others => '-');
--							HWRITE_sig <= '0';
--						end if;
--					else
--						HADDR_sig <= std_logic_vector(patterns(index-1).addr);
--						if(patterns(index).write = true) then
--							HWDATA_sig <= std_logic_vector(patterns(index-1).data);
--							HWRITE_sig <= '1';
--						else
--							HWDATA_sig <= (others => '-');
--							HWRITE_sig <= '0';
--						end if;
--					end if;
--					--if not patterns(index-1).write then
--					--	assert patterns(index).data = unsigned(HRDATA) report "Mismatch between expected and actual read-back value" severity failure;
--					--end if;
--					case patterns(index).size is
--						when 1 =>
--							HSIZE_sig <= "000";
--						when 2 =>
--							HSIZE_sig <= "001";
--						when 3 =>
--								--HSIZE_sig <= "000";
--							assert true report "Trying to send 3 bytes of data over the bus (should be either 1, 2 or 4" severity failure;
--						when 4 =>
--							HSIZE_sig <= "010";
--						when others => -- shouldn't happen
--							assert true report "Invalid HSIZE" severity failure;
--					end case;
--
--						--report "patterns'length" &  integer'image(integer(patterns'length));
--					if(HREADYOUT = '1') then
--						if patterns'length = index+1 then
--							index <= patterns'length - 1;
--							ENDSIM <= true;
--						else
--							index <= index+1;
--							ENDSIM <= false;
--						end if;
--					end if;
--				else
--					delay_counter := delay_counter - 1;
--				end if;
--			end if;
--		end if;
--	end process;
--	--}}}
--end read_sequence;
----}}}
--
