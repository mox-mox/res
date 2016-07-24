library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity AHBL_DUMMY is
	port(
		--ENDSIM            : out    boolean;                       -- Stop the simulation when patterns run out
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

	type HTRANS_type is (idle, busy, nonseq, seq);
	constant HTRANS_idle     : std_logic_vector( 1 downto 0) := "00";
	constant HTRANS_busy     : std_logic_vector( 1 downto 0)  := "01";
	constant HTRANS_nonseq   : std_logic_vector( 1 downto 0) := "10";
	constant HTRANS_seq      : std_logic_vector( 1 downto 0)  := "11";
	constant zeros           : std_logic_vector( 7 downto 0) := "00000000";
	constant HSIZE1          : std_logic_vector( 2 downto 0) := "000";
	constant HSIZE2          : std_logic_vector( 2 downto 0) := "001";
	constant HSIZE4          : std_logic_vector( 2 downto 0) := "010";


	--{{{ Logic wiring

	--constant wire_delay      : time := 17 ns;
	constant wire_delay      : time := 3 ns;
	signal   reset_sig       : boolean                        := true;
	signal   HTRANS_sig      : htrans_type                    := idle;
	--}}}

	--{{{ addr and data conversion functions

	--{{{
	function to_hstring (value : STD_LOGIC_VECTOR) return STRING is
		-- Taken from http://www.edaboard.com/thread148826.html#post638955
	constant ne     : INTEGER := (value'length+3)/4;
	variable pad    : STD_LOGIC_VECTOR(0 to (ne*4 - value'length) - 1);
	variable ivalue : STD_LOGIC_VECTOR(0 to ne*4 - 1);
	variable result : STRING(1 to ne);
	variable quad   : STD_LOGIC_VECTOR(0 to 3);
	begin
		if value'length < 1 then
			return "undef";
		else
			if value (value'left) = 'Z' then
				pad := (others => 'Z');
			else
				pad := (others => '0');
			end if;
			ivalue := pad & value;
			for i in 0 to ne-1 loop
				quad := To_X01Z(ivalue(4*i to 4*i+3));
				case quad is
					when x"0"   => result(i+1) := '0';
					when x"1"   => result(i+1) := '1';
					when x"2"   => result(i+1) := '2';
					when x"3"   => result(i+1) := '3';
					when x"4"   => result(i+1) := '4';
					when x"5"   => result(i+1) := '5';
					when x"6"   => result(i+1) := '6';
					when x"7"   => result(i+1) := '7';
					when x"8"   => result(i+1) := '8';
					when x"9"   => result(i+1) := '9';
					when x"A"   => result(i+1) := 'A';
					when x"B"   => result(i+1) := 'B';
					when x"C"   => result(i+1) := 'C';
					when x"D"   => result(i+1) := 'D';
					when x"E"   => result(i+1) := 'E';
					when x"F"   => result(i+1) := 'F';
					when "ZZZZ" => result(i+1) := 'Z';
					when others => result(i+1) := 'X';
				end case;
			end loop;
			return result;
		end if;
	end function to_hstring;



	--}}}

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
		( 0, read,  to_addr("ffffffff"), to_data("00000000"), 4),  -- dummy line
		( 0, read,  to_addr("00000004"), to_data("00000000"), 4),
		( 0, read,  to_addr("00000008"), to_data("00000000"), 4),
		( 0, read,  to_addr("0000000c"), to_data("00000000"), 4),
		( 0, read,  to_addr("ffffffff"), to_data("00000000"), 1),
		(99, read,  to_addr("ffffffff"), to_data("00000000"), 1),  -- dummy line
		(99, read,  to_addr("ffffffff"), to_data("00000000"), 1)); -- dummy line
	--}}}

	type ahbl_dummy_state is (reset, idle, write, write_stall, read, read_stall, end_state);
	signal current_state,       next_state        : ahbl_dummy_state := idle;
	signal current_delay_count, next_delay_count  : natural          := 0;
	signal current_index,       next_index        : natural          := 0;

begin

	--{{{ Output signals

	--ENDSIM <= false, true after 80 ns;

	reset_sig  <= true, false after 16 ns;
	HRESETn    <= '0' when reset_sig else
	              '0' when current_state=end_state else
	              '1';

	HSEL       <= '0' when reset_sig or current_state=reset else
				  '1' after wire_delay when bus_sequence(current_index+1).addr(31 downto 24) = zeros and current_delay_count=0 else -- TODO: not sure if good
	              '0' after wire_delay;

	HADDR       <= (others => '-') when reset_sig or current_state=reset else
				   bus_sequence(current_index+1).addr after wire_delay when current_delay_count=0 else
	               x"0000BEEF" after wire_delay; -- TODO

	HWRITE      <= '-' when reset_sig or current_state=reset else
				   '1' after wire_delay when bus_sequence(current_index+1).rw=write and current_delay_count=0 else
				   '0' after wire_delay when bus_sequence(current_index+1).rw=read  and current_delay_count=0 else
				   '-' after wire_delay;

	HSIZE       <= (others => '-') when reset_sig or current_state=reset else
				   HSIZE1 after wire_delay when bus_sequence(current_index+1).size=1 and current_delay_count=0 else
				   HSIZE2 after wire_delay when bus_sequence(current_index+1).size=2 and current_delay_count=0 else
				   HSIZE4 after wire_delay when bus_sequence(current_index+1).size=4 and current_delay_count=0 else
				   (others => '-') after wire_delay;

	HTRANS_sig  <= idle when reset_sig or current_state=reset else
                   nonseq after wire_delay when current_delay_count=0 else
                   idle   after wire_delay;


	HTRANS      <= HTRANS_idle   when HTRANS_sig=idle else
				   HTRANS_busy   when HTRANS_sig=busy else
				   HTRANS_nonseq when HTRANS_sig=nonseq else
				   HTRANS_seq    when HTRANS_sig=seq;

	HREADY     <= '0' when reset_sig or current_state=reset else HREADYOUT after 1 ns;

	HWDATA     <= (others => '-') when reset_sig or current_state=reset else
				  bus_sequence(current_index).data after wire_delay when current_delay_count=0 and (current_state=read or current_state=read_stall) else
				  (others => '-') after wire_delay;
	--}}}

	--{{{
	calculate_next_state : process (current_delay_count, current_index, hreadyout) --TODO
	begin
		next_state        <= current_state        after wire_delay; -- default assignement
		next_delay_count  <= current_delay_count  after wire_delay;
		next_index        <= current_index        after wire_delay;
		case current_state is
			when reset =>
				next_state <= idle;
			when idle =>
				if current_delay_count = 0 then
					if current_index = bus_sequence'length-2 then
						next_state <= end_state;
					else
						next_index <= current_index + 1 after wire_delay;
						if bus_sequence(current_index).rw=read then
							next_state <= read after wire_delay;
						else
							next_state <= write after wire_delay;
						end if;
					end if;
				else
						next_delay_count <= current_delay_count - 1;
				end if;
			when write =>
				if hreadyout = '0' then
					next_state <= write_stall after wire_delay;
				else
					if current_index = bus_sequence'length-2 then
						next_state <= end_state;
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
				end if;
			when write_stall =>
				if hreadyout = '0' then
					next_state <= write_stall after wire_delay;
				else
					if current_index = bus_sequence'length-2 then
						next_state <= end_state;
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
				end if;

			when read =>
				if hreadyout = '0' then
					next_state <= read_stall after wire_delay;
				else
					--assert hrdata = bus_sequence(current_index).data report ("Cache read error. Expected " & integer'image(to_integer(unsigned(bus_sequence(current_index).data))) & ", but got " & integer'image(to_integer(unsigned(hrdata))) & ".") severity warning;
					if current_index = bus_sequence'length-2 then
						next_state <= end_state;
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
				end if;
			when read_stall =>
				if hreadyout = '0' then
					next_state <= read_stall after wire_delay;
				else
					--assert hrdata = bus_sequence(current_index).data report ("Cache read error. Expected " & integer'image(to_integer(unsigned(bus_sequence(current_index).data))) & ", but got " & integer'image(to_integer(unsigned(hrdata))) & ".") severity warning;
					if current_index = bus_sequence'length-2 then
						next_state <= end_state;
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
				end if;
			when end_state =>
				next_state <= current_state;
			end case;
	end process;
	--}}}

	--{{{
	adopt_next_state : process (HCLK)
	begin
		if(rising_edge(HCLK)) then
			if  reset_sig then
				current_state         <= reset;
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

	--{{{
	check_read_data : process (HCLK)
	begin
		if(rising_edge(HCLK)) then
			if (current_state=read or current_state=read) and hreadyout='1' then
				--assert hrdata = bus_sequence(current_index).data report ("Cache read error. Expected " & to_hstring(bus_sequence(current_index).data) & ", but got " & to_hstring(hrdata) & ".") severity warning;
				if hrdata = bus_sequence(current_index).data then
					report ("Cache read worked. Read " & to_hstring(hrdata) & " correctly.");
				else
					report ("Cache read error. Expected " & to_hstring(bus_sequence(current_index).data) & ", but got " & to_hstring(hrdata) & ".");
				end if;
			end if;
		end if;
	end process;
	--}}}

end read_sequence;
--}}}
