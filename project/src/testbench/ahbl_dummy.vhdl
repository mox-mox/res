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

architecture read_sequence of AHBL_DUMMY is
	signal HADDR_sig : std_logic_vector(31 downto 0)     := (others => '0');
	signal HWDATA_sig : std_logic_vector(31 downto 0)    := (others => '0');
	signal HWRITE_sig : std_logic                        := '0';
	signal HSIZE_sig : std_logic_vector( 2 downto 0)     := (others => '0');
	signal HREADY_sig : std_logic                        := '0';
	signal reset : boolean                               := true;
	signal index         : natural                       := 1;
	constant delay : time := 17 ns;

begin

	reset       <= true, false after 12 ns;
	HRESETn     <= '0' when reset else '1';
	HSEL        <= '0' when reset else '1' after delay when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else '0' after delay;
	HTRANS      <= (others => 'X') when reset else "00" after delay when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else "10" after delay;
	HADDR       <= (others => 'X') when reset else HADDR_sig  after delay;
	HREADY      <= HREADY_sig after delay;

	HWDATA      <= (others => 'X') when reset else HWDATA_sig after delay;
	HWRITE      <= 'X'             when reset else HWRITE_sig after delay;
	HSIZE       <= (others => 'X') when reset else HSIZE_sig  after delay;

	set_hready : process(HCLK)
	begin
		if(rising_edge(HCLK)) then
			if reset then
				HREADY_sig <= '0';
			else
				HREADY_sig <= HREADYOUT;
			end if;
		end if;
	end process;


	--{{{
	drive_bus : process(HCLK)
		type bus_access_type is record
			write : boolean;                                 -- 1: write, 0: read
			addr  : unsigned(31 downto 0);                   -- Where to read or write
			size  : positive range 1 to 4;                    -- The number of byts to read or write
			data  : unsigned(31 downto 0);                   -- For writes, the datum to be written, for reads the datum expected.
			delay : natural;                                 -- How long to wait before sending this request over the bus.
		end record;
		--  The patterns to apply.
		type bus_access_array is array (natural range <>) of bus_access_type;
		constant patterns : bus_access_array := ( --{{{
			(false, to_unsigned(16#7fffffff#, 32), 4, to_unsigned(16#00000000#, 32), 0), -- dummy line
			(false, to_unsigned(16#00aaaaaa#, 32), 4, to_unsigned(16#00000000#, 32), 0),
			(false, to_unsigned(16#00bbbbbb#, 32), 4, to_unsigned(16#00000000#, 32), 0),
			(false, to_unsigned(16#00cccccc#, 32), 4, to_unsigned(16#00000000#, 32), 0),
			(false, to_unsigned(16#00dddddd#, 32), 4, to_unsigned(16#00000000#, 32), 0),
			(true,  to_unsigned(16#7fffffff#, 32), 4, to_unsigned(16#00000000#, 32), 0)); --}}}
		variable delay_counter : natural := 0;
	begin
		if(rising_edge(HCLK)) then
			if reset then
				HADDR_sig  <= (others => '0');
				HWDATA_sig <= (others => '0');
				HWRITE_sig <= '0';
				HSIZE_sig  <= (others => '0');
				delay_counter := patterns(0).delay;
			else
				if delay_counter = 0 then
					if(HREADYOUT = '1') then
						HADDR_sig <= std_logic_vector(patterns(index).addr);
						delay_counter := patterns(index).delay;
						if(patterns(index).write = true) then
							HWDATA_sig <= std_logic_vector(patterns(index).data);
							HWRITE_sig <= '1';
						else
							HWDATA_sig <= (others => '-');
							HWRITE_sig <= '0';
						end if;
					else
						HADDR_sig <= std_logic_vector(patterns(index-1).addr);
						if(patterns(index).write = true) then
							HWDATA_sig <= std_logic_vector(patterns(index-1).data);
							HWRITE_sig <= '1';
						else
							HWDATA_sig <= (others => '-');
							HWRITE_sig <= '0';
						end if;
					end if;
					if not patterns(index-1).write then
						assert patterns(index).data = unsigned(HRDATA) report "Mismatch between expected and actual read-back value" severity failure;
					end if;
					case patterns(index).size is
						when 1 =>
							HSIZE_sig <= "000";
						when 2 =>
							HSIZE_sig <= "001";
						when 3 =>
								--HSIZE_sig <= "000";
							assert true report "Trying to send 3 bytes of data over the bus (should be either 1, 2 or 4" severity failure;
						when 4 =>
							HSIZE_sig <= "010";
						when others => -- shouldn't happen
							assert true report "Invalid HSIZE" severity failure;
					end case;

						--report "patterns'length" &  integer'image(integer(patterns'length));
					if(HREADYOUT = '1') then
						if patterns'length = index+1 then
							index <= patterns'length - 1;
							ENDSIM <= true;
						else
							index <= index+1;
							ENDSIM <= false;
						end if;
					end if;
				else
					delay_counter := delay_counter - 1;
				end if;
			end if;
		end if;
	end process;
	--}}}




end read_sequence;

