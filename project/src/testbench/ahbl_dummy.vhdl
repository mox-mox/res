library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity AHBL_DUMMY is
	port(
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
	signal HADDR_sig : std_logic_vector(31 downto 0);

begin

	HRESETn <= '0', '1' after 30 ns;
	HSEL    <= '1' after 1 ns when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else '0' after 1 ns;
	HTRANS  <= "00" after 1 ns when HADDR_sig(31 downto 24) = (HADDR_sig(31 downto 24)'range => '0') else "10" after 1 ns;
	HADDR   <= HADDR_sig after 1 ns;
	HREADY  <= HREADYOUT after 2 ns;

--HWRITE
--HSIZE
--HWDATA

	drive_bus : process(HCLK)
		type bus_access_type is record
			read  : std_logic;                               -- 0: write, 1: read
			addr  : natural range 0 to (16 * 1024 * 1024);   -- Where to read or write
			size  : natural range 1 to 4;                    -- The number of byts to read or write
			data  : integer range -(2**31-1) to +(2**31-1);  -- For writes, the datum to be written, for reads the datum expected.
			delay : natural;                                 -- How long to wait before sending this request over the bus.
		end record;
		--  The patterns to apply.
		type bus_access_array is array (natural range <>) of bus_access_type;
		--constant patterns : bus_access_array :=
		--	(('0', 4, 16#00beeeef#, 127, 0), -- dummy line
		--	 ('1', 4, 16#00beeeef#, 127, 0),
		--	 ('0', 4, 16#00caffee#,   0, 0),
		--	 ('0', 4, 16#00beeeef#, 127, 0));
			variable index         : natural := 1;
			variable delay_counter : natural := 0;
	begin
		if(rising_edge(HCLK)) then
			if(delay_counter = 0) then

				HWRITE <= '0';
				HSIZE  <= "000";
				HWDATA <= (others => '0');

			end if;
		end if;

	end process;




end read_sequence;

