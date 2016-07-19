library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity AHBL_DUMMY is
	port(
-- Global signals ---------------------------------------------------------------------------------------------------------------
		HCLK              : out    std_logic;                     -- Bus clock
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

begin






end read_sequence;

