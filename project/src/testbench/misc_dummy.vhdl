library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity MISC_DUMMY is
	port(
-- Miscellaneous non-bus signals
		HCLK              : in    std_logic;                      -- Double speed internal clock used to speed up the internal logic.
		mem_calib_done    : out   std_logic;                      -- Clock used to speed up the internal logic. MUST BE SYNCHRONISED WITH HCLK!!
		HIT_COUNT         : in    std_logic_vector(31 downto 0);  -- The number of accesses that resulted in a cache hit since the last reset.
		MISS_COUNT        : in    std_logic_vector(31 downto 0);  -- The number of accesses that resulted in a cache miss since the last reset.
		INVALIDATE_LOW    : inout std_logic_vector(31 downto 0);  -- Writing this and INVALIDATE_HIGH to a value except 0xffffffff marks all lines in that...
		INVALIDATE_HIGH   : inout std_logic_vector(31 downto 0)   -- ...range as invalid. !!! SUCCESSIVE WRITES MUST WAIT UNTIL THE CACHE CONTROLLER HAS...
	);                                                            -- ...WRITTEN BOTH REGISTERS TO 0xffffffff AGAIN !!! }}} TODO: Implement this.
end MISC_DUMMY;

architecture passive of MISC_DUMMY is
begin
	mem_calib_done    <= '0', '1' after 3 ns;
end passive;

