 --
 -- Simple Dual-Port BRAM Write-First Mode (recommended template)
 --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 --use ieee.std_logic_unsigned.all;

entity TAG_SRAM is
	port (clk  : in std_logic;
	-- Port A
		en_A   : in  std_logic;
		we_A   : in  std_logic;
		addr_A : in  std_logic_vector( 9 downto 0);
		di_A   : in  std_logic_vector(15 downto 0);
		do_A   : out std_logic_vector(15 downto 0);

	-- Port B
		en_B   : in  std_logic;
		we_B   : in  std_logic;
		addr_B : in  std_logic_vector( 9 downto 0);
		di_B   : in  std_logic_vector(15 downto 0);
		do_B   : out std_logic_vector(15 downto 0)
	  );
end TAG_SRAM;

architecture syn of TAG_SRAM is
	type ram_type is array (0 to 1024) of std_logic_vector (15 downto 0);
	signal TAG_RAM : ram_type := (others => "0000000000000000"); -- TODO: Change this back
	--shared variable TAG_RAM : ram_type;
begin
	process (clk)
	begin
		if clk'event and clk = '1' then
			if en_A = '1' then
				if we_A = '1' then
					 TAG_RAM(to_integer(unsigned(addr_A))) <= di_A;  -- TODO: Change this back
					 --TAG_RAM(to_integer(unsigned(addr_A))) := di_A;
					do_A <= di_A;
				else
					--report "addr_A" &  integer'image(to_integer(unsigned(addr_A)));
					do_A <= TAG_RAM(to_integer(unsigned(addr_A)));
				end if;
			end if;
		end if;
	end process;

	process (clk)
	begin
		if clk'event and clk = '1' then
			if en_B = '1' then
				if we_B = '1' then
					TAG_RAM(to_integer(unsigned(addr_B))) <= di_B;  -- TODO: Change this back
					--TAG_RAM(to_integer(unsigned(addr_B))) := di_B;
					do_B <= di_B;
				else
					do_B <= TAG_RAM(to_integer(unsigned(addr_B)));
				end if;
			end if;
		end if;
	end process;
end syn;
