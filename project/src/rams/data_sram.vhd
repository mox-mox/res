--
-- Single-Port BRAM Write-First Mode (recommended template)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use ieee.std_logic_unsigned.all;

entity DATA_SRAM is
	port (clk     : in  std_logic;
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
end DATA_SRAM;

architecture syn of DATA_SRAM is
	type ram_type is array (0 to 1024) of std_logic_vector (31 downto 0); -- Let's see if the synthesis can create a ram consisting of two BRAMs.
	--signal DATA_RAM : ram_type := (others => "00000000000000000000000000000000"); -- TODO: Change this back
	shared variable DATA_RAM : ram_type;
begin
	process (clk)
	begin
		if clk'event and clk = '1' then
			if en_A = '1' then
				if we_A = '1' then
					for i in 0 to 3 loop
						if wr_mask_A(i) = '1' then
							--DATA_RAM(to_integer(unsigned(addr_A)))(((i+1)*8)-1 downto (i*8)) <= di_A(((i+1)*8)-1 downto (i*8));  -- TODO: Change this back
							DATA_RAM(to_integer(unsigned(addr_A)))(((i+1)*8)-1 downto (i*8)) := di_A(((i+1)*8)-1 downto (i*8));
						end if;
					end loop;
					do_A <= di_A;
				else
					do_A <= DATA_RAM(to_integer(unsigned(addr_A)));
				end if;
			end if;
		end if;
	end process;

	process (clk)
	begin
		if clk'event and clk = '1' then
			if en_B = '1' then
				if we_B = '1' then
					for i in 0 to 3 loop
						if wr_mask_B(i) = '1' then
							--DATA_RAM(to_integer(unsigned(addr_B)))(((i+1)*8)-1 downto (i*8)) <= di_B(((i+1)*8)-1 downto (i*8));  -- TODO: Change this back
							DATA_RAM(to_integer(unsigned(addr_B)))(((i+1)*8)-1 downto (i*8)) := di_B(((i+1)*8)-1 downto (i*8));
						end if;
					end loop;
					do_B <= di_B;
				else
					do_B <= DATA_RAM(to_integer(unsigned(addr_B)));
				end if;
			end if;
		end if;
	end process;
end syn;
