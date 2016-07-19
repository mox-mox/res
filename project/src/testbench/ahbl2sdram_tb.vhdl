----------------------------------------------------------------
 -- Authors: Rieber, Dennis; Noeltner, Moritz; Schneider, Tim
 -- Institute: University of Heidelberg, ZITI
 -- Lecture: Reconfigurable Embedded Systems
 -------------------------------------------------------------------------
 -- Content: This module is a testbench for the internal cache controller, imitating
 -- an AHB Master and Memeory Controller
--------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AHBL2SDRAM_TB is
end AHBL2SDRAM_TB;

architecture read_test of AHBL2SDRAM_TB is

	--{{{ Wiring logic

		signal rst               : std_logic;
		signal HCLK              : std_logic;
		signal HRESETn           : std_logic;
		signal HSEL              : std_logic;
		signal HADDR             : std_logic_vector(31 downto 0);
		signal HWRITE            : std_logic;
		signal HSIZE             : std_logic_vector( 2 downto 0);
		-- signal HBURST         : std_logic_vector( 2 downto 0);
		-- signal HPROT          : std_logic_vector( 3 downto 0);
		signal HTRANS            : std_logic_vector( 1 downto 0);
		-- signal HMASTLOCK      : std_logic;
		signal HREADY            : std_logic;
		signal HWDATA            : std_logic_vector(31 downto 0);
		signal HREADYOUT         : std_logic;
		signal HRESP             : std_logic;
		signal HRDATA            : std_logic_vector(31 downto 0);

		signal p1_cmd_instr      : std_logic_vector( 2 downto 0);
		signal p1_cmd_addr       : std_logic_vector(29 downto 0);
		signal p1_cmd_bl         : std_logic_vector( 5 downto 0);
		signal p1_cmd_en         : std_logic;
		signal p1_cmd_empty      : std_logic;
		signal p1_cmd_error      : std_logic;
		signal p1_cmd_full       : std_logic;
		signal p1_wr_data        : std_logic_vector(31 downto 0);
		signal p1_wr_mask        : std_logic_vector( 3 downto 0);
		signal p1_wr_en          : std_logic;
		signal p1_wr_count       : std_logic_vector( 6 downto 0);
		signal p1_wr_empty       : std_logic;
		signal p1_wr_error       : std_logic;
		signal p1_wr_full        : std_logic;
		signal p1_wr_underrun    : std_logic;
		signal p1_rd_en          : std_logic;
		signal p1_rd_data        : std_logic_vector(31 downto 0);
		signal p1_rd_full        : std_logic;
		signal p1_rd_empty       : std_logic;
		signal p1_rd_count       : std_logic_vector( 6 downto 0);
		signal p1_rd_overflow    : std_logic;
		signal p1_rd_error       : std_logic;

		signal DCLK              : std_logic;
		signal mem_calib_done    : std_logic;
		signal HIT_COUNT         : std_logic_vector(31 downto 0);
		signal MISS_COUNT        : std_logic_vector(31 downto 0);
		signal INVALIDATE_LOW    : std_logic_vector(31 downto 0);
		signal INVALIDATE_HIGH   : std_logic_vector(31 downto 0);
	--}}}

	--{{{ Components

	--{{{
	component AHBL2SDRAM is port (
		-- AHB-LITE Interface
		HCLK              : in    std_logic;
		HRESETn           : in    std_logic;
		HSEL              : in    std_logic;
		HADDR             : in    std_logic_vector(31 downto 0);
		HWRITE            : in    std_logic;
		HSIZE             : in    std_logic_vector( 2 downto 0);
		-- HBURST         : in    std_logic_vector( 2 downto 0);
		-- HPROT          : in    std_logic_vector( 3 downto 0);
		HTRANS            : in    std_logic_vector( 1 downto 0);
		-- HMASTLOCK      : in    std_logic;
		HREADY            : in    std_logic;
		HWDATA            : in    std_logic_vector(31 downto 0);
		HREADYOUT         : out   std_logic;
		HRESP             : out   std_logic;
		HRDATA            : out   std_logic_vector(31 downto 0);

		-- Memory Controller Interface
		p1_cmd_clk        : out   std_logic;
		p1_cmd_instr      : out   std_logic_vector( 2 downto 0);
		p1_cmd_addr       : out   std_logic_vector(29 downto 0);
		p1_cmd_bl         : out   std_logic_vector( 5 downto 0);
		p1_cmd_en         : out   std_logic;
		p1_cmd_empty      : in    std_logic;
		p1_cmd_error      : in    std_logic;
		p1_cmd_full       : in    std_logic;
		p1_wr_clk         : out   std_logic;
		p1_wr_data        : out   std_logic_vector(31 downto 0);
		p1_wr_mask        : out   std_logic_vector( 3 downto 0);
		p1_wr_en          : out   std_logic;
		p1_wr_count       : in    std_logic_vector( 6 downto 0);
		p1_wr_empty       : in    std_logic;
		p1_wr_error       : in    std_logic;
		p1_wr_full        : in    std_logic;
		p1_wr_underrun    : in    std_logic;
		p1_rd_clk         : out   std_logic;
		p1_rd_en          : out   std_logic;
		p1_rd_data        : in    std_logic_vector(31 downto 0);
		p1_rd_full        : in    std_logic;
		p1_rd_empty       : in    std_logic;
		p1_rd_count       : in    std_logic_vector( 6 downto 0);
		p1_rd_overflow    : in    std_logic;
		p1_rd_error       : in    std_logic;

		-- Miscellaneous non-bus signals
		DCLK              : in    std_logic;
		mem_calib_done    : in    std_logic;
		HIT_COUNT         : out   std_logic_vector(31 downto 0);
		MISS_COUNT        : out   std_logic_vector(31 downto 0);
		INVALIDATE_LOW    : inout std_logic_vector(31 downto 0);
		INVALIDATE_HIGH   : inout std_logic_vector(31 downto 0));
	end component AHBL2SDRAM;
	--}}}

	--{{{
	component AHBL_DUMMY is port(
		HCLK              : out    std_logic;
		HRESETn           : out    std_logic;
		HSEL              : out    std_logic;
		HADDR             : out    std_logic_vector(31 downto 0);
		HWRITE            : out    std_logic;
		HSIZE             : out    std_logic_vector( 2 downto 0);
		-- HBURST         : out    std_logic_vector( 2 downto 0);
		-- HPROT          : out    std_logic_vector( 3 downto 0);
		HTRANS            : out    std_logic_vector( 1 downto 0);
		-- HMASTLOCK      : out    std_logic;
		HREADY            : out    std_logic;
		HWDATA            : out    std_logic_vector(31 downto 0);
		HREADYOUT         : in    std_logic;
		HRESP             : in    std_logic;
		HRDATA            : in    std_logic_vector(31 downto 0));
	end component AHBL_DUMMY;
	--}}}

	--{{{
	component MEM_CTL_DUMMY is port(
		rst               : in    std_logic;
		p1_cmd_clk        : in    std_logic;
		p1_cmd_instr      : in    std_logic_vector( 2 downto 0);
		p1_cmd_addr       : in    std_logic_vector(29 downto 0);
		p1_cmd_bl         : in    std_logic_vector( 5 downto 0);
		p1_cmd_en         : in    std_logic;
		p1_cmd_empty      : out   std_logic;
		p1_cmd_error      : out   std_logic;
		p1_cmd_full       : out   std_logic;
		p1_wr_clk         : in    std_logic;
		p1_wr_data        : in    std_logic_vector(31 downto 0);
		p1_wr_mask        : in    std_logic_vector( 3 downto 0);
		p1_wr_en          : in    std_logic;
		p1_wr_count       : out   std_logic_vector( 6 downto 0);
		p1_wr_empty       : out   std_logic;
		p1_wr_error       : out   std_logic;
		p1_wr_full        : out   std_logic;
		p1_wr_underrun    : out   std_logic;
		p1_rd_clk         : in    std_logic;
		p1_rd_en          : in    std_logic;
		p1_rd_data        : out   std_logic_vector(31 downto 0);
		p1_rd_full        : out   std_logic;
		p1_rd_empty       : out   std_logic;
		p1_rd_count       : out   std_logic_vector( 6 downto 0);
		p1_rd_overflow    : out   std_logic;
		p1_rd_error       : out   std_logic);
	end component MEM_CTL_DUMMY;
	--}}}

	--{{{
	component MISC_DUMMY is port(
		HCLK              : in    std_logic;
		mem_calib_done    : in    std_logic;
		HIT_COUNT         : out   std_logic_vector(31 downto 0);
		MISS_COUNT        : out   std_logic_vector(31 downto 0);
		INVALIDATE_LOW    : inout std_logic_vector(31 downto 0);
		INVALIDATE_HIGH   : inout std_logic_vector(31 downto 0));
	end component MISC_DUMMY;
	--}}}
	--}}}

	for cache   : ahbl2sdram    use entity work.ahbl2sdram(cache);
	for ahbl    : ahbl_dummy    use entity work.ahbl_dummy(read_sequence);
	for mem_ctl : mem_ctl_dummy use entity work.mem_ctl_dummy(normal);
	for misc    : misc_dummy    use entity work.misc_dummy(passive);
begin

	--{{{ Wiring logic

		rst               <= not HRESETn;
	--}}}

	--{{{ Port Maps

	--{{{
	cache :  AHBL2SDRAM port map (
		HCLK              => HCLK,
		HRESETn           => HRESETn,
		HSEL              => HSEL,
		HADDR             => HADDR,
		HWRITE            => HWRITE,
		HSIZE             => HSIZE,
		-- HBURST         => -- HBURST,
		-- HPROT          => -- HPROT,
		HTRANS            => HTRANS,
		-- HMASTLOCK      => -- HMASTLOCK,
		HREADY            => HREADY,
		HWDATA            => HWDATA,
		HREADYOUT         => HREADYOUT,
		HRESP             => HRESP,
		HRDATA            => HRDATA,
		p1_cmd_clk        => DCLK,
		p1_cmd_instr      => p1_cmd_instr,
		p1_cmd_addr       => p1_cmd_addr,
		p1_cmd_bl         => p1_cmd_bl,
		p1_cmd_en         => p1_cmd_en,
		p1_cmd_empty      => p1_cmd_empty,
		p1_cmd_error      => p1_cmd_error,
		p1_cmd_full       => p1_cmd_full,
		p1_wr_clk         => DCLK,
		p1_wr_data        => p1_wr_data,
		p1_wr_mask        => p1_wr_mask,
		p1_wr_en          => p1_wr_en,
		p1_wr_count       => p1_wr_count,
		p1_wr_empty       => p1_wr_empty,
		p1_wr_error       => p1_wr_error,
		p1_wr_full        => p1_wr_full,
		p1_wr_underrun    => p1_wr_underrun,
		p1_rd_clk         => DCLK,
		p1_rd_en          => p1_rd_en,
		p1_rd_data        => p1_rd_data,
		p1_rd_full        => p1_rd_full,
		p1_rd_empty       => p1_rd_empty,
		p1_rd_count       => p1_rd_count,
		p1_rd_overflow    => p1_rd_overflow,
		p1_rd_error       => p1_rd_error,
		DCLK              => DCLK,
		mem_calib_done    => mem_calib_done,
		HIT_COUNT         => HIT_COUNT,
		MISS_COUNT        => MISS_COUNT,
		INVALIDATE_LOW    => INVALIDATE_LOW,
		INVALIDATE_HIGH   => INVALIDATE_HIGH);
	--}}}

	--{{{
	ahbl :  AHBL_DUMMY port map(
		HCLK              => HCLK,
		HRESETn           => HRESETn,
		HSEL              => HSEL,
		HADDR             => HADDR,
		HWRITE            => HWRITE,
		HSIZE             => HSIZE,
		-- HBURST         => -- HBURST
		-- HPROT          => -- HPROT
		HTRANS            => HTRANS,
		-- HMASTLOCK      => -- HMASTL
		HREADY            => HREADY,
		HWDATA            => HWDATA,
		HREADYOUT         => HREADYOUT,
		HRESP             => HRESP,
		HRDATA            => HRDATA);
	--}}}

	--{{{
	mem_ctl : MEM_CTL_DUMMY port map(
		rst               => rst,
		p1_cmd_clk        => DCLK,
		p1_cmd_instr      => p1_cmd_instr,
		p1_cmd_addr       => p1_cmd_addr,
		p1_cmd_bl         => p1_cmd_bl,
		p1_cmd_en         => p1_cmd_en,
		p1_cmd_empty      => p1_cmd_empty,
		p1_cmd_error      => p1_cmd_error,
		p1_cmd_full       => p1_cmd_full,
		p1_wr_clk         => DCLK,
		p1_wr_data        => p1_wr_data,
		p1_wr_mask        => p1_wr_mask,
		p1_wr_en          => p1_wr_en,
		p1_wr_count       => p1_wr_count,
		p1_wr_empty       => p1_wr_empty,
		p1_wr_error       => p1_wr_error,
		p1_wr_full        => p1_wr_full,
		p1_wr_underrun    => p1_wr_underrun,
		p1_rd_clk         => DCLK,
		p1_rd_en          => p1_rd_en,
		p1_rd_data        => p1_rd_data,
		p1_rd_full        => p1_rd_full,
		p1_rd_empty       => p1_rd_empty,
		p1_rd_count       => p1_rd_count,
		p1_rd_overflow    => p1_rd_overflow,
		p1_rd_error       => p1_rd_error);
	--}}}

	--{{{
	misc : MISC_DUMMY port map(
		HCLK              => HCLK,
		mem_calib_done    => mem_calib_done,
		HIT_COUNT         => HIT_COUNT,
		MISS_COUNT        => MISS_COUNT,
		INVALIDATE_LOW    => INVALIDATE_LOW,
		INVALIDATE_HIGH   => INVALIDATE_HIGH);
	--}}}
	--}}}





end read_test;


