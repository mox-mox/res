library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity MEM_CTL_DUMMY is
	--{{{
	port(
		rst               : in    std_logic;
-- Command Path -------------------------------------------------------------------------------------------------------
		p1_cmd_clk        : in    std_logic;                     -- User clock for the command FIFO
		p1_cmd_instr      : in    std_logic_vector( 2 downto 0); -- Current instruction. 000: Wrtie, 001: Read, 010: Read w. precharge, 011: ...
		p1_cmd_addr       : in    std_logic_vector(29 downto 0); -- Byte start address for current transaction.
		p1_cmd_bl         : in    std_logic_vector( 5 downto 0); -- Busrst length-1, eg. 0 indicates a burst of one word
		p1_cmd_en         : in    std_logic;                     -- Write enable for the command FIFO: 0: Diabled, 1: Enabled
		p1_cmd_empty      : out   std_logic;                     -- Command FIFO empty bit: 0: Not empty, 1: Empty
		p1_cmd_error      : out   std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_cmd_full       : out   std_logic;                     -- Command FIFO full bit: 0: Not full, 1: Full
-- Write Datapath -----------------------------------------------------------------------------------------------------
		p1_wr_clk         : in    std_logic;                     -- Clock for the write data FIFO
		p1_wr_data        : in    std_logic_vector(31 downto 0); -- Data to be stored in the FIFO and be written to the DDR2-DRAM.
		p1_wr_mask        : in    std_logic_vector( 3 downto 0); -- Mask write data. A high bit means corresponding byte is not written to the RAM.
		p1_wr_en          : in    std_logic;                     -- Write enable for the write data FIFO
		p1_wr_count       : out   std_logic_vector( 6 downto 0); -- Write data FIFO fill level: 0: empty. Note longer latency than p1_wr_empty!
		p1_wr_empty       : out   std_logic;                     -- Write data FIFO empty bit: 0: Not empty, 1: Empty
		p1_wr_error       : out   std_logic;                     -- Error bit. Need to reset the MCB to resolve.
		p1_wr_full        : out   std_logic;                     -- Write data FIFO full bit: 0: Not full, 1: Full
		p1_wr_underrun    : out   std_logic;                     -- Underrun flag. 0: All ok, 1: Underrun. Last valid data is written repeatedly.
-- Read Datapath ------------------------------------------------------------------------------------------------------
		p1_rd_clk         : in    std_logic;                     -- Clock for the read data FIFO
		p1_rd_en          : in    std_logic;                     -- Read enable bit for the read data FIFO: 0: Diabled, 1: Enabled
		p1_rd_data        : out   std_logic_vector(31 downto 0); -- Data read from the RAM
		p1_rd_full        : out   std_logic;                     -- Read data FIFO full bit: 0: All ok, 1: Full. Data will be discarded.
		p1_rd_empty       : out   std_logic;                     -- Read data FIFO empty bit: 0: Not empty, 1: Empty. Cannot read data from FIFO.
		p1_rd_count       : out   std_logic_vector( 6 downto 0); -- Read data FIFO fill level: 0: empty. Note longer latency than p1_rd_full!
		p1_rd_overflow    : out   std_logic;                     -- Overflow flag: 0: All ok, 1: Data was lost because the FIFO overflowed.
		p1_rd_error       : out   std_logic);                    -- Error bit. Need to reset the MCB to resolve. }}}
end MEM_CTL_DUMMY;

architecture normal of MEM_CTL_DUMMY is

	--{{{ Wiring logic

	signal p1_cmd_instr_bl_addr_concat : std_logic_vector(38 downto 0);
	signal cmd_readen                  : std_logic;
	signal cmd_instr_bl_addr_concat    : std_logic_vector(38 downto 0);
	alias cmd_instr                    is cmd_instr_bl_addr_concat(38 downto 36);
	alias cmd_bl                       is cmd_instr_bl_addr_concat(35 downto 30);
	alias cmd_addr                     is cmd_instr_bl_addr_concat(29 downto 0);

	signal p1_wr_mask_data_concat      : std_logic_vector(35 downto 0);
	signal wr_readen                   : std_logic;
	signal wr_mask_data_concat         : std_logic_vector(35 downto 0);
	alias  wr_mask                     is wr_mask_data_concat(35 downto 32);
	alias  wr_data                     is wr_mask_data_concat(31 downto  0);

	signal rd_data                     : std_logic_vector(31 downto 0);
	signal rd_writeen                  : std_logic;

	--}}}

	--{{{ Components

	--{{{
    component CMD_FIFO
        Generic (
            constant DATA_WIDTH : positive := 38;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(38 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(38 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	--{{{
    component WR_FIFO
        Generic (
            constant DATA_WIDTH : positive := 35;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(35 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(35 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	--{{{
    component RD_FIFO
        Generic (
            constant DATA_WIDTH : positive := 32;
            constant FIFO_DEPTH : positive := 64
        );
        port (
            CLK     : in std_logic;
            RST     : in std_logic;
            DataIn  : in std_logic_vector(31 downto 0);
            WriteEn : in std_logic;
            ReadEn  : in std_logic;
            DataOut : out std_logic_vector(31 downto 0);
            Full    : out std_logic;
            Empty   : out std_logic
        );
    end component;
	--}}}

	for fifo_cmd   : CMD_FIFO    use entity work.FWFT_FIFO(Behavioral);
	for fifo_wr    : WR_FIFO     use entity work.FWFT_FIFO(Behavioral);
	for fifo_rd    : RD_FIFO     use entity work.FWFT_FIFO(Behavioral);
	--}}}

	--{{{ The Dummy RAM

	type ram_type is array (4194304 downto 0) of std_logic_vector (31 downto 0);
	shared variable RAM : ram_type;
	--}}}


begin

	--{{{ Port Maps

	--{{{
	fifo_cmd : CMD_FIFO port map (
		CLK     => p1_cmd_clk,
		DataIn  => p1_cmd_instr_bl_addr_concat,
		WriteEn => p1_cmd_en,
		ReadEn  => cmd_readen,
		DataOut => cmd_instr_bl_addr_concat,
		Empty   => p1_cmd_empty,
		Full    => p1_cmd_full,
		RST     => rst
	);
	p1_cmd_instr_bl_addr_concat <= p1_cmd_instr & p1_cmd_bl & p1_cmd_addr;
	--}}}

	--{{{
	fifo_wr : wr_FIFO port map (
		CLK     => p1_wr_clk,
		DataIn  => p1_wr_mask_data_concat,
		WriteEn => p1_wr_en,
		ReadEn  => wr_readen,
		DataOut => wr_mask_data_concat,
		Empty   => p1_wr_empty,
		Full    => p1_wr_full,
		RST     => rst
	);
	p1_wr_mask_data_concat <= p1_wr_mask & p1_wr_data;
	--}}}

	--{{{
	fifo_rd : RD_FIFO port map (
		CLK     => p1_rd_clk,
		DataIn  => rd_data,
		WriteEn => rd_writeen,
		ReadEn  => p1_rd_en,
		DataOut => p1_rd_data,
		Empty   => p1_rd_empty,
		Full    => p1_rd_full,
		RST     => rst
	);
	--}}}
	--}}}


end normal;

