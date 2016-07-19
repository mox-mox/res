
WAVE_FILE  := tb.ghw
TOP_MODULE := ahbl2sdram_tb
#SRCS       := rams/tag_sram.vhd rams/data_sram.vhd fsm/write_fsm_pkg.vhdl fsm/read_fsm_pkg.vhdl fsm/write_fsm.vhdl fsm/read_fsm.vhdl ahbl2sdram.vhdl tb_fifo.vhd cache_tb.vhd
SRCS       := rams/tag_sram.vhd rams/data_sram.vhd fsm/write_fsm_pkg.vhdl fsm/read_fsm_pkg.vhdl fsm/write_fsm.vhdl fsm/read_fsm.vhdl ahbl2sdram.vhdl testbench/ahbl_dummy.vhdl testbench/mem_ctl_dummy_fifo.vhdl testbench/mem_ctl_dummy.vhdl testbench/misc_dummy.vhdl testbench/ahbl2sdram_tb.vhdl


All: wave


${WAVE_FILE}: work-obj93.cf Makefile
	@echo "Starting the simulation..."
	@-ghdl -r --ieee=standard ${TOP_MODULE} --wave=${WAVE_FILE}
	@echo "...finished the simulation."

compile: work-obj93.cf Makefile

work-obj93.cf: ${SRCS} Makefile
	@echo "Analysing the VHDL code..."
	@ghdl -a --ieee=standard ${SRCS}
	@echo "...done with the analysis."
	@echo "Elaborating the design..."
	@ghdl -e --ieee=standard ${TOP_MODULE}
	@echo "...done with the elaboration."

wave: ${WAVE_FILE} Makefile
	@gtkwave ${WAVE_FILE} &
