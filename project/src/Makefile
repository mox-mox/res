
WAVE_FILE  := tb.ghw
TOP_MODULE := cache_tb
SRCS       := rams/tag_sram.vhd rams/data_sram.vhd fsm/write_fsm_pkg.vhdl fsm/read_fsm_pkg.vhdl fsm/write_fsm.vhdl fsm/read_fsm.vhdl ahbl2sdram.vhdl cache_tb.vhd


All: wave


${WAVE_FILE}: work-obj93.cf Makefile
	@echo "Starting the simulation..."
	-ghdl -r --ieee=standard ${TOP_MODULE} --wave=${WAVE_FILE}
	@echo "...finished the simulation."

work-obj93.cf: ${SRCS} Makefile
	@echo "Analysing the VHDL code..."
	ghdl -a --ieee=standard ${SRCS}
	@echo "...done with the analysis."
	@echo "Elaborating the design..."
	ghdl -e --ieee=standard ${TOP_MODULE}
	@echo "...done with the elaboration."

wave: ${WAVE_FILE} Makefile
	gtkwave ${WAVE_FILE} -W
