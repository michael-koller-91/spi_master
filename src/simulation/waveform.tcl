set signals [list]

lappend signals "clk"
lappend signals "start"
lappend signals "ready"
lappend signals "d_from_peripheral"
lappend signals "d_to_peripheral"
lappend signals "sclk"
lappend signals "sd_from_peripheral"
lappend signals "sd_to_peripheral"
lappend signals "scs"

lappend signals "e_dut.counter_n_bits"
lappend signals "e_dut.counter_n_clks_sclk_to_scs"
lappend signals "e_dut.counter_n_clks_scs_to_sclk"
lappend signals "e_dut.counter_clk_divide"
lappend signals "e_dut.reset_sclk"

lappend signals "e_dut.sclk"
lappend signals "e_dut.sclk_edge"
lappend signals "e_dut.sample_sdi"
lappend signals "e_dut.sample_sdo"

lappend signals "e_dut.state"

gtkwave::addSignalsFromList $signals

