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

lappend signals "dut_i.counter_n_bits"
lappend signals "dut_i.counter_n_clks_sclk_to_scs"
lappend signals "dut_i.counter_n_clks_scs_to_sclk"
lappend signals "dut_i.counter_clk_divide"
lappend signals "dut_i.reset_sclk"

lappend signals "dut_i.sclk"
lappend signals "dut_i.sclk_edge"
lappend signals "dut_i.sample_sdi"
lappend signals "dut_i.sample_sdo"

lappend signals "dut_i.state"

gtkwave::addSignalsFromList $signals

