add wave -expand -color blue -vgroup "-- tb/* --" \
    (/tb_spi_master/*)

add wave -expand -color blue -vgroup "-- tb/dut/* --" \
    (/tb_spi_master/e_dut/*)
