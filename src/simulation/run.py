from pathlib import Path
from vunit import VUnit


vu = VUnit.from_argv(compile_builtins=False)
vu.add_vhdl_builtins()
vu.add_osvvm()

work = vu.add_library("spi_master")
work.add_source_files(Path(__file__).parent / "*.vhd")
work.add_source_files(Path(__file__).parent / ".." / "*.vhd")

vu.set_sim_option(
    "ghdl.gtkwave_script.gui", str(Path(__file__).parent / "waveform.tcl")
)

vu.main()
