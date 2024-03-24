import os
import re
from pathlib import Path
from vunit import VUnit


def parse_port_names(filename, p):
    port_names = []
    with open(filename, "r") as f:
        for line in f.readlines():
            if p.match(line.strip()):
                port_names.append(line.split(":")[0].strip())
    return port_names


def parse_signal_names(filename, p):
    signal_names = []
    with open(filename, "r") as f:
        for line in f.readlines():
            if p.match(line.strip()):
                signal_names.append(line.split(":")[0].split("signal")[1].strip())
    return signal_names


def generate_waveform_file(filename):
    p_port = re.compile(r".*\s:\s[in|out].*;")
    p_signal = re.compile(r"\bsignal.*:.*;")
    lines = ["set signals [list]\n"]
    for signal in parse_signal_names("tb_spi_master.vhd", p_signal):
       lines.append(f'lappend signals "{signal}"\n')
    for port in parse_port_names(os.path.join("..", "spi_master.vhd"), p_port):
       lines.append(f'lappend signals "e_dut.{port}"\n')
    for signal in parse_signal_names(os.path.join("..", "spi_master.vhd"), p_signal):
       lines.append(f'lappend signals "e_dut.{signal}"\n')
    lines.append("gtkwave::addSignalsFromList $signals\n")

    with open(filename, "w") as f:
        f.writelines(lines)


vu = VUnit.from_argv(compile_builtins=False)
vu.add_vhdl_builtins()
vu.add_osvvm()

work = vu.add_library("spi_master")
work.add_source_files(Path(__file__).parent / "*.vhd")
work.add_source_files(Path(__file__).parent.parent / "*.vhd")

tb = work.test_bench("tb_spi_master")
test001 = tb.test("SCS_SCLK_timings")
for sclk_idle_state in ["'0'", "'1'"]:
    test001.add_config(name=f"sclk_idle_state={sclk_idle_state}", generics={"G_SCLK_IDLE_STATE": sclk_idle_state})

waveform_filename = "waveform.tcl"
generate_waveform_file(waveform_filename)

vu.set_sim_option(
    "ghdl.gtkwave_script.gui", str(Path(__file__).parent / waveform_filename)
)
vu.set_sim_option(
    "nvc.gtkwave_script.gui", str(Path(__file__).parent / waveform_filename)
)

vu.main()
