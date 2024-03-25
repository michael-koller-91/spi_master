import os
import re
import random
from pathlib import Path
from vunit import VUnit


def parse_port_names(filename):
    p = re.compile(r".*\s:\s[in|out].*")
    port_names = []
    with open(filename, "r") as f:
        for line in f.readlines():
            if p.match(line.strip()):
                port_names.append(line.split(":")[0].strip())
    return port_names


def parse_signal_names(filename):
    p = re.compile(r"\bsignal.*:.*;")
    signal_names = []
    with open(filename, "r") as f:
        for line in f.readlines():
            if p.match(line.strip()):
                signal_names.append(line.split(":")[0].split("signal")[1].strip())
    return signal_names


def generate_waveform_file(filename):
    lines = ["# THIS IS AUTOGENERATED CODE\n"]
    lines.append("set signals [list]\n")
    for signal in parse_signal_names("tb_spi_master.vhd"):
        lines.append(f'lappend signals "{signal}"\n')
    for port in parse_port_names(os.path.join("..", "spi_master.vhd")):
        lines.append(f'lappend signals "e_dut.{port}"\n')
    for signal in parse_signal_names(os.path.join("..", "spi_master.vhd")):
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

test = tb.test("01_all_sclk_scs_idle_cases")
counter = 0
for sclk_idle_state in ["'0'", "'1'"]:
    for scs_idle_state in ["'0'", "'1'"]:
        counter += 1
        test.add_config(
            name=f"c{counter}.sclk_idle={sclk_idle_state}.scs_idle={scs_idle_state}",
            generics={
                "G_SCLK_IDLE_STATE": sclk_idle_state,
                "G_SCS_IDLE_STATE": scs_idle_state,
            },
        )

test = tb.test("02_all_sclk_transmit_edge_cases")
counter = 0
for sclk_idle_state in ["'0'", "'1'"]:
    for transmit_edge in ["'0'", "'1'"]:
        counter += 1
        test.add_config(
            name=f"c{counter}.sclk_idle={sclk_idle_state}.transmit_toward_idle={transmit_edge}",
            generics={
                "G_SCLK_IDLE_STATE": sclk_idle_state,
                "G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE": transmit_edge,
            },
        )

test = tb.test("03_sclk_divide")
counter = 0
for divide in [2, 5, 10]:
    counter += 1
    test.add_config(
        name=f"c{counter}.max_sclk_divide={divide}",
        generics={
            "G_MAX_SCLK_DIVIDE_HALF": divide,
        },
    )

test = tb.test("04_n_bits")
counter = 0
for n_bits_minus_1 in [0, 1, 2]:
    counter += 1
    test.add_config(
        name=f"c{counter}.max_n_bits_minus_1={n_bits_minus_1}",
        generics={
            "G_MAX_N_BITS_MINUS_1": n_bits_minus_1,
        },
    )

seed = random.randint(1_000_000, 9_999_999)

test = tb.test("05_transmit")
test.add_config(name=f"rng_seed={seed}", generics={"G_RNG_SEED": seed})

test = tb.test("06_receive")
test.add_config(name=f"rng_seed={seed}", generics={"G_RNG_SEED": seed})

waveform_filename = "waveform.tcl"
generate_waveform_file(waveform_filename)

vu.set_sim_option(
    "ghdl.gtkwave_script.gui", str(Path(__file__).parent / waveform_filename)
)
vu.set_sim_option(
    "nvc.gtkwave_script.gui", str(Path(__file__).parent / waveform_filename)
)

vu.main()
