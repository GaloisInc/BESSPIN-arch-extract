#!/bin/bash
set -e

piccolo="$1"

cat >cur-config.txt

define_flags=$(
    grep '^1 ' cur-config.txt |
        sed -e 's/^[01] /-D /'
)

mkdir -p build_dir Verilog_RTL
rm -f build_dir/*

bsc -u -elab -verilog \
    -vdir Verilog_RTL -bdir build_dir -info-dir build_dir \
    $define_flags \
    -keep-fires -aggressive-conditions -no-warn-action-shadowing \
    -check-assert -suppress-warnings G0020 -show-range-conflict \
    +RTS -K128M -RTS \
    -p "$piccolo/src_Core/CPU:$piccolo/src_Core/ISA:$piccolo/src_Core/RegFiles:$piccolo/src_Core/Core:$piccolo/src_Core/Near_Mem_VM:$piccolo/src_Core/PLIC:$piccolo/src_Core/Near_Mem_IO:$piccolo/src_Core/Debug_Module:$piccolo/src_Core/BSV_Additional_Libs:$piccolo/src_Testbench/Top:$piccolo/src_Testbench/SoC:$piccolo/src_Testbench/Fabrics/AXI4:+" \
    "$piccolo/src_Testbench/Top/Top_HW_Side.bsv"
