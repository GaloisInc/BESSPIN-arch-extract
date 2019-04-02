#!/bin/bash
set -e

work=$(dirname "$0")
inst=$1

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$-$inst.log 1>&2

driver=$PWD/driver
piccolo=$PWD/../gfe/bluespec-processors/P1/Piccolo

inst_dir=$work/$inst

if [ ! -d $inst_dir ]; then
    echo "initialize $inst_dir"
    mkdir $inst_dir
    cp -a $piccolo/{src_Core,src_Testbench} $inst_dir/
    mkdir -p $inst_dir/build/{build_dir,Verilog_RTL}
    touch $inst_dir/last-config.txt
    cat >$inst_dir/config.toml <<EOF
[src.main]
type = "verilog"
src-files = [ "$inst_dir/src_Core/*/*.bsv" ]
EOF
fi

cat >$inst_dir/cur-config.txt
echo ' --- begin config ---'
cat $inst_dir/cur-config.txt
echo ' --- end config ---'

changed_flags=$(
    diff -u $inst_dir/last-config.txt $inst_dir/cur-config.txt |
        grep '^+[01]' |
        sed -e 's/^+[01] //'
)
define_flags=$(
    grep '^1 ' $inst_dir/cur-config.txt |
        sed -e 's/^[01] /-D /'
)
mv $inst_dir/cur-config.txt $inst_dir/last-config.txt

echo "changed flags: $changed_flags"

$driver "$inst_dir/config.toml" list-pp-flag-users $changed_flags | \
    while read user; do
        echo "touch $user"
        touch $user
    done

echo "define flags are $define_flags"


(
    cd $inst_dir/build
    rm -f build_dir/*
    bsc -u -elab -verilog \
        -vdir Verilog_RTL -bdir build_dir -info-dir build_dir \
        $define_flags \
        -keep-fires -aggressive-conditions -no-warn-action-shadowing \
        -check-assert -suppress-warnings G0020 -show-range-conflict \
        +RTS -K128M -RTS \
        -p ../src_Core/CPU:../src_Core/ISA:../src_Core/RegFiles:../src_Core/Core:../src_Core/Near_Mem_VM:../src_Core/PLIC:../src_Core/Near_Mem_IO:../src_Core/Debug_Module:../src_Core/BSV_Additional_Libs:../src_Testbench/Top:../src_Testbench/SoC:../src_Testbench/Fabrics/AXI4:+ \
        ../src_Testbench/Top/Top_HW_Side.bsv
)
