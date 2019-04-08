#!/bin/bash
set -e

work=$(dirname "$0")
inst=$1

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$-$inst.log 1>&2

piccolo=$PWD/../gfe/bluespec-processors/P1/Piccolo

inst_dir=$work/$inst

if [ ! -d $inst_dir ]; then
    echo "initialize $inst_dir"
    mkdir $inst_dir
    cp -a $piccolo/{src_Core,src_Testbench} $inst_dir/
    mkdir -p $inst_dir/build
fi

cd $inst_dir/build
# stdin of this script is passed through to build.sh
exec "$work/build.sh" "$piccolo"
