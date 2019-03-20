#!/bin/bash
export RV_ROOT=$PWD/../swerv_eh1
cd "$(dirname "$0")"

while read on feat; do
    echo "\`undef $feat"
    if [ "$on" -eq 1 ]; then
        echo "\`define $feat"
    fi
done >feature_config.$$.sv

verilator -UASSERT_ON --cc -CFLAGS "-std=c++11" \
    $RV_ROOT/configs/snapshots/default/common_defines.vh \
    feature_config.$$.sv \
    $RV_ROOT/design/include/def.sv \
    -I$RV_ROOT/design/include \
    -I$RV_ROOT/design/lib \
    -I$RV_ROOT/design/dmi \
    -I$RV_ROOT/configs/snapshots/default \
	-f $RV_ROOT/design/flist.verilator \
    --top-module swerv_wrapper \
    --Mdir out_dir_$$ 2>/dev/null
ok=$?

rm -rf "out_dir_$$/" "feature_config.$$.sv"

exit $ok

