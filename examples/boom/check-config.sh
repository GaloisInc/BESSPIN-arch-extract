#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$.log 1>&2

export BESSPIN_ROCKET_CHIP_HELPER=besspin-boom-helper
export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

python3 rocket-chip-check-config/check.py check \
    $(grep '^1 ' | sed -e 's/^1 //') \
    -- \
    'boom.common.DefaultBoomConfig' \
    'boom.system.WithNBoomCores(1)' \
    'freechips.rocketchip.subsystem.WithNExtTopInterrupts(16)' \
    'freechips.rocketchip.subsystem.WithTimebase(BigInt(100000000))' \
    'freechips.rocketchip.subsystem.WithDTS("freechips,rocketchip-unknown", Nil)' \
    'freechips.rocketchip.subsystem.BaseSubsystemConfig'
