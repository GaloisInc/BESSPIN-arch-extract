#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$.log 1>&2

export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

python3 rocket-chip-check-config/check.py check \
    $(grep '^1 ' | sed -e 's/^1 //') \
    -- \
    'galois.subsystem.WithNExtTopInterrupts(16)' \
    'galois.subsystem.WithL1ICacheSets(32)' \
    'galois.subsystem.WithL1DCacheSets(32)' \
    'galois.subsystem.WithNBigCores(1)' \
    'galois.subsystem.WithEdgeDataBits(64)' \
    'galois.subsystem.WithDTS("galois,rocketchip-p2", Nil)' \
    'galois.subsystem.WithTimebase(BigInt(100000000))' \
    'galois.subsystem.BaseSubsystemConfig'
