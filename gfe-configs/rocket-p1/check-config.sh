#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$.log 1>&2

export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

besspin-rocket-chip-check-config check \
    $(grep '^1 ' | sed -e 's/^1 //') \
    -- \
    'galois.subsystem.WithL1ICacheSets(64)' \
    'galois.subsystem.WithL1DCacheSets(64)' \
    'galois.subsystem.WithNSmallCores(1)' \
    'galois.subsystem.WithEdgeDataBits(64)' \
    'galois.subsystem.WithNExtTopInterrupts(16)' \
    'galois.subsystem.WithDTS("galois,rocketchip-p1", Nil)' \
    'galois.subsystem.WithTimebase(BigInt(100000000))' \
    'galois.subsystem.BaseSubsystemConfig'
