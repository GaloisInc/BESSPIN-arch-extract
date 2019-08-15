#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

export BESSPIN_ROCKET_CHIP_HELPER=besspin-boom-helper
export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

besspin-rocket-chip-check-config list \
    freechips.rocketchip.subsystem.With boom.common.With boom.galois.system.With
