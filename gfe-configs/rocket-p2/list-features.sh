#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

besspin-rocket-chip-check-config list \
    galois.subsystem.With galois.system.With
