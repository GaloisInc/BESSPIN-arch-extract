#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

export BESSPIN_CHECK_CONFIG_CACHE_DIR=$work/check-config-cache

python3 rocket-chip-check-config/check.py list \
    galois.subsystem.With galois.system.With
