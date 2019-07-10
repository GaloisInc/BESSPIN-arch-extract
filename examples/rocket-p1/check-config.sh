#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$.log 1>&2

python3 rocket-chip-check-config/check.py check \
    $(grep '^1 ' | sed -e 's/^1 //')
