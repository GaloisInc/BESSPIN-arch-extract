#!/bin/bash
set -e

work="$(realpath "$(dirname "$0")")"
config=$(realpath $1)
inst=$2

mkdir -p $work/logs
exec 2>$work/logs/$(date +%Y%m%d-%H%M%S)-$$-$inst.log 1>&2

./driver $config check-config \
    $(grep '^1 ' | sed -e 's/^1 //')
