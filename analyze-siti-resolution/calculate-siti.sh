#!/usr/bin/env bash

set -e

pip3 install siti

cd "$(dirname "$0")" || exit 1

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <path-to-AVT-VQDB-UHD1-segments>"
  exit 1
fi

find "$1" -name '*h264.mp4' -print0 | parallel --eta --progress -0 'siti --full-range -q -of csv {} > data/{/.}.csv'

# regularize data names
rename 's/_(\d+)kbps_(\d+)p_([\d\.]+)fps_(\w+)/-$1-$2-$3-$4/' data/*
rename 's/LeagueOfLegends-1/LeagueOfLegends/' data/*

echo "Calculation done"
