#!/usr/bin/env bash

set -e

cd "$(dirname "$0")" || exit 1

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <path-to-VQEG-HDTV-videos>"
  exit 1
fi

# to unzip:
# 7z x VQEG_HDTV_1.zip 'vqegHD1/*_original.avi'

parallel --jobs 1 --eta --progress 'ffmpeg -i {} -c:v rawvideo -pix_fmt yuv420p {.}-420p.avi' ::: "$1"/*.avi

parallel --tag --eta 'siti-tools --color-range=full -f csv {} > {.}-siti.csv' ::: "$1/"*420p.avi

cp "$1"/*-siti.csv data/
