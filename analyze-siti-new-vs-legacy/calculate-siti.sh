#!/usr/bin/env bash

set -e

pip3 install scenecut-extractor
pip3 install siti

cd "$(dirname "$0")" || exit 1

parallel --eta 'scenecut_extractor -of csv -o frames {} > {.}-scene_cuts.csv' ::: CosmosLaundromat_2048x858_24fps_SDR.mp4 Meridian_3840x2160_5994fps_SDR.mp4 Sparks_4096x2160_5994fps_SDR.mp4
parallel --eta 'siti {} -f -of csv -n 3000 > {.}-siti_legacy.csv' ::: *.mp4
parallel --eta 'python3 -m siti_tools {} --color-range full --num-frames 3000 -f csv > {.}-siti.csv' ::: *SDR.mp4
parallel --eta 'python3 -m siti_tools {} --hdr-mode hdr10 --bit-depth 10 --color-range full --num-frames 3000 -f csv > {.}-siti.csv' ::: *HDR10.mp4

for f in *SDR.mp4; do
  echo "$f"
  ffmpeg -loglevel error -hide_banner -i "$f" \
    -filter:v "signalstats,metadata=mode=print:file=-" \
    -frames:v 3000 -f null /dev/null 2>/dev/null | \
    grep -E -v '^frame' | grep 'YAVG' | cut -d '=' -f 2 \
    > "${f%%.mp4}-YAVG.csv"
done
