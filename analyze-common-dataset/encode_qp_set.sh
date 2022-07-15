#!/usr/bin/env bash
#
# Create a set of encoded videos for analysis

set -e


if [ $# -ne 2 ]; then
    echo "Usage: $0 <dataset> <output>"
    exit 1
fi

if ! command -v ffmpeg; then
    echo "ffmpeg not found"
    exit 1
fi

if ! command -v parallel; then
    echo "parallel not found"
    exit 1
fi

dataset="$1"
output="$2"

cd "$(dirname "$0")" || exit 1

files=$(find "$dataset" -name "*.y4m")

echo "Encoding on $dataset with $(wc -l <<< "$files") files"

# qpValues=($(seq 16 3 51))
# resolutions=(360 480 720 1080)
qpValues=(16 18 20 22 24 26 28 30 32 34 36 38 40)
resolutions=(1080)

mkdir -p "$output"
for qp in "${qpValues[@]}"; do
    for resolution in "${resolutions[@]}"; do
      echo "Encoding $resolution@$qp"
      
      suffix="x264-$qp-$resolution"
      parallel \
          --eta --progress --bar \
          --joblog "logs/encode-$suffix-jobs.log" \
          "ffmpeg -y -i {} -c:v libx264 -qp $qp -filter:v scale=-2:$resolution $output/{/.}-$suffix.mkv" ::: "$files"

      
      suffix="x265-$qp-$resolution"
      parallel \
          --eta --progress --bar \
          --joblog "logs/encode-$suffix-jobs.log" \
          "ffmpeg -y -i {} -c:v libx265 -qp $qp -filter:v scale=-2:$resolution $output/{/.}-$suffix.mkv" ::: "$files"

      
      suffix="vp9-$qp-$resolution"
      parallel \
          --eta --progress --bar \
          --joblog "logs/encode-$suffix--jobs.log" \
          "ffmpeg -y -i {} -c:v libvpx-vp9 -crf $qp -b:v 0 -filter:v scale=-2:$resolution $output/{/.}-$suffix.mkv" ::: "$files"

      
      suffix="av1-$qp-$resolution"
      parallel \
          --eta --progress --bar \
          --joblog "logs/encode-$suffix--jobs.log" \
          "ffmpeg -y -i {} -c:v libaom-av1 -crf $qp -b:v 0 -filter:v scale=-2:$resolution $output/{/.}-$suffix.mkv" ::: "$files"
    done
done
