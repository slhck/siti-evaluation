#!/usr/bin/env bash
# Analyze CRF-based complexity estimate.
#
# Idea from Shahid Satti, see VQEG presentation "Encoding complexity of short video sequences"

set -e

cd "$(dirname "$0")" || exit 1

# add a custom exit handler
cleanup() {
  echo "Exited!"
  # nothing to do
}

trap cleanup EXIT

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <path-to-AVT-VQDB-UHD1-sources>"
  exit 1
fi

srcDir="$1"

echo "Analyzing data from $srcDir"

mkdir -p tmp


echo "Encoding x264"
find "$srcDir" -name '*.mkv' | parallel --jobs 1 --eta --progress 'ffmpeg -n -i {} -c:v libx264 -crf 35 -an ./tmp/{/.}-libx264-crf35.mkv'
# Note: multithreading means this is not much faster than parallel jobs

echo "Encoding VP9"
find "$srcDir" -name '*.mkv' | parallel --jobs 1 --eta --progress 'ffmpeg -n -i {} -c:v libvpx-vp9 -crf 23 -b:v 0 -an ./tmp/{/.}-vp9-crf23.mkv'

echo "Encoded all to ./tmp"

echo "Getting file sizes for each file"

echo "input_file,width,height,duration,framerate,size_in_bytes,encoder,crf_value" > ./data/crf_file_sizes.csv
for f in ./tmp/*.mkv; do
  duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$f")
  width=$(ffprobe -v error -select_streams v:0 -show_entries stream=width -of default=noprint_wrappers=1:nokey=1 "$f")
  height=$(ffprobe -v error -select_streams v:0 -show_entries stream=height -of default=noprint_wrappers=1:nokey=1 "$f")
  framerate=$(ffprobe -v error -select_streams v:0 -show_entries stream=r_frame_rate -of default=noprint_wrappers=1:nokey=1 "$f" | bc -l)
  size=$(stat -c %s "$f")

  # match the CRF value via perl from the filename
  crf_value=$(echo "$f" | perl -ne 'm/crf([0-9]+)/ && print $1')

  # match the encoder via perl from the filename
  encoder=$(echo "$f" | perl -ne 'm/(vp9|libx264).*\.mkv/ && print $1')

  # this is so hacky, but the file names should have been regular, and not mix dashes and underscores
  # https://twitter.com/jennybryan/status/807805087544328192?lang=en
  filename=$(basename "$f" | sed -e 's/-vp9-crf23.mkv//' | sed -e 's/-libx264-crf35.mkv//')
  echo "$filename,$width,$height,$duration,$framerate,$size,$encoder,$crf_value" >> ./data/crf_file_sizes.csv
done
