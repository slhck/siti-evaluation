#!/usr/bin/env bash
#
# Calculate SI/TI on El Fuente dataset

if [ $# -ne 1 ]; then
    echo "Usage: $0 <dataset>"
    exit 1
fi

if ! command -v siti-tools; then
    echo "siti-tools not found"
    exit 1
fi

if ! command -v parallel; then
    echo "parallel not found"
    exit 1
fi

dataset="$1"
shift

cd "$(dirname "$0")" || exit 1

files=$(find "$dataset" -name "*.y4m")

echo "Calculating SI/TI on $dataset with $(wc -l <<< "$files") files"

rm -f "siti-jobs.log"
mkdir -p data/siti

# record the execution time
start_time=$(date +%s)

# process files in parallel
parallel \
  --eta --progress --bar \
  --joblog "siti-jobs.log" \
  "siti-tools {} -f json -r full > data/siti/{/.}.json" ::: "$files"

end_time=$(date +%s)
echo "Execution time: $((end_time - start_time)) seconds"

echo "Done"

echo "Converting logs"
parallel "./convert-json-to-csv.py {} {.}.csv" ::: data/siti/*.json

echo "Done"

