#!/usr/bin/env bash
#
# Calculate VCA on El Fuente dataset

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <dataset>"
    exit 1
fi

if ! command -v vca; then
    echo "vca not found"
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

echo "Calculating VCA on $dataset with $(wc -l <<< "$files") files"

mkdir -p data/vca

# record the execution time
start_time=$(date +%s)

# process files in parallel
parallel \
  --eta --progress --bar \
  --joblog "logs/vca-jobs.log" \
  "vca --input {} --complexity-csv data/vca/{/.}-complexity.csv" ::: "$files"

end_time=$(date +%s)
echo "Execution time: $((end_time - start_time)) seconds"

echo "Done"
