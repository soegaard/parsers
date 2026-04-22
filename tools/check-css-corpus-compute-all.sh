#!/bin/sh

# Run the computed-style corpus checker in bounded chunks and aggregate the
# per-chunk summaries.

set -eu

corpus_dir="${1:-/tmp/lexers-css-corpus}"
chunk_size="${CHUNK_SIZE:-100}"
selector_groups_per_file="${SELECTOR_GROUPS_PER_FILE:-2}"
memory_limit_mb="${MEMORY_LIMIT_MB:-256}"
progress_every="${PROGRESS_EVERY:-25}"

if [ ! -d "$corpus_dir" ]; then
  printf 'CSS corpus not found at %s; skipping.\n' "$corpus_dir"
  exit 0
fi

total_files=$(find "$corpus_dir" -type f -name '*.css' | wc -l | tr -d ' ')
start_index=0
total_selector_groups=0
total_parse_failures=0
total_compute_failures=0

printf 'Checking computed styles for %s CSS files in %s using chunks of %s\n' \
  "$total_files" "$corpus_dir" "$chunk_size"

while [ "$start_index" -lt "$total_files" ]; do
  printf '=== chunk %s ===\n' "$start_index"
  output=$(racket tools/check-css-corpus-compute.rkt \
    --memory-limit-mb "$memory_limit_mb" \
    --start-index "$start_index" \
    --max-files "$chunk_size" \
    --progress-every "$progress_every" \
    --max-selector-groups-per-file "$selector_groups_per_file" \
    "$corpus_dir")
  printf '%s\n' "$output"

  summary=$(printf '%s\n' "$output" | awk '/^Computed-style checks:/ { print $3, $7, $10 }' | tail -n 1)
  if [ -z "$summary" ]; then
    printf 'Missing chunk summary for start index %s\n' "$start_index" >&2
    exit 1
  fi

  selector_groups=$(printf '%s\n' "$summary" | awk '{ print $1 }')
  parse_failures=$(printf '%s\n' "$summary" | awk '{ print $2 }')
  compute_failures=$(printf '%s\n' "$summary" | awk '{ print $3 }')

  total_selector_groups=$((total_selector_groups + selector_groups))
  total_parse_failures=$((total_parse_failures + parse_failures))
  total_compute_failures=$((total_compute_failures + compute_failures))
  start_index=$((start_index + chunk_size))
done

printf '=== aggregate ===\n'
printf 'Computed-style checks: %s selector groups ok, %s parse failures, %s compute failures\n' \
  "$total_selector_groups" "$total_parse_failures" "$total_compute_failures"
