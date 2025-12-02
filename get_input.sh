#!/usr/bin/env bash
# aoc_input.sh — fetch AoC input and save to inputs/NN/input.txt
# Usage:
#   ./aoc_input.sh DAY [YEAR]
#   # defaults YEAR to current year if omitted
#

set -euo pipefail

if [ ! -d "inputs" ]; then
    # Directory does not exist, so create it
    mkdir inputs
    echo "Directory inputs/ created."
fi
if [ ! -d "samples" ]; then
    # Directory does not exist, so create it
    mkdir samples
    echo "Directory samples/ created."
fi

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 DAY [YEAR]" >&2
  exit 1
fi

DAY="$1"
# YEAR="${2:-$(date +%Y)}"
YEAR="2025"
DAY_PAD=$(printf "day%02d" "$DAY")

# Get your session token from the AoC site (browser cookie named 'session')
# Provide it via env var AOC_SESSION or a file named .aoc_session in this dir.
SESSION="${AOC_SESSION:-}"
if [[ -z "${SESSION}" && -f ".aoc_session" ]]; then
  SESSION="$(< .aoc_session)"
fi
if [[ -z "${SESSION}" ]]; then
  echo "Missing session token. Set AOC_SESSION or create a .aoc_session file." >&2
  exit 1
fi

URL="https://adventofcode.com/${YEAR}/day/${DAY}/input"
DEST_DIR="inputs"
DEST="${DEST_DIR}/${DAY_PAD}.txt"

# mkdir -p "${DEST_DIR}"

curl -fsS "${URL}" \
  -H "Cookie: session=${SESSION}" \
  -H "User-Agent: aoc-input-script (+https://adventofcode.com)" \
  -o "${DEST}"

echo "Saved: ${DEST}"
