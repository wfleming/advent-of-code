#!/bin/sh
#
# Fetch the input for a given day. Run from the day dir (e.g. foo/2022/d01 will
# fetch for day 1 of 2022)
# Store value of session cookie from site in .session file next to this script
set -e

if [ -f "$(pwd)/input.txt" ]; then
  echo "Looks like you already have the input for today."
  exit 1
fi

script_path="$(dirname "$(readlink -f "$0")")"

if [ ! -e "$script_path/.session" ]; then
  echo "Session data missing"
  exit 1
fi
session_key="$(cat "$script_path/.session")"

year=$(basename "$(readlink -f "$(pwd)/..")")
day=$(echo "$(basename "$(pwd)" | grep --only-matching '[0-9]\+')" | sed 's/^0//')

if ! echo "$year" | grep --quiet '^[0-9]\+$' && ! echo "$day" | grep --quiet '^[0-9]\+$'; then
  echo "Couldn't find year and day to fetch input for. Got year=$year day=$day"
  exit 1
fi

url="https://adventofcode.com/$year/day/$day/input"

curl -o input.txt --user-agent 'fetch_input.sh <will@flemi.ng>' --cookie "session=$session_key" "$url"

echo "Done."
