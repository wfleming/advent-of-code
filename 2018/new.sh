#!/bin/sh
set -e

## data

makefile_template=$(cat <<EOF
.PHONY: test run

test:
\tcrystal spec

run:
\tcrystal run "src/\${1}.cr"
EOF)

## logic

crystal init app "$1"
printf "%s" "$makefile_template" | envsubst > "$1/Makefile"
rm -rf "$1/.git"
