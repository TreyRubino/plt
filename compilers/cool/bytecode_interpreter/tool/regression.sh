#!/bin/bash

# @author Trey Rubino
# @date 02/16/2026

ROOT_DIR="$(dirname "$0")/.."

TEST_FILES="$ROOT_DIR/test/files"

TEST_OUT="$ROOT_DIR/test/out"
REF_OUT="$ROOT_DIR/ref/out"

COOLI="$ROOT_DIR/cooli"
COOLC="$ROOT_DIR/ref/coolc"

rm -f "$TEST_OUT"/* "$REF_OUT"/*

for t in "$TEST_FILES"/*.cl; do
  base="$(basename "$t")"
  stem="${base%.*}"

  test_out_tmp="${t%.*}.out"
  : > "$test_out_tmp"
  "$COOLI" "$t" < /dev/null > "$test_out_tmp" 
  if [[ -s "$test_out_tmp" ]]; then
    mv "$test_out_tmp" "$TEST_OUT/$stem.out"
  else
    rm -f "$test_out_tmp"
  fi

  ref_out_tmp="${t%.*}.out"
  : > "$ref_out_tmp"
  "$COOLC" "$t" < /dev/null > "$ref_out_tmp"
  if [[ -s "$ref_out_tmp" ]]; then
    mv "$ref_out_tmp" "$REF_OUT/$stem.out"
  else
    rm -f "$ref_out_tmp"
  fi
done