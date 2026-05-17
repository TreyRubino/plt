#!/bin/bash

# @author Trey Rubino

ROOT_DIR="$(dirname "$0")/.."
TEST_CASES="$ROOT_DIR/$1"

TEST_FILES="$ROOT_DIR/test/files"
TEST_EXE="$ROOT_DIR/scripts/test_cool.sh"

REF_FILES="$ROOT_DIR/reference/files"
REF_EXE="$ROOT_DIR/reference/cool"

rm -f "$TEST_FILES"/* "$REF_FILES"/*

for t in "$TEST_CASES"/*.cl; do
  base="$(basename "$t")"
  stem="${base%.*}"

  test_out_tmp="${t%.*}.out"
  : > "$test_out_tmp"
  "$TEST_EXE" -p "$t" > "$test_out_tmp"
  mv "${t%.*}".cl-* "$TEST_FILES"/ 2>/dev/null
  if [[ -s "$test_out_tmp" ]]; then
    mv "$test_out_tmp" "$TEST_FILES/$stem.out"
  else
    rm -f "$test_out_tmp"
  fi

  ref_out_tmp="${t%.*}.out"
  : > "$ref_out_tmp"

  stage_out=$("$REF_EXE" --lex "$t" 2>&1)
  if [[ -n "$stage_out" ]]; then
    printf "%s\n" "$stage_out" > "$ref_out_tmp"
  else
    stage_out=$("$REF_EXE" --parse "$t" 2>&1)
    if [[ -n "$stage_out" ]]; then
      printf "%s\n" "$stage_out" > "$ref_out_tmp"
    fi
    #else
    #  stage_out=$("$REF_EXE" --type "$t" 2>&1)
    #  if [[ -n "$stage_out" ]]; then
    #    printf "%s\n" "$stage_out" > "$ref_out_tmp"
    #  else 
    #    stage_out=$("$REF_EXE" "$t" 2>&1)
    #    if [[ -n "$stage_out" ]]; then
    #      printf "%s\n" "$stage_out" > "$ref_out_tmp"
     #   fi
     # fi
    #fi
  fi

  mv "${t%.*}".cl-* "$REF_FILES"/ 2>/dev/null
  if [[ -s "$ref_out_tmp" ]]; then
    mv "$ref_out_tmp" "$REF_FILES/$stem.out"
  else
    rm -f "$ref_out_tmp"
  fi
done
