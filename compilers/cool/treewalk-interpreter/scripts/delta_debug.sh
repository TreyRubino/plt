#!/bin/bash

# @author Trey Rubino

ROOT_DIR="$(dirname "$0")/.."
REGRESSION="$ROOT_DIR/scripts/regression.sh"
DELTA="$ROOT_DIR/scripts/delta.sh"

TEST_CASES="$ROOT_DIR/$1"

"$REGRESSION" $1
"$DELTA" $2

