#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

echo "Answer on Test:"
./target/debug/main test.txt

echo "Answer:"
./target/debug/main input.txt
