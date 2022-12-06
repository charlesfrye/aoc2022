#!/bin/bash
set -euo pipefail

# test
cat test.txt | stack --verbosity error exec aoc-exe
# real inputs
cat input.txt | stack --verbosity error exec aoc-exe
