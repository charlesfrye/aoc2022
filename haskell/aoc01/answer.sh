#!/bin/bash
set -euo pipefail

export DAY=01

# test
cat ./test.txt | stack --verbosity error exec aoc$DAY-exe
# real inputs
cat ./input.txt | stack --verbosity error exec aoc$DAY-exe
