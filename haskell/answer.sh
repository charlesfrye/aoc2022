#!/bin/bash
set -euo pipefail

export DAY=00

# test
stack --verbosity error exec aoc$DAY-exe
# real inputs
stack --verbosity error exec aoc$DAY-exe
