#!/bin/sh
echo "Answer on test:"
npx ts-node main.ts test.txt

echo "Answer on input:"
npx ts-node main.ts input.txt