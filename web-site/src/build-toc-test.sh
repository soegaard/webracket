#!/bin/bash
set -euo pipefail

echo "-- Compiling test-toc.rkt --"
racket ../../webracket.rkt --browser --ffi dom --ffi standard --ffi js --stdlib test-toc.rkt

echo "-- Copying Assets --"
cp test-toc.wasm ../public
cp test-toc.html ../public/test-toc.html

echo "-- Done --"
if [[ "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff
fi
