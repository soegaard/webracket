#!/bin/bash
echo "-- Compiling web-site.rkt --"
racket ../../webracket.rkt --browser --ffi dom --ffi standard --stdlib web-site.rkt

echo "-- Copying Assets --"
cp web-site.wasm ../public
cp web-site.html ../public
cp installation.html ../public

echo "-- Done --"
if [[ "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff
fi
