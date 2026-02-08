#!/bin/bash
echo "-- Compiling web-site.rkt --"
racket ../../webracket.rkt --browser --ffi dom --ffi standard --stdlib web-site.rkt

echo "-- Copying Assets --"
cp web-site.wasm ../public
cp web-site.html ../public/index.html
cp installation.html ../public
cp community.html ../public
cp documentation.html ../public
cp examples.html ../public
cp implementation-status.html ../public
cp mathjax.html ../public
cp formula1.html ../public
cp matrix-rain.html ../public
cp quick-start.html ../public

echo "-- Done --"
if [[ "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff
fi
