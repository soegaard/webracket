#!/bin/bash
echo "-- Compiling web-site.rkt --"
racket ../../webracket.rkt --browser --ffi xtermjs --ffi dom --ffi standard --stdlib web-site.rkt

cp web-site.html xtermjs-demo.html
cp web-site.html minischeme.html

echo "-- Copying Assets --"
cp web-site.wasm ../public
cp web-site.html ../public/index.html
cp installation.html ../public
cp community.html ../public
cp documentation.html ../public
cp documentation-compiler-overview.html ../public
cp documentation-js-ffi.html ../public
cp examples.html ../public
cp implementation-status.html ../public
cp mathjax.html ../public
cp formula1.html ../public
cp matrix-rain.html ../public
cp xtermjs-demo.html ../public
cp minischeme.html ../public
cp space-invaders.html ../public
cp quick-start.html ../public

echo "-- Done --"
if [[ "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff
fi
