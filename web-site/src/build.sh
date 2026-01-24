#!/bin/bash
echo "-- Compiling web-site.rkt --"
racket ../../webracket.rkt --browser --ffi dom --stdlib web-site.rkt

echo "-- Copying Assets --"
cp web-site.wasm ../public
cp web-site.html ../public

echo "-- Done --"
