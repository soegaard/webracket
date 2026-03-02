#!/usr/bin/env bash
set -euo pipefail

# Compile the parity menu keyboard smoke example using WebRacket + FFIs.
ROOT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
WEB_EASY_DIR="$ROOT_DIR/lib/web-easy"

cd "$WEB_EASY_DIR/smoke"
racket ../../../webracket.rkt --browser --ffi dom --ffi standard example-browser-parity-menu-keys.rkt

echo "browser parity menu-keys smoke compile: ok"
