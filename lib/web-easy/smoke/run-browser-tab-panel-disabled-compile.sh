#!/usr/bin/env bash
set -euo pipefail

# Compile the web-easy browser tab-panel-disabled smoke example using WebRacket + FFIs.
ROOT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
WEB_EASY_DIR="$ROOT_DIR/lib/web-easy"

cd "$WEB_EASY_DIR/smoke"
racket ../../../webracket.rkt --browser --ffi dom --ffi standard example-browser-tab-panel-disabled.rkt

echo "browser tab-panel-disabled smoke compile: ok"
