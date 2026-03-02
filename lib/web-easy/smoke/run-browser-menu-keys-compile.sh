#!/usr/bin/env bash
set -euo pipefail

# Compile the menu keyboard smoke example using WebRacket + FFIs.
ROOT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
WEB_EASY_DIR="$ROOT_DIR/lib/web-easy"

cd "$WEB_EASY_DIR/smoke"
racket ../../../webracket.rkt --browser --ffi dom --ffi standard example-browser-menu-keys.rkt

echo "browser menu-keys smoke compile: ok"
