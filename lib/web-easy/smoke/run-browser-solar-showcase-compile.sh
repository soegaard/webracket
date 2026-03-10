#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/compile-browser-example.sh" \
  example-browser-solar-showcase.rkt \
  "browser solar showcase compile: ok"
