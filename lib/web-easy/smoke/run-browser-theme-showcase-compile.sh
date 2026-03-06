#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/compile-browser-example.sh" \
  example-browser-theme-showcase.rkt \
  "browser theme showcase compile: ok"
