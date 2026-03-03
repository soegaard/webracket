#!/usr/bin/env bash
set -euo pipefail

# Compile via the smoke-all capsule driver.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/run-browser-smoke-all-compile.sh"
echo "browser controls smoke compile: ok (via smoke-all)"
