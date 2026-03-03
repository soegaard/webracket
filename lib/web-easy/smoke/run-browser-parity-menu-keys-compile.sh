#!/usr/bin/env bash
set -euo pipefail

# Compile via the parity-all capsule driver.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/run-browser-parity-all-compile.sh"
echo "browser parity menu-keys compile: ok (via parity-all)"
