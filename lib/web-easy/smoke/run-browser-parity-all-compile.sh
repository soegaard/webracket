#!/usr/bin/env bash
set -euo pipefail

# Compile the parity-all driver with isolated parity capsules.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/compile-browser-example.sh" "example-browser-parity-all.rkt" "browser parity-all compile: ok"
