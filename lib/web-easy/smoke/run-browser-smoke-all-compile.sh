#!/usr/bin/env bash
set -euo pipefail

# Compile the browser example and place generated artifacts in ./generated.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
"$SCRIPT_DIR/compile-browser-example.sh" "example-browser-smoke-all.rkt" "browser smoke-all compile: ok"
