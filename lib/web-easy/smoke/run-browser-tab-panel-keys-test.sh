#!/usr/bin/env bash
set -euo pipefail

# Build browser tab-panel artifacts and print keyboard-only runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-tab-panel-compile.sh"

echo
echo "browser tab-panel keyboard smoke runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-tab-panel-keys.html"
echo "4. Expect: PASS (keyboard-only: ArrowRight/ArrowRight/Home/End + wrap)"
