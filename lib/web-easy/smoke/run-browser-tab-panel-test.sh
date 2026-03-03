#!/usr/bin/env bash
set -euo pipefail

# Build browser tab-panel smoke artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-smoke-all-compile.sh"

echo
echo "browser tab-panel smoke runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-tab-panel.html"
echo "4. Expect: PASS (click Settings, ArrowRight, Home)"
