#!/usr/bin/env bash
set -euo pipefail

# Build browser theme-vars smoke artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-smoke-all-compile.sh"

echo
echo "browser theme-vars runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-theme-vars.html"
echo "4. Expect: PASS (tab selected styles follow CSS custom properties)"
