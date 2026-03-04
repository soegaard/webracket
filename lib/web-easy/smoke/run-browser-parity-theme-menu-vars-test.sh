#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-parity-all-compile.sh"

echo
echo "browser parity theme menu-vars runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-parity-theme-menu-vars.html"
echo "4. Expect: PASS (parity menu-bar background/border follow CSS custom properties)"
