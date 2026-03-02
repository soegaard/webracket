#!/usr/bin/env bash
set -euo pipefail

# Build browser parity dynamic-tabs artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-parity-tabs-dynamic-compile.sh"

echo
echo "browser parity dynamic-tabs runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-parity-tabs-dynamic.html"
echo "4. Expect: PASS (add faq, remove selected, remove details)"
