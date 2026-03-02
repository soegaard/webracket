#!/usr/bin/env bash
set -euo pipefail

# Build browser operators smoke artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-operators-compile.sh"

echo
echo "browser operators smoke runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-operators.html"
echo "4. Expect: PASS (count 0->1->2; even 0->0->2)"
