#!/usr/bin/env bash
set -euo pipefail

# Build browser parity counter artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-parity-counter-compile.sh"

echo
echo "browser parity counter runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-parity-counter.html"
echo "4. Expect: PASS (counter starts at 0 and increments to 1)"
