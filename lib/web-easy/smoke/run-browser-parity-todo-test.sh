#!/usr/bin/env bash
set -euo pipefail

# Build browser parity todo artifacts and print runtime test instructions.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-parity-todo-compile.sh"

echo
echo "browser parity todo runtime: manual steps"
echo "1. cd \"$SCRIPT_DIR\""
echo "2. raco static-web --port 8000 --dir ."
echo "3. Open http://localhost:8000/test-browser-parity-todo.html"
echo "4. Expect: PASS (add, edit/cancel, edit/save, toggle, mark-all-done, clear-done)"
