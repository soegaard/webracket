#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-parity-hello-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-counter-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-dynamic-list-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-counters-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-tabs-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-tabs-dynamic-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-list-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-todo-test.sh"
