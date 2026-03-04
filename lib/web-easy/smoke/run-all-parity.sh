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
"$SCRIPT_DIR/run-browser-parity-profile-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-settings-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-table-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-theme-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-theme-dialog-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-theme-menu-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-menu-keys-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-menu-full-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-dialog-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-list-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-todo-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-incident-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-release-test.sh"
echo
"$SCRIPT_DIR/run-browser-parity-workspace-test.sh"
