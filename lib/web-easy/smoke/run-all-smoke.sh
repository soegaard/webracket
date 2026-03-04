#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/run-browser-smoke-test.sh"
echo
"$SCRIPT_DIR/run-browser-dialog-test.sh"
echo
"$SCRIPT_DIR/run-browser-input-test.sh"
echo
"$SCRIPT_DIR/run-browser-checkbox-test.sh"
echo
"$SCRIPT_DIR/run-browser-list-test.sh"
echo
"$SCRIPT_DIR/run-browser-destroy-test.sh"
echo
"$SCRIPT_DIR/run-browser-branch-test.sh"
echo
"$SCRIPT_DIR/run-browser-controls-test.sh"
echo
"$SCRIPT_DIR/run-browser-operators-test.sh"
echo
"$SCRIPT_DIR/run-browser-tab-panel-test.sh"
echo
"$SCRIPT_DIR/run-browser-tab-panel-keys-test.sh"
echo
"$SCRIPT_DIR/run-browser-tab-panel-disabled-test.sh"
echo
"$SCRIPT_DIR/run-browser-theme-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-theme-dialog-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-theme-menu-vars-test.sh"
echo
"$SCRIPT_DIR/run-browser-theme-token-contract-test.sh"
