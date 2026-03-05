#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

labels=(
  "smoke-all capsules (smoke/input/checkbox/list/branch/destroy/controls/width/theme-vars/theme-dialog-vars/theme-menu-vars/theme-token-contract/menu-keys/menu-full/group/collapse/accordion/dialog/operators/tab-panel/tab-panel-disabled/tab-panel-dynamic)"
  "visual-check"
  "parity-all capsules (hello/counter/dynamic-list/counters/tabs/tabs-disabled/tabs-dynamic/profile/settings/table/theme-vars/theme-dialog-vars/theme-menu-vars/menu-keys/menu-full/dialog/list/todo/incident/release/workspace)"
)

cmds=(
  "$SCRIPT_DIR/run-browser-smoke-all-compile.sh"
  "$SCRIPT_DIR/run-browser-visual-check-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-all-compile.sh"
)

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

pids=()
logs=()

for i in "${!cmds[@]}"; do
  log="$tmp_dir/$i.log"
  logs+=("$log")
  echo "[start $((i + 1))/${#cmds[@]}] compile ${labels[$i]}"
  ("${cmds[$i]}") >"$log" 2>&1 &
  pids+=("$!")
done

failed=0
for i in "${!pids[@]}"; do
  if wait "${pids[$i]}"; then
    echo "[ ok  $((i + 1))/${#pids[@]}] ${labels[$i]}"
  else
    echo "[FAIL $((i + 1))/${#pids[@]}] ${labels[$i]}"
    cat "${logs[$i]}"
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  echo
  echo "Smoke compile failed."
  exit 1
fi

echo
echo "All smoke examples compiled."
echo "Serve this directory and open these pages:"
echo "  http://localhost:8000/test-browser-contract-dashboard.html"
echo "  http://localhost:8000/test-browser-theme-contract-dashboard.html"
echo "  http://localhost:8000/test-browser-smoke.html"
echo "  http://localhost:8000/test-browser-group.html"
echo "  http://localhost:8000/test-browser-collapse.html"
echo "  http://localhost:8000/test-browser-accordion.html"
echo "  http://localhost:8000/test-browser-dialog.html"
echo "  http://localhost:8000/test-browser-menu-keys.html"
echo "  http://localhost:8000/test-browser-menu-full.html"
echo "  http://localhost:8000/test-browser-a11y-contract.html"
echo "  http://localhost:8000/test-browser-style-hook-contract.html"
echo "  http://localhost:8000/test-browser-keyboard-contract.html"
echo "  http://localhost:8000/test-browser-focus-order.html"
echo "  http://localhost:8000/test-browser-disabled-contract.html"
echo "  http://localhost:8000/test-browser-dialog-contract.html"
echo "  http://localhost:8000/test-browser-menu-single-open-contract.html"
echo "  http://localhost:8000/test-browser-menu-roving-focus-contract.html"
echo "  http://localhost:8000/test-browser-menu-close-reason-contract.html"
echo "  http://localhost:8000/test-browser-dialog-close-reason-contract.html"
echo "  http://localhost:8000/test-browser-tab-close-style-contract.html"
echo "  http://localhost:8000/test-browser-menu-typeahead-contract.html"
echo "  http://localhost:8000/test-browser-tab-aria-linkage-contract.html"
echo "  http://localhost:8000/test-browser-menu-aria-state-contract.html"
echo "  http://localhost:8000/test-browser-width.html"
echo "  http://localhost:8000/test-browser-theme-vars.html"
echo "  http://localhost:8000/test-browser-theme-dialog-vars.html"
echo "  http://localhost:8000/test-browser-theme-menu-vars.html"
echo "  http://localhost:8000/test-browser-theme-token-contract.html"
echo "  http://localhost:8000/test-browser-input.html"
echo "  http://localhost:8000/test-browser-checkbox.html"
echo "  http://localhost:8000/test-browser-list.html"
echo "  http://localhost:8000/test-browser-destroy.html"
echo "  http://localhost:8000/test-browser-branch.html"
echo "  http://localhost:8000/test-browser-controls.html"
echo "  http://localhost:8000/test-browser-operators.html"
echo "  http://localhost:8000/test-browser-tab-panel.html"
echo "  http://localhost:8000/test-browser-tab-panel-keys.html"
echo "  http://localhost:8000/test-browser-tab-panel-disabled.html"
echo "  http://localhost:8000/test-browser-tab-panel-dynamic.html"
echo "  http://localhost:8000/test-browser-visual-check.html"
echo "  http://localhost:8000/test-browser-parity-hello.html"
echo "  http://localhost:8000/test-browser-parity-counter.html"
echo "  http://localhost:8000/test-browser-parity-dynamic-list.html"
echo "  http://localhost:8000/test-browser-parity-counters.html"
echo "  http://localhost:8000/test-browser-parity-tabs.html"
echo "  http://localhost:8000/test-browser-parity-tabs-dynamic.html"
echo "  http://localhost:8000/test-browser-parity-profile.html"
echo "  http://localhost:8000/test-browser-parity-settings.html"
echo "  http://localhost:8000/test-browser-parity-table.html"
echo "  http://localhost:8000/test-browser-parity-theme-vars.html"
echo "  http://localhost:8000/test-browser-parity-theme-dialog-vars.html"
echo "  http://localhost:8000/test-browser-parity-theme-menu-vars.html"
echo "  http://localhost:8000/test-browser-parity-menu-keys.html"
echo "  http://localhost:8000/test-browser-parity-menu-full.html"
echo "  http://localhost:8000/test-browser-parity-dialog.html"
echo "  http://localhost:8000/test-browser-parity-a11y-contract.html"
echo "  http://localhost:8000/test-browser-parity-keyboard-contract.html"
echo "  http://localhost:8000/test-browser-parity-focus-order.html"
echo "  http://localhost:8000/test-browser-parity-disabled-contract.html"
echo "  http://localhost:8000/test-browser-parity-dialog-contract.html"
echo "  http://localhost:8000/test-browser-parity-menu-single-open-contract.html"
echo "  http://localhost:8000/test-browser-parity-menu-roving-focus-contract.html"
echo "  http://localhost:8000/test-browser-parity-menu-close-reason-contract.html"
echo "  http://localhost:8000/test-browser-parity-dialog-close-reason-contract.html"
echo "  http://localhost:8000/test-browser-parity-tab-close-style-contract.html"
echo "  http://localhost:8000/test-browser-parity-menu-typeahead-contract.html"
echo "  http://localhost:8000/test-browser-parity-tab-aria-linkage-contract.html"
echo "  http://localhost:8000/test-browser-parity-menu-aria-state-contract.html"
echo "  http://localhost:8000/test-browser-parity-list.html"
echo "  http://localhost:8000/test-browser-parity-todo.html"
echo "  http://localhost:8000/test-browser-parity-incident.html"
echo "  http://localhost:8000/test-browser-parity-release.html"
echo "  http://localhost:8000/test-browser-parity-workspace.html"
