#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

labels=(
  "smoke-all capsules (smoke/input/checkbox/list/branch/destroy/controls/width/menu-keys/group/operators/tab-panel/tab-panel-disabled)"
  "visual-check"
  "parity-all capsules (hello/counter/dynamic-list/counters/tabs/tabs-dynamic/profile/settings/table/menu-keys/list/todo/incident/release)"
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
echo "  http://localhost:8000/test-browser-smoke.html"
echo "  http://localhost:8000/test-browser-group.html"
echo "  http://localhost:8000/test-browser-menu-keys.html"
echo "  http://localhost:8000/test-browser-width.html"
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
echo "  http://localhost:8000/test-browser-parity-menu-keys.html"
echo "  http://localhost:8000/test-browser-parity-list.html"
echo "  http://localhost:8000/test-browser-parity-todo.html"
echo "  http://localhost:8000/test-browser-parity-incident.html"
echo "  http://localhost:8000/test-browser-parity-release.html"
