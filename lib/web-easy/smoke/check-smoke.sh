#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

labels=(
  "smoke counter"
  "input"
  "checkbox"
  "list"
  "destroy"
  "branch"
  "controls"
  "operators"
  "tab-panel"
  "tab-panel-disabled"
  "parity-hello"
  "parity-counter"
  "parity-dynamic-list"
  "parity-counters"
  "parity-tabs"
  "parity-tabs-dynamic"
  "parity-list"
  "parity-todo"
)

cmds=(
  "$SCRIPT_DIR/run-browser-smoke-compile.sh"
  "$SCRIPT_DIR/run-browser-input-compile.sh"
  "$SCRIPT_DIR/run-browser-checkbox-compile.sh"
  "$SCRIPT_DIR/run-browser-list-compile.sh"
  "$SCRIPT_DIR/run-browser-destroy-compile.sh"
  "$SCRIPT_DIR/run-browser-branch-compile.sh"
  "$SCRIPT_DIR/run-browser-controls-compile.sh"
  "$SCRIPT_DIR/run-browser-operators-compile.sh"
  "$SCRIPT_DIR/run-browser-tab-panel-compile.sh"
  "$SCRIPT_DIR/run-browser-tab-panel-disabled-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-hello-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-counter-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-dynamic-list-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-counters-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-tabs-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-tabs-dynamic-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-list-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-todo-compile.sh"
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
echo "  http://localhost:8000/test-browser-parity-hello.html"
echo "  http://localhost:8000/test-browser-parity-counter.html"
echo "  http://localhost:8000/test-browser-parity-dynamic-list.html"
echo "  http://localhost:8000/test-browser-parity-counters.html"
echo "  http://localhost:8000/test-browser-parity-tabs.html"
echo "  http://localhost:8000/test-browser-parity-tabs-dynamic.html"
echo "  http://localhost:8000/test-browser-parity-list.html"
echo "  http://localhost:8000/test-browser-parity-todo.html"
