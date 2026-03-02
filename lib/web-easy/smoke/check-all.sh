#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HEADLESS=0

for arg in "$@"; do
  case "$arg" in
    --headless) HEADLESS=1 ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--headless]"
      exit 2
      ;;
  esac
done

if [ "$HEADLESS" -eq 0 ]; then
  echo "Headless step skipped. Use --headless to run browser dashboard automation."
  echo
fi

echo "[1/3] core tests (real Racket)"
racket "$ROOT_DIR/lib/web-easy/test-web-easy.rkt"

echo "[2/3] core tests (webracket -r)"
WEBRACKET_OUT="$(cd "$ROOT_DIR/lib/web-easy" && racket ../../webracket.rkt -r test-web-easy-run.rkt)"
if [ -n "$WEBRACKET_OUT" ] && [ "$WEBRACKET_OUT" != "#<void>" ]; then
  printf '%s\n' "$WEBRACKET_OUT"
fi
echo "web-easy webracket tests passed"

echo "[3/3] smoke compile"
"$SCRIPT_DIR/check-smoke.sh"

if [ "$HEADLESS" -eq 1 ]; then
  echo "[4/4] smoke headless"
  PORT="${SMOKE_PORT:-8765}"
  HOST="${SMOKE_HOST:-localhost}"
  BASE_URL="http://${HOST}:${PORT}"
  LOCAL_TOOLS_DIR="${SMOKE_LOCAL_TOOLS_DIR:-$ROOT_DIR/.local-tools}"

  if ! command -v raco >/dev/null 2>&1; then
    echo "raco is required to serve smoke files via static-web."
    exit 2
  fi
  if ! raco static-web --help >/dev/null 2>&1; then
    echo "raco static-web is required to serve smoke files."
    exit 2
  fi

  raco static-web --port "$PORT" --dir "$SCRIPT_DIR" >/tmp/web-easy-smoke-headless.log 2>&1 &
  SERVER_PID=$!
  trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT
  sleep 1

  SMOKE_BASE_URL="$BASE_URL" \
  SMOKE_NODE_MODULES="$LOCAL_TOOLS_DIR/node_modules" \
  node "$SCRIPT_DIR/check-smoke-headless.mjs"
fi
