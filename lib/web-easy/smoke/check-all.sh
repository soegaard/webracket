#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HEADLESS=0
CONTRACT_FIRST=0

for arg in "$@"; do
  case "$arg" in
    --headless) HEADLESS=1 ;;
    --contract-first) CONTRACT_FIRST=1 ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--headless] [--contract-first]"
      exit 2
      ;;
  esac
done

if [ "$CONTRACT_FIRST" -eq 1 ] && [ "$HEADLESS" -eq 0 ]; then
  echo "--contract-first requires --headless."
  echo "Usage: $0 [--headless] [--contract-first]"
  exit 2
fi

if [ "$HEADLESS" -eq 0 ]; then
  echo "Headless step skipped. Use --headless to run browser dashboard automation."
  echo
fi

if [ "$CONTRACT_FIRST" -eq 1 ]; then
  echo "[0/4] contract headless"
  "$SCRIPT_DIR/headless.sh" contract
  echo
fi

echo "[1/3] core tests (real Racket)"
racket "$ROOT_DIR/lib/web-easy/test/test-web-easy.rkt"

echo "[2/3] core tests (webracket -r)"
WEBRACKET_OUT="$(cd "$ROOT_DIR/lib/web-easy/test" && racket ../../../webracket.rkt -r test-web-easy-run.rkt)"
if [ -n "$WEBRACKET_OUT" ] && [ "$WEBRACKET_OUT" != "#<void>" ]; then
  printf '%s\n' "$WEBRACKET_OUT"
fi
echo "web-easy webracket tests passed"

echo "[3/3] smoke compile"
"$SCRIPT_DIR/check-smoke.sh"

if [ "$HEADLESS" -eq 1 ]; then
  echo "[4/4] smoke headless"
  SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-smoke-headless.sh"
fi
