#!/bin/zsh
set -eu

cd "$(dirname "$0")"

files=(
  "define-key-negative/unknown-keyword.rkt"
  "define-key-negative/duplicate-keyword.rkt"
  "define-key-negative/missing-keyword-value.rkt"
  "define-key-negative/wrong-positional-arity.rkt"
)

for f in "${files[@]}"; do
  if racket "$f" >/dev/null 2>&1; then
    echo "FAIL (unexpected success): $f"
    exit 1
  fi
  echo "PASS (expected failure): $f"
done

echo "PASS define/key negative cases"
