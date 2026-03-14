#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

files=(
  "define-key-negative/include-lib-not-top-level.rkt"
  "define-key-negative/call-key-missing-value.rkt"
  "define-key-negative/unknown-keyword.rkt"
  "define-key-negative/duplicate-keyword.rkt"
  "define-key-negative/constructor-unknown-keyword.rkt"
  "define-key-negative/constructor-duplicate-keyword.rkt"
  "define-key-negative/missing-keyword-value.rkt"
  "define-key-negative/missing-required-keyword.rkt"
  "define-key-negative/wrong-positional-arity.rkt"
  "define-key-negative/removed-wrapper-api.rkt"
)

for f in "${files[@]}"; do
  if racket "$f" >/dev/null 2>&1; then
    echo "FAIL (unexpected success): $f"
    exit 1
  fi
  echo "PASS (expected failure): $f"
done

echo "PASS define/key negative cases"
