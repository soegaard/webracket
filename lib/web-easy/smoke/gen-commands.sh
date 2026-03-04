#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUT_FILE="$SCRIPT_DIR/COMMANDS.tsv"

{
  echo -e "scope\tcommand\tdescription"

  while IFS=$'\t' read -r mode desc; do
    case "$mode" in
      list)       cmd="./headless.sh list" ;;
      doctor)     cmd="./headless.sh doctor" ;;
      smoke)      cmd="./headless.sh smoke" ;;
      parity)     cmd="./headless.sh parity" ;;
      contract)   cmd="./headless.sh contract" ;;
      dashboards) cmd="./headless.sh dashboards" ;;
      ci)         cmd="./headless.sh ci" ;;
      ci-lite)    cmd="./headless.sh ci-lite" ;;
      timings)    cmd="./headless.sh timings" ;;
      guard)      cmd="./headless.sh guard" ;;
      all)        cmd="./headless.sh all" ;;
      single)     cmd="./headless.sh single <compile-script> <test-page>" ;;
      *)
        continue
        ;;
    esac
    echo -e "headless\t$cmd\t$desc"
  done < <("$SCRIPT_DIR/headless.sh" list)

  echo -e "smoke\t./smoke.sh headless-run <mode> [args]\tForward to headless dispatcher."
  echo -e "smoke\t./smoke.sh check\tCompile smoke artifacts."
  echo -e "smoke\t./smoke.sh open\tServe smoke pages locally."

  echo -e "make\tmake smoke-ci\tRun canonical local CI headless gate."
  echo -e "make\tmake smoke-headless-lite\tRun contract+smoke+parity+guard without compile."
  echo -e "make\tmake smoke-verify\tRun local headless verify preflight."
  echo -e "make\tmake smoke-quick\tRun smoke-verify + smoke-headless-lite."
  echo -e "make\tmake smoke-release\tRun smoke-commands + smoke-quick + smoke-ci."
  echo -e "make\tmake smoke-smoke\tRun full smoke dashboard headless."
  echo -e "make\tmake smoke-parity\tRun parity-only dashboard headless."
  echo -e "make\tmake smoke-dashboards\tRun contract+smoke dashboards headless."
  echo -e "make\tmake smoke-list\tPrint canonical commands."
  echo -e "make\tmake smoke-commands\tRegenerate COMMANDS.tsv."
  echo -e "make\tmake smoke-one SINGLE_COMPILE=... SINGLE_PAGE=...\tRun one page headless via Make variables."
} > "$OUT_FILE"

echo "Wrote $OUT_FILE"
