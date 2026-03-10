#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/2] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/2] navbar computed parity sweep"
tmp_report="$(mktemp)"
strict="${SOLAR_NAVBAR_STRICT:-0}"
total_diffs=0
for variant in primary dark light subtle; do
  node "$SCRIPT_DIR/compare-navbars.cjs" "$variant" > "$tmp_report"
  node -e '
    const fs = require("fs");
    const data = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
    const strict = process.argv[2] === "1";
    const rows = Object.values(data.report || {});
    const diffs = rows.reduce((n, xs) => n + (Array.isArray(xs) ? xs.length : 0), 0);
    const a = JSON.stringify(data.content?.bootswatchDropdownItems || []);
    const b = JSON.stringify(data.content?.webEasyDropdownItems || []);
    if (a !== b) {
      console.error(`FAIL variant=${data.variant}: dropdown items mismatch`);
      process.exit(1);
    }
    if (strict && diffs !== 0) {
      console.error(`FAIL variant=${data.variant}: ${diffs} computed-style diffs`);
      process.exit(1);
    }
    process.stdout.write(String(diffs));
  ' "$tmp_report" "$strict" > /tmp/solar-navbar-diffs-count.txt
  variant_diffs="$(cat /tmp/solar-navbar-diffs-count.txt)"
  echo "INFO variant=${variant}: computed-style diffs=${variant_diffs}"
  total_diffs=$((total_diffs + variant_diffs))
done
rm -f "$tmp_report"
rm -f /tmp/solar-navbar-diffs-count.txt

if [ "$strict" = "1" ]; then
  echo "PASS: strict navbar parity"
else
  echo "PASS: navbar parity advisory mode (total diffs=$total_diffs, set SOLAR_NAVBAR_STRICT=1 to fail on deltas)"
fi
