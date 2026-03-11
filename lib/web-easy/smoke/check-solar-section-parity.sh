#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/3] capture section screenshots + computed metrics"
"$SCRIPT_DIR/check-solar-sections-diff.sh"

echo "[2/3] summarize per-section deltas (includes RMSE)"
node "$SCRIPT_DIR/check-solar-polish-summary.mjs"

echo "[3/3] enforce section RMSE thresholds"
node -e '
const fs = require("node:fs");
const path = require("node:path");
const { execFileSync } = require("node:child_process");

const cwd = process.cwd();
const reportPath = path.resolve(cwd, "generated/solar-section-diff/report.json");
const report = JSON.parse(fs.readFileSync(reportPath, "utf8"));

const thresholds = {
  indicators: 0.40,
  cards:      0.22,
  accordions: 0.22,
  dialogs:    0.20
};

const rmseFor = (section) => {
  const generated = path.resolve(cwd, `generated/solar-section-diff/${section}-generated.png`);
  const reference = path.resolve(cwd, `generated/solar-section-diff/${section}-reference.png`);
  try {
    execFileSync("compare", ["-metric", "RMSE", generated, reference, "null:"], { stdio: ["ignore", "pipe", "pipe"] });
    return 0;
  } catch (e) {
    const stderr = String(e.stderr || "").trim();
    const m = stderr.match(/\(([\d.]+)\)/);
    return m ? Number(m[1]) : NaN;
  }
};

const failures = [];
for (const section of Object.keys(thresholds)) {
  const exists = report.some((r) => r.section === section && !r.error);
  if (!exists) {
    failures.push(`${section}: missing section data in report`);
    continue;
  }
  const rmse = rmseFor(section);
  const max = thresholds[section];
  if (!Number.isFinite(rmse)) {
    failures.push(`${section}: RMSE unavailable`);
  } else if (rmse > max) {
    failures.push(`${section}: rmse ${rmse.toFixed(5)} > ${max.toFixed(5)}`);
  } else {
    console.log(`${section}: PASS (rmse=${rmse.toFixed(5)} <= ${max.toFixed(5)})`);
  }
}

if (failures.length > 0) {
  console.error("FAIL");
  for (const f of failures) console.error(f);
  process.exit(1);
}
console.log("PASS");
console.log("section RMSE thresholds satisfied");
'
