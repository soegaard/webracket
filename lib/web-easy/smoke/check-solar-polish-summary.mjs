#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { execFileSync } from "node:child_process";

const reportPath = path.resolve(process.cwd(), "generated/solar-section-diff/report.json");
const text = await fs.readFile(reportPath, "utf8");
const report = JSON.parse(text);

function px(v) {
  if (!v) return NaN;
  return Number(String(v).replace("px", ""));
}

const rows = report
  .filter((r) => !r.error && r.generatedMetrics && r.referenceMetrics)
  .map((r) => {
    const g = r.generatedMetrics;
    const ref = r.referenceMetrics;
    return {
      section: r.section,
      fsDelta: px(g.fontSize) - px(ref.fontSize),
      mtDelta: px(g.marginTop) - px(ref.marginTop),
      mbDelta: px(g.marginBottom) - px(ref.marginBottom),
      widthDelta: Number(g.width) - Number(ref.width),
      xDelta: Number(g.x) - Number(ref.x),
      colorMatch: g.color === ref.color
    };
  });

function rmseFor(section) {
  const generated = path.resolve(process.cwd(), `generated/solar-section-diff/${section}-generated.png`);
  const reference = path.resolve(process.cwd(), `generated/solar-section-diff/${section}-reference.png`);
  try {
    execFileSync("compare", ["-metric", "RMSE", generated, reference, "null:"], { stdio: ["ignore", "pipe", "pipe"] });
    return 0;
  } catch (e) {
    const stderr = String(e.stderr || "").trim();
    const m = stderr.match(/\(([\d.]+)\)/);
    if (m) return Number(m[1]);
    return NaN;
  }
}

const pixelRows = rows.map((r) => ({ section: r.section, rmse: rmseFor(r.section) }));

const avg = (xs) => xs.reduce((a, b) => a + b, 0) / (xs.length || 1);

console.log(`sections: ${rows.length}`);
console.log(`avg font-size delta px: ${avg(rows.map((r) => r.fsDelta)).toFixed(2)}`);
console.log(`avg margin-top delta px: ${avg(rows.map((r) => r.mtDelta)).toFixed(2)}`);
console.log(`avg margin-bottom delta px: ${avg(rows.map((r) => r.mbDelta)).toFixed(2)}`);
console.log(`avg width delta px: ${avg(rows.map((r) => r.widthDelta)).toFixed(2)}`);
console.log(`avg x delta px: ${avg(rows.map((r) => r.xDelta)).toFixed(2)}`);
console.log(`color matches: ${rows.filter((r) => r.colorMatch).length}/${rows.length}`);
console.log(`avg pixel RMSE (0..1): ${avg(pixelRows.map((r) => Number.isFinite(r.rmse) ? r.rmse : 1)).toFixed(5)}`);
console.log("");
for (const r of rows) {
  const p = pixelRows.find((x) => x.section === r.section);
  console.log(
    `${r.section}: fs ${r.fsDelta.toFixed(1)} px, mt ${r.mtDelta.toFixed(1)} px, mb ${r.mbDelta.toFixed(1)} px, w ${r.widthDelta.toFixed(1)} px, x ${r.xDelta.toFixed(1)} px, rmse ${(p?.rmse ?? NaN).toFixed(5)}`
  );
}
