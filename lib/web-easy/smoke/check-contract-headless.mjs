#!/usr/bin/env node

import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";

function parseTimingBaseline(tsv) {
  const rows = tsv.split(/\r?\n/).slice(1).filter(Boolean);
  const out = new Map();
  for (const row of rows) {
    const parts = row.split("\t");
    if (parts.length < 2) continue;
    const page = parts[0].trim();
    const ms = Number(parts[1]);
    if (page && Number.isFinite(ms) && ms >= 0) {
      out.set(page, ms);
    }
  }
  return out;
}

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8765";
  const timeoutRaw = process.env.SMOKE_TIMEOUT_MS || "600000";
  const parsedTimeoutMs = Number(timeoutRaw);
  const timeoutMs = Number.isFinite(parsedTimeoutMs) && parsedTimeoutMs > 0
    ? parsedTimeoutMs
    : 600000;
  const warnRaw = process.env.SMOKE_WARN_MS_CONTRACT || process.env.SMOKE_WARN_MS || "2000";
  const parsedWarnMs = Number(warnRaw);
  const warnMs = Number.isFinite(parsedWarnMs) && parsedWarnMs > 0
    ? parsedWarnMs
    : 2000;
  const timingOut = process.env.SMOKE_TIMING_OUT || "/tmp/web-easy-contract-timings.tsv";
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";
  const regressionPctRaw = process.env.SMOKE_TIMING_REGRESSION_PCT || "150";
  const regressionPct = Number.isFinite(Number(regressionPctRaw))
    ? Number(regressionPctRaw)
    : 150;
  const baselinePath = process.env.SMOKE_TIMING_BASELINE ||
    path.join(path.dirname(new URL(import.meta.url).pathname), "contract-timings-baseline.tsv");

  let baseline = new Map();
  try {
    const baselineRaw = await readFile(baselinePath, "utf8");
    baseline = parseTimingBaseline(baselineRaw);
    if (baseline.size > 0) {
      console.log(`Loaded timing baseline: ${baselinePath}`);
    }
  } catch (_err) {
    // Optional baseline file.
  }

  let playwrightPkg = null;
  const requireHere = createRequire(import.meta.url);
  try {
    playwrightPkg = requireHere("playwright");
  } catch (_err) {
    if (localNodeModules) {
      try {
        const requireFromLocal = createRequire(path.join(localNodeModules, "_resolver.js"));
        playwrightPkg = requireFromLocal("playwright");
      } catch (_err2) {
        playwrightPkg = null;
      }
    }
  }

  if (!playwrightPkg) {
    console.error("Playwright is not installed.");
    console.error("Install with one of:");
    console.error("  npm install --save-dev playwright");
    console.error("  npm --prefix .local-tools install --save-dev playwright");
    process.exit(2);
  }

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  page.setDefaultTimeout(timeoutMs);
  page.setDefaultNavigationTimeout(timeoutMs);

  async function runDashboard(dashboardPath) {
    await page.goto(`${baseUrl}/${dashboardPath}`, { waitUntil: "domcontentloaded" });

    const summary = page.locator("#summary");
    await summary.waitFor({ timeout: timeoutMs });

    const result = await page.waitForFunction(
      () => {
        const el = document.getElementById("summary");
        if (!el) return null;
        const text = (el.textContent || "").trim();
        if (text.startsWith("PASS")) return { ok: true, text };
        if (text.startsWith("FAIL")) return { ok: false, text };
        return null;
      },
      { timeout: timeoutMs }
    );

    const summaryObj = await result.jsonValue();
    const rows = await page.$$eval("#results tbody tr", (trs, dash) =>
      trs.map((tr) => {
        const tds = Array.from(tr.querySelectorAll("td"));
        const durationText = tds[3]?.textContent?.trim() || "";
        const durationMs = Number(durationText);
        return {
          page: tds[0]?.textContent?.trim() || "",
          status: tds[1]?.textContent?.trim() || "",
          message: tds[2]?.textContent?.trim() || "",
          durationMs: Number.isFinite(durationMs) ? durationMs : -1,
          dashboard: dash,
        };
      })
    , dashboardPath);

    return { summary: summaryObj, rows };
  }

  try {
    console.log(`contract headless timeout: ${timeoutMs}ms`);

    const dashboards = [
      "test-browser-contract-dashboard-core.html",
      "test-browser-contract-dashboard-parity.html",
    ];

    const runs = [];
    for (const dash of dashboards) {
      const run = await runDashboard(dash);
      console.log(`[${dash}] ${run.summary.text}`);
      runs.push(run);
      if (!run.summary.ok) {
        break;
      }
    }

    const rows = runs.flatMap((r) => r.rows);
    const withTiming = rows
      .filter((r) => r.durationMs >= 0)
      .sort((a, b) => b.durationMs - a.durationMs);

    if (withTiming.length > 0) {
      const timingBody = withTiming.map((r) => `${r.page}\t${r.durationMs}`).join("\n");
      await writeFile(timingOut, `page\tduration_ms\n${timingBody}\n`);
      console.log(`Timing TSV written: ${timingOut}`);
      console.log("Timing rows (page\\tduration_ms):");
      for (const r of withTiming) {
        console.log(`TIMING\t${r.page}\t${r.durationMs}`);
      }
    }

    if (withTiming.length > 0) {
      console.log("Slowest pages:");
      for (const r of withTiming.slice(0, 5)) {
        console.log(`- ${r.page}: ${r.durationMs}ms`);
      }
    }

    const slow = withTiming.filter((r) => r.durationMs > warnMs);
    if (slow.length > 0) {
      console.log(`Slow page warnings (>${warnMs}ms):`);
      for (const r of slow) {
        console.log(`- ${r.page}: ${r.durationMs}ms`);
      }
    }

    let regressionFailed = false;
    if (baseline.size > 0) {
      const regressions = [];
      for (const row of withTiming) {
        const base = baseline.get(row.page);
        if (!Number.isFinite(base) || base <= 0) continue;
        const limit = base * (1 + regressionPct / 100);
        if (row.durationMs > limit) {
          regressions.push({ page: row.page, base, got: row.durationMs, limit });
        }
      }
      if (regressions.length > 0) {
        regressionFailed = true;
        console.log(`Timing regression failures (> ${regressionPct}% over baseline):`);
        for (const r of regressions) {
          console.log(`- ${r.page}: got ${r.got}ms, baseline ${r.base}ms, limit ${Math.round(r.limit)}ms`);
        }
      }
    }

    const failed = rows.filter((r) => r.status === "FAIL");
    if (failed.length > 0) {
      console.log("Failed pages:");
      for (const f of failed) {
        const timing = f.durationMs >= 0 ? ` (${f.durationMs}ms)` : "";
        console.log(`- ${f.page}${timing}: ${f.message}`);
      }
    }

    const ok = failed.length === 0 && runs.length === dashboards.length && runs.every((r) => r.summary.ok) && !regressionFailed;
    if (ok) {
      const passed = rows.filter((r) => r.status === "PASS").length;
      const total = rows.length;
      console.log(`PASS\n${passed}/${total} contract tests passed`);
    }

    await browser.close();
    process.exit(ok ? 0 : 1);
  } catch (err) {
    await browser.close();
    console.error("Headless contract run failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
