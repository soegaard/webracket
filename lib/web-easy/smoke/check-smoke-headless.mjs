#!/usr/bin/env node

import { writeFile } from "node:fs/promises";
import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8765";
  const timeoutRaw = process.env.SMOKE_TIMEOUT_MS || "300000";
  const parsedTimeoutMs = Number(timeoutRaw);
  const timeoutMs = Number.isFinite(parsedTimeoutMs) && parsedTimeoutMs > 0
    ? parsedTimeoutMs
    : 300000;
  const warnRaw = process.env.SMOKE_WARN_MS || "2000";
  const parsedWarnMs = Number(warnRaw);
  const warnMs = Number.isFinite(parsedWarnMs) && parsedWarnMs > 0
    ? parsedWarnMs
    : 2000;
  const timingOut = process.env.SMOKE_TIMING_OUT || "/tmp/web-easy-smoke-timings.tsv";
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";

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

  try {
    console.log(`smoke headless timeout: ${timeoutMs}ms`);
    await page.goto(`${baseUrl}/test-browser-dashboard.html`, { waitUntil: "domcontentloaded" });

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

    const { ok, text } = await result.jsonValue();
    console.log(text);

    const rows = await page.$$eval("#results tbody tr", (trs) =>
      trs.map((tr) => {
        const tds = Array.from(tr.querySelectorAll("td"));
        const durationText = tds[3]?.textContent?.trim() || "";
        const durationMs = Number(durationText);
        return {
          page: tds[0]?.textContent?.trim() || "",
          status: tds[1]?.textContent?.trim() || "",
          message: tds[2]?.textContent?.trim() || "",
          durationMs: Number.isFinite(durationMs) ? durationMs : -1,
        };
      })
    );

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

    const failed = rows.filter((r) => r.status === "FAIL");
    if (failed.length > 0) {
      console.log("Failed pages:");
      for (const f of failed) {
        const timing = f.durationMs >= 0 ? ` (${f.durationMs}ms)` : "";
        console.log(`- ${f.page}${timing}: ${f.message}`);
      }
    }

    await browser.close();
    process.exit(ok ? 0 : 1);
  } catch (err) {
    await browser.close();
    console.error("Headless smoke run failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
