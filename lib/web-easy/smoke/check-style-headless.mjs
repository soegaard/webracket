#!/usr/bin/env node

import { writeFile } from "node:fs/promises";
import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";

async function resolvePlaywright(localNodeModules) {
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
  return playwrightPkg;
}

async function runOne(page, baseUrl, timeoutMs, testPage) {
  await page.goto(`${baseUrl}/${testPage}`, { waitUntil: "domcontentloaded" });

  const status = page.locator("#status");
  await status.waitFor({ timeout: timeoutMs });
  const start = Date.now();

  const result = await page.waitForFunction(
    () => {
      const el = document.getElementById("status");
      if (!el) return null;
      const text = (el.textContent || "").trim();
      if (text.startsWith("PASS")) return { ok: true, text };
      if (text.startsWith("FAIL")) return { ok: false, text };
      return null;
    },
    { timeout: timeoutMs }
  );
  const durationMs = Date.now() - start;
  const { ok, text } = await result.jsonValue();
  return { page: testPage, ok, text, durationMs };
}

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8765";
  const timeoutRaw = process.env.SMOKE_TIMEOUT_MS || "300000";
  const parsedTimeoutMs = Number(timeoutRaw);
  const timeoutMs = Number.isFinite(parsedTimeoutMs) && parsedTimeoutMs > 0
    ? parsedTimeoutMs
    : 300000;
  const warnRaw = process.env.SMOKE_WARN_MS_STYLE || process.env.SMOKE_WARN_MS || "2000";
  const parsedWarnMs = Number(warnRaw);
  const warnMs = Number.isFinite(parsedWarnMs) && parsedWarnMs > 0
    ? parsedWarnMs
    : 2000;
  const timingOut = process.env.SMOKE_TIMING_OUT || "/tmp/web-easy-style-timings.tsv";
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";

  const playwrightPkg = await resolvePlaywright(localNodeModules);
  if (!playwrightPkg) {
    console.error("Playwright is not installed.");
    console.error("Install with one of:");
    console.error("  npm install --save-dev playwright");
    console.error("  npm --prefix .local-tools install --save-dev playwright");
    process.exit(2);
  }

  const tests = [
    "test-browser-style-hook-contract.html",
    "test-browser-parity-style-hook-contract.html",
  ];

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  page.setDefaultTimeout(timeoutMs);
  page.setDefaultNavigationTimeout(timeoutMs);

  try {
    console.log(`style headless timeout: ${timeoutMs}ms`);
    const results = [];
    for (const testPage of tests) {
      const result = await runOne(page, baseUrl, timeoutMs, testPage);
      results.push(result);
      console.log(result.text);
      console.log(`TIMING\t${result.page}\t${result.durationMs}`);
    }

    const timingBody = results.map((r) => `${r.page}\t${r.durationMs}`).join("\n");
    await writeFile(timingOut, `page\tduration_ms\n${timingBody}\n`);
    console.log(`Timing TSV written: ${timingOut}`);

    const slow = results.filter((r) => r.durationMs > warnMs);
    if (slow.length > 0) {
      console.log(`Slow page warnings (>${warnMs}ms):`);
      for (const r of slow) {
        console.log(`- ${r.page}: ${r.durationMs}ms`);
      }
    }

    const failed = results.filter((r) => !r.ok);
    if (failed.length > 0) {
      console.log("Failed pages:");
      for (const r of failed) {
        console.log(`- ${r.page} (${r.durationMs}ms): ${r.text.replace(/\s+/g, " ")}`);
      }
      await browser.close();
      process.exit(1);
    }

    console.log(`PASS\n${results.length}/${results.length} style tests passed`);
    await browser.close();
    process.exit(0);
  } catch (err) {
    await browser.close();
    console.error("Headless style run failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
