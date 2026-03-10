#!/usr/bin/env node

import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8766";
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";
  const timeoutMs = Number(process.env.SMOKE_TIMEOUT_MS || "30000");

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
    process.exit(2);
  }

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  const guard = installPlaywrightErrorGuard(page);

  try {
    const mark = guard.mark();
    await page.goto(`${baseUrl}/test-browser-dashboard-guard.html`, { waitUntil: "domcontentloaded" });
    const summary = page.locator("#summary");
    await summary.waitFor({ timeout: timeoutMs });

    const result = await page.waitForFunction(
      () => {
        const el = document.getElementById("summary");
        if (!el) return null;
        const text = (el.textContent || "").trim();
        if (text.startsWith("PASS") || text.startsWith("FAIL")) {
          return text;
        }
        return null;
      },
      { timeout: timeoutMs }
    );

    const normalized = await result.jsonValue();
    guard.assertSince(mark, "Dashboard guard self-test run");

    const hasLeakMessage = normalized.includes("Guard correctly detected forbidden token leakage in all fixtures");
    const startsWithFail = normalized.startsWith("FAIL");

    await browser.close();
    if (startsWithFail && hasLeakMessage) {
      console.log("PASS");
      console.log("Guard correctly detected forbidden token leakage in all fixtures");
      process.exit(0);
    }

    console.log("FAIL");
    console.log(normalized);
    console.error("Dashboard guard self-test did not produce expected FAIL result.");
    process.exit(1);
  } catch (err) {
    await browser.close();
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
