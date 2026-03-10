#!/usr/bin/env node

import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8765";
  const testPage = process.env.SMOKE_TEST_PAGE || "";
  const timeoutMs = Number(process.env.SMOKE_TIMEOUT_MS || "120000");
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";

  if (!testPage) {
    console.error("SMOKE_TEST_PAGE is required.");
    process.exit(2);
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
  const guard = installPlaywrightErrorGuard(page);

  try {
    const mark = guard.mark();
    await page.goto(`${baseUrl}/${testPage}`, { waitUntil: "domcontentloaded" });

    const status = page.locator("#status");
    await status.waitFor({ timeout: timeoutMs });

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

    const { ok, text } = await result.jsonValue();
    guard.assertSince(mark, `Single-page run (${testPage})`);
    console.log(text);

    await browser.close();
    process.exit(ok ? 0 : 1);
  } catch (err) {
    await browser.close();
    console.error("Single-page headless smoke run failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
