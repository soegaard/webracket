#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";

// Return the page-specific smoke probe for browser fixtures that need one.
async function runPageProbe(page, testPage) {
  if (testPage !== "test-console-bridge-smoke.html") {
    return;
  }

  await page.waitForFunction(() => typeof globalThis.WR === "function");

  const probe = await page.evaluate(() => {
    const names = globalThis.WR.names();
    const initial = globalThis.WR.raw("x");
    const add1Result = globalThis.WR.raw("add1*", 41);
    const displayResult = globalThis.WR.raw("display", "bridge smoke");
    const mutationResult = globalThis.WR.raw("set-x!", 99);
    const mutated = globalThis.WR.raw("x");
    const missing = globalThis.WR.raw("definitely-missing");

    return {
      hasWR: typeof globalThis.WR === "function",
      names,
      initial,
      add1Result,
      displayResult,
      mutationResult,
      mutated,
      missing,
    };
  });

  if (!probe.hasWR) {
    throw new Error("window.WR was not installed");
  }
  if (!Array.isArray(probe.names) || !probe.names.includes("x") || !probe.names.includes("add1*")) {
    throw new Error(`WR.names() missing expected bindings: ${JSON.stringify(probe.names)}`);
  }
  if (!probe.names.includes("display")) {
    throw new Error(`WR.names() missing expected stdlib binding 'display': ${JSON.stringify(probe.names)}`);
  }
  if (!probe.initial?.ok || probe.initial.value !== 41) {
    throw new Error(`WR.raw("x") returned ${JSON.stringify(probe.initial)}`);
  }
  if (!probe.add1Result?.ok || probe.add1Result.value !== 42) {
    throw new Error(`WR.raw("add1*", 41) returned ${JSON.stringify(probe.add1Result)}`);
  }
  if (!probe.displayResult?.ok || probe.displayResult.value !== undefined) {
    throw new Error(`WR.raw("display", ...) returned ${JSON.stringify(probe.displayResult)}`);
  }
  if (!probe.mutationResult?.ok || probe.mutationResult.value !== 99) {
    throw new Error(`WR.raw("set-x!", 99) returned ${JSON.stringify(probe.mutationResult)}`);
  }
  if (!probe.mutated?.ok || probe.mutated.value !== 99) {
    throw new Error(`WR.raw("x") after mutation returned ${JSON.stringify(probe.mutated)}`);
  }
  if (probe.missing?.ok || probe.missing?.kind !== "missing-binding") {
    throw new Error(`WR.raw("definitely-missing") returned ${JSON.stringify(probe.missing)}`);
  }
}

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:9989";
  const tests = (process.env.SMOKE_TESTS || "")
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
  const chromeExecutable = process.env.SMOKE_CHROME_EXECUTABLE || "";

  if (tests.length === 0) {
    console.error("SMOKE_TESTS is required.");
    process.exit(2);
  }

  const requireHere = createRequire(import.meta.url);
  let playwrightPkg = null;

  try {
    playwrightPkg = requireHere("playwright");
  } catch (_err) {
    const localNodeModules = process.env.SMOKE_NODE_MODULES || "";
    if (localNodeModules) {
      try {
        const requireFromLocal = createRequire(`${localNodeModules}/_resolver.js`);
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
  const browser = await chromium.launch({
    headless: true,
    executablePath: chromeExecutable || undefined,
  });

  try {
    for (const testPage of tests) {
      const page = await browser.newPage();
      const consoleMessages = [];
      const pageErrors = [];

      page.on("console", (msg) => {
        consoleMessages.push(msg.text());
      });
      page.on("pageerror", (err) => {
        pageErrors.push(err);
      });

      try {
        await page.goto(`${baseUrl}/${testPage}`, { waitUntil: "load" });
        await page.waitForTimeout(1000);

        if (pageErrors.length > 0) {
          throw pageErrors[0];
        }

        await runPageProbe(page, testPage);

        const meaningful = consoleMessages.filter((text) => text.trim().length > 0);
        if (meaningful.length === 0) {
          throw new Error(`No console output observed for ${testPage}`);
        }

        console.log(`PASS ${testPage}`);
      } finally {
        await page.close();
      }
    }
  } catch (err) {
    console.error("DOM browser runner failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exitCode = 1;
  } finally {
    await browser.close();
  }
}

main();
