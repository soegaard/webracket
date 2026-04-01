#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:9989";
  const tests = (process.env.SMOKE_TESTS || "")
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);

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
  const browser = await chromium.launch({ headless: true });

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
