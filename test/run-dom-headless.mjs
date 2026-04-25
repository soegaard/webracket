#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";

// Return the page-specific smoke probe for browser fixtures that need one.
async function runPageProbe(page, testPage, consoleMessages) {
  if (testPage === "test-vfs-tgz-browser.html") {
    for (let i = 0; i < 30; i++) {
      if (consoleMessages.some((text) => text.includes("hello from browser tgz"))) {
        return;
      }
      await page.waitForTimeout(100);
    }
    throw new Error(
      `Expected VFS tgz output not observed. Console: ${JSON.stringify(consoleMessages)}`
    );
  }

  if (testPage !== "test-console-bridge-smoke.html") {
    return;
  }

  await page.waitForFunction(() => typeof globalThis.WR === "function");

  const probe = await page.evaluate(() => {
    const helpText = globalThis.WR.help();
    const names = globalThis.WR.names();
    const searchDisplay = globalThis.WR.search("display");
    const searchAdd1 = globalThis.WR.search("ADD1");
    const namesDetailed = globalThis.WR.namesDetailed();
    const initial = globalThis.WR.raw("x");
    const add1Result = globalThis.WR.raw("add1*", 41);
    const zeroArgCall = globalThis.WR.call("current-x");
    const displayResult = globalThis.WR.raw("display", "bridge smoke");
    const mutationResult = globalThis.WR.raw("set-x!", 99);
    const mutated = globalThis.WR.raw("x");
    const missing = globalThis.WR.raw("definitely-missing");
    const missingThrown = (() => {
      try {
        globalThis.WR("definitely-missing");
        return null;
      } catch (err) {
        return err && err.message ? err.message : String(err);
      }
    })();
    const writeResult = globalThis.WR.write("x");
    const printResult = globalThis.WR.print("x");
    const formattedPair = globalThis.WR.format(["bridge", 7]);
    const exceptionResult = globalThis.WR.raw("explode!", 0);
    const exceptionThrown = (() => {
      try {
        globalThis.WR.value("explode!", 0);
        return null;
      } catch (err) {
        return err && err.message ? err.message : String(err);
      }
    })();

    return {
      hasWR: typeof globalThis.WR === "function",
      helpText,
      names,
      searchDisplay,
      searchAdd1,
      namesDetailed,
      initial,
      add1Result,
      zeroArgCall,
      displayResult,
      mutationResult,
      mutated,
      missing,
      missingThrown,
      writeResult,
      printResult,
      formattedPair,
      exceptionResult,
      exceptionThrown,
    };
  });

  if (!probe.hasWR) {
    throw new Error("window.WR was not installed");
  }
  if (typeof probe.helpText !== "string" || !probe.helpText.includes("WR.namesDetailed()")) {
    throw new Error(`WR.help() returned ${JSON.stringify(probe.helpText)}`);
  }
  if (!Array.isArray(probe.names) || !probe.names.includes("x") || !probe.names.includes("add1*")) {
    throw new Error(`WR.names() missing expected bindings: ${JSON.stringify(probe.names)}`);
  }
  if (!Array.isArray(probe.searchDisplay) || !probe.searchDisplay.includes("display")) {
    throw new Error(`WR.search("display") returned ${JSON.stringify(probe.searchDisplay)}`);
  }
  if (!Array.isArray(probe.searchAdd1) || !probe.searchAdd1.includes("add1*")) {
    throw new Error(`WR.search("ADD1") returned ${JSON.stringify(probe.searchAdd1)}`);
  }
  if (!probe.names.includes("display")) {
    throw new Error(`WR.names() missing expected stdlib binding 'display': ${JSON.stringify(probe.names)}`);
  }
  if (!Array.isArray(probe.namesDetailed) || !probe.namesDetailed.some((entry) =>
    entry.name === "x" &&
    entry.origin === "program" &&
    entry.mutable === true &&
    typeof entry.source === "string" &&
    entry.source.endsWith("/test/test-console-bridge-smoke.rkt"))) {
    throw new Error(`WR.namesDetailed() missing expected program metadata: ${JSON.stringify(probe.namesDetailed)}`);
  }
  if (!probe.initial?.ok || probe.initial.value !== 41) {
    throw new Error(`WR.raw("x") returned ${JSON.stringify(probe.initial)}`);
  }
  if (!probe.add1Result?.ok || probe.add1Result.value !== 42) {
    throw new Error(`WR.raw("add1*", 41) returned ${JSON.stringify(probe.add1Result)}`);
  }
  if (probe.zeroArgCall !== 41) {
    throw new Error(`WR.call("current-x") returned ${JSON.stringify(probe.zeroArgCall)}`);
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
  if (typeof probe.missing?.message !== "string" || !probe.missing.message.includes("missing")) {
    throw new Error(`WR.raw("definitely-missing") missing message: ${JSON.stringify(probe.missing)}`);
  }
  if (typeof probe.missingThrown !== "string" || !probe.missingThrown.includes("missing")) {
    throw new Error(`WR("definitely-missing") threw ${JSON.stringify(probe.missingThrown)}`);
  }
  if (probe.writeResult !== 99) {
    throw new Error(`WR.write("x") returned ${JSON.stringify(probe.writeResult)}`);
  }
  if (probe.printResult !== 99) {
    throw new Error(`WR.print("x") returned ${JSON.stringify(probe.printResult)}`);
  }
  if (typeof probe.formattedPair !== "string" || !probe.formattedPair.includes("bridge")) {
    throw new Error(`WR.format(...) returned ${JSON.stringify(probe.formattedPair)}`);
  }
  if (probe.exceptionResult?.ok || probe.exceptionResult?.kind !== "exception") {
    throw new Error(`WR.raw("explode!", 0) returned ${JSON.stringify(probe.exceptionResult)}`);
  }
  if (typeof probe.exceptionThrown !== "string" || !probe.exceptionThrown.includes("boom")) {
    throw new Error(`WR.value("explode!", 0) threw ${JSON.stringify(probe.exceptionThrown)}`);
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

        await runPageProbe(page, testPage, consoleMessages);

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
