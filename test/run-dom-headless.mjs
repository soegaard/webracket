#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";
import { findCachedPlaywrightHeadlessShell } from "../tools/find-playwright-headless-shell.mjs";

function parseTimeoutMs(value, fallback) {
  const parsed = Number.parseInt(value ?? "", 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

async function withTimeout(label, timeoutMs, thunk) {
  let timeoutId = null;

  try {
    return await Promise.race([
      thunk(),
      new Promise((_, reject) => {
        timeoutId = setTimeout(() => {
          reject(new Error(`Timeout after ${timeoutMs}ms while running ${label}`));
        }, timeoutMs);
      }),
    ]);
  } finally {
    if (timeoutId !== null) {
      clearTimeout(timeoutId);
    }
  }
}

function resolveBrowserExecutablePath(explicitPath) {
  if (explicitPath) {
    return explicitPath;
  }
  return findCachedPlaywrightHeadlessShell() || "";
}

function formatConsoleEntry(entry) {
  const parts = [entry.type.toUpperCase()];
  if (entry.location) {
    const { url = "", lineNumber = 0, columnNumber = 0 } = entry.location;
    if (url) {
      parts.push(`${url}:${lineNumber}:${columnNumber}`);
    }
  }
  parts.push(entry.text);
  return parts.join(" ");
}

function formatRequestFailure(entry) {
  return `${entry.method} ${entry.url} ${entry.failureText || "request failed"}`;
}

function isGenericCompletionMessage(text) {
  const trimmed = text.trim();
  return trimmed === "true" || trimmed.includes("-ok");
}

async function waitForGenericCompletion(page, testPage, consoleMessages, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let lastMessageCount = consoleMessages.length;
  let quietSince = null;

  while (Date.now() < deadline) {
    if (consoleMessages.some(isGenericCompletionMessage)) {
      return;
    }
    const meaningful = consoleMessages.filter((text) => text.trim().length > 0);
    if (meaningful.length > 0) {
      if (quietSince === null || consoleMessages.length !== lastMessageCount) {
        quietSince = Date.now();
        lastMessageCount = consoleMessages.length;
      } else if (Date.now() - quietSince >= 1000) {
        return;
      }
    } else {
      lastMessageCount = consoleMessages.length;
    }
    await page.waitForTimeout(100);
  }

  const meaningful = consoleMessages.filter((text) => text.trim().length > 0);
  if (meaningful.length === 0) {
    throw new Error(`No console output observed for ${testPage}`);
  }
  throw new Error(
    `No completion marker observed for ${testPage}. Console: ${JSON.stringify(meaningful)}`
  );
}

async function capturePageSnapshot(page) {
  try {
    return await page.evaluate(() => ({
      href: globalThis.location ? String(globalThis.location.href) : null,
      readyState: globalThis.document ? globalThis.document.readyState : null,
      title: globalThis.document ? globalThis.document.title : null,
      bodyState: globalThis.document?.body?.getAttribute("data-state") ?? null,
      hasDomTest: !!globalThis.__domTest,
      domTestKeys: globalThis.__domTest ? Object.keys(globalThis.__domTest) : [],
    }));
  } catch (_err) {
    return null;
  }
}

async function reportFailure({
  page,
  testPage,
  err,
  consoleEntries,
  pageErrors,
  requestFailures,
}) {
  const snapshot = await capturePageSnapshot(page);
  const recentConsole = consoleEntries.slice(-12).map(formatConsoleEntry);
  const recentPageErrors = pageErrors.slice(-6).map((entry) =>
    entry && entry.stack ? entry.stack : String(entry));
  const recentRequestFailures = requestFailures.slice(-6).map(formatRequestFailure);

  console.error(`FAIL ${testPage}`);
  console.error(`URL ${page.url() || testPage}`);
  if (snapshot) {
    console.error(`READY ${JSON.stringify(snapshot)}`);
  }
  if (recentConsole.length > 0) {
    console.error("CONSOLE");
    for (const line of recentConsole) {
      console.error(`  ${line}`);
    }
  }
  if (recentPageErrors.length > 0) {
    console.error("PAGEERROR");
    for (const line of recentPageErrors) {
      console.error(`  ${line}`);
    }
  }
  if (recentRequestFailures.length > 0) {
    console.error("REQUESTFAIL");
    for (const line of recentRequestFailures) {
      console.error(`  ${line}`);
    }
  }
  console.error(err && err.stack ? err.stack : String(err));
}

// Return the page-specific smoke probe for browser fixtures that need one.
async function runPageProbe(page, testPage, consoleMessages) {
  if (testPage === "test-vfs-tgz-browser.html") {
    for (let i = 0; i < 30; i++) {
      if (consoleMessages.some((text) => text.includes("hello from browser tgz"))) {
        return true;
      }
      await page.waitForTimeout(100);
    }
    throw new Error(
      `Expected VFS tgz output not observed. Console: ${JSON.stringify(consoleMessages)}`
    );
  }

  if (testPage === "test-console-bridge-module-smoke.html") {
    await page.waitForFunction(() => typeof globalThis.WR === "function");

    const probe = await page.evaluate(() => {
      const names = globalThis.WR.names();
      const namesDetailed = globalThis.WR.namesDetailed();
      const currentCounter = globalThis.WR.call("current-counter");
      const incremented = globalThis.WR.raw("increment!", 5);
      const mutated = globalThis.WR.raw("counter");
      return {
        hasWR: typeof globalThis.WR === "function",
        names,
        namesDetailed,
        currentCounter,
        incremented,
        mutated,
      };
    });

    if (!probe.hasWR) {
      throw new Error("window.WR was not installed for module smoke");
    }
    const expectedNames = ["counter", "current-counter", "explode!", "increment!"];
    if (!Array.isArray(probe.names) ||
        JSON.stringify([...probe.names].sort()) !== JSON.stringify([...expectedNames].sort())) {
      throw new Error(`Module WR.names() mismatch: ${JSON.stringify(probe.names)}`);
    }
    if (probe.names.some((name) => String(name).startsWith("webify-export-"))) {
      throw new Error(`Module WR.names() leaked generated bindings: ${JSON.stringify(probe.names)}`);
    }
    if (!Array.isArray(probe.namesDetailed) || !probe.namesDetailed.every((entry) =>
      typeof entry.source === "string" &&
      entry.source.endsWith("/test/test-console-bridge-module-smoke.rkt") &&
      entry.origin === "program")) {
      throw new Error(`Module WR.namesDetailed() mismatch: ${JSON.stringify(probe.namesDetailed)}`);
    }
    if (probe.currentCounter !== 0) {
      throw new Error(`WR.call(\"current-counter\") returned ${JSON.stringify(probe.currentCounter)}`);
    }
    if (!probe.incremented?.ok || probe.incremented.value !== 5) {
      throw new Error(`WR.raw(\"increment!\", 5) returned ${JSON.stringify(probe.incremented)}`);
    }
    if (!probe.mutated?.ok || probe.mutated.value !== 5) {
      throw new Error(`WR.raw(\"counter\") after increment returned ${JSON.stringify(probe.mutated)}`);
    }
    return true;
  }

  if (testPage === "test-browser-canvas-draw-image-integers.html") {
    await page.waitForFunction(() =>
      ["loaded", "error"].includes(document.body?.getAttribute("data-state")));
    const probe = await page.evaluate(() => ({
      state: document.body?.getAttribute("data-state") ?? null,
      bodyText: document.body?.innerText ?? "",
    }));
    if (probe.state !== "loaded") {
      throw new Error(`canvas draw-image integer smoke failed: ${JSON.stringify(probe)}`);
    }
    if (!probe.bodyText.includes("canvas-draw-image-integers-ok")) {
      throw new Error(`canvas draw-image integer marker missing: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage === "test-js-string-symbol-ffi.html") {
    await page.waitForFunction(() =>
      document.body?.getAttribute("data-state") === "loaded");
    const probe = await page.evaluate(() => ({
      state: document.body?.getAttribute("data-state") ?? null,
      bodyText: document.body?.innerText ?? "",
      assigned: globalThis.WebRacketStringSymbolFFITest ?? null,
    }));
    if (probe.state !== "loaded" ||
        probe.assigned !== "assigned" ||
        !probe.bodyText.includes("js-string-symbol-ffi-ok")) {
      throw new Error(`string/symbol FFI smoke mismatch: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage === "test-keyword-fasl-bridge.html") {
    await page.waitForFunction(() =>
      ["loaded", "error"].includes(document.body?.getAttribute("data-state")));
    const probe = await page.evaluate(() => ({
      state: document.body?.getAttribute("data-state") ?? null,
      bodyText: document.body?.innerText ?? "",
    }));
    if (probe.state !== "loaded" || !probe.bodyText.includes("keyword-fasl-bridge-ok")) {
      throw new Error(`keyword fasl bridge smoke failed: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage === "test-webracket-base-format.html") {
    await page.waitForFunction(() =>
      ["loaded", "error"].includes(document.body?.getAttribute("data-state")));
    const probe = await page.evaluate(() => ({
      state: document.body?.getAttribute("data-state") ?? null,
      bodyText: document.body?.innerText ?? "",
    }));
    if (probe.state !== "loaded" || probe.bodyText.trim() !== "42|value=42") {
      throw new Error(`webracket/base format smoke failed: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage === "test-web-pict-text-binding.html") {
    await page.waitForFunction(() =>
      ["loaded", "error"].includes(document.querySelector("#status")?.getAttribute("data-state")));
    const probe = await page.evaluate(() => {
      const status = document.querySelector("#status");
      return {
        state: status?.getAttribute("data-state") ?? null,
        text: status?.textContent ?? "",
      };
    });
    if (probe.state !== "loaded" || probe.text.trim() !== "web-pict-text-binding-ok") {
      throw new Error(`web-pict text binding smoke failed: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage === "test-web-pict-rational-coordinates.html" ||
      testPage === "test-web-pict-rational-coordinates-legacy.html") {
    await page.waitForFunction(() =>
      document.body?.getAttribute("data-state") === "loaded");
    return true;
  }

  if (testPage === "test-racket-port-call-with-output-string.html") {
    await page.waitForFunction(() =>
      ["loaded", "error"].includes(document.body?.getAttribute("data-state")));
    const probe = await page.evaluate(() => ({
      state: document.body?.getAttribute("data-state") ?? null,
      bodyText: document.body?.innerText ?? "",
    }));
    if (probe.state !== "loaded" || probe.bodyText.trim() !== "abc|ab") {
      throw new Error(`racket/port call-with-output-string smoke failed: ${JSON.stringify(probe)}`);
    }
    return true;
  }

  if (testPage !== "test-console-bridge-smoke.html") {
    return false;
  }

  await page.waitForFunction(() => typeof globalThis.WR === "function");

  const probe = await page.evaluate(() => {
    const helpText = globalThis.WR.help();
    const names = globalThis.WR.names();
    const searchAdd1 = globalThis.WR.search("ADD1");
    const namesDetailed = globalThis.WR.namesDetailed();
    const initial = globalThis.WR.raw("x");
    const add1Result = globalThis.WR.raw("add1*", 41);
    const zeroArgCall = globalThis.WR.call("current-x");
    const largeResult = globalThis.WR.call("large-result");
    const largeProperList = globalThis.WR.call("large-proper-list-result");
    let largeProperListLength = 0;
    let largeProperListAllZero = true;
    let largeProperListTail = largeProperList;
    while (largeProperListTail !== null) {
      if (largeProperListTail?.tag !== "pair") {
        largeProperListAllZero = false;
        break;
      }
      if (largeProperListTail.car !== 0) {
        largeProperListAllZero = false;
      }
      largeProperListLength += 1;
      largeProperListTail = largeProperListTail.cdr;
    }
    const largeValue = globalThis.WR.value("large-value");
    const largeValueResult = globalThis.WR.value("large-value-result", 0);
    const rawLargeValue = globalThis.WR.raw("large-value");
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
      searchAdd1,
      namesDetailed,
      initial,
      add1Result,
      zeroArgCall,
      largeResult,
      largeProperListLength,
      largeProperListAllZero,
      largeValue,
      largeValueResult,
      rawLargeValue,
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
  if (!Array.isArray(probe.searchAdd1) || !probe.searchAdd1.includes("add1*")) {
    throw new Error(`WR.search("ADD1") returned ${JSON.stringify(probe.searchAdd1)}`);
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
  if (typeof probe.largeResult !== "string" || probe.largeResult.length !== 70000 ||
      probe.largeResult[0] !== "x" || probe.largeResult.at(-1) !== "x") {
    throw new Error(`WR.call("large-result") returned an invalid ${typeof probe.largeResult} result`);
  }
  if (probe.largeProperListLength !== 5000 || !probe.largeProperListAllZero) {
    throw new Error("WR.call(\"large-proper-list-result\") did not return 5,000 zeroes");
  }
  if (typeof probe.largeValue !== "string" || probe.largeValue.length !== 70000 ||
      probe.largeValue[0] !== "v" || probe.largeValue.at(-1) !== "v") {
    throw new Error(`WR.value("large-value") returned an invalid ${typeof probe.largeValue} result`);
  }
  if (typeof probe.largeValueResult !== "string" || probe.largeValueResult.length !== 70000 ||
      probe.largeValueResult[0] !== "w" || probe.largeValueResult.at(-1) !== "w") {
    throw new Error(`WR.value("large-value-result", 0) returned an invalid ${typeof probe.largeValueResult} result`);
  }
  if (typeof probe.rawLargeValue?.printed !== "string" ||
      probe.rawLargeValue.printed.length !== 70000 ||
      probe.rawLargeValue.printed[0] !== "v" || probe.rawLargeValue.printed.at(-1) !== "v") {
    throw new Error(`WR.raw("large-value") did not retain its formatted result`);
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
  if (typeof probe.exceptionThrown !== "string" || probe.exceptionThrown.length === 0) {
    throw new Error(`WR.value("explode!", 0) threw ${JSON.stringify(probe.exceptionThrown)}`);
  }
  return true;
}

async function main() {
  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:9989";
  const tests = (process.env.SMOKE_TESTS || "")
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
  const chromeExecutable = process.env.SMOKE_CHROME_EXECUTABLE || "";
  const perTestTimeoutMs = parseTimeoutMs(process.env.SMOKE_TEST_TIMEOUT_MS, 45000);

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
  let browser = null;
  const browserExecutablePath = resolveBrowserExecutablePath(chromeExecutable);

  try {
    browser = await chromium.launch({
      headless: true,
      executablePath: browserExecutablePath || undefined,
    });

    for (const testPage of tests) {
      console.log(`START ${testPage}`);
      const page = await browser.newPage();
      const consoleEntries = [];
      const consoleMessages = [];
      const pageErrors = [];
      const requestFailures = [];

      page.setDefaultTimeout(perTestTimeoutMs);

      page.on("console", (msg) => {
        const location = msg.location ? msg.location() : null;
        const entry = {
          type: msg.type ? msg.type() : "log",
          text: msg.text(),
          location,
        };
        consoleEntries.push(entry);
        consoleMessages.push(entry.text);
      });
      page.on("pageerror", (err) => {
        pageErrors.push(err);
      });
      page.on("requestfailed", (request) => {
        const failure = request.failure ? request.failure() : null;
        requestFailures.push({
          url: request.url(),
          method: request.method(),
          failureText: failure?.errorText ?? null,
        });
      });

      try {
        await withTimeout(testPage, perTestTimeoutMs, async () => {
          await page.goto(`${baseUrl}/${testPage}`, { waitUntil: "domcontentloaded" });

          if (pageErrors.length > 0) {
            throw pageErrors[0];
          }

          const handled = await runPageProbe(page, testPage, consoleMessages);

          if (!handled) {
            await waitForGenericCompletion(page, testPage, consoleMessages, perTestTimeoutMs);
          }

          if (pageErrors.length > 0) {
            throw pageErrors[0];
          }
        });
        console.log(`PASS ${testPage}`);
      } catch (err) {
        await reportFailure({
          page,
          testPage,
          err,
          consoleEntries,
          pageErrors,
          requestFailures,
        });
        throw err;
      } finally {
        await page.close();
      }
    }
  } catch (err) {
    console.error("DOM browser runner failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exitCode = 1;
  } finally {
    if (browser) {
      await browser.close();
    }
  }
}

main();
