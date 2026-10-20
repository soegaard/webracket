#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";
import { findCachedPlaywrightHeadlessShell } from "../tools/find-playwright-headless-shell.mjs";

const require = createRequire(import.meta.url);
const { chromium } = require("playwright");
const url = process.env.MATH_EDITOR_BINDINGS_B1_URL;

if (!url) {
  throw new Error("MATH_EDITOR_BINDINGS_B1_URL must name the compiled interop probe page");
}

const executablePath = process.env.MATH_EDITOR_BINDINGS_B1_EXECUTABLE
  || findCachedPlaywrightHeadlessShell();

if (!executablePath) {
  throw new Error("No cached Chromium headless-shell is available for the interop probe");
}

const browser = await chromium.launch({ headless: true, executablePath });
const page = await browser.newPage();
const consoleEntries = [];
const pageErrors = [];

page.on("console", (message) => consoleEntries.push(`${message.type()}:${message.text()}`));
page.on("pageerror", (error) => pageErrors.push(error.stack || String(error)));

try {
  const response = await page.goto(url, { waitUntil: "domcontentloaded" });
  if (!response || !response.ok()) {
    throw new Error(`Interop probe page request failed: ${response?.status() ?? "no response"}`);
  }

  await page.waitForFunction(
    () => document.body?.getAttribute("data-math-editor-bindings-interop-state") === "ready",
    undefined,
    { timeout: 30000 },
  );

  const receipt = await page.evaluate(async () => {
    globalThis.__webracketBindingsHostSuccessor = (value) => value + 1;
    const sum = globalThis.__webracketBindingsAdd(20, 22);
    const veto = globalThis.__webracketBindingsVeto();
    const nested = globalThis.__webracketBindingsNested(41);
    const asynchronousCall = globalThis.__webracketBindingsAsync();
    await new Promise((resolve) => queueMicrotask(resolve));
    let failure = "";
    try {
      globalThis.__webracketBindingsFailure();
    } catch (error) {
      failure = String(error?.message || error);
    }
    return {
      state: document.body?.getAttribute("data-math-editor-bindings-interop-state") ?? null,
      sum,
      veto,
      nested,
      asynchronousCall,
      failure,
      consoleBridgePresent: Object.hasOwn(globalThis, "WR"),
    };
  });

  const expected = {
    state: "async",
    sum: 42,
    veto: false,
    nested: 42,
    asynchronousCall: true,
    consoleBridgePresent: false,
  };
  for (const [key, value] of Object.entries(expected)) {
    if (receipt[key] !== value) {
      throw new Error(`interop receipt ${key}: got ${JSON.stringify(receipt[key])}, expected ${JSON.stringify(value)}`);
    }
  }
  if (!receipt.failure.includes("WebRacket callback error")
      || !receipt.failure.includes("intentional failure")) {
    throw new Error(`interop callback failure receipt was not preserved: ${JSON.stringify(receipt.failure)}`);
  }
  if (pageErrors.length > 0) {
    throw new Error(`Interop page errors: ${JSON.stringify(pageErrors)}`);
  }

  console.log(`PASS math-editor-bindings-interop ${JSON.stringify(receipt)}`);
} catch (error) {
  console.error(`FAIL math-editor-bindings-interop ${error.stack || error}`);
  if (consoleEntries.length > 0) console.error(`CONSOLE ${JSON.stringify(consoleEntries)}`);
  if (pageErrors.length > 0) console.error(`PAGEERROR ${JSON.stringify(pageErrors)}`);
  process.exitCode = 1;
} finally {
  await browser.close();
}
