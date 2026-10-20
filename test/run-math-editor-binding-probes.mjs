#!/usr/bin/env node

import { createRequire } from "node:module";
import process from "node:process";
import { findCachedPlaywrightHeadlessShell } from "../tools/find-playwright-headless-shell.mjs";

const require = createRequire(import.meta.url);
const { chromium } = require("playwright");
const url = process.env.MATH_EDITOR_BINDINGS_B1_URL;
const expectedAssertions = process.env.MATH_EDITOR_BINDINGS_B1_EXPECT_ASSERTIONS || "20";
const mode = process.env.MATH_EDITOR_BINDINGS_B1_MODE || "full";
const probeId = process.env.MATH_EDITOR_BINDINGS_PROBE_ID || "b1";
const timeoutMs = Number(process.env.MATH_EDITOR_BINDINGS_TIMEOUT_MS || "30000");
const stateAttribute = `data-math-editor-bindings-${probeId}-state`;
const assertionsAttribute = `data-math-editor-bindings-${probeId}-assertions`;
const phaseAttribute = `data-math-editor-bindings-${probeId}-phase`;

if (!url) {
  throw new Error("MATH_EDITOR_BINDINGS_B1_URL must name the compiled probe page");
}
if (mode !== "full" && mode !== "startup") {
  throw new Error(`MATH_EDITOR_BINDINGS_B1_MODE must be "full" or "startup", got ${JSON.stringify(mode)}`);
}
if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
  throw new Error(`MATH_EDITOR_BINDINGS_TIMEOUT_MS must be a positive number, got ${JSON.stringify(timeoutMs)}`);
}

const executablePath = process.env.MATH_EDITOR_BINDINGS_B1_EXECUTABLE
  || findCachedPlaywrightHeadlessShell();

if (!executablePath) {
  throw new Error("No cached Chromium headless-shell is available for the B1 probe");
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
    throw new Error(`Probe page request failed: ${response?.status() ?? "no response"}`);
  }

  await page.waitForFunction(
    (attribute) => document.body?.getAttribute(attribute) === "pass",
    stateAttribute,
    undefined,
    { timeout: timeoutMs },
  );

  const receipt = await page.evaluate(({ stateAttribute, assertionsAttribute, phaseAttribute }) => {
    const math = document.querySelector('math[data-binding-probe="mathml"]');
    const fraction = math?.querySelector("mfrac");
    const denominator = fraction?.lastElementChild;
    return {
      state: document.body?.getAttribute(stateAttribute) ?? null,
      assertions: document.body?.getAttribute(assertionsAttribute) ?? null,
      phase: document.body?.getAttribute(phaseAttribute) ?? null,
      mathNamespace: math?.namespaceURI ?? null,
      fractionNamespace: fraction?.namespaceURI ?? null,
      fractionName: fraction?.localName ?? null,
      numeratorText: fraction?.firstElementChild?.textContent ?? null,
      denominatorText: denominator?.textContent ?? null,
      textareaCount: document.querySelectorAll("textarea").length,
    };
  }, { stateAttribute, assertionsAttribute, phaseAttribute });

  const expected = {
    state: "pass",
    assertions: expectedAssertions,
    ...(probeId === "b1" && mode === "full" ? {
      mathNamespace: "http://www.w3.org/1998/Math/MathML",
      fractionNamespace: "http://www.w3.org/1998/Math/MathML",
      fractionName: "mfrac",
      numeratorText: "𝑥",
      denominatorText: "y",
      textareaCount: 0,
    } : {}),
  };

  for (const [key, value] of Object.entries(expected)) {
    if (receipt[key] !== value) {
      throw new Error(`${probeId} receipt ${key}: got ${JSON.stringify(receipt[key])}, expected ${JSON.stringify(value)}`);
    }
  }
  if (pageErrors.length > 0) {
    throw new Error(`B1 page errors: ${JSON.stringify(pageErrors)}`);
  }

  console.log(`PASS math-editor-bindings-${probeId}-${mode} ${JSON.stringify(receipt)}`);
} catch (error) {
  console.error(`FAIL math-editor-bindings-${probeId}-${mode} ${error.stack || error}`);
  const snapshot = await page.evaluate(({ stateAttribute, assertionsAttribute, phaseAttribute }) => ({
    state: document.body?.getAttribute(stateAttribute) ?? null,
    assertions: document.body?.getAttribute(assertionsAttribute) ?? null,
    phase: document.body?.getAttribute(phaseAttribute) ?? null,
    mathNamespace: document.querySelector('math[data-binding-probe="mathml"]')?.namespaceURI ?? null,
    fractionNamespace: document.querySelector('math[data-binding-probe="mathml"] mfrac')?.namespaceURI ?? null,
    fractionName: document.querySelector('math[data-binding-probe="mathml"] mfrac')?.localName ?? null,
    bodyText: (document.body?.textContent ?? "").slice(0, 500),
  }), { stateAttribute, assertionsAttribute, phaseAttribute }).catch(() => null);
  if (snapshot) console.error(`SNAPSHOT ${JSON.stringify(snapshot)}`);
  if (consoleEntries.length > 0) console.error(`CONSOLE ${JSON.stringify(consoleEntries)}`);
  if (pageErrors.length > 0) console.error(`PAGEERROR ${JSON.stringify(pageErrors)}`);
  process.exitCode = 1;
} finally {
  await browser.close();
}
