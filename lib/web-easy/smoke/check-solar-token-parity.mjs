#!/usr/bin/env node
import path from "node:path";
import { pathToFileURL } from "node:url";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

const cwd = process.cwd();
const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;

async function loadPlaywright() {
  try {
    return await import("playwright");
  } catch (_e) {
    const localPath = path.resolve(cwd, "../../../.local-tools/node_modules/playwright/index.mjs");
    return await import(pathToFileURL(localPath).href);
  }
}

function normalizeColor(s) {
  return String(s || "").replace(/\s+/g, "").toLowerCase();
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 3000 } });
  const guard = installPlaywrightErrorGuard(page);
  const mark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(mark, "Solar token parity page");

  const checks = [
    { name: "body font family", selector: "body", prop: "fontFamily", includes: "Source Sans Pro" },
    { name: "body text line-height", selector: ".showcase-main span", prop: "lineHeight", expected: "24px", textIncludes: "Nullam quis risus eget" },
    { name: "input placeholder color", selector: ".showcase-main input.we-input", pseudo: "::placeholder", prop: "color", expectedColor: "rgba(131,148,150,0.75)" },
    { name: "table header color", selector: ".showcase-main .we-table th", prop: "color", expectedColor: "rgb(108,138,143)" },
    { name: "primary emphasis color", selector: ".showcase-text-primary-emphasis", prop: "color", expectedColor: "rgb(211,184,102)" },
    { name: "info emphasis color", selector: ".showcase-text-info-emphasis", prop: "color", expectedColor: "rgb(125,185,228)" },
    { name: "badge font size", selector: ".showcase-main .we-badge", prop: "fontSize", expected: "12px" },
    { name: "progress min height", selector: ".showcase-main .we-progress", prop: "minHeight", expected: "16px" }
  ];

  let failed = 0;
  for (const check of checks) {
    const result = await page.evaluate((c) => {
      let el = null;
      if (c.textIncludes) {
        const candidates = Array.from(document.querySelectorAll(c.selector));
        el = candidates.find((n) => (n.textContent || "").includes(c.textIncludes)) || null;
      } else {
        el = document.querySelector(c.selector);
      }
      if (!el) return { ok: false, actual: "missing element", reason: `selector not found: ${c.selector}` };
      const cs = getComputedStyle(el, c.pseudo || null);
      const actual = cs[c.prop];
      if (c.includes) {
        const ok = String(actual).includes(c.includes);
        return { ok, actual, expected: `contains ${c.includes}` };
      }
      if (c.expectedColor) {
        const ok = (String(actual || "").replace(/\s+/g, "").toLowerCase() === String(c.expectedColor).replace(/\s+/g, "").toLowerCase());
        return { ok, actual, expected: c.expectedColor };
      }
      const ok = String(actual) === String(c.expected);
      return { ok, actual, expected: c.expected };
    }, check);

    if (!result.ok) {
      failed += 1;
      console.error(`FAIL: ${check.name}: expected ${result.expected}; got ${result.actual}`);
    } else {
      console.log(`PASS: ${check.name}`);
    }
  }

  await browser.close();
  if (failed > 0) {
    process.exit(1);
  }
  console.log(`PASS: ${checks.length}/${checks.length} token checks`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
