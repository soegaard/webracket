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

function assertEqual(actual, expected, label) {
  if (actual !== expected) {
    throw new Error(`${label}: expected ${expected}, got ${actual}`);
  }
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 2200 } });
  const guard = installPlaywrightErrorGuard(page);
  const mark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(mark, "Solar accordion contract");

  const got = await page.evaluate(() => {
    const root = document.querySelector(".showcase-accordion-wrap.we-accordion");
    const openTrigger = root?.querySelector(".we-accordion-trigger.is-open");
    const openPanel = root?.querySelector(".we-accordion-content.is-open");
    const first = root?.querySelector(".we-accordion-section:first-child");
    const csTrigger = openTrigger ? getComputedStyle(openTrigger) : null;
    const csPanel = openPanel ? getComputedStyle(openPanel) : null;
    const csFirst = first ? getComputedStyle(first) : null;
    return {
      triggerBg: csTrigger?.backgroundColor ?? "",
      triggerColor: csTrigger?.color ?? "",
      triggerPadding: csTrigger?.padding ?? "",
      triggerRadius: csTrigger?.borderTopLeftRadius ?? "",
      triggerLineHeight: csTrigger?.lineHeight ?? "",
      panelRadius: csPanel?.borderTopLeftRadius ?? "",
      panelPadding: csPanel?.padding ?? "",
      firstRadius: csFirst ? `${csFirst.borderTopLeftRadius} ${csFirst.borderTopRightRadius}` : ""
    };
  });

  assertEqual(got.triggerBg, "rgb(240, 231, 204)", "open trigger background");
  assertEqual(got.triggerColor, "rgb(72, 55, 0)", "open trigger color");
  assertEqual(got.triggerPadding, "16px 20px", "open trigger padding");
  assertEqual(got.triggerRadius, "5px", "open trigger top-left radius");
  assertEqual(got.triggerLineHeight, "19.2px", "open trigger line-height");
  assertEqual(got.panelRadius, "0px", "open panel radius");
  assertEqual(got.panelPadding, "16px 20px", "open panel padding");
  assertEqual(got.firstRadius, "6px 6px", "first section top radii");

  await browser.close();
  console.log("PASS");
  console.log("accordion computed contract matches expected Solar2 values");
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});

