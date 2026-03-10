#!/usr/bin/env node
import path from "node:path";
import { pathToFileURL } from "node:url";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

const cwd = process.cwd();
const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;
const referenceUrl = process.env.SOLAR_REFERENCE_URL || `${baseUrl}/generated/reference-solar/index.html`;

async function loadPlaywright() {
  try {
    return await import("playwright");
  } catch (_e) {
    const localPath = path.resolve(cwd, "../../../.local-tools/node_modules/playwright/index.mjs");
    return await import(pathToFileURL(localPath).href);
  }
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const generated = await browser.newPage({ viewport: { width: 1600, height: 2600 } });
  const reference = await browser.newPage({ viewport: { width: 1600, height: 2600 } });
  const generatedGuard = installPlaywrightErrorGuard(generated);
  const referenceGuard = installPlaywrightErrorGuard(reference);

  const generatedMark = generatedGuard.mark();
  await generated.goto(generatedUrl, { waitUntil: "networkidle" });
  generatedGuard.assertSince(generatedMark, "Solar accordion diff generated page");

  const referenceMark = referenceGuard.mark();
  await reference.goto(referenceUrl, { waitUntil: "networkidle" });
  referenceGuard.assertSince(referenceMark, "Solar accordion diff reference page");

  const g = await generated.evaluate(() => {
    const grab = (el) => {
      if (!el) {
        return null;
      }
      const cs = getComputedStyle(el);
      return {
        color: cs.color,
        backgroundColor: cs.backgroundColor,
        borderColor: cs.borderColor,
        borderRadius: cs.borderRadius,
        padding: cs.padding,
        fontSize: cs.fontSize,
        fontWeight: cs.fontWeight,
        lineHeight: cs.lineHeight,
        boxShadow: cs.boxShadow
      };
    };
    const root = document.querySelector(".showcase-accordion-wrap.we-accordion");
    const openTrigger = root?.querySelector(".we-accordion-trigger.is-open");
    const closedTrigger = root?.querySelector(".we-accordion-trigger:not(.is-open)");
    const openPanel = root?.querySelector(".we-accordion-content.is-open");
    const firstSection = root?.querySelector(".we-accordion-section:first-child");
    const lastSection = root?.querySelector(".we-accordion-section:last-child");
    return {
      openTrigger: grab(openTrigger),
      closedTrigger: grab(closedTrigger),
      openPanel: grab(openPanel),
      firstSection: grab(firstSection),
      lastSection: grab(lastSection)
    };
  });

  const r = await reference.evaluate(() => {
    const grab = (el) => {
      if (!el) {
        return null;
      }
      const cs = getComputedStyle(el);
      return {
        color: cs.color,
        backgroundColor: cs.backgroundColor,
        borderColor: cs.borderColor,
        borderRadius: cs.borderRadius,
        padding: cs.padding,
        fontSize: cs.fontSize,
        fontWeight: cs.fontWeight,
        lineHeight: cs.lineHeight,
        boxShadow: cs.boxShadow
      };
    };
    const section = [...document.querySelectorAll("h2")]
      .find((n) => n.textContent.trim() === "Accordions")
      ?.closest(".bs-docs-section");
    const root = section?.querySelector(".accordion");
    const openTrigger = root?.querySelector(".accordion-button:not(.collapsed)");
    const closedTrigger = root?.querySelector(".accordion-button.collapsed");
    const openPanel = root?.querySelector(".accordion-collapse.show .accordion-body");
    const firstSection = root?.querySelector(".accordion-item:first-of-type");
    const lastSection = root?.querySelector(".accordion-item:last-of-type");
    return {
      openTrigger: grab(openTrigger),
      closedTrigger: grab(closedTrigger),
      openPanel: grab(openPanel),
      firstSection: grab(firstSection),
      lastSection: grab(lastSection)
    };
  });

  await browser.close();
  console.log(JSON.stringify({ generated: g, reference: r }, null, 2));
}

main().catch((e) => {
  console.error(String(e?.message || e));
  process.exit(1);
});
