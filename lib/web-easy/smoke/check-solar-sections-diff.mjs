#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

const cwd = process.cwd();
const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;
const referenceUrl = process.env.SOLAR_REFERENCE_URL || `file://${path.resolve(cwd, "../../../tmp/bootswatch-solar/bootswatch-solar.html")}`;
const outDir = path.resolve(cwd, "generated", "solar-section-diff");

async function loadPlaywright() {
  try {
    return await import("playwright");
  } catch (_e) {
    const localPath = path.resolve(cwd, "../../../.local-tools/node_modules/playwright/index.mjs");
    return await import(pathToFileURL(localPath).href);
  }
}

const allSections = [
  { name: "navbars", generated: "#solar2-navbars", reference: "#navbars" },
  { name: "buttons", generated: "#solar2-buttons", reference: "#buttons" },
  { name: "typography", generated: "#solar2-typography", reference: "#typography" },
  { name: "tables", generated: "#solar2-tables", reference: "#tables" },
  { name: "forms", generated: "#solar2-forms", reference: "#forms" },
  { name: "navs", generated: "#solar2-navs-section", reference: ".bs-docs-section:has(#navs)" },
  { name: "indicators", generated: "#solar2-indicators-section", reference: ".bs-docs-section:has(#indicators)" },
  { name: "progress", generated: "#solar2-progress", reference: "#progress" },
  { name: "containers", generated: "#solar2-containers", reference: "#containers" },
  { name: "cards", generated: "#solar2-cards-body", reference: ".bs-docs-section:has(#containers) .row:has(.card)" },
  { name: "accordions", generated: "#solar2-accordions-body", reference: ".bs-docs-section:has(#containers) .accordion" },
  { name: "dialogs", generated: "#solar2-dialogs-body", reference: ".bs-docs-section:has(#dialogs) .row:has(.modal, .offcanvas, .popover, .tooltip, .toast)" }
];

const requestedSections = (process.env.SOLAR_SECTIONS || "")
  .split(",")
  .map((x) => x.trim())
  .filter(Boolean);

const sections = requestedSections.length === 0
  ? allSections
  : allSections.filter((s) => requestedSections.includes(s.name));

function safe(v) {
  return v == null ? null : v;
}

async function captureSection(page, selector, pngPath) {
  await page.locator(selector).first().scrollIntoViewIfNeeded();
  const locator = page.locator(selector).first();
  await locator.screenshot({ path: pngPath });
  return await locator.evaluate((el) => {
    const cs = getComputedStyle(el);
    const r = el.getBoundingClientRect();
    return {
      fontSize: cs.fontSize,
      fontWeight: cs.fontWeight,
      fontFamily: cs.fontFamily,
      color: cs.color,
      marginTop: cs.marginTop,
      marginBottom: cs.marginBottom,
      width: r.width,
      x: r.x
    };
  });
}

async function main() {
  const { chromium } = await loadPlaywright();
  await fs.mkdir(outDir, { recursive: true });
  const browser = await chromium.launch({ headless: true });
  const generatedPage = await browser.newPage({ viewport: { width: 1600, height: 3000 } });
  const referencePage = await browser.newPage({ viewport: { width: 1600, height: 3000 } });
  const generatedGuard = installPlaywrightErrorGuard(generatedPage);
  const referenceGuard = installPlaywrightErrorGuard(referencePage);

  const generatedMark = generatedGuard.mark();
  await generatedPage.goto(generatedUrl, { waitUntil: "networkidle" });
  generatedGuard.assertSince(generatedMark, "Solar section diff generated page");
  const referenceMark = referenceGuard.mark();
  await referencePage.goto(referenceUrl, { waitUntil: "domcontentloaded" });
  referenceGuard.assertSince(referenceMark, "Solar section diff reference page");

  const report = [];
  for (const section of sections) {
    const generatedPng = path.join(outDir, `${section.name}-generated.png`);
    const referencePng = path.join(outDir, `${section.name}-reference.png`);
    let generatedMetrics = null;
    let referenceMetrics = null;
    let error = null;
    try {
      generatedMetrics = await captureSection(generatedPage, section.generated, generatedPng);
    } catch (e) {
      error = `generated selector failed: ${section.generated}: ${String(e)}`;
    }
    try {
      referenceMetrics = await captureSection(referencePage, section.reference, referencePng);
    } catch (e) {
      error = `${safe(error) ? `${error}; ` : ""}reference selector failed: ${section.reference}: ${String(e)}`;
    }
    report.push({
      section: section.name,
      generatedSelector: section.generated,
      referenceSelector: section.reference,
      generatedMetrics,
      referenceMetrics,
      error
    });
  }

  const reportPath = path.join(outDir, "report.json");
  await fs.writeFile(reportPath, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  await browser.close();
  console.log(`solar section diff report: ${reportPath}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
