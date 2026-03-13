#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { pathToFileURL } from "node:url";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

const cwd = process.cwd();
const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;
const referenceUrl = process.env.SOLAR_REFERENCE_URL || `${baseUrl}/generated/reference-solar/index.html`;
const baseOutDir = path.resolve(cwd, "generated", "solar-accordion-diff");
const runStamp = process.env.SOLAR_RUNSTAMP || "";
const outDir = runStamp ? path.resolve(baseOutDir, runStamp) : baseOutDir;
const expectedThemeClass = process.env.SOLAR_EXPECT_THEME_CLASS || "we-theme-solar2";
const expectedGeneralCss = process.env.SOLAR_EXPECT_GENERAL_CSS || "themes/theme-solar-2.css";
const expectedShowcaseCss = process.env.SOLAR_EXPECT_SHOWCASE_CSS || "theme-showcase-solar2.css";

async function loadPlaywright() {
  try {
    return await import("playwright");
  } catch (_e) {
    const localPath = path.resolve(cwd, "../../../.local-tools/node_modules/playwright/index.mjs");
    return await import(pathToFileURL(localPath).href);
  }
}

function rmse(generatedPng, referencePng) {
  try {
    execFileSync("compare", ["-metric", "RMSE", generatedPng, referencePng, "null:"], { stdio: ["ignore", "pipe", "pipe"] });
    return 0;
  } catch (e) {
    const stderr = String(e.stderr || "").trim();
    const match = stderr.match(/\(([\d.]+)\)/);
    return match ? Number(match[1]) : NaN;
  }
}

function assertTrue(v, label) {
  if (!v) {
    throw new Error(`${label}: expected true, got ${String(v)}`);
  }
}

async function main() {
  await fs.mkdir(outDir, { recursive: true });
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const generated = await browser.newPage({ viewport: { width: 1600, height: 2600 } });
  const reference = await browser.newPage({ viewport: { width: 1600, height: 2600 } });
  const generatedGuard = installPlaywrightErrorGuard(generated);
  const referenceGuard = installPlaywrightErrorGuard(reference);

  const generatedMark = generatedGuard.mark();
  await generated.goto(generatedUrl, { waitUntil: "networkidle" });
  generatedGuard.assertSince(generatedMark, "Solar accordion screenshot generated");
  const referenceMark = referenceGuard.mark();
  await reference.goto(referenceUrl, { waitUntil: "networkidle" });
  referenceGuard.assertSince(referenceMark, "Solar accordion screenshot reference");

  const generatedSel = "#solar2-accordions-body.we-accordion";
  const referenceSel = "#accordionExample";
  const generatedPng = path.join(outDir, "accordion-generated.png");
  const referencePng = path.join(outDir, "accordion-reference.png");
  const metaPath = path.join(outDir, "accordion-metadata.json");

  const generatedPreflight = await generated.evaluate(({ selector }) => {
    const root = document.documentElement;
    const links = [...document.querySelectorAll("link[rel='stylesheet']")]
      .map((n) => n.getAttribute("href"))
      .filter(Boolean);
    const node = document.querySelector(selector);
    const rect = node?.getBoundingClientRect();
    return {
      pageUrl: String(window.location.href),
      themeClass: root.className,
      stylesheets: links,
      selectorFound: !!node,
      selectorVisible: !!(node && rect && rect.width > 0 && rect.height > 0),
      selectorRect: rect ? { x: rect.x, y: rect.y, width: rect.width, height: rect.height } : null
    };
  }, { selector: generatedSel });

  assertTrue(generatedPreflight.selectorFound, `generated selector exists (${generatedSel})`);
  assertTrue(generatedPreflight.selectorVisible, `generated selector visible (${generatedSel})`);
  assertTrue(generatedPreflight.themeClass.includes(expectedThemeClass), `generated theme class includes ${expectedThemeClass}`);
  assertTrue(
    generatedPreflight.stylesheets.some((href) => href.includes(expectedGeneralCss)),
    `generated stylesheet includes ${expectedGeneralCss}`
  );
  assertTrue(
    generatedPreflight.stylesheets.some((href) => href.includes(expectedShowcaseCss)),
    `generated stylesheet includes ${expectedShowcaseCss}`
  );

  const referencePreflight = await reference.evaluate(({ selector }) => {
    const node = document.querySelector(selector);
    const rect = node?.getBoundingClientRect();
    return {
      pageUrl: String(window.location.href),
      selectorFound: !!node,
      selectorVisible: !!(node && rect && rect.width > 0 && rect.height > 0),
      selectorRect: rect ? { x: rect.x, y: rect.y, width: rect.width, height: rect.height } : null
    };
  }, { selector: referenceSel });

  assertTrue(referencePreflight.selectorFound, `reference selector exists (${referenceSel})`);
  assertTrue(referencePreflight.selectorVisible, `reference selector visible (${referenceSel})`);

  await generated.locator(generatedSel).first().scrollIntoViewIfNeeded();
  await generated.locator(generatedSel).first().screenshot({ path: generatedPng });
  await reference.locator(referenceSel).first().scrollIntoViewIfNeeded();
  await reference.locator(referenceSel).first().screenshot({ path: referencePng });

  await browser.close();

  const score = rmse(generatedPng, referencePng);
  const metadata = {
    capturedAt: new Date().toISOString(),
    generated: generatedPreflight,
    reference: referencePreflight,
    expected: {
      themeClass: expectedThemeClass,
      generalCss: expectedGeneralCss,
      showcaseCss: expectedShowcaseCss
    },
    artifacts: {
      generatedPng,
      referencePng
    },
    rmse: Number.isFinite(score) ? Number(score.toFixed(5)) : null
  };
  await fs.writeFile(metaPath, `${JSON.stringify(metadata, null, 2)}\n`, "utf8");

  console.log(`generated: ${generatedPng}`);
  console.log(`reference: ${referencePng}`);
  console.log(`rmse: ${Number.isFinite(score) ? score.toFixed(5) : "NaN"}`);
  console.log(`meta: ${metaPath}`);
}

main().catch((e) => {
  console.error(String(e?.message || e));
  process.exit(1);
});
