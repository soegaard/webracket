#!/usr/bin/env node

import fs from "node:fs/promises";
import path from "node:path";
import { createRequire } from "node:module";
import process from "node:process";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

async function resolvePlaywright() {
  const localNodeModules = process.env.SMOKE_NODE_MODULES || "";
  const requireHere = createRequire(import.meta.url);
  try {
    return requireHere("playwright");
  } catch (_err) {
    if (!localNodeModules) {
      try {
        const localFallback = path.resolve(process.cwd(), "../../../.local-tools/node_modules");
        const requireFromFallback = createRequire(path.join(localFallback, "_resolver.js"));
        return requireFromFallback("playwright");
      } catch (_err3) {
        return null;
      }
    }
    try {
      const requireFromLocal = createRequire(path.join(localNodeModules, "_resolver.js"));
      return requireFromLocal("playwright");
    } catch (_err2) {
      return null;
    }
  }
}

async function comparePngBuffers(page, leftBytes, rightBytes) {
  const leftBase64 = Buffer.from(leftBytes).toString("base64");
  const rightBase64 = Buffer.from(rightBytes).toString("base64");
  return page.evaluate(async ({ leftBase64In, rightBase64In }) => {
    const decodePng = async (b64) => {
      const bin = atob(b64);
      const bytes = new Uint8Array(bin.length);
      for (let i = 0; i < bin.length; i += 1) bytes[i] = bin.charCodeAt(i);
      const blob = new Blob([bytes], { type: "image/png" });
      return createImageBitmap(blob);
    };

    const left = await decodePng(leftBase64In);
    const right = await decodePng(rightBase64In);
    const width = Math.min(left.width, right.width);
    const height = Math.min(left.height, right.height);

    const canvasA = document.createElement("canvas");
    const canvasB = document.createElement("canvas");
    canvasA.width = width;
    canvasA.height = height;
    canvasB.width = width;
    canvasB.height = height;

    const ctxA = canvasA.getContext("2d");
    const ctxB = canvasB.getContext("2d");
    ctxA.drawImage(left, 0, 0, width, height);
    ctxB.drawImage(right, 0, 0, width, height);

    const dataA = ctxA.getImageData(0, 0, width, height).data;
    const dataB = ctxB.getImageData(0, 0, width, height).data;

    let sumSq = 0;
    const n = width * height * 4;
    for (let i = 0; i < dataA.length; i += 1) {
      const d = dataA[i] - dataB[i];
      sumSq += d * d;
    }

    const rmse255 = Math.sqrt(sumSq / n);
    return { width, height, rmse: rmse255 / 255 };
  }, { leftBase64In: leftBase64, rightBase64In: rightBase64 });
}

async function main() {
  const playwrightPkg = await resolvePlaywright();
  if (!playwrightPkg) {
    console.error("Playwright is not installed.");
    process.exit(2);
  }

  const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
  const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;
  const referenceUrl = process.env.SOLAR_REFERENCE_URL || `file://${path.resolve(process.cwd(), "../../../tmp/bootswatch-solar/bootswatch-solar.html")}`;
  const outDir = path.resolve(process.cwd(), "generated", "solar-indicators-diff");
  const thresholdRaw = process.env.SOLAR_INDICATORS_DIFF_MAX || "0.34";
  const threshold = Number.isFinite(Number(thresholdRaw)) ? Number(thresholdRaw) : 0.34;

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 2400 } });
  const guard = installPlaywrightErrorGuard(page);

  const generatedMark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(generatedMark, "Solar indicators diff generated page");
  const generatedSection = page.locator("#solar2-indicators-section").first();
  await generatedSection.scrollIntoViewIfNeeded();
  const generatedPng = await generatedSection.screenshot();

  const referenceMark = guard.mark();
  await page.goto(referenceUrl, { waitUntil: "domcontentloaded" });
  guard.assertSince(referenceMark, "Solar indicators diff reference page");
  const referenceSection = page.locator('.bs-docs-section:has(#indicators)').first();
  await referenceSection.scrollIntoViewIfNeeded();
  const referencePng = await referenceSection.screenshot();

  await fs.mkdir(outDir, { recursive: true });
  await fs.writeFile(path.join(outDir, "indicators-generated.png"), generatedPng);
  await fs.writeFile(path.join(outDir, "indicators-reference.png"), referencePng);

  const comparePage = await browser.newPage();
  const imageDiff = await comparePngBuffers(comparePage, generatedPng, referencePng);
  await comparePage.close();

  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  const generatedCounts = await page.evaluate(() => {
    const root = document.querySelector("#solar2-indicators-section");
    if (!root) return null;
    return {
      alerts: root.querySelectorAll(".we-alert").length,
      badges: root.querySelectorAll(".we-badge").length,
    };
  });

  const report = {
    rmse: imageDiff.rmse,
    threshold,
    generatedCounts,
  };
  await fs.writeFile(path.join(outDir, "report.json"), `${JSON.stringify(report, null, 2)}\n`, "utf8");

  console.log(`indicators rmse: ${imageDiff.rmse.toFixed(5)} (threshold ${threshold.toFixed(5)})`);
  if (generatedCounts) {
    console.log(`counts: alerts=${generatedCounts.alerts}, badges=${generatedCounts.badges}`);
  }
  console.log(`artifacts: ${outDir}`);

  await browser.close();

  if (!generatedCounts || generatedCounts.alerts < 7 || generatedCounts.badges < 16) {
    console.error("Indicators structure check failed.");
    process.exit(1);
  }

  if (imageDiff.rmse > threshold) {
    console.error(`Indicators visual diff too large: ${imageDiff.rmse.toFixed(5)} > ${threshold.toFixed(5)}`);
    process.exit(1);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
