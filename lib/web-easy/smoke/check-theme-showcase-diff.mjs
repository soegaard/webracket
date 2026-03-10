#!/usr/bin/env node

import { writeFile } from "node:fs/promises";
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
      return null;
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
      for (let i = 0; i < bin.length; i += 1) {
        bytes[i] = bin.charCodeAt(i);
      }
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

    let changed = 0;
    let sum = 0;
    const n = width * height;
    for (let i = 0; i < dataA.length; i += 4) {
      const dr = Math.abs(dataA[i] - dataB[i]);
      const dg = Math.abs(dataA[i + 1] - dataB[i + 1]);
      const db = Math.abs(dataA[i + 2] - dataB[i + 2]);
      const da = Math.abs(dataA[i + 3] - dataB[i + 3]);
      const px = (dr + dg + db + da) / 4;
      sum += px;
      if (px > 8) {
        changed += 1;
      }
    }

    return {
      width,
      height,
      changedPixels: changed,
      changedPercent: (changed / n) * 100,
      meanChannelDiff: sum / n,
    };
  }, { leftBase64In: leftBase64, rightBase64In: rightBase64 });
}

async function main() {
  const playwrightPkg = await resolvePlaywright();
  if (!playwrightPkg) {
    console.error("Playwright is not installed.");
    console.error("Install with one of:");
    console.error("  npm install --save-dev playwright");
    console.error("  npm --prefix .local-tools install --save-dev playwright");
    process.exit(2);
  }

  const baseUrl = process.env.SMOKE_BASE_URL || "http://127.0.0.1:8765";
  const thresholdRaw = process.env.SMOKE_SHOWCASE_DIFF_MAX || "20";
  const parsed = Number(thresholdRaw);
  const threshold = Number.isFinite(parsed) && parsed >= 0 ? parsed : 20;
  const outDir = process.env.SMOKE_SHOWCASE_DIFF_OUT || "/tmp";
  const topOnly = process.env.SMOKE_SHOWCASE_TOP_ONLY === "1";
  const clipHeightRaw = process.env.SMOKE_SHOWCASE_TOP_CLIP_HEIGHT || "360";
  const clipHeightParsed = Number(clipHeightRaw);
  const clipHeight = Number.isFinite(clipHeightParsed) && clipHeightParsed > 0 ? clipHeightParsed : 360;

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
  const guard = installPlaywrightErrorGuard(page);

  try {
    const demoMark = guard.mark();
    await page.goto(`${baseUrl}/demo-theme-showcase.html`, { waitUntil: "networkidle" });
    guard.assertSince(demoMark, "Theme diff demo page");
    const demoTop = await page.screenshot({ clip: { x: 0, y: 0, width: 1440, height: clipHeight } });
    const demoFull = await page.screenshot({ fullPage: true });

    const generatedMark = guard.mark();
    await page.goto(`${baseUrl}/generated/example-browser-theme-showcase.html`, { waitUntil: "networkidle" });
    guard.assertSince(generatedMark, "Theme diff generated page");
    const genTop = await page.screenshot({ clip: { x: 0, y: 0, width: 1440, height: clipHeight } });
    const genFull = await page.screenshot({ fullPage: true });

    const top = await comparePngBuffers(page, demoTop, genTop);
    const full = topOnly ? null : await comparePngBuffers(page, demoFull, genFull);

    await writeFile(path.join(outDir, "web-easy-demo-top.png"), demoTop);
    await writeFile(path.join(outDir, "web-easy-generated-top.png"), genTop);
    if (!topOnly) {
      await writeFile(path.join(outDir, "web-easy-demo-full.png"), demoFull);
      await writeFile(path.join(outDir, "web-easy-generated-full.png"), genFull);
    }

    console.log(`top diff:  ${top.changedPercent.toFixed(2)}% changed, mean=${top.meanChannelDiff.toFixed(2)}`);
    if (full) {
      console.log(`full diff: ${full.changedPercent.toFixed(2)}% changed, mean=${full.meanChannelDiff.toFixed(2)}`);
    }
    console.log(`artifacts: ${outDir}/web-easy-demo-top.png`);
    console.log(`           ${outDir}/web-easy-generated-top.png`);
    if (full) {
      console.log(`           ${outDir}/web-easy-demo-full.png`);
      console.log(`           ${outDir}/web-easy-generated-full.png`);
    }

    await browser.close();

    if (top.changedPercent > threshold) {
      console.error(`FAIL: top diff ${top.changedPercent.toFixed(2)}% is above threshold ${threshold.toFixed(2)}%`);
      process.exit(1);
    }

    console.log(`PASS: top diff is within threshold (${threshold.toFixed(2)}%)`);
    process.exit(0);
  } catch (err) {
    await browser.close();
    console.error("Theme showcase diff run failed.");
    console.error(err && err.message ? err.message : String(err));
    process.exit(1);
  }
}

main();
