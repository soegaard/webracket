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

function parsePx(v) {
  const n = Number(String(v || "").replace("px", ""));
  return Number.isFinite(n) ? n : NaN;
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

async function metricsFor(page, selector) {
  if (selector.startsWith("text:")) {
    const needle = selector.slice(5).trim().toLowerCase();
    return page.$$eval("label, legend, [data-we-widget=\"text\"], .we-text, span, div", (els, n) => {
      const match = els.find((el) => {
        const hay = (el.textContent || "").replace(/\s+/g, " ").trim().toLowerCase();
        return hay === n;
      });
      if (!match) {
        throw new Error(`text selector not found: ${n}`);
      }
      const cs = getComputedStyle(match);
      const r = match.getBoundingClientRect();
      return {
        fontSize: cs.fontSize,
        lineHeight: cs.lineHeight,
        paddingTop: cs.paddingTop,
        paddingBottom: cs.paddingBottom,
        paddingLeft: cs.paddingLeft,
        paddingRight: cs.paddingRight,
        borderRadius: cs.borderRadius,
        borderColor: cs.borderColor,
        borderTopWidth: cs.borderTopWidth,
        borderTopStyle: cs.borderTopStyle,
        backgroundColor: cs.backgroundColor,
        backgroundImage: cs.backgroundImage,
        color: cs.color,
        y: `${r.y}px`,
        height: `${r.height}px`,
        width: `${r.width}px`,
      };
    }, needle);
  }
  return page.$eval(selector, (el) => {
    const cs = getComputedStyle(el);
    const r = el.getBoundingClientRect();
    return {
      fontSize: cs.fontSize,
      lineHeight: cs.lineHeight,
      paddingTop: cs.paddingTop,
      paddingBottom: cs.paddingBottom,
      paddingLeft: cs.paddingLeft,
      paddingRight: cs.paddingRight,
      borderRadius: cs.borderRadius,
      borderColor: cs.borderColor,
      borderTopWidth: cs.borderTopWidth,
      borderTopStyle: cs.borderTopStyle,
      backgroundColor: cs.backgroundColor,
      backgroundImage: cs.backgroundImage,
      color: cs.color,
      y: `${r.y}px`,
      height: `${r.height}px`,
      width: `${r.width}px`,
    };
  });
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
  const outDir = path.resolve(process.cwd(), "generated", "solar-forms-diff");
  const thresholdRaw = process.env.SOLAR_FORMS_DIFF_MAX || "0.085";
  const threshold = Number.isFinite(Number(thresholdRaw)) ? Number(thresholdRaw) : 0.085;

  const checks = [
    ["label-email-address", "text:Email address", "label[for=\"exampleInputEmail1\"]"],
    ["label-password", "text:Password", "label[for=\"exampleInputPassword1\"]"],
    ["label-select", "text:Example select", "label[for=\"exampleSelect1\"]"],
    ["label-select-multiple", "text:Example multiple select", "label[for=\"exampleSelect2\"]"],
    ["label-textarea", "text:Example textarea", "label[for=\"exampleTextarea\"]"],
    ["disabled-input", ".we-form-state-disabled .we-input, .we-form-state-disabled input.we-input, .we-form-state-disabled.we-input", "#disabledInput"],
    ["readonly-input", ".showcase-forms-right input[readonly], .showcase-forms-right .we-input[readonly]", "#readOnlyInput"],
    ["plaintext", ".we-form-control-plaintext .we-input, .we-form-control-plaintext input.we-input, .we-form-control-plaintext.we-input, input.we-form-control-plaintext.we-input", "#staticEmail"],
    ["email", "#exampleInputEmail1", "#exampleInputEmail1"],
    ["password-input", "#exampleInputPassword1", "#exampleInputPassword1"],
    ["select", "#exampleSelect1", "#exampleSelect1"],
    ["select-disabled", "#exampleDisabledSelect1", "#exampleDisabledSelect1"],
    ["select-multiple", "#exampleSelect2", "#exampleSelect2"],
    ["textarea", "#exampleTextarea", "#exampleTextarea"],
    ["range", ".we-range-default .we-slider, .we-range-default.we-slider", "#customRange1"],
    ["valid", ".showcase-field-valid .we-input, .showcase-field-valid.we-input", "#inputValid"],
    ["invalid", ".showcase-field-invalid .we-input, .showcase-field-invalid.we-input", "#inputInvalid"],
    ["input-large", ".we-input-lg .we-input, .we-input-lg.we-input", "#inputLarge"],
    ["input-small", ".we-input-sm .we-input, .we-input-sm.we-input", "#inputSmall"],
    ["float-email", ".we-floating-field .we-input, .we-floating-field.we-input", "#floatingInput"],
  ];

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 2400 } });
  const guard = installPlaywrightErrorGuard(page);

  const generatedMark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(generatedMark, "Solar forms diff generated page");
  const generatedSection = page.locator("#solar2-forms").first();
  await generatedSection.scrollIntoViewIfNeeded();
  const generatedPng = await generatedSection.screenshot();

  const referenceMark = guard.mark();
  await page.goto(referenceUrl, { waitUntil: "domcontentloaded" });
  guard.assertSince(referenceMark, "Solar forms diff reference page");
  const referenceSection = page.locator("#forms").first();
  await referenceSection.scrollIntoViewIfNeeded();
  const referencePng = await referenceSection.screenshot();

  await fs.mkdir(outDir, { recursive: true });
  await fs.writeFile(path.join(outDir, "forms-generated.png"), generatedPng);
  await fs.writeFile(path.join(outDir, "forms-reference.png"), referencePng);

  const comparePage = await browser.newPage();
  const imageDiff = await comparePngBuffers(comparePage, generatedPng, referencePng);
  await comparePage.close();

  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  const computed = [];
  for (const [name, generatedSelector, referenceSelector] of checks) {
    try {
      const gm = await metricsFor(page, generatedSelector);
      await page.goto(referenceUrl, { waitUntil: "domcontentloaded" });
      const rm = await metricsFor(page, referenceSelector);
      computed.push({ name, generatedSelector, referenceSelector, generated: gm, reference: rm });
      await page.goto(generatedUrl, { waitUntil: "networkidle" });
    } catch (e) {
      computed.push({ name, generatedSelector, referenceSelector, danger: String(e) });
      await page.goto(generatedUrl, { waitUntil: "networkidle" });
    }
  }

  const report = {
    rmse: imageDiff.rmse,
    threshold,
    checks: computed,
  };
  await fs.writeFile(path.join(outDir, "report.json"), `${JSON.stringify(report, null, 2)}\n`, "utf8");

  console.log(`forms rmse: ${imageDiff.rmse.toFixed(5)} (threshold ${threshold.toFixed(5)})`);
  const errorCount = computed.filter((row) => row.danger).length;
  for (const row of computed) {
    if (row.danger) {
      console.log(`- ${row.name}: ERROR ${row.danger}`);
      continue;
    }
    const gfs = parsePx(row.generated.fontSize);
    const rfs = parsePx(row.reference.fontSize);
    const gh = parsePx(row.generated.height);
    const rh = parsePx(row.reference.height);
    const fsDelta = Number.isFinite(gfs) && Number.isFinite(rfs) ? (gfs - rfs) : NaN;
    const hDelta = Number.isFinite(gh) && Number.isFinite(rh) ? (gh - rh) : NaN;
    console.log(`- ${row.name}: fs Δ ${fsDelta.toFixed(2)}px, h Δ ${hDelta.toFixed(2)}px`);
  }
  console.log(`artifacts: ${outDir}`);

  const byName = new Map(computed.filter((r) => !r.danger).map((r) => [r.name, r]));
  const gapPairs = [
    ["email label->input", "label-email-address", "email"],
    ["password label->input", "label-password", "password-input"],
    ["select label->input", "label-select", "select"],
    ["select multiple label->input", "label-select-multiple", "select-multiple"],
    ["textarea label->input", "label-textarea", "textarea"],
  ];
  console.log("");
  console.log("vertical gap deltas (generated-reference):");
  for (const [label, topName, bottomName] of gapPairs) {
    const top = byName.get(topName);
    const bottom = byName.get(bottomName);
    if (!top || !bottom) {
      console.log(`- ${label}: n/a`);
      continue;
    }
    const gTopY = parsePx(top.generated.y);
    const gTopH = parsePx(top.generated.height);
    const gBottomY = parsePx(bottom.generated.y);
    const rTopY = parsePx(top.reference.y);
    const rTopH = parsePx(top.reference.height);
    const rBottomY = parsePx(bottom.reference.y);
    const gGap = gBottomY - (gTopY + gTopH);
    const rGap = rBottomY - (rTopY + rTopH);
    const deltaGap = Number.isFinite(gGap) && Number.isFinite(rGap) ? (gGap - rGap) : NaN;
    console.log(`- ${label}: gap Δ ${deltaGap.toFixed(2)}px (g=${gGap.toFixed(2)} r=${rGap.toFixed(2)})`);
  }

  await browser.close();

  if (errorCount > 0) {
    console.error(`FAIL: ${errorCount} computed-style check(s) could not resolve selectors.`);
    process.exit(1);
  }

  if (imageDiff.rmse > threshold) {
    console.error("FAIL: forms screenshot RMSE exceeds threshold.");
    process.exit(1);
  }
  console.log("PASS: forms screenshot RMSE within threshold.");
}

main().catch((err) => {
  console.error(err && err.message ? err.message : String(err));
  process.exit(1);
});
