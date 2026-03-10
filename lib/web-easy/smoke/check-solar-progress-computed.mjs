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

function assertTrue(v, label) {
  if (!v) {
    throw new Error(`${label}: expected true, got ${String(v)}`);
  }
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 2200 } });
  const guard = installPlaywrightErrorGuard(page);
  const mark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(mark, "Solar progress computed check");

  const got = await page.evaluate(() => {
    const stripe = document.querySelector(".showcase-progress-fill-striped");
    const animated = document.querySelector(".showcase-progress-fill-animated");
    const success = document.querySelector(".showcase-progress-fill-success");
    const warn = document.querySelector(".showcase-progress-fill-warn");
    const css = (el) => (el ? getComputedStyle(el) : null);
    return {
      stripedBg: css(stripe)?.backgroundImage ?? "",
      stripedSize: css(stripe)?.backgroundSize ?? "",
      animatedName: css(animated)?.animationName ?? "",
      animatedDuration: css(animated)?.animationDuration ?? "",
      successBg: css(success)?.backgroundColor ?? "",
      warnBg: css(warn)?.backgroundColor ?? ""
    };
  });

  assertTrue(got.stripedBg.includes("linear-gradient"), "striped background image");
  assertTrue(
    got.stripedBg.includes("45deg") || got.stripedBg.includes("135deg"),
    "striped gradient angle"
  );
  assertTrue(got.stripedSize.length > 0, "striped background size");
  assertTrue(got.animatedName.includes("we-progress-bar-stripes"), "animated stripe keyframes");
  assertTrue(got.animatedDuration === "1s", "animated stripe duration");
  assertTrue(got.successBg === "rgb(42, 161, 152)", "success fill color");
  assertTrue(got.warnBg === "rgb(203, 75, 22)", "warning fill color");

  await browser.close();
  console.log("PASS");
  console.log("progress striped/animated/color contract satisfied");
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});
