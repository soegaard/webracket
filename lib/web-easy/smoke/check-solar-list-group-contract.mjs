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
    throw new Error(`${label}: expected ${String(expected)}, got ${String(actual)}`);
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
  guard.assertSince(mark, "Solar list-group contract");

  const result = await page.evaluate(() => {
    const grid = document.querySelector("#solar2-containers + .we-heading-2 + .we-grid");
    const col1 = grid?.children?.[0]?.getBoundingClientRect();
    const col2 = grid?.children?.[1]?.getBoundingClientRect();
    const overlap = !!(col1 && col2 && col1.right > col2.left);

    const row = document.querySelector(".showcase-list-group-row");
    const badge = row?.querySelector(".we-badge");
    const rowRect = row?.getBoundingClientRect();
    const badgeRect = badge?.getBoundingClientRect();
    const rowStyle = row ? getComputedStyle(row) : null;
    const badgeStyle = badge ? getComputedStyle(badge) : null;
    const listStyle = document.querySelector(".we-list-group")
      ? getComputedStyle(document.querySelector(".we-list-group"))
      : null;

    const clipping = rowRect && badgeRect
      ? (badgeRect.top < rowRect.top || badgeRect.bottom > rowRect.bottom)
      : true;

    const richMeta = [...document.querySelectorAll(".showcase-rich-list-meta")]
      .map((n) => n.textContent?.trim())
      .filter(Boolean);
    const richFoot = [...document.querySelectorAll(".showcase-rich-list-footnote")]
      .map((n) => n.textContent?.trim())
      .filter(Boolean);

    return {
      overlap,
      clipping,
      rowHeight: rowRect?.height ?? null,
      badgeHeight: badgeRect?.height ?? null,
      rowLineHeight: rowStyle?.lineHeight ?? null,
      badgeLineHeight: badgeStyle?.lineHeight ?? null,
      badgePosition: badgeStyle?.position ?? null,
      listOverflow: listStyle?.overflow ?? null,
      richMetaCount: richMeta.length,
      richFootCount: richFoot.length
    };
  });

  assertEqual(result.overlap, false, "containers columns overlap");
  assertEqual(result.clipping, false, "list-group badge clipping");
  assertEqual(result.badgePosition, "static", "badge position");
  assertTrue(result.rowHeight >= 41, "list-group row height >= 41");
  assertTrue(result.badgeHeight >= 20, "badge height >= 20");
  assertTrue(result.richMetaCount >= 2, "rich list-group meta lines");
  assertTrue(result.richFootCount >= 2, "rich list-group footnote lines");

  console.log("PASS");
  console.log(
    `row=${result.rowHeight}, badge=${result.badgeHeight}, row-lh=${result.rowLineHeight}, badge-lh=${result.badgeLineHeight}, overflow=${result.listOverflow}`
  );
  await browser.close();
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});

