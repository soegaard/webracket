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

    const rows = Array.from(document.querySelectorAll(".showcase-list-group-row"));
    const list = document.querySelector(".we-list-group");
    const listStyle = list ? getComputedStyle(list) : null;
    const clippingRows = rows
      .map((row, i) => {
        const badge = row.querySelector(".we-badge");
        const label = row.querySelector("[data-we-widget='text']");
        if (!badge || !label) {
          return { i, reason: "missing badge or label" };
        }
        const rr = row.getBoundingClientRect();
        const br = badge.getBoundingClientRect();
        const lr = label.getBoundingClientRect();
        const rowMid = rr.top + (rr.height / 2);
        const badgeMid = br.top + (br.height / 2);
        const labelMid = lr.top + (lr.height / 2);
        const clipped = br.top < rr.top || br.bottom > rr.bottom;
        const centered = Math.abs(badgeMid - rowMid) <= 2 && Math.abs(labelMid - rowMid) <= 2;
        return { i, clipped, centered, rowHeight: rr.height, badgeHeight: br.height };
      })
      .filter((r) => r.reason || r.clipped || !r.centered);

    const firstRow = rows[0] || null;
    const firstBadge = firstRow ? firstRow.querySelector(".we-badge") : null;
    const firstRowRect = firstRow?.getBoundingClientRect();
    const firstBadgeRect = firstBadge?.getBoundingClientRect();
    const firstRowStyle = firstRow ? getComputedStyle(firstRow) : null;
    const firstBadgeStyle = firstBadge ? getComputedStyle(firstBadge) : null;

    const richMeta = [...document.querySelectorAll(".showcase-rich-list-meta")]
      .map((n) => n.textContent?.trim())
      .filter(Boolean);
    const richFoot = [...document.querySelectorAll(".showcase-rich-list-footnote")]
      .map((n) => n.textContent?.trim())
      .filter(Boolean);

    return {
      overlap,
      clippingRows,
      rowCount: rows.length,
      rowHeight: firstRowRect?.height ?? null,
      badgeHeight: firstBadgeRect?.height ?? null,
      rowLineHeight: firstRowStyle?.lineHeight ?? null,
      badgeLineHeight: firstBadgeStyle?.lineHeight ?? null,
      badgePosition: firstBadgeStyle?.position ?? null,
      listOverflowX: listStyle?.overflowX ?? null,
      listOverflowY: listStyle?.overflowY ?? null,
      richMetaCount: richMeta.length,
      richFootCount: richFoot.length
    };
  });

  assertEqual(result.overlap, false, "containers columns overlap");
  assertTrue(result.rowCount >= 8, "list-group row count >= 8");
  assertEqual(result.clippingRows.length, 0, "list-group badge clipping/alignment rows");
  assertEqual(result.badgePosition, "static", "badge position");
  assertTrue(result.rowHeight >= 41, "list-group row height >= 41");
  assertTrue(result.badgeHeight >= 20, "badge height >= 20");
  assertEqual(result.listOverflowX, "visible", "list-group overflow-x");
  assertEqual(result.listOverflowY, "visible", "list-group overflow-y");
  assertTrue(result.richMetaCount >= 2, "rich list-group meta lines");
  assertTrue(result.richFootCount >= 2, "rich list-group footnote lines");

  console.log("PASS");
  console.log(
    `rows=${result.rowCount}, row=${result.rowHeight}, badge=${result.badgeHeight}, row-lh=${result.rowLineHeight}, badge-lh=${result.badgeLineHeight}, overflow=${result.listOverflowX}/${result.listOverflowY}`
  );
  await browser.close();
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});
