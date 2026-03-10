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

function configs() {
  return [
    { label: "dpr1-z100", dpr: 1, zoom: 1 },
    { label: "dpr1-z125", dpr: 1, zoom: 1.25 },
    { label: "dpr1-z150", dpr: 1, zoom: 1.5 },
    { label: "dpr2-z100", dpr: 2, zoom: 1 },
    { label: "dpr2-z125", dpr: 2, zoom: 1.25 },
    { label: "dpr2-z150", dpr: 2, zoom: 1.5 }
  ];
}

async function checkOne(browser, cfg) {
  const context = await browser.newContext({
    viewport: { width: 1600, height: 2200 },
    deviceScaleFactor: cfg.dpr
  });
  const page = await context.newPage();
  const guard = installPlaywrightErrorGuard(page);
  const mark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(mark, `Solar list-group zoom contract (${cfg.label})`);

  const result = await page.evaluate((zoom) => {
    document.body.style.zoom = String(zoom);

    const rows = Array.from(document.querySelectorAll(".showcase-list-group-row")).slice(0, 8);
    const bad = rows
      .map((row, i) => {
        const badge = row.querySelector(".we-badge");
        const label = row.querySelector("[data-we-widget='text']");
        if (!badge) return { i, reason: "missing badge" };
        const rr = row.getBoundingClientRect();
        const br = badge.getBoundingClientRect();
        const lr = label ? label.getBoundingClientRect() : null;
        const clipped = br.top < rr.top || br.bottom > rr.bottom;
        const rowMid = rr.top + rr.height / 2;
        const badgeMid = br.top + br.height / 2;
        const labelMid = lr ? lr.top + lr.height / 2 : rowMid;
        const centered = Math.abs(badgeMid - rowMid) <= 2 && Math.abs(labelMid - rowMid) <= 2;
        return {
          i,
          clipped,
          centered,
          rowHeight: rr.height,
          badgeHeight: br.height,
          rowTop: rr.top,
          rowBottom: rr.bottom,
          badgeTop: br.top,
          badgeBottom: br.bottom,
          rowMid,
          badgeMid,
          labelMid
        };
      })
      .filter((r) => r.clipped || r.reason || !r.centered);

    const list = document.querySelector(".we-list-group");
    const listStyle = list ? getComputedStyle(list) : null;
    return {
      bad,
      listOverflow: listStyle ? `${listStyle.overflowX}/${listStyle.overflowY}` : "n/a"
    };
  }, cfg.zoom);

  await context.close();
  return result;
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const failures = [];

  for (const cfg of configs()) {
    const r = await checkOne(browser, cfg);
    if (r.bad.length > 0) {
      failures.push({ cfg: cfg.label, ...r });
    }
  }

  await browser.close();

  if (failures.length > 0) {
    console.error("FAIL");
    console.error(JSON.stringify(failures, null, 2));
    process.exit(1);
  }

  console.log("PASS");
  console.log("list-group clipping absent across zoom/DPR matrix");
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});
