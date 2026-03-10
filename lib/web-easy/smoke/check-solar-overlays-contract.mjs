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
  guard.assertSince(mark, "Solar overlays contract");

  const result = await page.evaluate(() => {
    const accWrap = document.querySelector(".showcase-accordion-wrap");
    const acc = accWrap?.querySelector(".we-accordion");
    const trigger = acc?.querySelector(".we-accordion-trigger");
    const panel = acc?.querySelector(".we-accordion-panel");

    const dialogsGrid = document.querySelector(".showcase-dialogs-grid.we-grid");
    const left = dialogsGrid?.children?.[0]?.getBoundingClientRect();
    const right = dialogsGrid?.children?.[1]?.getBoundingClientRect();

    const popoverRow = document.querySelector(".showcase-dialogs-row.we-inline");
    const tooltipButtons = [...document.querySelectorAll(".showcase-tooltip-left .we-button, .showcase-tooltip-top .we-button, .showcase-tooltip-bottom .we-button, .showcase-tooltip-right .we-button")];
    const staticToast = document.querySelector(".showcase-static-toast");
    const staticToastTitle = staticToast?.querySelector(".showcase-static-toast-title");
    const staticToastBody = staticToast?.querySelector(".showcase-static-toast-body");

    const ts = trigger ? getComputedStyle(trigger) : null;
    const ps = panel ? getComputedStyle(panel) : null;

    return {
      accordionWidth: accWrap?.getBoundingClientRect().width ?? 0,
      triggerPadding: ts?.padding ?? "",
      panelPadding: ps?.padding ?? "",
      dialogsTwoCols: !!(left && right && left.right <= right.left + 1),
      popoverRowExists: !!popoverRow,
      tooltipBtnCount: tooltipButtons.length,
      staticToastExists: !!staticToast,
      staticToastTitleText: staticToastTitle?.textContent?.trim() ?? "",
      staticToastBodyText: staticToastBody?.textContent?.trim() ?? ""
    };
  });

  assertTrue(result.accordionWidth > 300, "accordion width is constrained");
  assertTrue(result.dialogsTwoCols, "dialogs is two-column layout");
  assertTrue(result.popoverRowExists, "popover row exists");
  assertTrue(result.tooltipBtnCount >= 4, "tooltip triggers count");
  assertTrue(result.staticToastExists, "static toast exists");
  assertTrue(result.staticToastTitleText.length > 0, "static toast title");
  assertTrue(result.staticToastBodyText.length > 0, "static toast body");

  console.log("PASS");
  console.log(
    `accordion-width=${result.accordionWidth}, trigger-padding=${result.triggerPadding}, panel-padding=${result.panelPadding}`
  );
  await browser.close();
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});
