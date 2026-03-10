#!/usr/bin/env node
import path from "node:path";
import { pathToFileURL } from "node:url";
import { installPlaywrightErrorGuard } from "./playwright-error-guard.mjs";

const cwd = process.cwd();
const baseUrl = process.env.SMOKE_BASE_URL || "http://localhost:8000";
const generatedUrl = process.env.SOLAR_GENERATED_URL || `${baseUrl}/generated/example-browser-solar-showcase.html`;
const referenceUrl = process.env.SOLAR_REFERENCE_URL || `${baseUrl}/generated/reference-solar/index.html`;

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
    throw new Error(`${label}: expected ${expected}, got ${actual}`);
  }
}

function assertNonEmpty(value, label) {
  if (!value) {
    throw new Error(`${label}: got empty value`);
  }
}

async function main() {
  const { chromium } = await loadPlaywright();
  const browser = await chromium.launch({ headless: true });
  const generated = await browser.newPage({ viewport: { width: 1600, height: 2200 } });
  const reference = await browser.newPage({ viewport: { width: 1600, height: 2200 } });
  const generatedGuard = installPlaywrightErrorGuard(generated);
  const referenceGuard = installPlaywrightErrorGuard(reference);

  const gMark = generatedGuard.mark();
  await generated.goto(generatedUrl, { waitUntil: "networkidle" });
  generatedGuard.assertSince(gMark, "Solar table generated page");
  await generated.waitForFunction(() => !!document.querySelector("#solar2-tables") && document.querySelectorAll("table.we-table").length > 0);
  const rMark = referenceGuard.mark();
  await reference.goto(referenceUrl, { waitUntil: "networkidle" });
  referenceGuard.assertSince(rMark, "Solar table reference page");
  await reference.waitForFunction(() => !!document.querySelector("#tables") && document.querySelectorAll("table.table").length > 0);

  const g = await generated.evaluate(() => {
    const sectionTitle = document.querySelector("#solar2-tables");
    const table = sectionTitle?.nextElementSibling?.matches("table.we-table")
      ? sectionTitle.nextElementSibling
      : document.querySelector("table.we-table");
    const rows = Array.from(table?.querySelectorAll("tr") ?? []);
    const headerRow = rows.length > 0 ? rows[0] : null;
    const dataRows = rows.slice(1);
    const rowByLabel = (label) =>
      dataRows.find((row) => {
        const cell = row.querySelector("th,td");
        return cell && cell.textContent && cell.textContent.trim() === label;
      }) ?? null;
    const rowActive = rowByLabel("Active");
    const rowDefault = rowByLabel("Default");
    const rowPrimary = rowByLabel("Primary");
    const th = headerRow?.querySelector("th");
    const cs = (el) => (el ? getComputedStyle(el) : null);
    return {
      tableFound: !!table,
      headerWeight: cs(th)?.fontWeight ?? "",
      activeBg: cs(rowActive)?.backgroundColor ?? "",
      defaultBg: cs(rowDefault)?.backgroundColor ?? "",
      primaryBg: cs(rowPrimary)?.backgroundColor ?? "",
      borderColor: cs(rowDefault)?.borderTopColor ?? "",
      hoverBg: cs(rowDefault)?.backgroundColor ?? ""
    };
  });

  const r = await reference.evaluate(() => {
    const sec = document.querySelector("#tables")?.closest(".bs-docs-section");
    const table = sec?.querySelector("table.table.table-hover") || sec?.querySelector("table.table");
    const rowActive = table?.querySelector("tbody tr.table-active");
    const rowDefault = table?.querySelector("tbody tr:nth-child(2)");
    const rowPrimary = table?.querySelector("tbody tr.table-primary");
    const th = table?.querySelector("thead th");
    const cs = (el) => (el ? getComputedStyle(el) : null);
    return {
      tableFound: !!table,
      headerWeight: cs(th)?.fontWeight ?? "",
      activeBg: cs(rowActive)?.backgroundColor ?? "",
      defaultBg: cs(rowDefault)?.backgroundColor ?? "",
      primaryBg: cs(rowPrimary)?.backgroundColor ?? "",
      borderColor: cs(rowDefault)?.borderTopColor ?? ""
    };
  });

  await browser.close();

  assertEqual(g.tableFound, true, "generated table located");
  assertEqual(r.tableFound, true, "reference table located");
  assertNonEmpty(g.headerWeight, "generated table header font weight");
  assertNonEmpty(r.headerWeight, "reference table header font weight");
  assertEqual(g.headerWeight, r.headerWeight, "table header font weight");
  assertEqual(g.activeBg, r.activeBg, "active row background");
  assertEqual(g.defaultBg, r.defaultBg, "default row background");
  assertEqual(g.primaryBg, r.primaryBg, "primary row background");
  assertEqual(g.borderColor, r.borderColor, "row border color");

  console.log("PASS");
  console.log("table computed values match reference for key row variants");
}

main().catch((e) => {
  console.error(`FAIL\n${String(e.message || e)}`);
  process.exit(1);
});
