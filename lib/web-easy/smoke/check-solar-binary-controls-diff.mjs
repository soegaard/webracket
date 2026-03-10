#!/usr/bin/env node

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
  if (v === undefined || v === null) {
    return NaN;
  }
  const n = Number(String(v).replace("px", ""));
  return Number.isFinite(n) ? n : NaN;
}

async function captureControls(page) {
  return page.evaluate(() => {
    function sectionByLegend(text) {
      const legends = Array.from(document.querySelectorAll("legend"));
      const legend = legends.find((el) =>
        (el.textContent || "").replace(/\s+/g, " ").trim().toLowerCase() === text.toLowerCase()
      );
      return legend ? legend.closest("fieldset") : null;
    }

    function checkboxState(section) {
      if (!section) {
        return null;
      }
      const inputs = Array.from(section.querySelectorAll("input[type='checkbox']"));
      return inputs.map((el) => ({
        checked: el.checked,
        disabled: el.disabled,
      }));
    }

    function radioState(section) {
      if (!section) {
        return null;
      }
      const inputs = Array.from(section.querySelectorAll("input[type='radio']"));
      return inputs.map((el) => ({
        checked: el.checked,
        disabled: el.disabled,
      }));
    }

    function firstLabelMetrics(section) {
      if (!section) {
        return null;
      }
      const label = section.querySelector("label, .we-text, [data-we-widget='text']");
      if (!label) {
        return null;
      }
      const cs = getComputedStyle(label);
      return {
        fontSize: cs.fontSize,
        lineHeight: cs.lineHeight,
      };
    }

    function firstSwitchMetrics(section) {
      if (!section) {
        return null;
      }
      const input = section.querySelector("input[type='checkbox']");
      if (!input) {
        return null;
      }
      const cs = getComputedStyle(input);
      return {
        width: cs.width,
        height: cs.height,
        borderRadius: cs.borderRadius,
        borderColor: cs.borderColor,
        backgroundColor: cs.backgroundColor,
      };
    }

    const radios = sectionByLegend("Radio buttons");
    const checks = sectionByLegend("Checkboxes");
    const switches = sectionByLegend("Switches");

    return {
      radios: {
        state: radioState(radios),
        label: firstLabelMetrics(radios),
      },
      checkboxes: {
        state: checkboxState(checks),
        label: firstLabelMetrics(checks),
      },
      switches: {
        state: checkboxState(switches),
        label: firstLabelMetrics(switches),
        firstSwitch: firstSwitchMetrics(switches),
      },
    };
  });
}

function delta(a, b) {
  const x = parsePx(a);
  const y = parsePx(b);
  if (!Number.isFinite(x) || !Number.isFinite(y)) {
    return "n/a";
  }
  return `${(x - y).toFixed(2)}px`;
}

function sameState(a, b) {
  return JSON.stringify(a) === JSON.stringify(b);
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

  const { chromium } = playwrightPkg;
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 2400 } });
  const guard = installPlaywrightErrorGuard(page);

  const genMark = guard.mark();
  await page.goto(generatedUrl, { waitUntil: "networkidle" });
  guard.assertSince(genMark, "Generated solar showcase");
  const generated = await captureControls(page);

  const refMark = guard.mark();
  await page.goto(referenceUrl, { waitUntil: "domcontentloaded" });
  guard.assertSince(refMark, "Reference solar showcase");
  const reference = await captureControls(page);

  await browser.close();

  let failCount = 0;

  const groups = ["radios", "checkboxes", "switches"];
  for (const group of groups) {
    const gState = generated[group]?.state || null;
    const rState = reference[group]?.state || null;
    const ok = sameState(gState, rState);
    if (!ok) {
      failCount += 1;
    }
    console.log(`[${group}] state match: ${ok ? "yes" : "no"}`);
    console.log(`  generated: ${JSON.stringify(gState)}`);
    console.log(`  reference: ${JSON.stringify(rState)}`);
  }

  console.log("");
  console.log("[labels] font deltas (generated-reference):");
  for (const group of groups) {
    const gLabel = generated[group]?.label || null;
    const rLabel = reference[group]?.label || null;
    console.log(`  ${group}: fs ${delta(gLabel?.fontSize, rLabel?.fontSize)}, lh ${delta(gLabel?.lineHeight, rLabel?.lineHeight)}`);
  }

  console.log("");
  console.log("[switch] first toggle style deltas (generated-reference):");
  const gs = generated.switches?.firstSwitch || null;
  const rs = reference.switches?.firstSwitch || null;
  console.log(`  width ${delta(gs?.width, rs?.width)}`);
  console.log(`  height ${delta(gs?.height, rs?.height)}`);
  console.log(`  radius ${delta(gs?.borderRadius, rs?.borderRadius)}`);
  console.log(`  border-color generated=${gs?.borderColor || "n/a"} reference=${rs?.borderColor || "n/a"}`);
  console.log(`  background generated=${gs?.backgroundColor || "n/a"} reference=${rs?.backgroundColor || "n/a"}`);

  if (failCount > 0) {
    console.error(`FAIL: ${failCount} control state group(s) differ.`);
    process.exit(1);
  }
  console.log("PASS: radio/checkbox/switch state patterns match reference.");
}

main().catch((err) => {
  console.error(err && err.message ? err.message : String(err));
  process.exit(1);
});
