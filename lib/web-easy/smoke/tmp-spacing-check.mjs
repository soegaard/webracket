#!/usr/bin/env node
import { createRequire } from 'node:module';
import path from 'node:path';

const requireHere = createRequire(import.meta.url);
let playwright;
try { playwright = requireHere('playwright'); }
catch {
  const requireLocal = createRequire('/Users/soegaard/Dropbox/GitHub/webracket/.local-tools/node_modules/_resolver.js');
  playwright = requireLocal('playwright');
}

const base = 'http://localhost:8000';
const generatedUrl = `${base}/generated/example-browser-solar-showcase.html`;
const referenceUrl = `file://${path.resolve(process.cwd(), '../../../tmp/bootswatch-solar/bootswatch-solar.html')}`;

const points = [
  ['legend', '.showcase-forms-left .we-group-legend', '#forms'],
  ['email-label', 'text:Email', 'label[for="staticEmail"]'],
  ['email-value', '.we-form-control-plaintext.we-input', '#staticEmail'],
  ['email-address-label', 'text:Email address', 'label[for="exampleInputEmail1"]'],
  ['email-input', '.showcase-field-email.we-input', '#exampleInputEmail1'],
  ['password-label', 'text:Password', 'label[for="exampleInputPassword1"]'],
  ['password-input', '.showcase-field-password.we-input', '#exampleInputPassword1'],
  ['select-label', 'text:Example select', 'label[for="exampleSelect1"]'],
  ['select-input', '.showcase-field-select.we-choice', '#exampleSelect1'],
  ['textarea-label', 'text:Example textarea', 'label[for="exampleTextarea"]'],
  ['textarea-input', '.showcase-field-textarea.we-textarea', '#exampleTextarea'],
  ['submit', '.showcase-forms-left button.we-button', 'button.btn.btn-primary']
];
async function q(page, sel) {
  if (sel.startsWith('text:')) {
    const text = sel.slice(5);
    return page.$$eval('legend,label,[data-we-widget=\"text\"],span,div', (els, t) => {
      const needle = t.trim().toLowerCase();
      const found = els.find((el) => {
        const hay = (el.textContent || '').replace(/\\s+/g, ' ').trim().toLowerCase();
        return hay.includes(needle);
      });
      if (!found) { throw new Error(`text not found: ${t}`); }
      const r = found.getBoundingClientRect();
      return { y: r.y, h: r.height };
    }, text);
  }
  return page.$eval(sel, el => { const r = el.getBoundingClientRect(); return {y:r.y,h:r.height}; });
}

const browser = await playwright.chromium.launch({ headless: true });
const page = await browser.newPage({ viewport: { width: 1600, height: 2800 } });

await page.goto(generatedUrl, { waitUntil: 'networkidle' });
const g = {};
for (const [name, sel] of points) {
  try { g[name] = await q(page, sel); } catch { g[name] = null; }
}

await page.goto(referenceUrl, { waitUntil: 'domcontentloaded' });
const r = {};
for (const [name,, sel] of points) {
  try { r[name] = await q(page, sel); } catch { r[name] = null; }
}

function delta(a,b){ if(!a||!b) return null; return +(a.y-b.y).toFixed(2); }
function gap(obj,a,b){ if(!obj[a]||!obj[b]) return null; return +((obj[b].y)-(obj[a].y+obj[a].h)).toFixed(2); }

const rows = [
  ['legend->email-label','legend','email-label'],
  ['email-label->email-value','email-label','email-value'],
  ['email-value->email-address-label','email-value','email-address-label'],
  ['email-address-label->email-input','email-address-label','email-input'],
  ['email-input->password-label','email-input','password-label'],
  ['password-label->password-input','password-label','password-input'],
  ['password-input->select-label','password-input','select-label'],
  ['select-label->select-input','select-label','select-input'],
  ['select-input->textarea-label','select-input','textarea-label'],
  ['textarea-label->textarea-input','textarea-label','textarea-input']
];

console.log('absolute y deltas (generated-reference):');
for (const [name] of points) {
  console.log(name, delta(g[name], r[name]));
}

console.log('\nvertical gap deltas (generated-reference):');
for (const [label,a,b] of rows) {
  const gg = gap(g,a,b); const rr = gap(r,a,b);
  console.log(label, gg===null||rr===null?null:+(gg-rr).toFixed(2), `(g=${gg}, r=${rr})`);
}

await browser.close();
