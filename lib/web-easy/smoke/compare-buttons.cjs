const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(
  process.env.PLAYWRIGHT_LIB || path.resolve(__dirname, '../../../.local-tools/node_modules/playwright')
);

const bootswatchHtml = fs.readFileSync(path.resolve(__dirname, '../../../tmp/bootswatch.html'), 'utf8');
const bootswatchCss = fs.readFileSync(path.resolve(__dirname, '../../../tmp/solar-bootstrap.css'), 'utf8');

const row = (process.argv[2] || 'solid').toLowerCase();
const label = process.argv[3] || (row === 'sizes' ? 'Default button' : 'Primary');

const props = [
  'display', 'position', 'height', 'min-height',
  'padding-top', 'padding-right', 'padding-bottom', 'padding-left',
  'margin-top', 'margin-right', 'margin-bottom', 'margin-left',
  'background-color', 'color',
  'border-top-width', 'border-top-style', 'border-top-color', 'border-radius',
  'font-family', 'font-size', 'font-weight', 'line-height',
  'opacity', 'cursor', 'text-decoration-line', 'box-shadow'
];

const norm = (v) => (v ?? '').toString().replace(/\s+/g, ' ').trim();
const diffMap = (a, b) => {
  const out = [];
  for (const p of props) {
    const av = norm(a[p]);
    const bv = norm(b[p]);
    if (av !== bv) out.push({ prop: p, bootswatch: av, webEasy: bv });
  }
  return out;
};

const rowIndexMap = {
  solid:    0,
  disabled: 1,
  outline:  2,
  sizes:    3
};

if (!(row in rowIndexMap)) {
  console.error(`Unknown row '${row}'. Use one of: ${Object.keys(rowIndexMap).join(', ')}`);
  process.exit(2);
}

const rowIndex = rowIndexMap[row];

(async () => {
  const browser = await chromium.launch({ headless: true });
  const pageA = await browser.newPage();
  const pageB = await browser.newPage();

  const htmlA = bootswatchHtml
    .replace(/<link[^>]*bootstrap\.css[^>]*>/, '')
    .replace('</head>', `<style>${bootswatchCss}</style></head>`);
  await pageA.setContent(htmlA, { waitUntil: 'domcontentloaded' });

  await pageB.goto('http://localhost:8000/generated/example-browser-solar-showcase.html', { waitUntil: 'domcontentloaded' });
  await pageB.waitForSelector('.showcase-main .showcase-card', { timeout: 30000 });

  const collect = async (page, fnSource) => {
    return page.evaluate(({ props, fnSource }) => {
      const getTarget = new Function(`return (${fnSource});`)();
      const targets = getTarget();
      if (!targets.target) {
        throw new Error(`missing target: ${targets.diag || 'no diag'}`);
      }
      const cs = getComputedStyle(targets.target);
      const style = {};
      for (const p of props) style[p] = cs.getPropertyValue(p);
      return {
        style,
        text: (targets.target.textContent || '').trim(),
        rowTexts: targets.rowTexts || []
      };
    }, { props, fnSource });
  };

  const a = await collect(pageA, `() => {
    const heading = document.querySelector('#buttons');
    const section = heading && heading.closest('.bs-docs-section');
    if (!section) {
      return { target: null, diag: 'buttons section missing' };
    }
    const rows = Array.from(section.querySelectorAll('.col-lg-7 > p.bs-component, .col-lg-7 > div.bs-component'));
    const rowNode = ${JSON.stringify(row)} === 'sizes'
      ? rows.find((r) => Array.from(r.querySelectorAll('button')).some((b) => (b.textContent || '').trim() === ${JSON.stringify(label)}))
      : rows[${rowIndex}];
    if (!rowNode) {
      return { target: null, diag: 'bootstrap row missing' };
    }
    const buttons = Array.from(rowNode.querySelectorAll('button'));
    const match = buttons.find((b) => (b.textContent || '').trim() === ${JSON.stringify(label)});
    return {
      target: match || null,
      rowTexts: buttons.map((b) => (b.textContent || '').trim()),
      diag: 'bootstrap label not found'
    };
  }`);

  const b = await collect(pageB, `() => {
    const main = document.querySelector('.showcase-main');
    if (!main) {
      return { target: null, diag: 'web-easy showcase main missing' };
    }
    const rows = Array.from(main.querySelectorAll('.showcase-button-row'));
    const rowNode = ${JSON.stringify(row)} === 'sizes'
      ? rows.find((r) => Array.from(r.querySelectorAll('button.we-button')).some((b) => (b.textContent || '').trim() === ${JSON.stringify(label)}))
      : rows[${rowIndex}];
    if (!rowNode) {
      return { target: null, diag: 'web-easy row missing' };
    }
    const buttons = Array.from(rowNode.querySelectorAll('button.we-button'));
    const match = buttons.find((b) => (b.textContent || '').trim() === ${JSON.stringify(label)});
    return {
      target: match || null,
      rowTexts: buttons.map((b) => (b.textContent || '').trim()),
      diag: 'web-easy label not found'
    };
  }`);

  const report = diffMap(a.style, b.style);
  console.log(JSON.stringify({
    row,
    rowIndex,
    label,
    report,
    content: {
      bootswatchRowTexts: a.rowTexts,
      webEasyRowTexts: b.rowTexts
    }
  }, null, 2));

  await browser.close();
})();
