const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(process.env.PLAYWRIGHT_LIB || path.resolve(__dirname, '../../../.local-tools/node_modules/playwright'));

const bootswatchHtml = fs.readFileSync(path.resolve(__dirname, '../../../tmp/bootswatch.html'), 'utf8');
const bootswatchCss = fs.readFileSync(path.resolve(__dirname, '../../../tmp/solar-bootstrap.css'), 'utf8');

const props = [
  'background-color', 'color', 'border-top-color', 'border-top-width', 'border-radius',
  'font-family', 'font-size', 'font-weight', 'line-height', 'box-shadow'
];

const norm = (v) => (v ?? '').toString().replace(/\s+/g, ' ').trim();
const diffMap = (a, b) => {
  if (a && a.__missing) return [{ prop: '__missing', bootswatch: 'missing', webEasy: 'present' }];
  if (b && b.__missing) return [{ prop: '__missing', bootswatch: 'present', webEasy: 'missing' }];
  const out = [];
  for (const p of props) {
    const av = norm(a[p]);
    const bv = norm(b[p]);
    if (av !== bv) out.push({ prop: p, bootswatch: av, webEasy: bv });
  }
  return out;
};

(async () => {
  const browser = await chromium.launch({ headless: true });
  const pageA = await browser.newPage();
  const pageB = await browser.newPage();

  const htmlA = bootswatchHtml
    .replace(/<link[^>]*bootstrap\.css[^>]*>/, '')
    .replace('</head>', `<style>${bootswatchCss}</style></head>`);
  await pageA.setContent(htmlA, { waitUntil: 'domcontentloaded' });

  await pageB.goto('http://localhost:8000/generated/example-browser-solar-showcase.html', { waitUntil: 'networkidle' });
  await pageB.waitForSelector('#solar2-dialogs', { timeout: 30000 });

  const collectComputed = async (page, fnSource) => {
    return page.evaluate(({ props, fnSource }) => {
      const getTarget = new Function(`return (${fnSource});`)();
      const targets = getTarget();
      const result = {};
      for (const [name, el] of Object.entries(targets)) {
        if (!el) {
          result[name] = { __missing: true };
          continue;
        }
        const cs = getComputedStyle(el);
        const out = {};
        for (const p of props) out[p] = cs.getPropertyValue(p);
        result[name] = out;
      }
      return result;
    }, { props, fnSource });
  };

  const a = await collectComputed(pageA, `() => {
    const section = document.querySelector('.bs-docs-section:has(#dialogs)');
    const root = section && section.querySelector('.modal .modal-content');
    const footer = root && root.querySelector('.modal-footer');
    return {
      modal: root,
      header: root && root.querySelector('.modal-header'),
      body: root && root.querySelector('.modal-body'),
      footer,
      btnPrimary: footer && footer.querySelector('.btn-primary'),
      btnSecondary: footer && footer.querySelector('.btn-secondary')
    };
  }`);

  const b = await collectComputed(pageB, `() => {
    const root = document.querySelector('#solar2-dialogs-body .showcase-static-modal');
    const footer = root && root.querySelector('.we-button-row');
    const allButtons = root ? Array.from(root.querySelectorAll('button.we-button, button')) : [];
    const byText = (needle) => allButtons.find((b) => ((b.textContent || '').trim() === needle)) || null;
    return {
      modal: root,
      header: root && root.querySelector('.showcase-static-modal-header'),
      body: root && root.querySelector('.showcase-static-modal-body'),
      footer,
      btnPrimary: byText('Save changes'),
      btnSecondary: byText('Close')
    };
  }`);

  const report = {};
  for (const k of Object.keys(a)) report[k] = diffMap(a[k], b[k]);
  console.log(JSON.stringify({ report }, null, 2));

  await browser.close();
})();
