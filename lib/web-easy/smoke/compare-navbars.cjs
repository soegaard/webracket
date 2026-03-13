const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(process.env.PLAYWRIGHT_LIB || path.resolve(__dirname, '../../../.local-tools/node_modules/playwright'));

const bootswatchHtml = fs.readFileSync(path.resolve(__dirname, '../../../tmp/bootswatch.html'), 'utf8');
const bootswatchCss = fs.readFileSync(path.resolve(__dirname, '../../../tmp/solar-bootstrap.css'), 'utf8');
const variant = (process.argv[2] || 'primary').toLowerCase();
const variantOrder = ['primary', 'dark', 'light', 'subtle'];
const bsIndex = Number.isFinite(Number(process.argv[3]))
  ? Number(process.argv[3])
  : Math.max(0, variantOrder.indexOf(variant));

const props = [
  'display','position','height','min-height','padding-top','padding-right','padding-bottom','padding-left',
  'margin-right','margin-left','gap','align-items','justify-content',
  'background-color','color','border-top-width','border-top-style','border-top-color','border-radius',
  'font-family','font-size','font-weight','line-height','box-shadow'
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

(async () => {
  const browser = await chromium.launch({ headless: true });
  const pageA = await browser.newPage();
  const pageB = await browser.newPage();

  const htmlA = bootswatchHtml
    .replace(/<link[^>]*bootstrap\.css[^>]*>/, '')
    .replace('</head>', `<style>${bootswatchCss}</style></head>`);
  await pageA.setContent(htmlA, { waitUntil: 'domcontentloaded' });

  await pageB.goto('http://localhost:8000/generated/example-browser-solar-showcase.html', { waitUntil: 'networkidle' });
  await pageB.waitForSelector('#solar2-navbars', { timeout: 30000 });
  await pageB.waitForSelector('.we-navigation-bar', { timeout: 30000 });
  await pageB.waitForFunction(() => {
    const html = document.documentElement;
    const nav = document.querySelector('#solar2-navbars')
      ? document.querySelector('#solar2-navbars').parentElement.querySelector('.we-navigation-bar')
      : document.querySelector('.we-navigation-bar');
    if (!html || !nav) return false;
    const hasTheme = html.classList.contains('we-theme-solar2');
    const fontFamily = getComputedStyle(document.body).fontFamily || '';
    const navHeight = parseFloat(getComputedStyle(nav).height || '0');
    return hasTheme && fontFamily.includes('Source Sans Pro') && navHeight > 20;
  }, { timeout: 30000 });
  await pageB.evaluate(async () => {
    if (document.fonts && document.fonts.ready) {
      await document.fonts.ready;
    }
  });

  const collectComputed = async (page, fnSource) => {
    return page.evaluate(({ props, fnSource }) => {
      const getTarget = new Function(`return (${fnSource});`)();
      const targets = getTarget();
      const result = {};
      for (const [name, el] of Object.entries(targets)) {
        if (!el) throw new Error(`missing element for ${name}`);
        const cs = getComputedStyle(el);
        const out = {};
        for (const p of props) out[p] = cs.getPropertyValue(p);
        result[name] = out;
      }
      return result;
    }, { props, fnSource });
  };

  const a = await collectComputed(pageA, `() => {
    const h = document.querySelector('#navbars');
    const section = h && h.closest('.bs-docs-section');
    const navs = section ? Array.from(section.querySelectorAll('nav.navbar')) : [];
    const nav = navs[${bsIndex}] || navs[0] || null;
    const dropdown = nav && nav.querySelector('.nav-item.dropdown');
    const menu = dropdown && dropdown.querySelector('.dropdown-menu');
    return {
      navbar: nav,
      brand: nav && nav.querySelector('.navbar-brand'),
      navLink: nav && (nav.querySelectorAll('.nav-link')[1] || nav.querySelector('.nav-link')),
      searchInput: nav && nav.querySelector('input.form-control'),
      searchBtn: nav && nav.querySelector('button.btn'),
      dropdownLabel: dropdown && dropdown.querySelector('.dropdown-toggle'),
      dropdownMenu: menu,
      dropdownItem: menu && menu.querySelector('.dropdown-item'),
      dropdownDivider: menu && menu.querySelector('.dropdown-divider')
    };
  }`);

  const b = await collectComputed(pageB, `() => {
    const nav = document.querySelector('.we-variant-${variant} .we-navigation-bar')
             || document.querySelector('.we-navigation-bar.we-variant-${variant}')
             || document.querySelector('#solar2-navbars')?.parentElement?.querySelector('.we-navigation-bar')
             || document.querySelector('.we-navigation-bar');
    const itemsRoot = nav ? nav.querySelector('[data-we-widget="navigation-bar-items"]') : null;
    const navButtons = itemsRoot ? itemsRoot.querySelectorAll(':scope > .we-button') : [];
    const searchPanel = itemsRoot ? itemsRoot.querySelector('.we-hpanel') : null;
    const dropdown = nav ? nav.querySelector('[data-we-widget="menu"]') : null;
    const menu = dropdown ? dropdown.querySelector('[data-we-widget="menu-popup"]') : null;
    return {
      navbar: nav,
      brand: nav && nav.querySelector('.we-navbar-brand'),
      navLink: navButtons.length > 1 ? navButtons[1] : (navButtons.length ? navButtons[0] : null),
      searchInput: searchPanel && searchPanel.querySelector('input.we-input'),
      searchBtn: searchPanel && searchPanel.querySelector('.we-button'),
      dropdownLabel: dropdown && dropdown.querySelector('[data-we-widget="menu-label"]'),
      dropdownMenu: menu,
      dropdownItem: menu && menu.querySelector('[data-we-widget="menu-item"]'),
      dropdownDivider: menu && menu.querySelector('[data-we-widget="divider"]')
    };
  }`);

  const report = {};
  for (const k of Object.keys(a)) report[k] = diffMap(a[k], b[k]);
  const aText = await pageA.evaluate((idx) => {
    const h = document.querySelector('#navbars');
    const section = h && h.closest('.bs-docs-section');
    const navs = section ? Array.from(section.querySelectorAll('nav.navbar')) : [];
    const nav = navs[idx] || navs[0] || null;
    const dropdown = nav && nav.querySelector('.nav-item.dropdown');
    const items = dropdown ? Array.from(dropdown.querySelectorAll('.dropdown-item')).map((n) => (n.textContent || '').trim()) : [];
    return items;
  }, bsIndex);
  const bText = await pageB.evaluate((variantName) => {
    const nav = document.querySelector('.we-variant-' + variantName + ' .we-navigation-bar')
             || document.querySelector('.we-navigation-bar.we-variant-' + variantName)
             || document.querySelector('#solar2-navbars')?.parentElement?.querySelector('.we-navigation-bar')
             || document.querySelector('.we-navigation-bar');
    const dropdown = nav ? nav.querySelector('[data-we-widget="menu"]') : null;
    const items = dropdown ? Array.from(dropdown.querySelectorAll('[data-we-widget="menu-item"]')).map((n) => (n.textContent || '').trim()) : [];
    return items;
  }, variant);
  console.log(JSON.stringify({ variant, bsIndex, report, content: { bootswatchDropdownItems: aText, webEasyDropdownItems: bText } }, null, 2));

  await browser.close();
})();
