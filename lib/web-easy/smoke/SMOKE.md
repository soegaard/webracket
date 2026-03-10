# web-easy Smoke Operations

Operational commands for `lib/web-easy/smoke`.

## Canonical Entrypoints

Use `make` from repo root:

```bash
make smoke-commands
make smoke-headless-lite
make smoke-style
make smoke-ci
```

Use `headless.sh` from `lib/web-easy/smoke`:

```bash
./headless.sh list
./headless.sh verify
./headless.sh doctor
./headless.sh contract
./headless.sh style
./headless.sh smoke
./headless.sh parity
./headless.sh dashboards
./headless.sh ci-fast
./headless.sh ci
./headless.sh timings
./headless.sh guard
./headless.sh theme
./headless.sh theme-visual
./headless.sh single <compile-script> <test-page>
```

## Recommended Local Flow

Fast pre-push (no compile):

```bash
make smoke-headless-lite
```

Full pre-release (compile + headless):

```bash
make smoke-ci
```

Fast local headless gate (contract + theme + guard, skips full dashboard run):

```bash
./check-all.sh --headless --fast-theme
```

## Task To Command

| Task | Command |
| --- | --- |
| Refresh command inventory | `make smoke-commands` |
| Fast pre-push gate (skip compile) | `make smoke-headless-lite` |
| Full local CI gate | `make smoke-ci` |
| Headless preflight only | `make smoke-verify` |
| One-page headless test | `make smoke-one SINGLE_COMPILE=... SINGLE_PAGE=...` |
| Style-hook contracts only | `./headless.sh style` |
| Deep keyboard contracts only | `./headless.sh deep` |
| Style-hook contracts only (direct script) | `./check-style-headless.sh` |
| Theme token smoke page (single) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-theme-token-contract.html` |
| Theme token API smoke page (single) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-theme-token-api.html` |
| Theme token API contract page (single) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-theme-token-api-contract.html` |
| Layout primitives smoke page (single) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-layout-primitives.html` |
| Theme-only dashboard headless | `./headless.sh theme` |
| Theme visual-diff lane (separate from contracts) | `./headless.sh theme-visual` |
| Demo-vs-generated showcase diff | `./check-theme-showcase-diff.sh` |
| Refresh contract timing baseline | `./refresh-contract-timing-baseline.sh` |
| Solar Forms-only screenshot+computed diff | `./check-solar-forms-diff.sh` |
| Solar post-cards diff gate (`containers/cards/accordions/dialogs`) | `./check-solar-post-cards.sh` |
| Solar list-group contract (computed style) | `node ./check-solar-list-group-contract.mjs` |
| Solar list-group zoom/DPR contract | `node ./check-solar-listgroup-zoom-contract.mjs` |
| Solar overlays contract (accordions/dialogs) | `node ./check-solar-overlays-contract.mjs` |
| Solar accordion parity (computed + screenshot) | `./check-solar-accordion-parity.sh` |
| Solar navbar parity sweep (all variants) | `./check-solar-navbar-parity.sh` |
| Solar table computed contract | `node ./check-solar-table-computed.mjs` |
| Solar progress computed contract | `node ./check-solar-progress-computed.mjs` |
| Solar forms plaintext scope contract | `./check-solar-forms-plaintext-contract.sh` |
| Solar full parity sweep | `./check-solar-parity-sweep.sh` |
| Solar full workflow (compile + contracts + accordion parity + post-cards diff) | `./check-solar-workflow.sh` |
| Navbar Bootswatch-vs-generated style diff | `node ./compare-navbars.cjs primary` |
| Buttons Bootswatch-vs-generated style diff | `node ./compare-buttons.cjs solid Primary` |
| Buttons parity sweep (`solid/disabled/outline/sizes`) | `for r in solid disabled outline sizes; do node ./compare-buttons.cjs $r; done` |
| Navbar parity sweep (`primary/dark/light/subtle`) | `for v in primary dark light subtle; do node ./compare-navbars.cjs $v; done` |
| Compile smoke artifacts only | `./smoke.sh check` |
| Run headless timing snapshot | `./headless.sh timings` |
| Serve local smoke pages | `./smoke.sh open` |
| Guard self-test only | `./headless.sh guard` |

## Post-Cards Quick Commands

- `./check-solar-post-cards.sh`
- `./check-solar-accordion-parity.sh`
- `./check-solar-navbar-parity.sh`
- `./check-solar-forms-plaintext-contract.sh`
- `./check-solar-parity-sweep.sh`
- `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-theme-showcase-compile.sh test-browser-solar2-postcards-contract.html`
- `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-theme-showcase-compile.sh test-browser-solar2-dialogs-contract.html`

Accordion screenshot diff now includes strict preflight guards:
- expected theme class present (`we-theme-solar2` by default)
- expected stylesheet hrefs present (`theme-solar-2.css`, `theme-showcase-solar2.css` by default)
- required selectors visible on generated and reference pages
- metadata artifact written to:
  - `generated/solar-accordion-diff/accordion-metadata.json`
  - optional run-stamped artifact directory: set `SOLAR_RUNSTAMP=YYYYMMDD-HHMMSS`

## Stale CSS / Screenshot Debug Playbook

When screenshot/computed output does not match what you see manually:

1. Run in sequence (never parallel):
   - `./run-browser-solar-showcase-compile.sh`
   - then the diff/contract script.
2. Confirm generated page is on expected theme:
   - `document.documentElement.className` should include `we-theme-solar2`.
3. Confirm stylesheet links:
   - `#we-theme-core-css` should include `web-easy-core.css`
   - `#we-theme-external-css` should include `theme-solar-2.css`
   - `#we-theme-showcase-css` should include `theme-showcase-solar2.css`
4. Use preflight metadata:
   - `generated/solar-accordion-diff/accordion-metadata.json`
   - verify `themeClass`, `stylesheets`, `selectorVisible`.
5. Hard refresh page (`Cmd+Shift+R`) if browser still shows old CSS.
| Deep keyboard contract (scrollspy, core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-scrollspy-keyboard-deep.html` |
| Deep keyboard contract (scrollspy, parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-scrollspy-keyboard-deep.html` |
| Deep keyboard contract (dropdown, core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-dropdown-keyboard-deep.html` |
| Deep keyboard contract (dropdown, parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-dropdown-keyboard-deep.html` |
| Deep keyboard contract (choice, core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-choice-keyboard-deep.html` |
| Deep keyboard contract (choice, parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-choice-keyboard-deep.html` |
| Dynamic tab ARIA contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-tab-aria-dynamic-contract.html` |
| Dynamic tab ARIA contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-tab-aria-dynamic-contract.html` |
| Table-align contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-table-align-contract.html` |
| Table-align contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-table-align-contract.html` |
| Dialog no-desc contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-dialog-no-desc-contract.html` |
| Dialog no-desc contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-dialog-no-desc-contract.html` |
| Menu typeahead-timeout contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-menu-typeahead-timeout-contract.html` |
| Menu typeahead-timeout contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-menu-typeahead-timeout-contract.html` |
| Dropdown focus-return contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-dropdown-focus-return-contract.html` |
| Dropdown focus-return contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-dropdown-focus-return-contract.html` |
| Choice decode contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-choice-decode-contract.html` |
| Choice decode contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-choice-decode-contract.html` |
| Tooltip contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-tooltip-contract.html` |
| Tooltip contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-tooltip-contract.html` |
| Modal contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-modal-contract.html` |
| Modal close-reason contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-modal-close-reason-contract.html` |
| Modal contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-modal-contract.html` |
| Modal close-reason contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-modal-close-reason-contract.html` |
| Toast behavior contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-toast-behavior-contract.html` |
| Toast behavior contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-toast-behavior-contract.html` |
| Toast timer contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-toast-timer-contract.html` |
| Toast hover-pause contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-toast-hover-pause-contract.html` |
| Toast timer contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-toast-timer-contract.html` |
| Toast hover-pause contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-toast-hover-pause-contract.html` |
| Carousel wrap contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-carousel-wrap-contract.html` |
| Carousel keyboard contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-carousel-keyboard-contract.html` |
| Carousel autoplay contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-carousel-autoplay-contract.html` |
| Carousel autoplay no-wrap contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-carousel-autoplay-nowrap-contract.html` |
| Carousel wrap contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-carousel-wrap-contract.html` |
| Carousel keyboard contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-carousel-keyboard-contract.html` |
| Carousel autoplay contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-carousel-autoplay-contract.html` |
| Carousel autoplay no-wrap contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-carousel-autoplay-nowrap-contract.html` |
| Navigation-bar orientation contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-navigation-bar-orientation-contract.html` |
| Navigation-bar collapsed contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-navigation-bar-collapsed-contract.html` |
| Navigation-bar toggle a11y contract (core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-navigation-bar-toggle-a11y-contract.html` |
| Navigation-bar orientation contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-navigation-bar-orientation-contract.html` |
| Navigation-bar collapsed contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-navigation-bar-collapsed-contract.html` |
| Navigation-bar toggle a11y contract (parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-navigation-bar-toggle-a11y-contract.html` |
| Theme external-css contract (core-before-theme order) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-theme-external-css-contract.html` |
| Theme showcase contract | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-theme-showcase-compile.sh test-browser-theme-showcase-contract.html` |

## Make Targets

- `make smoke-ci`
- `make smoke-headless-lite`
- `make smoke-style`
- `make smoke-verify`
- `make smoke-quick`
- `make smoke-release`
- `make smoke-list`
- `make smoke-commands`
- `make smoke-one SINGLE_COMPILE=... SINGLE_PAGE=...`
- `make smoke-compare-navbars`
- `make smoke-compare-buttons`
- `make smoke-compare-all`

## Local Utility Wrapper (`smoke.sh`)

`smoke.sh` is for local/manual workflows only (serve + compile utilities), not headless orchestration.

```bash
./smoke.sh help
```

Available utility commands:

- `./smoke.sh check`
- `./smoke.sh parity`
- `./smoke.sh parity-check`
- `./smoke.sh rebuild`
- `./smoke.sh status`
- `./smoke.sh urls`
- `./smoke.sh dashboards`
- `./smoke.sh parity-open`
- `./smoke.sh open`
- `./smoke.sh doctor`
- `./smoke.sh clean`
- `./smoke.sh clean-dry`

## Compile Architecture

- `check-smoke.sh` runs a preflight wrapper-arity scan before compile:
  - `racket ../../../tools/check-wrapper-arity.rkt lib/web-easy/smoke`
  - set `SMOKE_SKIP_WRAPPER_ARITY_CHECK=1` to skip temporarily.
- Generated browser artifacts are written to `smoke/generated/`.
- Core smoke pages compile via `example-browser-smoke-all.rkt` selected by `?test=...`.
- Parity pages compile via `example-browser-parity-all.rkt` selected by `?test=...`.
- Repeated iframe harness helpers are centralized in `smoke-harness.js` for new/updated pages.
- Contract dashboards are split to avoid monolithic headless timeout risk:
  - `test-browser-contract-dashboard-core.html`
  - `test-browser-contract-dashboard-parity.html`
  - wrapper: `test-browser-contract-dashboard.html`

## Single-Page Debug Loop

Fastest iteration path for one failing page:

1. Compile once:
   - `./check-smoke.sh`
2. Re-run one page headless with compile skipped:
   - `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-<page>.html`
   - parity variant: `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-<page>.html`
3. Repeat step 2 while editing.

For lean CI parity with local behavior:

- `SMOKE_SKIP_COMPILE=1 ./headless.sh ci-fast`

Full no-compile baseline (local):

1. `SMOKE_SKIP_COMPILE=1 ./headless.sh contract`
2. `SMOKE_SKIP_COMPILE=1 ./headless.sh smoke`
3. `SMOKE_SKIP_COMPILE=1 ./headless.sh parity`
4. `SMOKE_SKIP_COMPILE=1 ./headless.sh theme`

Compare policy:

- Contracts are the release gate.
- Compare scripts (`compare-buttons.cjs`, `compare-navbars.cjs`) are advisory drift detectors.
- Target is strict zero-delta (`report.length = 0`) for tracked rows/variants unless a documented exception exists in this file.

Failure triage order:

1. `contract` dashboard failures first.
2. `smoke` dashboard failures next.
3. `parity` dashboard failures next.
4. `theme` dashboard failures next.
5. compare-script deltas last (visual tuning pass).

## Dashboard Flake Policy

- Contract dashboards are behavior gates. Visual-diff checks run in the separate `theme-visual` lane.
- The Playwright guard ignores only bare `pageerror: unreachable` teardown events seen during iframe unload.
- Console `error` events and explicit page `FAIL` statuses still fail the run.
- If a page is flaky in dashboard mode but stable in `check-single-headless.sh`, treat it as harness/teardown first; isolate before changing component behavior.

## Layout Recipes Demo

- Manual recipes page:
  - `demo-layout-recipes.html`
- Recipes currently include:
  - primitives baseline (`layout-primitives`)
  - width policy reference (`width`)
  - realistic workspace composition (`parity-workspace`)
- Dedicated progress pages:
  - `test-browser-progress.html` (`?test=progress`)
  - `test-browser-parity-progress.html` (`?test=parity-progress`)
- Dedicated layout smoke page:
  - `test-browser-layout-primitives.html` (`?test=layout-primitives`)
- Dedicated component API smoke pages:
  - `test-browser-link.html` (`?test=link`)
  - `test-browser-toolbar.html` (`?test=toolbar`)
  - `test-browser-divider.html` (`?test=divider`)
  - `test-browser-choice-labeled.html` (`?test=choice-labeled`)
  - `test-browser-dropdown-labeled.html` (`?test=dropdown-labeled`)
  - `test-browser-button-icons.html` (`?test=button-icons`)
  - `test-browser-menu-icons.html` (`?test=menu-icons`)
  - `test-browser-card-variants.html` (`?test=card-variants`)
  - `test-browser-theme-token-api.html` (`?test=theme-token-api`)
- Dedicated component API contract pages:
  - `test-browser-link-contract.html`
  - `test-browser-toolbar-contract.html`
  - `test-browser-divider-contract.html`
  - `test-browser-choice-labeled-contract.html`
  - `test-browser-dropdown-labeled-contract.html`
  - `test-browser-button-icons-contract.html`
  - `test-browser-menu-icons-contract.html`
  - `test-browser-card-variants-contract.html`
  - `test-browser-theme-token-api-contract.html`
- `check-smoke.sh` compiles:
  - `smoke-all`
  - `visual-check`
  - `parity-all`
- Theme token coverage is validated by:
  - `test-browser-theme-vars.html` (tab selected styles)
  - `test-browser-theme-dialog-vars.html` (dialog overlay/panel)
  - `test-browser-theme-menu-vars.html` (menu-bar surface)
  - `test-browser-theme-token-contract.html` (required token presence + override contract)
  - parity mirrors for tab/dialog/menu pages

## Theming Verification

Smoke pages:

- `test-browser-theme-vars.html`
- `test-browser-theme-dialog-vars.html`
- `test-browser-theme-menu-vars.html`
- `test-browser-theme-progress-vars.html`
- `test-browser-theme-token-contract.html`
- `test-browser-theme-token-api-contract.html`
- `test-browser-theme-external-css-contract.html`
- `test-browser-solar2-navbar-variants-contract.html`
- `test-browser-parity-solar2-navbar-variants-contract.html`
- `test-browser-solar2-button-disabled-contract.html`
- `test-browser-solar2-button-row-display-contract.html`
- `test-browser-solar2-indicators-contract.html`
- `test-browser-parity-theme-vars.html`
- `test-browser-parity-theme-dialog-vars.html`
- `test-browser-parity-theme-menu-vars.html`
- `test-browser-parity-theme-progress-vars.html`

All are part of the standard dashboard/headless runs (`check-all.sh --headless`, `headless.sh smoke`, `headless.sh parity`).
Theme-only fast gate:

## Solar Forms Visual Checklist

- Left column should visually read like `Legend` fieldset with consistent `mt-4` spacing rhythm.
- Static email row should align label/value in one row (`Email` + `email@example.com`).
- Disabled select/input should be clearly muted versus enabled controls.
- Multiple select should be visibly taller than single select.
- Textarea should match Bootswatch-like interior spacing and line-height.
- File input should align in height with nearby controls.
- Valid and invalid inputs should show both border color and feedback text.
- Range controls should have Solar-like track/thumb contrast and disabled muted state.
- Input addon rows (`$ ... .00` and `... + Button`) should have joined borders and equal heights.
- Floating label demo should present distinct label text and field spacing.

## Solar 2 Polish Metrics (2026-03-09)

Commands run:

- `SMOKE_BASE_URL=http://localhost:8000 ./check-solar-sections-diff.sh`
- `node /tmp/solar-computed-compare-2.mjs`
- `node /tmp/solar-cards-compare.mjs`

Artifacts:

- before snapshots:
  - `generated/solar-polish-metrics/before-progress-containers.json`
  - `generated/solar-polish-metrics/before-cards.json`
- after snapshots:
  - `generated/solar-polish-metrics/after-progress-containers.json`
  - `generated/solar-polish-metrics/after-cards.json`
- section-diff report:
  - `generated/solar-section-diff/report.json`

Selected before/after computed metrics:

| Metric | Before | After |
| --- | --- | --- |
| Progress h3 color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Progress bar text color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Containers h2 color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Containers list color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Cards h2 color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Cards sample card color | rgb(147, 161, 161) | rgb(131, 148, 150) |
| Cards sample body background | rgba(0, 0, 0, 0) | rgba(0, 0, 0, 0) |
| Cards sample subtitle color | rgba(131, 148, 150, 0.75) | rgba(131, 148, 150, 0.75) |

- `./headless.sh theme` runs `test-browser-theme-contract-dashboard.html`
- Manual external CSS theming example:
- `test-browser-theme-external-css.html`
  - `test-browser-listgroup-clipping-debug.html` (manual clipping debugger with zoom control)
  - editable stylesheets: `theme-external-light.css`, `theme-external-dark.css`, `theme-external-solar.css`
  - gallery covers workspace/menu/tabs/dialog/controls/width/profile/list pages

Close-button icon theming:

- The close icon is rendered through CSS (`.we-close-button-icon::before`), so custom themes can swap the glyph without changing Racket code.
- Example override for external themes:

```css
.we-close-button-icon::before {
  content: "✕"; /* replace with another symbol if desired */
}
```

Beginner token walkthrough:

- Change keyboard focus ring color:
  - edit `--we-focus`
- Change keyboard focus fill tint:
  - edit `--we-focus-tint`
- Change main text color:
  - edit `--we-fg`
- Change page/panel base background:
  - edit `--we-bg`
- Change subtle panel/menu-bar background:
  - edit `--we-bg-subtle`
- Change selected tab/menu background:
  - edit `--we-bg-selected`
- Change hover background for menu items/buttons:
  - edit `--we-bg-hover`
- Change primary border color:
  - edit `--we-border`
- Change menu container border color:
  - edit `--we-border-menu`
- Change muted helper text:
  - edit `--we-fg-muted`
- Change overlay backdrop darkness:
  - edit `--we-overlay`

Example tweak sequence:

1. Set `--we-bg` and `--we-fg` first for overall light/dark/solar feel.
2. Tune `--we-bg-hover` and `--we-bg-selected` for interactive clarity.
3. Tune `--we-focus` and `--we-focus-tint` for keyboard accessibility.
4. Adjust `--we-border` and `--we-border-menu` for component separation.

## CSS Hook Inspection

For browser-side theming/debugging, inspect runtime nodes using:

- `data-we-widget` (stable semantic widget id)
- class names (default visual contract)

Quick check flow:

1. Run `./smoke.sh open`.
2. Open `http://localhost:8000/test-browser-visual-check.html`.
3. In DevTools, verify representative nodes expose expected hooks, for example:
   - `data-we-widget="menu-popup"` + class `we-menu-popup`
   - `data-we-widget="tab-button"` + class `we-tab-btn`
   - `data-we-widget="table-data-cell"` + class `we-table-data-cell`

## Guard + Forbidden Tokens

Guard self-test:

- `./check-dashboard-guard.sh`

Forbidden tokens checked in dashboard DOM attributes:

- `#<value>`
- `#<void>`
- `#<procedure>`
- `#<eof>`
- `[object Object]`
- `undefined`
- `NaN`
- `Infinity`

Additional dashboard guard:

- fails if any rendered widget node (`[data-we-widget]`) has a non-empty inline `style` attribute.
- current allowlist for this check: empty (no widget exceptions).

## Timing Output

Headless timing behavior:

- runners print `TIMING\tpage\tduration_ms` rows
- warning threshold from `SMOKE_WARN_MS` (default `2000`)
- style-only warning override: `SMOKE_WARN_MS_STYLE` (falls back to `SMOKE_WARN_MS`)
- per-suite timing TSV files in `/tmp`:
  - `/tmp/web-easy-contract-timings.tsv`
  - `/tmp/web-easy-smoke-timings.tsv`
  - `/tmp/web-easy-parity-timings.tsv`
  - `/tmp/web-easy-theme-timings.tsv`
  - `/tmp/web-easy-style-timings.tsv`
- combined timing snapshot:
  - `./headless.sh timings`
  - writes `/tmp/web-easy-all-timings.tsv`
  - includes suites: contract, smoke, parity, theme, style

## CI

- local helper: `./check-ci-smoke.sh`
- GitHub workflow: `.github/workflows/web-easy-smoke.yml`
  - runs `./headless.sh verify`
  - runs compile path + `./headless.sh ci` (`SMOKE_SKIP_COMPILE=1`)
  - `ci` mode runs contract dashboard + theme dashboard + full smoke dashboard + guard
  - publishes headless timing artifacts

## Contract Reference

Contract behavior and expected PASS lines are documented in:

- `SMOKE-CONTRACTS.md`

## Deep Keyboard Contracts

- Gating pages:
  - `test-browser-scrollspy-keyboard-deep.html`
  - `test-browser-parity-scrollspy-keyboard-deep.html`
  - `test-browser-dropdown-keyboard-deep.html`
  - `test-browser-parity-dropdown-keyboard-deep.html`
- Purpose: enforce multi-step keyboard behavior for scrollspy and dropdown in the same contract gate as other keyboard semantics.

## Style vs Contract

- Use `./headless.sh style` (or `./check-style-headless.sh`) when iterating only on CSS hooks/classes and `data-we-widget` stability.
- Use `./headless.sh contract` for the full contract suite (a11y, keyboard, focus, disabled, dialog, menu, tabs, style, progress).

## Notes

- Do not run multiple headless commands in parallel; they share generated artifacts.
- After command inventory changes, run `make smoke-commands` and commit `COMMANDS.tsv`.
