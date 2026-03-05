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
./headless.sh ci
./headless.sh timings
./headless.sh guard
./headless.sh theme
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
| Theme-only dashboard headless | `./headless.sh theme` |
| Compile smoke artifacts only | `./smoke.sh check` |
| Run headless timing snapshot | `./headless.sh timings` |
| Serve local smoke pages | `./smoke.sh open` |
| Guard self-test only | `./headless.sh guard` |
| Deep keyboard contract (scrollspy, core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-scrollspy-keyboard-deep.html` |
| Deep keyboard contract (scrollspy, parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-scrollspy-keyboard-deep.html` |
| Deep keyboard contract (dropdown, core) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-smoke-all-compile.sh test-browser-dropdown-keyboard-deep.html` |
| Deep keyboard contract (dropdown, parity) | `SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-dropdown-keyboard-deep.html` |

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

- Generated browser artifacts are written to `smoke/generated/`.
- Core smoke pages compile via `example-browser-smoke-all.rkt` selected by `?test=...`.
- Parity pages compile via `example-browser-parity-all.rkt` selected by `?test=...`.
- Dedicated progress pages:
  - `test-browser-progress.html` (`?test=progress`)
  - `test-browser-parity-progress.html` (`?test=parity-progress`)
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
- `test-browser-theme-external-css-contract.html`
- `test-browser-parity-theme-vars.html`
- `test-browser-parity-theme-dialog-vars.html`
- `test-browser-parity-theme-menu-vars.html`
- `test-browser-parity-theme-progress-vars.html`

All are part of the standard dashboard/headless runs (`check-all.sh --headless`, `headless.sh smoke`, `headless.sh parity`).
Theme-only fast gate:

- `./headless.sh theme` runs `test-browser-theme-contract-dashboard.html`
- Manual external CSS theming example:
  - `test-browser-theme-external-css.html`
  - editable stylesheets: `theme-external-light.css`, `theme-external-dark.css`
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

1. Set `--we-bg` and `--we-fg` first for overall light/dark feel.
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
