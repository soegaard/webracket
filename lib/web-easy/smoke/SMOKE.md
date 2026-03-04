# web-easy Smoke Tests

Browser smoke tests for `lib/web-easy`.

## Smoke Test Conventions

All browser smoke pages under `smoke/test-browser-*.html` should follow these rules:

1. Status protocol:
   - expose a top-level `#status` node.
   - set status text to start with `PASS` or `FAIL`.
2. No leaked placeholder/sentinel values in DOM attributes:
   - rendered DOM attributes must not contain any forbidden token.
   - current forbidden tokens:
     - `#<value>`, `#<void>`, `#<procedure>`, `#<eof>`,
       `[object Object]`, `undefined`, `NaN`, `Infinity`.
   - full/parity dashboards enforce this automatically after each page reports `PASS`.
3. Dashboard compatibility:
   - pages intended for automation must be listed in the dashboard test arrays.
   - manual pages (for example visual inspection pages) should be excluded explicitly.

Guard self-test command:

- `./check-dashboard-guard.sh`
- `./smoke.sh guard`
- expected result: script exits `0` when guard correctly reports a `FAIL` for the intentional fixture page.

## Smoke Grouping

Current grouping policy:

1. Core feature smoke:
   - pages named `test-browser-*.html` and `test-browser-parity-*.html`.
   - included in `test-browser-dashboard.html` and headless pass/fail summary.
2. Parity example smoke:
   - pages named `test-browser-parity-*.html`.
   - included in dashboard/headless after phase-1 promotion.

Rationale:

- Keeps the dashboard as a stable regression signal for core runtime/features.
- Keeps parity verification in the same automation path as core smoke checks.

## Compile Architecture

- Generated browser artifacts are written to `smoke/generated/`.
- Core smoke pages compile through `example-browser-smoke-all.rkt` selected by `?test=...`.
- Parity pages compile through `example-browser-parity-all.rkt` selected by `?test=...`.
- Canonical compile runner: `run-browser-target-compile.sh <smoke-all|parity-all|visual-check>`.
- `check-smoke.sh` runs 3 compile targets:
  - `smoke-all`
  - `visual-check`
  - `parity-all`

## Current Test Counts

- full dashboard (`test-browser-dashboard.html`): `40` tests
- parity dashboard (`test-browser-parity-dashboard.html`): `20` tests

Update these counts whenever test pages are added or removed.

Last validated (2026-03-04):

- full headless dashboard: `40/40` PASS
- parity-only headless dashboard: `20/20` PASS
- latest local `check-all.sh --headless`: PASS (`40/40`)

## Menu Behavior Contract

Menu contract pages define the expected menu UX and keyboard semantics:

- `test-browser-menu-full.html`
- `test-browser-parity-menu-full.html`
- `test-browser-keyboard-contract.html`
- `test-browser-parity-keyboard-contract.html`

Expected behavior:

- Opening one top-level menu closes any other open top-level menu.
- `ArrowRight`/`ArrowLeft` on top-level menu labels move between labels and wrap.
- `ArrowRight`/`ArrowLeft` on menu items switch to sibling top-level menu and focus its label.
- `ArrowDown`/`ArrowUp` within a popup move item focus but do not wrap at edges.
- `Home`/`End` on menu labels focus/open first/last top-level menu.
- Type-ahead on menu label or menu items focuses matching menu items.
- `Escape` closes popup and returns focus to owning top-level menu label.
- Focus leaving the menu container closes the currently open menu.
- Hovering another top-level menu label while one menu is open switches open menu.

## Command Wrapper

From `lib/web-easy/smoke`:

```bash
./smoke.sh help
```

Wrapper commands:

- `./smoke.sh check`
- `./smoke.sh parity`
- `./smoke.sh parity-check`
- `./smoke.sh parity-headless`
- `./smoke.sh contract`
- `./smoke.sh all`
- `./smoke.sh headless`
- `./smoke.sh rebuild`
- `./smoke.sh quick`
- `./smoke.sh ci`
- `./smoke.sh status`
- `./smoke.sh urls`
- `./smoke.sh dashboards`
- `./smoke.sh parity-open`
- `./smoke.sh guard`
- `./smoke.sh open`
- `./smoke.sh doctor`
- `./smoke.sh clean`
- `./smoke.sh clean-dry`

CI helper script:

- `./check-ci-smoke.sh` (runs full headless smoke + guard self-test)

Repository-root aliases:

- `make smoke-ci`
- `make smoke-headless`
- `make smoke-parity-headless`
- `make smoke-one SINGLE_COMPILE=run-browser-parity-all-compile.sh SINGLE_PAGE=test-browser-parity-menu-keys.html`

Command notes:

- `quick`: runs `doctor` preflight, then `all`.
- `ci`: CI smoke entrypoint (`doctor` + `check-ci-smoke.sh`).
- `status`: shows tool/artifact status and suggests the next command.

`tab-panel` entry forms:

- `(cons tab-id view)`
- `(list tab-id view disabled?)`
- `disabled?` defaults to `#f` in pair form.

## Compile All

From repository root:

```bash
lib/web-easy/smoke/check-smoke.sh
```

Or from `lib/web-easy/smoke`:

```bash
./check-smoke.sh
```

Fresh compile cycle:

```bash
./smoke.sh rebuild
```

## Clean Generated Artifacts

From `lib/web-easy/smoke`:

```bash
./clean-smoke.sh
```

Optional preview without deleting files:

```bash
./clean-smoke.sh --dry-run
```

This cleans generated browser artifacts under `smoke/generated/` (and legacy root artifacts, if present).

## End-To-End Script

From `lib/web-easy/smoke`:

```bash
./check-all.sh
```

This runs:

1. `racket ../test/test-web-easy.rkt` (full Racket path)
2. `webracket.rkt -r ../test/test-web-easy-run.rkt` (WebRacket compiler + Node path)
3. smoke compile

Use `./check-all.sh --headless` to add dashboard-driven headless smoke execution.

Core input-test note:

- Text input state is validated via node attribute `value` (not `dom-node-text`).

## Run In Browser

From `lib/web-easy/smoke`:

```bash
./smoke.sh urls
```

Then start server:

```bash
./smoke.sh open
```

Equivalent direct command:

```bash
raco static-web --port 8000 --dir .
```

Optional auto-launch:

```bash
SMOKE_LAUNCH=1 ./smoke.sh open
```

Then open:

- `http://localhost:8000/test-browser-dashboard.html` (runs all smoke pages)
- `http://localhost:8000/test-browser-parity-dashboard.html` (runs parity-only smoke pages)
- `http://localhost:8000/test-browser-contract-dashboard.html` (runs contract-only smoke pages)
- `http://localhost:8000/test-browser-smoke.html`
- `http://localhost:8000/test-browser-group.html`
- `http://localhost:8000/test-browser-dialog.html`
- `http://localhost:8000/test-browser-menu-keys.html`
- `http://localhost:8000/test-browser-menu-full.html`
- `http://localhost:8000/test-browser-a11y-contract.html`
- `http://localhost:8000/test-browser-keyboard-contract.html`
- `http://localhost:8000/test-browser-focus-order.html`
- `http://localhost:8000/test-browser-disabled-contract.html`
- `http://localhost:8000/test-browser-width.html`
- `http://localhost:8000/test-browser-input.html`
- `http://localhost:8000/test-browser-checkbox.html`
- `http://localhost:8000/test-browser-list.html`
- `http://localhost:8000/test-browser-destroy.html`
- `http://localhost:8000/test-browser-branch.html`
- `http://localhost:8000/test-browser-controls.html`
- `http://localhost:8000/test-browser-operators.html`
- `http://localhost:8000/test-browser-tab-panel.html`
- `http://localhost:8000/test-browser-tab-panel-keys.html`
- `http://localhost:8000/test-browser-tab-panel-disabled.html`
- `http://localhost:8000/test-browser-visual-check.html` (manual visual sanity page)
- `http://localhost:8000/test-browser-parity-hello.html`
- `http://localhost:8000/test-browser-parity-counter.html`
- `http://localhost:8000/test-browser-parity-dynamic-list.html`
- `http://localhost:8000/test-browser-parity-counters.html`
- `http://localhost:8000/test-browser-parity-tabs.html`
- `http://localhost:8000/test-browser-parity-tabs-dynamic.html`
- `http://localhost:8000/test-browser-parity-list.html`
- `http://localhost:8000/test-browser-parity-todo.html`
- `http://localhost:8000/test-browser-parity-incident.html`
- `http://localhost:8000/test-browser-parity-release.html`
- `http://localhost:8000/test-browser-parity-workspace.html`
- `http://localhost:8000/test-browser-parity-profile.html`
- `http://localhost:8000/test-browser-parity-settings.html`
- `http://localhost:8000/test-browser-parity-table.html`
- `http://localhost:8000/test-browser-parity-menu-keys.html`
- `http://localhost:8000/test-browser-parity-menu-full.html`
- `http://localhost:8000/test-browser-parity-a11y-contract.html`
- `http://localhost:8000/test-browser-parity-keyboard-contract.html`
- `http://localhost:8000/test-browser-parity-focus-order.html`
- `http://localhost:8000/test-browser-parity-disabled-contract.html`

Dashboards:

- full: `http://localhost:8000/test-browser-dashboard.html`
- parity: `http://localhost:8000/test-browser-parity-dashboard.html`
- contract: `http://localhost:8000/test-browser-contract-dashboard.html`

Manual visual page:

- `test-browser-visual-check.html` is intentionally not part of dashboard PASS/FAIL automation.
- compile helper: `./run-browser-visual-check-compile.sh`

## Optional Headless Run

Requires Node.js, `raco static-web`, and Playwright:

```bash
npm install --save-dev playwright
```

Alternative (repo-local ignored tools folder):

```bash
npm --prefix .local-tools install --save-dev playwright
```

`/.local-tools/` is intended for local developer tooling and is git-ignored.

From `lib/web-easy/smoke`:

```bash
./check-smoke-headless.sh
```

This command:

1. Compiles smoke artifacts (`smoke-all`, `visual-check`, `parity-all`).
2. Starts a local static server.
3. Opens `test-browser-dashboard.html` in headless Chromium.
4. Exits `0` on PASS summary, nonzero on FAIL.

Contract-only headless runner:

```bash
./check-contract-headless.sh
```

This command:

1. Compiles `smoke-all` and `parity-all`.
2. Starts a local static server.
3. Opens `test-browser-contract-dashboard.html` in headless Chromium.
4. Exits `0` on PASS summary, nonzero on FAIL.

Environment flags:

- `SMOKE_FORCE_COMPILE=1`:
  - forces recompilation in compile wrappers even when `generated/` artifacts already exist.
- `SMOKE_SKIP_COMPILE=1`:
  - skips compilation in headless wrapper scripts and reuses existing `generated/` artifacts.
  - used by `check-all.sh --headless` for step `[4/4]` to avoid redundant recompiles.

## Single Example Headless

From `lib/web-easy/smoke`:

```bash
./check-single-headless.sh <compile-script> <test-page>
```

Examples:

```bash
./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-hello.html
./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-profile.html
```

This command:

1. Compiles one example.
2. Starts a local static server.
3. Runs Playwright against one test page.
4. Exits `0` on PASS for that page, nonzero on FAIL.

## Troubleshooting

Common failure patterns and what to check first:

1. `Timed out waiting for ... UI`:
   - app page likely failed to render expected node(s) in time.
   - open the failing `test-browser-*.html` directly and inspect browser console/runtime errors.
2. `Playwright is not installed`:
   - install with `npm --prefix .local-tools install --save-dev playwright`.
3. Guard failures (`Found forbidden token in DOM attributes`):
   - inspect the cited element/attribute in the failure message.
   - ensure renderer/backend do not emit internal values into DOM attributes.
4. Web server/port issues:
   - ensure no stale `raco static-web` server is already bound to the port.
5. Compiler/runtime path mismatch:
   - rerun the specific compile script (for example `run-browser-target-compile.sh <target>`) before rechecking.

Note:

- `check-ci-smoke.sh` is the local CI-helper command (`headless dashboard + guard self-test`).
- GitHub workflow: `.github/workflows/web-easy-smoke.yml` runs `./smoke.sh ci` on pushes/PRs touching `lib/web-easy/**`.

## Recommended Daily Flow

From `lib/web-easy/smoke`:

1. Fast contract gate:
   - `./smoke.sh contract`
2. Full contract-first headless flow:
   - `./check-all.sh --headless --contract-first`
3. Optional guard self-test before push:
   - `./smoke.sh guard`

## Parity Commands

From `lib/web-easy/smoke`:

- compile parity examples and print URLs:
  - `./smoke.sh parity`
- compile all parity examples:
  - `./smoke.sh parity-check`
- print parity URLs:
  - `./smoke.sh parity-open`
- run parity-only headless dashboard:
  - `./smoke.sh parity-headless`
- run parity helper scripts in sequence:
  - `./run-all-parity.sh`
- run individual parity helpers:
  - `./run-browser-parity-hello-test.sh`
  - `./run-browser-parity-counter-test.sh`
  - `./run-browser-parity-dynamic-list-test.sh`
  - `./run-browser-parity-counters-test.sh`
  - `./run-browser-parity-tabs-test.sh`
  - `./run-browser-parity-tabs-dynamic-test.sh`
  - `./run-browser-parity-list-test.sh`
  - `./run-browser-parity-todo-test.sh`
  - `./run-browser-parity-incident-test.sh`
  - `./run-browser-parity-release-test.sh`
  - `./run-browser-parity-workspace-test.sh`
  - `./run-browser-parity-profile-test.sh`
  - `./run-browser-parity-settings-test.sh`
  - `./run-browser-parity-table-test.sh`
  - `./run-browser-parity-menu-keys-test.sh`
  - `./run-browser-parity-menu-full-test.sh`

Parity quickstart:

1. `./smoke.sh parity`
2. `./smoke.sh open`
3. open the parity URLs printed by `parity`.

Verification snippet (sequential):

1. `./smoke.sh dashboards`
2. `./smoke.sh contract`
3. `./smoke.sh parity-headless`
4. `./check-all.sh --headless`

Legacy parity-only snippet:

1. `./smoke.sh dashboards`
2. `./smoke.sh parity-headless`
3. `./check-all.sh --headless`

## Manual Release Checklist

Before merge/release, run in this order from `lib/web-easy/smoke`:

1. `./check-ci-smoke.sh`

Expected high-level outcomes:

1. full smoke: `PASS` with `40/40 smoke tests passed`
2. guard self-test: `FAIL` line that says guard correctly detected forbidden token leakage (this is expected/pass condition for the self-test command)

Concurrency note:

- Do not run `./smoke.sh parity-headless` and `./check-all.sh --headless` at the same time.
- Both commands write the same generated smoke artifacts in `lib/web-easy/smoke`.

## Expected PASS Lines

- smoke: `PASS initial=0, after-click=1`
- group: `PASS group uses fieldset + legend`
- dialog: `PASS dialog open/close: open button, Escape, cancel`
- menu-keys: `PASS menu popup + menu-item focus + Enter/Space activation`
- menu-full: `PASS menu-full: multi-menu items + click/Enter/Space + type-ahead`
- a11y-contract: `PASS a11y contract: menu/tab/group/table semantics`
- keyboard-contract: `PASS keyboard contract: menu + tabs keyboard-only flow`
- focus-order: `PASS focus-order: menu labels + tab headers are stable`
- disabled-contract: `PASS disabled contract: disabled tab is non-activating and skipped`
- width: `PASS input fill-width; checkbox/select/slider/progress/button/table content-width`
- input: `PASS initial=alice, after-change=bob`
- checkbox: `PASS initial=off, after-change=on`
- list: `PASS initial=a,b,c; after-click=c,a,b`
- destroy: `PASS before-destroy increment works; after-destroy UI stays unchanged`
- branch: `PASS initial=ON; after-toggle=OFF; after-toggle-again=ON`
- controls: `PASS color: red->green; level: 10->42; progress=42`
- operators: `PASS count: 0->1->2; even(~#>): 0->0->2; λ<~ increment works`
- tab-panel: `PASS initial=Info; click-Settings; key-ArrowRight=About; key-Home=Info`
- tab-panel-keys: `PASS selection+focus track keyboard: info->settings->about->info->about->info->settings`
- tab-panel-disabled: `PASS initial=left; disabled-click-kept-left; ArrowRight=right; ArrowLeft=left; wrap-left=right; wrap-right=left`
- parity-hello: `PASS hello text rendered`
- parity-counter: `PASS counter starts at 0 and increments to 1`
- parity-dynamic-list: `PASS initial: a:0,b:0; inc-a: a:1; reorder: b before a; add-c: c:0; drop-b: a:1,c:0`
- parity-counters: `PASS independent counters: c1 0->1->0, c2 0->2`
- parity-tabs: `PASS tabs switch content: overview->details->help`
- parity-tabs-dynamic: `PASS dynamic tabs parity: add faq, remove selected, remove details`
- parity-list: `PASS list parity: alpha,beta -> reverse -> +gamma -> -beta`
- parity-todo: `PASS todo parity: add, edit/cancel, edit/save, toggle, mark-all-done, clear-done`
- parity-settings: `PASS settings parity: table rows + menu actions`
- parity-table: `PASS table parity: multi-column cells + menu actions`
- parity-menu-keys: `PASS parity menu popup + menu-item focus + Enter/Space activation`
- parity-menu-full: `PASS parity menu-full: multi-menu items + click/Enter/Space + type-ahead`
- parity-a11y-contract: `PASS parity a11y contract: menu/tab/table semantics`
- parity-keyboard-contract: `PASS parity keyboard contract: menu + tabs keyboard-only flow`
- parity-focus-order: `PASS parity focus-order: menu labels + tab headers are stable`
- parity-disabled-contract: `PASS parity disabled contract: disabled tab is non-activating and skipped`
- parity-incident: `PASS incident parity: filter + query + assign/resolve/reset`
- parity-release: `PASS release parity: tabs + controls + list + menu actions`
- parity-workspace: `PASS workspace parity: tabs + form + menu preset flow`
