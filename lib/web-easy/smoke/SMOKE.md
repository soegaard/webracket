# web-easy Smoke Tests

Browser smoke tests for `lib/web-easy`.

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

## Current Test Counts

- full dashboard (`test-browser-dashboard.html`): `19` tests
- parity dashboard (`test-browser-parity-dashboard.html`): `8` tests

Update these counts whenever test pages are added or removed.

Last validated (2026-03-02):

- full headless dashboard: `19/19` PASS
- parity-only headless dashboard: `8/8` PASS

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
- `./smoke.sh all`
- `./smoke.sh headless`
- `./smoke.sh rebuild`
- `./smoke.sh quick`
- `./smoke.sh ci`
- `./smoke.sh status`
- `./smoke.sh urls`
- `./smoke.sh dashboards`
- `./smoke.sh parity-open`
- `./smoke.sh open`
- `./smoke.sh doctor`
- `./smoke.sh clean`
- `./smoke.sh clean-dry`

Command notes:

- `quick`: runs `doctor` preflight, then `all`.
- `ci`: stable non-headless CI entrypoint (`doctor` + `all`).
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

## End-To-End Script

From `lib/web-easy/smoke`:

```bash
./check-all.sh
```

This runs:

1. `racket ../test-web-easy.rkt` (full Racket path)
2. `webracket.rkt -r ../test-web-easy-run.rkt` (WebRacket compiler + Node path)
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
- `http://localhost:8000/test-browser-smoke.html`
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
- `http://localhost:8000/test-browser-parity-hello.html`
- `http://localhost:8000/test-browser-parity-counter.html`
- `http://localhost:8000/test-browser-parity-dynamic-list.html`
- `http://localhost:8000/test-browser-parity-counters.html`
- `http://localhost:8000/test-browser-parity-tabs.html`
- `http://localhost:8000/test-browser-parity-tabs-dynamic.html`
- `http://localhost:8000/test-browser-parity-list.html`
- `http://localhost:8000/test-browser-parity-todo.html`

Dashboards:

- full: `http://localhost:8000/test-browser-dashboard.html`
- parity: `http://localhost:8000/test-browser-parity-dashboard.html`

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

1. Compiles all smoke examples.
2. Starts a local static server.
3. Opens `test-browser-dashboard.html` in headless Chromium.
4. Exits `0` on PASS summary, nonzero on FAIL.

Note:

- CI wiring for the headless smoke path is not configured yet.

## Recommended Daily Flow

From `lib/web-easy/smoke`:

1. Clean generated artifacts:
   - `./smoke.sh clean`
2. Fresh compile smoke artifacts:
   - `./smoke.sh rebuild`
3. Run headless dashboard checks (when desired):
   - `./smoke.sh headless` (runs `doctor` preflight first)

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

Parity quickstart:

1. `./smoke.sh parity`
2. `./smoke.sh open`
3. open the parity URLs printed by `parity`.

Verification snippet (sequential):

1. `./smoke.sh dashboards`
2. `./smoke.sh parity-headless`
3. `./check-all.sh --headless`

Concurrency note:

- Do not run `./smoke.sh parity-headless` and `./check-all.sh --headless` at the same time.
- Both commands write the same generated smoke artifacts in `lib/web-easy/smoke`.

## Expected PASS Lines

- smoke: `PASS initial=0, after-click=1`
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
