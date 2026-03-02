# web-easy Design

## Purpose

`web-easy` is a browser-targeted port of `gui-easy` for WebRacket.
It keeps the same declarative programming model:

- state in observables
- views as pure descriptions
- renderer-managed lifecycle

The backend changes from `racket/gui` widgets to DOM/CSS/event objects.

## WebRacket Constraints

`web-easy` targets the WebRacket dialect, which is more limited than full Racket.

- No object/class system.
- No module system for library composition (current state).
- Design must avoid `class`, `interface`, `mixin`, and `is-a?/c`-style runtime assumptions.
- Core abstractions must be represented as plain data + procedures.
- Avoid depending on full `racket/base` convenience APIs that may be unavailable in WebRacket.

Rationale:

- Matching the real compiler/runtime constraints early avoids rework later.
- A design that assumes classes/modules would create architecture debt that cannot run in current WebRacket output.

## Goals

1. Preserve the core `gui-easy` user model (`obs`, `~>`, `<~`, `list-view`, `if-view`, etc.).
2. Run in browser via WebRacket output (Wasm + JS runtime).
3. Provide predictable updates with keyed incremental rendering.
4. Keep API compatibility high for examples and reusable components.

## Non-Goals (Initial Versions)

1. Full parity with every desktop GUI widget and style option on day one.
2. Pixel-identical behavior with native platform widgets.
3. Support for desktop-only features that have no web equivalent.

## Step 1: What We Must Preserve from gui-easy

Based on local `gui-easy` docs:

- Two-layer model:
  - `Observables`: mutable + derived reactive values.
  - `Views`: declarative trees that react to observable changes.
- Renderer lifecycle:
  - `render`, `renderer-root`, `renderer-destroy`.
- Dynamic composition:
  - `list-view` with stable `#:key`.
  - structural switching via `if-view` / `cond-view` / `case-view` / `observable-view`.
- Extension seam:
  - preserve `dependencies/create/update/destroy` semantics, but via a class-free protocol.

These are semantic contracts. Desktop widget classes and interfaces are implementation details and are not part of the `web-easy` runtime model.

## Proposed Architecture

## Layers

1. **Observable Layer**
   - Reuse existing `gui-easy` observable semantics where possible.
   - Preserve update API (`obs-update!`, `obs-set!`, `obs-map`, combine/filter/debounce/throttle).
   - Keep derived observables read-only.

2. **View IR Layer (`web-easy` internal)**
   - Normalize user view constructors into a backend-neutral representation.
   - Encode static props, dynamic props (observable-backed), event handlers, and keyed children.

3. **DOM Renderer Layer**
   - Convert IR nodes into DOM nodes.
   - Attach subscriptions once, then patch on observable updates.
   - Implement lifecycle (`mount`, `update`, `destroy`) with deterministic cleanup.

4. **Host Integration Layer**
   - Entry points for attaching into a chosen root element.
   - Optional embedding inside existing pages/components.

Current implementation note:

- The renderer currently targets a DOM-like backend boundary in `backend-dom-like.rkt`.
- Renderer logic consumes backend procedures (for example child-append and child-replace) rather than owning node representation directly.
- A browser backend now exists in `backend-browser.rkt` with the same renderer-facing surface.
- Browser assembly is isolated in `main-browser.rkt`; test assembly remains in `main.rkt`.

Rationale:

- Keeping representation and primitive mutations in a backend file makes future browser-FFI replacement incremental instead of invasive.
- This isolates semantic rendering logic from host-node storage details and keeps tests focused on behavior.
- Separate assembly entry points let us keep non-browser tests deterministic while enabling browser mount/integration work in parallel.

## Source Composition Model

`web-easy` is composed like stdlib:

- top-level file: `#lang webracket`
- include components with:
  - `(include/reader "...file.rkt" read-syntax/skip-first-line)`
- each component file starts with `#lang webracket`
- each component exports only selected names via `define-values (...) (let () ... (values ...))`

This replaces module `require/provide` boundaries with explicit include-time name control.

Rationale:

- This is the established pattern already used by WebRacket stdlib, so it aligns with current toolchain behavior.
- `define-values` wrappers preserve encapsulation by exposing only intended names from each file.
- Keeping the same composition style as stdlib lowers maintenance risk and eases debugging.

## Runtime Model

- A render call creates a renderer object with:
  - root DOM node
  - subscriptions set
  - keyed child index maps
  - teardown hooks
- Renderer object is a plain tagged record (struct/alist/vector), not a class instance.
- Updates are driven by observable notifications.
- Batch updates in microtasks to avoid excessive DOM writes.
- Ensure teardown unsubscribes observers and detaches DOM/event references.

## API Surface Plan

## Compatibility Strategy

1. Keep names and signatures where they map cleanly.
2. Keep behavior, document intentional differences when web constraints apply.
3. Stage unsupported features behind explicit errors with clear messages.

## Phase 1 API (MVP)

Planned target:

- Renderers:
  - `render`
  - `renderer?`
  - `renderer-root`
  - `renderer-destroy`
- Core views:
  - `window` (mapped to root container node)
  - `hpanel`, `vpanel`, `group`
  - `text`
  - `button`
  - `input`
  - `checkbox`
  - `choice`
  - `slider`
  - `progress`
  - `radios`
  - `image`
  - `menu-bar` (web-adapted)
  - `menu` (web-adapted)
  - `menu-item` (web-adapted)
  - `if-view`
  - `cond-view`
  - `case-view`
  - `tab-panel`
  - `observable-view`
  - `spacer`
  - `table` (minimal)
  - `list-view`
  - `if-view`, `cond-view`, `case-view`, `observable-view`
- Observable operators:
  - `@`, `:=`, `<~`, `~>`, `~#>`, `λ<~`

Implemented now:

- Renderers:
  - `render`
  - `renderer?`
  - `renderer-root`
  - `renderer-destroy`
- Core views:
  - `window`
  - `hpanel`
  - `vpanel`
  - `group`
  - `text`
  - `button`
  - `input`
  - `checkbox`
  - `choice`
  - `slider`
  - `progress`
  - `if-view`
  - `cond-view`
  - `case-view`
  - `tab-panel`
  - `observable-view`
  - `spacer`
  - `table` (minimal)
  - `list-view`
  - `radios`
  - `image`
  - `menu-bar` (web-adapted)
  - `menu` (web-adapted)
  - `menu-item` (web-adapted)
- Observables:
  - `obs?`, `obs`, `obs-name`, `obs-observe!`, `obs-unobserve!`
  - `obs-update!`, `obs-set!`, `obs-peek`, `obs-map`, `obs-filter`
- Operators:
  - `@`, `:=`, `<~`, `λ<~`, `~>`, `~#>`

Current compatibility notes:

- `obs` currently uses positional optional args (`name`, `derived?`) instead of keyword args.
- Observable operators now include `@`, `:=`, `<~`, `λ<~`, `~>`, and `~#>` in MVP.
- View props/attrs are represented with alists in the current MVP.
- `tab-panel` accepts both tab entry forms:
  - `(cons tab-id view)`
  - `(list tab-id view disabled?)`
  - where `disabled?` defaults to `#f` for pair entries.
- Renderer currently uses an in-memory DOM-like node model for testing and semantics work.
- Browser integration path now uses:
  - `main-browser.rkt`
  - `backend-browser.rkt`
  - `browser-host.rkt` (`mount-renderer!`)
- Browser smoke example:
  - `smoke/example-browser-smoke.rkt`
  - input smoke example: `smoke/example-browser-input.rkt`
  - checkbox smoke example: `smoke/example-browser-checkbox.rkt`
  - list-view smoke example: `smoke/example-browser-list.rkt`
  - destroy lifecycle smoke example: `smoke/example-browser-destroy.rkt`
  - branch switching smoke example: `smoke/example-browser-branch.rkt`
  - controls smoke example: `smoke/example-browser-controls.rkt`
  - operators smoke example: `smoke/example-browser-operators.rkt`
  - tab-panel smoke example: `smoke/example-browser-tab-panel.rkt`
  - tab-panel disabled smoke example: `smoke/example-browser-tab-panel-disabled.rkt`
  - compile helpers:
    - `smoke/run-browser-smoke-compile.sh`
    - `smoke/run-browser-input-compile.sh`
    - `smoke/run-browser-checkbox-compile.sh`
    - `smoke/run-browser-list-compile.sh`
    - `smoke/run-browser-destroy-compile.sh`
    - `smoke/run-browser-branch-compile.sh`
    - `smoke/run-browser-controls-compile.sh`
    - `smoke/run-browser-operators-compile.sh`
    - `smoke/run-browser-tab-panel-compile.sh`
    - `smoke/run-browser-tab-panel-disabled-compile.sh`
  - runtime harnesses:
    - `smoke/test-browser-smoke.html`
    - `smoke/test-browser-input.html`
    - `smoke/test-browser-checkbox.html`
    - `smoke/test-browser-list.html`
    - `smoke/test-browser-destroy.html`
    - `smoke/test-browser-branch.html`
    - `smoke/test-browser-controls.html`
    - `smoke/test-browser-operators.html`
    - `smoke/test-browser-tab-panel.html`
    - `smoke/test-browser-tab-panel-keys.html`
    - `smoke/test-browser-tab-panel-disabled.html`
  - aggregate runner: `smoke/run-all-smoke.sh`

Rationale:

- Positional optional args avoid keyword support assumptions that may not hold in current WebRacket.
- Alists avoid dependence on hash APIs that are not consistently available in the current dialect/runtime.
- The in-memory DOM model lets us validate reactive semantics and lifecycle behavior before binding to browser DOM FFI details.

## Step 2: Port Status Matrix

Current status snapshot (March 1, 2026):

| Area | Item | Status | Coverage | Notes |
|---|---|---|---|---|
| Renderer lifecycle | `render`, `renderer?`, `renderer-root`, `renderer-destroy` | Implemented | Core tests + smoke | Browser backend integrated via `backend-browser.rkt`. |
| Core layout/views | `window`, `hpanel`, `vpanel`, `group`, `spacer` | Implemented | Core tests + smoke | Uses DOM container mapping. |
| Basic controls | `text`, `button`, `input`, `checkbox`, `choice`, `slider`, `progress`, `radios`, `image` | Implemented | Core tests + smoke | Browser behavior validated in dedicated smoke pages. |
| Dynamic composition | `if-view`, `cond-view`, `case-view`, `observable-view`, `list-view` | Implemented | Core tests + smoke | Keyed reconciliation and branch switching covered. |
| Menus | `menu-bar`, `menu`, `menu-item` | Implemented (web-adapted MVP) | Core tests | Semantics intentionally differ from desktop-native menu systems. |
| Tabs | `tab-panel` | Implemented | Core tests + smoke | Includes keyboard navigation, disabled tabs, and focus tracking checks. |
| Observables core | `obs?`, `obs`, `obs-name`, `obs-observe!`, `obs-unobserve!`, `obs-update!`, `obs-set!`, `obs-peek`, `obs-map`, `obs-filter` | Implemented | Core tests | Compatible MVP surface. |
| Operators | `@`, `:=`, `<~`, `λ<~`, `~>`, `~#>` | Implemented | Core tests + smoke | Operator smoke covers filter + thunk behavior in browser runtime. |
| Operators | `~#>`, `λ<~` | Implemented | Core tests | `~#>` maps to `obs-filter`; `λ<~` builds update thunks. |
| Desktop-specific API parity | dialogs/snips/canvas/native window semantics | Deferred | N/A | Requires explicit web-specific capability design. |
| Automation | headless smoke in CI | Deferred | Local headless only | Local script exists; CI wiring intentionally postponed. |

Remaining Step 2 planning items:

1. Decide Phase 2 scope for desktop-specific APIs with explicit web fallbacks.
2. Keep compatibility matrix updated as each deferred item moves to implemented/deferred-by-design.

## Phase 2 API

- Menus as web-appropriate components (`menu-bar`, `menu`, `menu-item`, popup menus).
- Tabs/table/canvas/snip-related views with explicit capability notes.
- Escape hatches for custom DOM behavior and host integration.

## Mapping gui-easy Concepts to Web

1. `window`/`dialog`
   - Desktop: top-level native window/dialog.
   - Web: container node with role/class semantics; modal behavior via overlay/focus management.

2. Panels (`hpanel`, `vpanel`, `group`)
   - Map to `div` + flex layout.
   - `#:alignment`, `#:stretch`, `#:spacing`, `#:margin` map to CSS properties.

3. Controls
   - `text` -> text node/span/div.
   - `button` -> `<button>`.
   - `input` -> `<input>`/`<textarea>` depending on style.
   - `choice` -> `<select>` (or custom listbox when needed).
   - `checkbox` -> `<input type="checkbox">`.
   - `slider` -> `<input type="range">`.
   - `progress` -> `<progress>`.

4. Dynamic lists
   - `list-view` requires keyed reconciliation:
     - reuse nodes for unchanged keys
     - reorder/move when order changes
     - destroy removed keys

5. Menus
   - desktop menu APIs have no direct browser equivalent.
   - implement as accessible component primitives and document differences.

## Internal Contracts

Each internal view implementation should follow this shape:

- `dependencies` -> list of source observables
- `create(parent)` -> create/mount DOM subtree and context
- `update(node, dep, value)` -> targeted update for changed dependency
- `destroy(node)` -> unsubscribe + remove listeners + recursive cleanup

Implementation rule for WebRacket:

- represent views as plain tagged values (for example, structs with a `kind` field)
- dispatch with `case`/tag-based procedures, not methods
- keep per-node mutable runtime state in explicit renderer tables/maps

This preserves the `view<%>` behavior contract without requiring objects/classes.

Rationale:

- Tag-based dispatch is simple, portable, and directly compatible with current language limits.
- Explicit state tables make lifecycle and cleanup logic auditable, which is important for avoiding observer/listener leaks.

## Class-Free Core Data Model

Recommended initial representation:

- `View` value:
  - `kind` (symbol tag, e.g. `'text`, `'button`, `'vpanel`)
  - `props` (immutable property map/alist)
  - `dyn-props` (observable-backed property descriptors)
  - `children` (list of `View`)
  - `hooks` (optional create/destroy/update procedures)
- `Renderer` value:
  - `root-node`
  - `subscriptions`
  - `node-table` (view-id -> DOM node)
  - `cleanup-table` (view-id -> cleanup procedures)

All behavior is implemented by top-level procedures in included files:

- `render-view`
- `update-view!`
- `destroy-view!`
- `reconcile-children!`

No class-based API is required internally.

## Scheduling and Performance

1. Coalesce repeated observable updates within a microtask.
2. Avoid full subtree rerenders; patch only affected nodes/attributes/text.
3. Use keyed reconciliation for list updates.
4. Keep event handlers stable when possible to reduce churn.

## Testing

Current `web-easy` test workflows:

1. Real Racket semantics test:
   - `racket lib/web-easy/test-web-easy.rkt`
2. WebRacket compiler+node path:
   - `cd lib/web-easy && racket ../../webracket.rkt -r test-web-easy-run.rkt`
3. Browser smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-smoke-test.sh`
   - then open `http://localhost:8000/test-browser-smoke.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
4. Browser input smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-input-test.sh`
   - then open `http://localhost:8000/test-browser-input.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
5. Build both smoke apps:
   - `lib/web-easy/smoke/run-all-smoke.sh`
6. Browser checkbox smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-checkbox-test.sh`
   - then open `http://localhost:8000/test-browser-checkbox.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
7. Browser list-view smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-list-test.sh`
   - then open `http://localhost:8000/test-browser-list.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
8. Browser destroy lifecycle smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-destroy-test.sh`
   - then open `http://localhost:8000/test-browser-destroy.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
9. Browser branch switching smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-branch-test.sh`
   - then open `http://localhost:8000/test-browser-branch.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
10. Browser controls smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-controls-test.sh`
   - then open `http://localhost:8000/test-browser-controls.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
11. Browser operators smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-operators-test.sh`
   - then open `http://localhost:8000/test-browser-operators.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
12. Browser tab-panel smoke compile+runtime:
   - `lib/web-easy/smoke/run-browser-tab-panel-test.sh`
   - then open `http://localhost:8000/test-browser-tab-panel.html` while serving `lib/web-easy/smoke`.
   - compile uses `--ffi dom --ffi standard`.
13. Browser tab-panel keyboard-only smoke runtime:
   - `lib/web-easy/smoke/run-browser-tab-panel-keys-test.sh`
   - then open `http://localhost:8000/test-browser-tab-panel-keys.html` while serving `lib/web-easy/smoke`.
   - uses keyboard-only interaction checks (ArrowRight/Home/End).
14. Browser tab-panel disabled-tab smoke runtime:
   - `lib/web-easy/smoke/run-browser-tab-panel-disabled-test.sh`
   - then open `http://localhost:8000/test-browser-tab-panel-disabled.html` while serving `lib/web-easy/smoke`.
   - checks disabled click-ignore and ArrowLeft/ArrowRight skip behavior.
15. Browser no-dependency aggregate runner:
   - open `http://localhost:8000/test-browser-dashboard.html` to run all smoke pages in hidden iframes and aggregate PASS/FAIL.
16. Optional headless automation (available now):
   - headless browser execution can run `smoke/test-browser-dashboard.html` and return local pass/fail.
   - script path: `lib/web-easy/smoke/check-smoke-headless.sh` (requires Node + Playwright + `raco static-web`).
   - CI wiring for this headless path is not configured yet.
17. Parity hello smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-hello-test.sh`
   - then open `http://localhost:8000/test-browser-parity-hello.html` while serving `lib/web-easy/smoke`.
18. Parity counter smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-counter-test.sh`
   - then open `http://localhost:8000/test-browser-parity-counter.html` while serving `lib/web-easy/smoke`.
19. Parity dynamic-list smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-dynamic-list-test.sh`
   - then open `http://localhost:8000/test-browser-parity-dynamic-list.html` while serving `lib/web-easy/smoke`.
20. Parity aggregate helper (manual):
   - `lib/web-easy/smoke/run-all-parity.sh`
   - runs parity helper scripts in sequence.
21. Parity counters smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-counters-test.sh`
   - then open `http://localhost:8000/test-browser-parity-counters.html` while serving `lib/web-easy/smoke`.
22. Parity tabs smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-tabs-test.sh`
   - then open `http://localhost:8000/test-browser-parity-tabs.html` while serving `lib/web-easy/smoke`.
23. Parity list smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-list-test.sh`
   - then open `http://localhost:8000/test-browser-parity-list.html` while serving `lib/web-easy/smoke`.
24. Parity todo smoke compile+runtime (manual):
   - `lib/web-easy/smoke/run-browser-parity-todo-test.sh`
   - then open `http://localhost:8000/test-browser-parity-todo.html` while serving `lib/web-easy/smoke`.
25. Dashboard/headless parity coverage:
   - parity pages are included in `smoke/test-browser-dashboard.html`.
   - `check-all.sh --headless` now validates parity pages alongside core smoke pages.
26. Parity-only dashboard/headless loop:
   - parity dashboard: `smoke/test-browser-parity-dashboard.html`
   - parity-only headless runner: `smoke/check-parity-headless.sh`
   - wrapper command: `cd lib/web-easy/smoke && ./smoke.sh parity-headless`

Current dashboard test counts:

1. full dashboard (`smoke/test-browser-dashboard.html`): `19` tests
2. parity dashboard (`smoke/test-browser-parity-dashboard.html`): `8` tests

Update these counts whenever test pages are added or removed.

Smoke lifecycle quick commands:

1. Compile smoke artifacts:
   - `cd lib/web-easy/smoke && ./check-smoke.sh`
2. Run core+smoke flow:
   - `cd lib/web-easy/smoke && ./check-all.sh`
3. Run core+smoke+headless flow:
   - `cd lib/web-easy/smoke && ./check-all.sh --headless`
4. Clean generated smoke artifacts:
   - `cd lib/web-easy/smoke && ./clean-smoke.sh`

`smoke.sh` wrapper commands:

- `cd lib/web-easy/smoke && ./smoke.sh status`
- `cd lib/web-easy/smoke && ./smoke.sh doctor`
- `cd lib/web-easy/smoke && ./smoke.sh urls`
- `cd lib/web-easy/smoke && ./smoke.sh dashboards`
- `cd lib/web-easy/smoke && ./smoke.sh parity`
- `cd lib/web-easy/smoke && ./smoke.sh parity-open`
- `cd lib/web-easy/smoke && ./smoke.sh open`
- `cd lib/web-easy/smoke && ./smoke.sh check`
- `cd lib/web-easy/smoke && ./smoke.sh parity-check`
- `cd lib/web-easy/smoke && ./smoke.sh rebuild`
- `cd lib/web-easy/smoke && ./smoke.sh all`
- `cd lib/web-easy/smoke && ./smoke.sh quick`
- `cd lib/web-easy/smoke && ./smoke.sh ci`
- `cd lib/web-easy/smoke && ./smoke.sh headless`
- `cd lib/web-easy/smoke && ./smoke.sh parity-headless`
- `cd lib/web-easy/smoke && ./smoke.sh clean`
- `cd lib/web-easy/smoke && ./smoke.sh clean-dry`

Known operational caveats:

1. Do not run `./smoke.sh parity-headless` and `./check-all.sh --headless` concurrently.
2. Both commands write shared generated artifacts under `lib/web-easy/smoke`.

Verification sequence (run in order):

1. `cd lib/web-easy/smoke && ./smoke.sh dashboards`
2. `cd lib/web-easy/smoke && ./smoke.sh parity-headless`
3. `cd lib/web-easy/smoke && ./check-all.sh --headless`

## Error Handling

1. Validate arguments early and fail with clear function names.
2. Detect unsupported style/options and explain web-specific limitation.
3. Fail fast for impossible states in renderer bookkeeping.

## Testing Strategy

1. Observable semantics tests:
   - update ordering
   - derived observable behavior
   - unobserve/destroy cleanup
2. View rendering tests:
   - initial DOM structure snapshots
   - reactive updates after state changes
3. List reconciliation tests:
   - insertion/deletion/reordering by key
4. Lifecycle tests:
   - `renderer-destroy` removes subscriptions/listeners
5. Browser integration tests:
   - run representative examples in headless browser environment

Current test location:

- `lib/web-easy/test-web-easy.rkt` (include-based `#lang webracket` test entrypoint)
- `lib/web-easy/test-web-easy-run.rkt` (no-`#lang` entrypoint for `webracket.rkt -r`)
- `lib/web-easy/test-web-easy-body.rkt` (shared test definitions/assertions)

Current test commands:

- Real Racket run:
  - `cd /Users/soegaard/Dropbox/GitHub/webracket`
  - `racket lib/web-easy/test-web-easy.rkt`
- WebRacket compiler + node run:
  - `cd /Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy`
  - `racket ../../webracket.rkt -r test-web-easy-run.rkt`

## Milestones

1. **M1: Core Runtime**
   - renderer object
   - observable subscriptions
   - mount/destroy lifecycle
2. **M2: Basic View Set**
   - text/button/input/panels
   - conditional views
3. **M3: Dynamic Collections**
   - `list-view` keyed reconciliation
4. **M4: Menus + advanced controls**
5. **M5: Example parity pass**
   - port selected `gui-easy` examples and validate behavior

### M4 Closure: Deferred By Design

The following M4-adjacent items are deferred as web-specific design work, not blockers for current MVP:

1. Native window/dialog semantics:
   - desktop-native top-level window APIs do not map 1:1 to browser hosting.
   - current fallback: `window` maps to a root container; modal/dialog behavior is deferred.
2. Snip/canvas desktop APIs:
   - desktop editor/canvas integration surfaces are platform-specific.
   - current fallback: use web views (`image`, `table`, panels, custom DOM extension points) until a dedicated canvas component is specified.
3. Desktop-only menu affordances:
   - browser menubars differ from native app menubars.
   - current fallback: web-adapted `menu-bar/menu/menu-item` semantics already implemented.

### M5 Kickoff: Example Parity Checklist

Initial parity targets (from gui-easy quickstart progression):

1. Hello world (`1.1` style example)
   - status: implemented (`smoke/example-browser-parity-hello.rkt`)
   - pass criteria:
     - static UI renders expected text in browser.
     - no runtime errors in smoke dashboard/manual page.
2. Counter (`1.2` style example)
   - status: implemented (`smoke/example-browser-parity-counter.rkt`)
   - pass criteria:
     - counter starts at `0`.
     - button click increments count deterministically.
3. Dynamic counters/list (`1.4` style example)
   - status: implemented (`smoke/example-browser-parity-dynamic-list.rkt`)
   - pass criteria:
     - list of counters updates with keyed reconciliation behavior.
     - add/remove/reorder interactions preserve state by key.

Execution notes:

- Add each parity case as `smoke/example-browser-parity-*.rkt`.
- Add companion smoke page `smoke/test-browser-parity-*.html`.
- Keep `run-all-parity.sh` as a focused parity-only helper for local/manual checks.

M5 phase 1 status:

- Phase 1 complete for the initial 3 parity targets (hello, counter, dynamic-list).
- Current parity verification mode includes dashboard/headless automation and optional manual parity scripts.
- Expected parity PASS lines:
  - `PASS hello text rendered`
  - `PASS counter starts at 0 and increments to 1`
  - `PASS initial: a:0,b:0; inc-a: a:1; reorder: b before a; add-c: c:0; drop-b: a:1,c:0`
- Manual verification run (2026-03-01):
  - `test-browser-parity-hello.html`: PASS (`hello text rendered`)
  - `test-browser-parity-counter.html`: PASS (`counter starts at 0 and increments to 1`)
  - `test-browser-parity-dynamic-list.html`: PASS (`initial: a:0,b:0; inc-a: a:1; reorder: b before a; add-c: c:0; drop-b: a:1,c:0`)
  - report file: `lib/web-easy/smoke/parity-report.md`
- Last validated (2026-03-02):
  - full headless dashboard: `19/19` PASS
  - parity-only headless dashboard: `8/8` PASS

M5 phase 2 status:

- Phase 2 targets implemented:
  - counters (`smoke/example-browser-parity-counters.rkt`)
  - tabs (`smoke/example-browser-parity-tabs.rkt`)
  - dynamic tabs add/remove (`smoke/example-browser-parity-tabs-dynamic.rkt`)
  - list (`smoke/example-browser-parity-list.rkt`)
  - simple todo (`smoke/example-browser-parity-todo.rkt`)
- Expected phase-2 parity PASS lines:
  - `PASS independent counters: c1 0->1->0, c2 0->2`
  - `PASS tabs switch content: overview->details->help`
  - `PASS dynamic tabs parity: add faq, remove selected, remove details`
  - `PASS list parity: alpha,beta -> reverse -> +gamma -> -beta`
  - `PASS todo parity: add, edit/cancel, edit/save, toggle, mark-all-done, clear-done`

## Open Design Questions

1. Exact API shape for `window` in web context:
   - Should it require an explicit root element or default to document body?
2. How much of desktop style vocabulary should be accepted vs rejected?
3. Should we include a lightweight virtual-node IR, or patch DOM directly from view objects?
4. How should modal dialog semantics map to focus trapping and accessibility defaults?
5. Tab visual polish roadmap:
   - current tabs intentionally use button-like styling for behavior-first parity checks.
   - deferred UI pass should make tabs more tab-like (selected tab merged with panel, inactive tabs recessed, clearer tab strip hierarchy).

## Immediate Next Steps

1. Decide long-term dashboard policy:
   - keep parity pages in full dashboard permanently, or
   - keep full+parity split but run parity-only in selected workflows.
2. Define M5 phase-2 parity targets:
   - completed for counters/tabs/list/todo.
   - next: choose phase-3 parity targets.
3. Keep counts and validation stamps up to date:
   - update dashboard test counts and last-validated lines whenever smoke/parity inventory changes.
