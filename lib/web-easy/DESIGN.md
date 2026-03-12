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
  - `hpanel`, `vpanel`, `group`, `button-group`, `button-toolbar`, `card`, `navigation-bar`
  - `container`, `grid`, `stack`, `inline`
  - `text`
  - `spinner`
  - `button`
  - `input`
  - `textarea`
  - `checkbox`
  - `choice`
  - `slider`
  - `progress`
  - `radios`
  - `image`
  - `breadcrumb`
  - `list-group`
  - `dropdown`
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
  - `container`
  - `grid`
  - `stack`
  - `inline`
  - `group`
  - `button-group`
  - `button-toolbar`
  - `toolbar`
  - `toolbar-group`
  - `card`
  - `navigation-bar`
  - `link`
  - `divider`
  - `text`
  - `spinner`
  - `button`
  - `input`
  - `textarea`
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
  - `breadcrumb`
  - `list-group`
  - `dropdown`
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
- New constructor extensions in this slice:
  - `button`: optional leading/trailing icon args.
  - `menu-item`: optional leading/trailing icon args.
  - `input`: optional attrs list (for example `placeholder`, `autocomplete`).
  - `textarea`: optional `rows` (default `3`) and optional attrs list.
  - `card`: optional variant symbol/list (`compact`, `flat`, `headerless`) plus optional `tone` + `tone-style` in options (`fill`/`outline`).
  - `popover`: optional options alist with `title` and `footer` structured regions.
  - `tooltip`: optional options alist with `title` and `footer` structured regions.
  - `dialog`/`modal`: optional options include `tone` + `tone-style` hooks (`fill`/`outline`) in addition to title/description/footer/close.
  - `choice`/`dropdown`: option rows now support scalar, pair, or 2-element list forms; labels can differ from ids.
- New theme token API:
  - `theme-token-ref`
  - `theme-token-set!`
  - `theme-token-set-many!`
  - browser build installs a token applier to write CSS custom properties on `html`.
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
- Browser smoke/parity drivers:
  - `smoke/example-browser-smoke-all.rkt`
  - `smoke/example-browser-parity-all.rkt`
  - compile entrypoints:
    - `smoke/run-browser-smoke-all-compile.sh`
    - `smoke/run-browser-parity-all-compile.sh`
    - `smoke/run-browser-visual-check-compile.sh`
    - `smoke/run-browser-target-compile.sh <smoke-all|parity-all|visual-check>`
  - generated artifacts:
    - written to `smoke/generated/`
  - runtime harnesses:
    - `smoke/test-browser-smoke.html`
    - `smoke/test-browser-layout-primitives.html`
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
  - aggregate dashboard:
    - `smoke/test-browser-dashboard.html`

Rationale:

- Native keyword-call syntax is still constrained in WebRacket, so keyword-capable APIs use
  syntax-layer helpers (`define/key` and `call/key`) instead of relying on direct runtime
  keyword argument support.
- Alists avoid dependence on hash APIs that are not consistently available in the current dialect/runtime.
- The in-memory DOM model lets us validate reactive semantics and lifecycle behavior before binding to browser DOM FFI details.

## define/key Formal Grammar

`libs/define.rkt` provides `define/key` and `call/key` for keyword-capable function definitions and calls.

Supported `define/key` header grammar:

- `(define/key (name req ... [opt default] ... . rest #:kw req-id #:kw2 [opt-id default2] ...) body ...+)`
- Required positional args (`req`) must appear first.
- Optional positional args (`[opt default]`) follow required positional args.
- Rest arg (`. rest`) is optional and appears before keyword clauses.
- Keyword clauses are either required (`#:kw req-id`) or optional (`#:kw [opt-id default]`).

Call forms:

- Direct call to macro-bound name: `(name arg ... #:kw v ...)`
- First-class/aliased/higher-order call via helper macro: `(call/key f arg ... #:kw v ...)`

Limitations:

- Keyword tokens are consumed by macro expansion. For non-macro callees, use `call/key` instead of raw `(f ... #:kw ...)`.

## Layout Primitives

`web-easy` now includes first-class layout primitives for page-level structure:

- `container`: centered width-constrained wrapper
- `grid`: responsive grid layout with columns specification
- `stack`: vertical spacing/layout primitive
- `inline`: horizontal wrapping row primitive
- `spacer`: growable empty flex item (optional grow factor)

Rationale:

- Reduce reliance on CSS-only structure classes in examples and apps.
- Make layout intent explicit in view code instead of encoded in wrapper class names.
- Reduce parenthesis-heavy `with-class` composition for common layout patterns.
- Keep visual customization in CSS while preserving semantic structure in view constructors.

## Step 2: Port Status Matrix

Current status snapshot (March 1, 2026):

| Area | Item | Status | Coverage | Notes |
|---|---|---|---|---|
| Renderer lifecycle | `render`, `renderer?`, `renderer-root`, `renderer-destroy` | Implemented | Core tests + smoke | Browser backend integrated via `backend-browser.rkt`. |
| Core layout/views | `window`, `hpanel`, `vpanel`, `group`, `button-group`, `button-toolbar`, `card`, `navigation-bar`, `spacer`, `collapse`, `accordion`, `dialog` | Implemented | Core tests + smoke | Uses DOM container mapping; `accordion` is composed from `button` + `collapse` with keyboard navigation. |
| Basic controls | `text`, `spinner`, `alert`, `badge`, `toast`, `button`, `input`, `checkbox`, `choice`, `slider`, `progress`, `pagination`, `breadcrumb`, `list-group`, `radios`, `image`, `tooltip`, `popover` | Implemented | Core tests + smoke | Browser behavior validated in dedicated smoke pages. |
| Dynamic composition | `if-view`, `cond-view`, `case-view`, `observable-view`, `list-view` | Implemented | Core tests + smoke | Keyed reconciliation and branch switching covered. |
| Menus | `dropdown`, `menu-bar`, `menu`, `menu-item` | Implemented (web-adapted MVP) | Core tests + smoke | `dropdown` is a single-trigger menu wrapper over shared menu semantics. |
| Tabs | `tab-panel` | Implemented | Core tests + smoke | Includes keyboard navigation, disabled tabs, and focus tracking checks. |
| Observables core | `obs?`, `obs`, `obs-name`, `obs-observe!`, `obs-unobserve!`, `obs-update!`, `obs-set!`, `obs-peek`, `obs-map`, `obs-filter` | Implemented | Core tests | Compatible MVP surface. |
| Operators | `@`, `:=`, `<~`, `λ<~`, `~>`, `~#>` | Implemented | Core tests + smoke | Operator smoke covers filter + thunk behavior in browser runtime. |
| Operators | `~#>`, `λ<~` | Implemented | Core tests | `~#>` maps to `obs-filter`; `λ<~` builds update thunks. |
| Desktop-specific API parity | snips/canvas/native window semantics | Deferred | N/A | Requires explicit web-specific capability design. |
| Automation | headless smoke in CI | Implemented | Local + CI headless | GitHub workflow runs compile + dashboard headless checks. |

Remaining Step 2 planning items:

1. Decide Phase 2 scope for desktop-specific APIs with explicit web fallbacks.
2. Keep compatibility matrix updated as each deferred item moves to implemented/deferred-by-design.

### Bootstrap-Style Coverage Audit (Current)

`Y` means there is dedicated coverage in that lane.
`P` means partial/indirect coverage (for example via a broader dashboard contract page).

| Component | web-easy mapping | Implemented | Smoke | Parity | Contract |
|---|---|---|---|---|---|
| Accordion | `accordion` | Y | Y | Y | Y |
| Alerts | `alert` | Y | Y | Y | Y |
| Badge | `badge` | Y | Y | Y | Y |
| Breadcrumb | `breadcrumb` | Y | Y | Y | Y |
| Buttons | `button` | Y | Y | Y | P |
| Button group | `button-group` | Y | Y | Y | Y |
| Card | `card` | Y | Y | Y | Y |
| Carousel | `carousel` | Y | Y | Y | Y |
| Close button | `close-button` | Y | Y | Y | Y |
| Collapse | `collapse` | Y | Y | Y | Y |
| Dropdowns | `dropdown` | Y | Y | Y | Y |
| List group | `list-group` | Y | Y | Y | Y |
| Modal | `dialog` | Y | Y | Y | Y |
| Offcanvas | `offcanvas` | Y | Y | Y | Y |
| Navbar | `navigation-bar` | Y | Y | Y | Y |
| Navs & tabs | `tab-panel` | Y | Y | Y | Y |
| Pagination | `pagination` | Y | Y | Y | Y |
| Placeholders | `placeholder` | Y | Y | Y | Y |
| Popovers | `popover` | Y | Y | Y | Y |
| Progress | `progress` | Y | Y | Y | Y |
| Scrollspy | `scrollspy` | Y | Y | Y | Y |
| Spinners | `spinner` | Y | Y | Y | Y |
| Toasts | `toast` | Y | Y | Y | Y |
| Tooltips | `tooltip` | Y | Y | Y | P |

### Bootswatch Solar Page Parity Gaps (Current)

The Solar showcase page also demonstrates form/layout patterns that are not yet
first-class `web-easy` components (they are currently done via composition or CSS):

| Pattern on Solar page | Current web-easy status | Gap type |
|---|---|---|
| Input group/addons (prepend/append text or buttons) | composition only | missing first-class component |
| Floating labels | CSS pattern only | missing first-class component |
| Validation feedback pattern (`valid/invalid` + feedback text) | composition only | missing first-class component |
| File input pattern | plain file input styling only | missing first-class component |
| Switch-style checkbox variant | checkbox + CSS only | missing first-class component |
| List-group badge clipping in some browser/font combinations | under active polish | pending CSS baseline/line-box normalization |

Priority targets to add first:

1. `input-group` (high leverage; common in form-heavy pages).
2. `floating-label-field` (common modern form affordance).
3. Validation helpers (`field-state`/`field-feedback`) for consistent UX and testability.

Rationale:

- These are recurring page-building patterns, not one-off demo visuals.
- Making them first-class reduces CSS boilerplate and improves portability across themes.
- They can be tested as semantic components rather than ad-hoc class combinations.
- Cross-browser list-group badge rendering should be validated by computed-style contracts
  in addition to screenshot diff (to catch line-box clipping regressions early).

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
   - `input` -> `<input>`.
   - `textarea` -> `<textarea>`.
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

## CSS Strategy (Stylesheet-First)

`web-easy` should support user customization via CSS stylesheet only, while keeping
usable defaults out of the box.

Rules:

1. Renderer emits semantic classes/attributes, not visual inline styles.
2. Renderer-injected CSS is structural only (layout/visibility/focus plumbing), not
   visual theme skinning.
3. Shared visual defaults live in theme stylesheets, not in per-node style strings.
4. Runtime state is expressed through classes (for example `.is-open`,
   `.is-selected`, `.is-disabled`) and ARIA attributes.
5. Dynamic one-off geometry values may use inline style only when they cannot be
   represented as stable classes/tokens.
6. Selectors intended for user theming are part of the public contract and should be
   treated as stable.

Current migration note:

- `renderer.rkt` now injects structural base CSS only.
- Visual component defaults are kept in external theme files (for example
  `smoke/theme-external-*.css`, `smoke/theme-solar-2.css`).
- When later exact-match sections fully override earlier theme blocks, prune the dead
  earlier blocks to keep the stylesheet readable.

`data-we-widget` role:

- `data-we-widget` is a stable widget identity marker (for example
  `data-we-widget="dialog"`), independent of visual class naming.
- It allows user CSS to target component type reliably:
  - `[data-we-widget='dialog'] { ... }`
- It supports tooling/debugging/tests by giving a semantic hook that does not depend
  on DOM shape details.
- It complements classes instead of replacing them:
  - class selectors for style/state
  - `data-we-widget` for stable component identity.

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
   - `racket lib/web-easy/test/test-web-easy.rkt`
2. WebRacket compiler+node path:
   - `cd lib/web-easy/test && racket ../../../webracket.rkt -r test-web-easy-run.rkt`
3. Browser smoke compile matrix:
   - `cd lib/web-easy/smoke && ./check-smoke.sh`
4. Headless smoke orchestration:
   - `cd lib/web-easy/smoke && ./headless.sh ci`
   - Solar-focused loop:
     - `cd lib/web-easy/smoke && ./check-solar-workflow.sh`
5. Fast local headless gate (no compile):
   - `make smoke-headless-lite`

Command ownership:

- `smoke.sh`: local/manual utility wrapper (serve + compile helpers).
- `headless.sh`: canonical headless orchestration entrypoint.
- `make`: canonical repo-root automation entrypoints.

Canonical smoke documentation:

- `lib/web-easy/smoke/SMOKE.md` (operations, commands, CI usage)
- `lib/web-easy/smoke/SMOKE-CONTRACTS.md` (contract semantics + expected PASS prefixes)
- `lib/web-easy/smoke/COMMANDS.tsv` (machine-readable command inventory)

Rationale:

- Keep `DESIGN.md` focused on architecture and implementation semantics.
- Keep operational command details in smoke-specific docs where they can evolve independently.

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

- `lib/web-easy/test/test-web-easy.rkt` (include-based `#lang webracket` test entrypoint)
- `lib/web-easy/test/test-web-easy-run.rkt` (no-`#lang` entrypoint for `webracket.rkt -r`)
- `lib/web-easy/test/test-web-easy-body.rkt` (shared test definitions/assertions)

Current test commands:

- Real Racket run:
  - `cd /Users/soegaard/Dropbox/GitHub/webracket`
  - `racket lib/web-easy/test/test-web-easy.rkt`
- WebRacket compiler + node run:
  - `cd /Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/test`
  - `racket ../../../webracket.rkt -r test-web-easy-run.rkt`

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

1. Native window semantics:
   - desktop-native top-level window APIs do not map 1:1 to browser hosting.
   - current fallback: `window` maps to a root container.
2. Snip/canvas desktop APIs:
   - desktop editor/canvas integration surfaces are platform-specific.
   - current fallback: use web views (`image`, `table`, panels, custom DOM extension points) until a dedicated canvas component is specified.
3. Desktop-only menu affordances:
   - browser menubars differ from native app menubars.
   - current fallback: web-adapted `menu-bar/menu/menu-item` semantics already implemented.

### M5 Kickoff: Example Parity Checklist

Initial parity targets (from gui-easy quickstart progression):

1. Hello world (`1.1` style example)
   - status: implemented (served via `smoke/example-browser-parity-all.rkt?test=parity-hello`)
   - pass criteria:
     - static UI renders expected text in browser.
     - no runtime errors in smoke dashboard/manual page.
2. Counter (`1.2` style example)
   - status: implemented (served via `smoke/example-browser-parity-all.rkt?test=parity-counter`)
   - pass criteria:
     - counter starts at `0`.
     - button click increments count deterministically.
3. Dynamic counters/list (`1.4` style example)
   - status: implemented (served via `smoke/example-browser-parity-all.rkt?test=parity-dynamic-list`)
   - pass criteria:
     - list of counters updates with keyed reconciliation behavior.
     - add/remove/reorder interactions preserve state by key.

Execution notes:

- Add each parity case as a capsule in `smoke/smoke-capsule-parity-*.rkt`.
- Add companion smoke page `smoke/test-browser-parity-*.html`.
- Compile parity pages through `smoke/run-browser-parity-all-compile.sh`.

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
- Current validation baseline:
  - maintained in `lib/web-easy/smoke/SMOKE.md` under "Current Test Counts" and "Last validated".

M5 phase 2 status:

- Phase 2 targets implemented:
  - counters (`parity-counters` capsule in `example-browser-parity-all.rkt`)
  - tabs (`parity-tabs` capsule in `example-browser-parity-all.rkt`)
  - dynamic tabs add/remove (`parity-tabs-dynamic` capsule in `example-browser-parity-all.rkt`)
  - list (`parity-list` capsule in `example-browser-parity-all.rkt`)
  - simple todo (`parity-todo` capsule in `example-browser-parity-all.rkt`)
- Expected phase-2 parity PASS lines:
  - `PASS independent counters: c1 0->1->0, c2 0->2`
  - `PASS tabs switch content: overview->details->help`
  - `PASS dynamic tabs parity: add faq, remove selected, remove details`
  - `PASS list parity: alpha,beta -> reverse -> +gamma -> -beta`
  - `PASS todo parity: add, edit/cancel, edit/save, toggle, mark-all-done, clear-done`

## Current Widget-to-Element Mapping

Current browser backend element mapping (as implemented today):

| widget/tag | HTML element(s) | Notes |
|---|---|---|
| `window`, `vpanel`, `hpanel`, `button-group`, `button-toolbar`, `card`, `navigation-bar` | `div`/`nav` | `vpanel`/`hpanel` use flex layout styles; grouped controls use inline-flex/flex rows; `card` is sectioned with header/body/footer child nodes; `navigation-bar` uses `role="navigation"`. |
| `collapse` | `div` | Visibility container using `is-open` class and `aria-hidden` state. |
| `accordion` | `div` + `button` + `div` | Section widget composed as accordion root/section/trigger/collapse regions; panel region uses stable class hook `.we-accordion-content`. |
| `dialog`, `modal` | `dialog` + panel `div` | `data-we-widget` distinguishes `dialog` vs `modal`; both expose `role="dialog"` + `aria-modal` + observable `open` state, and panel uses `aria-describedby` when first child is descriptive text. |
| `group` | `fieldset` + `legend` | Group title is rendered as a real `legend` child. |
| `text` | `span` | Plain inline text node wrapper. |
| `spinner` | `div` + `span` | Animated loading indicator with optional status label. |
| `alert` | `div` | Inline status banner with severity classes (`info/success/warn/error`). |
| `badge` | `span` | Compact inline severity marker with class-based variants. |
| `toast` | `div` + optional `button` + optional title node | Non-modal notification anchored to viewport; supports level variants plus optional title and dismissibility. |
| `button` | `button` | Native clickable button. |
| `input` | `input` | Single-line text input; supports Enter callback wiring. |
| `textarea` | `textarea` | Multi-line text input with configurable `rows` and attrs. |
| `checkbox` | `input[type=checkbox]` | Boolean toggle control. |
| `choice`, `radios` | `select` | `radios` currently rendered as select in browser backend. |
| `slider` | `input[type=range]` | Numeric range input. |
| `progress` | `progress` | One-way progress display with variant classes (`info/success/warn/error`). |
| `pagination` | `nav` + `button` | Page navigation with prev/next and current-page classes/ARIA state. |
| `breadcrumb` | `nav` + `button` + `span` | Hierarchical navigation trail with current item marker. |
| `list-group` | `div` + `button` | Selectable list rows with current-item marker classes. |
| `tab-panel` | composite: `div` + `button` + `div` + injected `style` | Tab strip uses tab buttons with ARIA attrs and keyboard handling. |
| `spacer` | `div` | Empty layout spacer. |
| `table` | `table` + `tr` + `th` + `td` | Header row from columns, data rows from row values, optional per-column alignment (`left/center/right`) via 2-tuple column specs `(label align)`. |
| `image` | `img` | Optional `width`/`height`; keeps intrinsic size by default. |
| `dropdown` | `div` + `menu` composite | Single-trigger popup menu using same behavior as `menu`/`menu-item`. |
| `tooltip` | `div` + trigger child + `span` bubble (`header`/`body`/`footer`) | Hover/focus tooltip with `aria-describedby` from trigger to bubble id and optional structured regions. |
| `popover` | `div` + `button` trigger + `div` panel (`header`/`body`/`footer`) | Click-toggle detail panel with `aria-expanded`/`aria-hidden`, Escape close, and optional structured `title`/`footer`. |

### `navigation-bar` API (`expand`)

- Signature shape: `(navigation-bar [orientation] [collapsed?] [expand] child ...)`
- Defaults:
  - `orientation`: `'horizontal`
  - `collapsed?`: `#f`
  - `expand`: `'never` (default has no built-in toggle button)
- Expand modes:
  - `'never`: render items only (no toggle)
  - `'always`: render toggle button (`data-we-widget="navigation-bar-toggle"`) and collapsed-state behavior

Examples:

```racket
;; Default (no toggle button)
(navigation-bar
 (button "home" (lambda () (void)))
 (button "docs" (lambda () (void))))

;; Explicit toggle/collapse behavior
(navigation-bar 'vertical #t 'always
 (button "one" (lambda () (void)))
 (button "two" (lambda () (void))))
```
| `menu-bar` | `nav` (`role="menubar"`, `aria-orientation="horizontal"`) | Top-level menu label row. |
| `menu` | `div` (`role="menu"`, popup `id`) | Popup menu container linked from label via `aria-controls`. |
| `menu-item` | `button type="button"` (`role="menuitem"`) | Action entry with keyboard activation (`Enter`/`Space`). |

## Menu Interaction Contract (Current)

- Menu labels are rendered as focusable buttons with `aria-haspopup="menu"`, `aria-controls`, and `aria-expanded`.
- Clicking a label toggles that menu popup.
- When a popup is open, `Left`/`Right` on labels wraps across top-level labels.
- When focus is on a menu item, `Left`/`Right` switches to adjacent menu label and opens that menu.
- `Up`/`Down` inside a popup is clamped (no wrap), matching current macOS-style behavior.
- `Home`/`End` on labels moves to first/last label.
- `Enter`/`Space` activates menu items.
- Typing a letter while a menu label or menu item has focus performs type-ahead focus to the next matching menu item.
- `Escape` closes the popup and returns focus to the owning label.
- Leaving menu focus (focus moves outside menu container) closes the open popup.

## Width Policy (Current Defaults)

Current width defaults are intentionally split between layout containers and leaf controls.

- Fill-width by default:
  - `window`, `vpanel`, `hpanel`, `group`, tab content containers.
  - `input` (explicit `width: 100%` in current renderer output).
- Content-width by default:
  - `table`, `image`, `button`, `checkbox`, `choice`, `radios`, `slider`, `progress`, `menu-item`.
- Alignment default:
  - leaf/content-width controls are left-aligned via `align-self: flex-start` in column layouts.
- Rationale:
  - containers define structure and usually fill available width;
  - leaf widgets stay intrinsic by default to avoid accidental full-width controls.

## Theme/Class Contract (Current)

Renderer output now exposes two styling hooks:

1. Stable semantic marker: `data-we-widget="<widget-name>"`.
2. Class-based defaults for appearance/layout.

Recommended targeting order:

1. Use `data-we-widget` for component-level targeting that should survive class refactors.
2. Use classes for detailed visual tuning aligned with current default CSS.

Current default classes:

- Core controls:
  - `.we-button`, `.we-input`, `.we-textarea`, `.we-checkbox`, `.we-choice`, `.we-slider`, `.we-progress`, `.we-radios`, `.we-image`
- Table:
  - `.we-table`
  - density variants: `.we-density-normal`, `.we-density-compact`
  - cells: `.we-table-header-cell`, `.we-table-data-cell`
- Tabs:
  - `.we-tab-panel`, `.we-tab-list`, `.we-tab-content`, `.we-tab-btn`
  - tab state variants: `.is-selected`, `.is-disabled`
- Dialog:
  - `.we-dialog`, `.we-dialog-panel`
  - open-state variant: `.is-open`
- Menu:
  - `.we-menu-bar`, `.we-menu`, `.we-menu-label`, `.we-menu-popup`, `.we-menu-item`
  - open-state variant: `.is-open`

Default theme tokens (CSS custom properties):

- Focus/overlay:
  - `--we-focus`, `--we-overlay`, `--we-shadow`
- Surface/background:
  - `--we-bg`, `--we-bg-subtle`, `--we-bg-selected`, `--we-bg-disabled`, `--we-bg-hover`
- Borders/text:
  - `--we-border`, `--we-border-menu`, `--we-border-muted`, `--we-border-soft`, `--we-border-hover`, `--we-border-strong`, `--we-fg`, `--we-fg-muted`
- Progress variants:
  - `--we-progress-success`, `--we-progress-warn`, `--we-progress-error`
- Heading typography:
  - `--we-heading-fg`, `--we-display-heading-fg`, `--we-heading-subtitle-fg`, `--we-lead-fg`
  - `--we-heading-space-compact`, `--we-heading-space-normal`, `--we-heading-space-loose`
- Menu/tab/input extras:
  - `--we-menu-item-hover-bg`, `--we-menu-item-hover-fg`, `--we-tab-active-border`, `--we-input-placeholder`
- Spacing/gaps:
  - `--we-space-xs`, `--we-space-sm`, `--we-space-md`, `--we-space-lg`, `--we-gap`, `--we-gap-tab`

Stable styling contract (current baseline):

| Surface | Stable `data-we-widget` hooks | Stable classes | Primary token hooks |
|---|---|---|---|
| Buttons/inputs/select | `button`, `input`, `textarea`, `choice`, `checkbox` | `.we-button`, `.we-input`, `.we-textarea`, `.we-choice`, `.we-checkbox` | `--we-bg`, `--we-bg-hover`, `--we-border-soft`, `--we-focus`, `--we-fg`, `--we-input-placeholder` |
| Range/progress | `slider`, `progress` | `.we-slider`, `.we-progress`, `.we-progress-info`, `.we-progress-success`, `.we-progress-warn`, `.we-progress-error` | `--we-fg`, `--we-focus`, `--we-progress-success`, `--we-progress-warn`, `--we-progress-error` |
| Alert/badge/spinner | `alert`, `alert-title`, `alert-body`, `alert-link`, `alert-dismiss`, `badge`, `spinner` | `.we-alert`, `.we-alert-*`, `.we-alert-title`, `.we-alert-body`, `.we-alert-link`, `.we-alert-dismiss`, `.we-badge`, `.we-badge-*`, `.we-spinner`, `.we-spinner-icon`, `.we-spinner-label` | `--we-bg-subtle`, `--we-border-soft`, `--we-fg`, `--we-border-strong` |
| Toast/collapse/accordion | `toast`, `collapse`, `accordion`, `accordion-trigger` | `.we-toast`, `.we-toast-*`, `.we-collapse`, `.is-open`, `.we-accordion`, `.we-accordion-trigger`, `.we-accordion-content` | `--we-bg`, `--we-bg-subtle`, `--we-bg-selected`, `--we-bg-hover`, `--we-border-soft`, `--we-focus`, `--we-shadow` |
| Pagination/breadcrumb/list-group | `pagination`, `page-button`, `breadcrumb`, `breadcrumb-item`, `list-group`, `list-group-item` | `.we-pagination`, `.we-page-btn`, `.we-breadcrumb`, `.we-breadcrumb-item`, `.we-list-group`, `.we-list-group-item`, `.is-current` | `--we-bg`, `--we-bg-selected`, `--we-bg-hover`, `--we-border-soft`, `--we-fg`, `--we-fg-muted`, `--we-focus` |
| Headings/lead | `heading`, `display-heading`, `heading-with-subtitle`, `display-heading-with-subtitle`, `heading-title`, `heading-subtitle`, `lead` | `.we-heading`, `.we-display-heading`, `.we-heading-with-subtitle`, `.we-display-heading-with-subtitle`, `*-align-left/center/right`, `*-space-compact/normal/loose`, `.we-heading-subtitle`, `.we-lead` | `--we-heading-fg`, `--we-display-heading-fg`, `--we-heading-subtitle-fg`, `--we-lead-fg`, `--we-heading-space-compact/normal/loose` |
| Card/navigation | `card`, `card-header`, `card-subtitle`, `card-media`, `card-body`, `card-actions`, `card-footer`, `navigation-bar` | `.we-card`, `.we-card-header`, `.we-card-subtitle`, `.we-card-media`, `.we-card-body`, `.we-card-actions`, `.we-card-footer`, `.we-navigation-bar` | `--we-bg`, `--we-bg-subtle`, `--we-border-soft`, `--we-border-menu`, `--we-fg` |
| Table | `table`, `table-row`, `table-header-cell`, `table-data-cell` | `.we-table`, `.we-table-header-cell`, `.we-table-data-cell`, `.we-density-normal`, `.we-density-compact`, `.we-align-left`, `.we-align-center`, `.we-align-right` | `--we-border-muted`, `--we-border-soft`, `--we-fg` |
| Tabs | `tab-panel`, `tab-list`, `tab-button`, `tab-content` | `.we-tab-panel`, `.we-tab-list`, `.we-tab-btn`, `.we-tab-content`, `.is-selected`, `.is-disabled` | `--we-bg`, `--we-bg-selected`, `--we-bg-disabled`, `--we-border-muted`, `--we-border-strong`, `--we-focus`, `--we-tab-active-border` |
| Dialog | `dialog`, `dialog-panel`, `dialog-header`, `dialog-title`, `dialog-close`, `dialog-body`, `dialog-description`, `dialog-footer`, `modal`, `modal-panel`, `modal-header`, `modal-title`, `modal-close`, `modal-body`, `modal-description`, `modal-footer` | `.we-dialog`, `.we-dialog-panel`, `.we-dialog-*`, `.we-modal`, `.we-modal-panel`, `.we-modal-*`, `.is-open` | `--we-overlay`, `--we-bg`, `--we-border`, `--we-shadow`, `--we-focus` |
| Menu | `menu-bar`, `menu`, `menu-label`, `menu-popup`, `menu-item` | `.we-menu-bar`, `.we-menu`, `.we-menu-label`, `.we-menu-popup`, `.we-menu-item`, `.is-open` | `--we-bg-subtle`, `--we-bg`, `--we-bg-hover`, `--we-border-menu`, `--we-border`, `--we-border-soft`, `--we-focus`, `--we-fg`, `--we-menu-item-hover-bg`, `--we-menu-item-hover-fg` |

Structured options for richer composition:

- `alert-rich body title link-text link-href [level] [options]`
  - options:
    - `dismiss-action` -> procedure used by `alert-dismiss`.
    - `dismiss-label` -> aria-label for dismiss affordance.
- `card [title] [footer] [variants] [options] child ...`
  - options:
    - `subtitle` -> `card-subtitle`.
    - `media` -> `card-media`.
    - `actions` -> list rendered in `card-actions`.
- `dialog open on-close [size] [options] child ...`
- `modal open on-close [size] [options] child ...`
  - options:
    - `title` -> `*-title` inside `*-header`.
    - `description` -> `*-description` inside `*-body` (also used for `aria-describedby`).
    - `footer` -> `*-footer` content (text or view).
    - `show-close?` -> include `*-close` button (default `#f`).
    - `close-label` -> aria-label for close button.

Heading API mapping:

- `heading level content [align] [spacing]`
  - element: `h1..h6` (normalized from `level`)
  - widget/class: `data-we-widget="heading"`, `.we-heading`, `.we-heading-N`, `.we-heading-align-*`, `.we-heading-space-*`
- `display-heading level content [align] [spacing]`
  - element: `h1..h6`
  - widget/class: `data-we-widget="display-heading"`, `.we-display-heading`, `.we-display-heading-N`, `.we-display-heading-align-*`, `.we-display-heading-space-*`
- `heading-with-subtitle level title subtitle [align] [spacing]`
  - element: `h1..h6` root with `span` title + `small` subtitle
  - widget/class: `data-we-widget="heading-with-subtitle"`, plus `heading-title`, `heading-subtitle`
- `display-heading-with-subtitle level title subtitle [align] [spacing]`
  - element: `h1..h6` root with `span` title + `small` subtitle
  - widget/class: `data-we-widget="display-heading-with-subtitle"`, plus `heading-title`, `heading-subtitle`

Rationale:

- keeps semantic heading tags stable for accessibility/SEO while exposing display-scale styling as classes/tokens.
- align/spacing variants avoid ad hoc wrapper CSS in app code and keep typography choices in component API.
- subtitle variants reuse the same child widget hooks (`heading-title`, `heading-subtitle`) for consistent theming/tests.

Contract enforcement status:

- Automated smoke contract page: `smoke/test-browser-style-hook-contract.html`.
- Automated parity contract page: `smoke/test-browser-parity-style-hook-contract.html`.
- Included in `smoke/test-browser-contract-dashboard.html` and headless contract CI path.
- Theme token contract page: `smoke/test-browser-theme-token-contract.html`.
  - verifies token definition presence and runtime token override effects across menu/card/pagination/breadcrumb/headings.

`data-we-widget` examples:

- containers: `window`, `vpanel`, `hpanel`, `group`, `list-view`, `if-view`, `cond-view`, `case-view`, `observable-view`, `collapse`, `spacer`
- controls: `text`, `button`, `input`, `textarea`, `checkbox`, `choice`, `slider`, `progress`, `radios`, `image`
  - complex widgets:
  - alert/badge/spinner: `alert`, `alert-title`, `alert-body`, `alert-link`, `alert-dismiss`, `badge`, `spinner`
  - toast/collapse: `toast`, `collapse`
  - pagination/breadcrumb/list-group: `pagination`, `page-button`, `breadcrumb`, `breadcrumb-item`, `breadcrumb-sep`, `list-group`, `list-group-item`
  - card/navigation: `card`, `card-header`, `card-subtitle`, `card-media`, `card-body`, `card-actions`, `card-footer`, `navigation-bar`
  - menu: `menu-bar`, `menu`, `menu-label`, `menu-popup`, `menu-item`
  - dialog/modal: `dialog`, `dialog-panel`, `dialog-header`, `dialog-title`, `dialog-close`, `dialog-body`, `dialog-description`, `dialog-footer`, `modal`, `modal-panel`, `modal-header`, `modal-title`, `modal-close`, `modal-body`, `modal-description`, `modal-footer`
  - table: `table`, `table-row`, `table-header-cell`, `table-data-cell`
  - tabs: `tab-panel`, `tab-list`, `tab-content`, `tab-button`
  - accordion: `accordion`, `accordion-section`, `accordion-trigger`, `collapse` (with `.we-accordion-content` panel class)

## Contract Taxonomy

Contract pages in `lib/web-easy/smoke/` are grouped by behavioral domain:

1. Semantics and ARIA:
   - `test-browser-a11y-contract.html`
   - `test-browser-dialog-contract.html`
   - `test-browser-tab-aria-linkage-contract.html`
   - `test-browser-tab-aria-dynamic-contract.html`
   - `test-browser-menu-aria-state-contract.html`
   - `test-browser-dialog-no-desc-contract.html`
   - parity mirrors with `test-browser-parity-...`
2. Keyboard and focus:
   - `test-browser-keyboard-contract.html`
   - `test-browser-focus-order.html`
   - `test-browser-menu-roving-focus-contract.html`
   - `test-browser-menu-typeahead-contract.html`
   - `test-browser-menu-typeahead-timeout-contract.html`
   - `test-browser-dropdown-focus-return-contract.html`
   - deep pages: `test-browser-dropdown-keyboard-deep.html`, `test-browser-choice-keyboard-deep.html`, `test-browser-scrollspy-keyboard-deep.html`
3. Style and DOM-shape invariants:
   - `test-browser-style-hook-contract.html`
   - `test-browser-parity-style-hook-contract.html`
   - dashboard guard checks for forbidden leaked tokens and inline widget styles
4. Widget API contracts:
   - list/toolbar/divider/link/navigation/card/button/icon/theme-token/progress/dropdown/choice pages under `test-browser-*-contract.html`
   - explicit decode/alignment contracts:
     - `test-browser-choice-decode-contract.html`
     - `test-browser-table-align-contract.html`
     - `test-browser-headings-style-contract.html`
5. Parity contracts:
   - all core contract families have parity mirrors under `test-browser-parity-...-contract.html`

Execution model:

- Core and parity contracts are split to reduce timeout coupling:
  - `test-browser-contract-dashboard-core.html`
  - `test-browser-contract-dashboard-parity.html`
- `check-contract-headless.mjs` runs both dashboards and aggregates pass/fail + timing.
- Release gate policy:
  - contract dashboards are normative pass/fail gates.
  - visual compare scripts (`compare-buttons.cjs`, `compare-navbars.cjs`) are advisory drift detectors for theme tuning, not hard blockers by themselves.

## Baseline Changelog

- 2026-03-02 baseline update:
  - full dashboard baseline: `38` automated smoke tests.
  - parity dashboard baseline: `19` automated parity tests.
  - recent additions to full dashboard baseline:
    - `test-browser-group.html` (fieldset + legend semantics)
    - `test-browser-menu-keys.html` (menu popup + menu-item focus + Enter/Space activation)
    - `test-browser-menu-full.html` (multi-menu/multi-item activation and status updates)
    - `test-browser-a11y-contract.html` (cross-widget semantic/ARIA contract checks for menu/tab/group/table)
    - `test-browser-keyboard-contract.html` (keyboard-only contract checks for menu/tab navigation and activation)
    - `test-browser-focus-order.html` (focus-order contract checks for menu-label and tab-header ordering)
    - `test-browser-disabled-contract.html` (disabled-tab contract checks for non-activation and keyboard skip behavior)
    - `test-browser-width.html` (width-policy runtime assertions)
    - `test-browser-parity-incident.html` (real-world incident triage parity flow)
    - `test-browser-parity-release.html` (real-world release checklist parity flow)
    - `test-browser-parity-menu-full.html` (richer parity menu interaction coverage)
    - `test-browser-parity-a11y-contract.html` (parity-side semantic/ARIA contract checks for menu/tab/table)
    - `test-browser-parity-keyboard-contract.html` (parity keyboard-only contract checks for menu/tab flows)
    - `test-browser-parity-focus-order.html` (parity focus-order contract checks for menu/tab ordering)
    - `test-browser-parity-disabled-contract.html` (parity disabled-tab contract checks for non-activation and keyboard skip behavior)

## Open Design Questions

1. Exact API shape for `window` in web context:
   - Should it require an explicit root element or default to document body?
2. How much of desktop style vocabulary should be accepted vs rejected?
3. Should we include a lightweight virtual-node IR, or patch DOM directly from view objects?
4. How should modal dialog semantics map to focus trapping and accessibility defaults?
5. Tab visual polish roadmap:
   - current tabs intentionally use button-like styling for behavior-first parity checks.
   - deferred UI pass should make tabs more tab-like (selected tab merged with panel, inactive tabs recessed, clearer tab strip hierarchy).
6. Group/legend follow-up:
   - `group` now renders as `fieldset` with a `legend` child.
   - follow up: decide whether additional class-based styling defaults are needed for legend consistency.

## Immediate Next Steps

1. Decide long-term dashboard policy:
   - keep parity pages in full dashboard permanently, or
   - keep full+parity split but run parity-only in selected workflows.
2. Define M5 phase-2 parity targets:
   - completed for counters/tabs/list/todo.
   - next: choose phase-3 parity targets.
3. Keep counts and validation stamps up to date:
   - update dashboard test counts and last-validated lines whenever smoke/parity inventory changes.
4. Revisit DOM element choices per widget:
   - review whether each widget currently maps to the best semantic HTML element.
   - document intended element mapping policy and any accessibility tradeoffs.
5. Revisit CSS strategy (inline vs stylesheet/classes):
   - continue consolidating defaults into shared class-based styles where possible.
   - keep inline style usage only for dynamic geometry values that cannot be represented via classes.

## Component Backlog (Remembered List)

Requested detour backlog for future component work:

1. `heading-group` component.
2. Heading size presets (`sm/md/lg`) via tokens.
3. `text-muted` view.
4. `text-emphasis` view.
5. Heading anchor links (auto id + optional glyph).
6. `code-inline` text component.
7. `code-block` component with monospace defaults.
8. `kbd` component for keyboard hints.
9. `description-list` component (`<dl>` mapping).
10. `stat` component (label + value + help).
11. `empty-state` component (title/body/actions).
12. `skeleton-list` helper component (loading placeholders).
13. `avatar` component (image/initials/icon fallback).
14. `chip`/`pill` component with removable option.
15. `tag-input` primitive (chips + input).
16. `form-row` layout primitive (label/control/help/error).
17. `help-text` + `field-error` components.
18. `switch` component (separate from checkbox).
19. `textarea` component with auto-grow option.
20. `select-labeled` component with explicit label/value rows (not raw tuples).

## Phase 2 Component Roadmap

Priority order:

1. `collapse` / `accordion` (Implemented in current baseline)
   - Purpose: enable common settings/help disclosure patterns with keyboard-friendly section toggles.
   - Why first: high utility, low conceptual overhead, composes directly with existing panel/layout primitives.
   - Validation focus: `aria-expanded`/`aria-controls`, Enter/Space toggle, deterministic open/close state.

2. `alert` (inline status banner)
   - Purpose: provide standardized non-modal feedback (info/success/warn/error).
   - Why now: current examples hand-roll status text; a first-class alert simplifies parity examples and docs.
   - Validation focus: severity variants, optional dismiss action, focus behavior after dismiss, role semantics.

3. `pagination`
   - Purpose: pair naturally with `table` and `list-view` for paged datasets.
   - Why now: practical for real-world examples and data-heavy parity pages.
   - Validation focus: prev/next/first/last semantics, disabled-state behavior, keyboard navigation contracts.

4. `toast` (transient non-modal notifications)
   - Purpose: queue and show transient notifications without dialog interruption.
   - Why now: common app requirement (save/deploy/completion feedback) not well served by modal flows.
   - Validation focus: queue order, auto-dismiss timing, manual dismiss, hover/focus pause policy.

5. `tooltip` / `popover` (Implemented in current baseline)
   - Purpose: concise contextual help and inline details for compact controls.
   - Validation focus: tooltip `aria-describedby`, popover open/close aria state, Escape close behavior.

Notes:

- This roadmap is intentionally web-native, not desktop widget parity for parity’s sake.
- Desktop-specific surfaces (for example snip/canvas/native window semantics) remain deferred until explicit web capability design is finalized.

## Element/CSS Cleanup Status (2026-03-04)

Current status:

1. `renderer.rkt` no longer emits inline visual style attributes for core controls, menu, dialog, tab-panel, or table density/cell spacing.
2. Injected renderer stylesheet is now structural-only (layout/positioning/show-hide mechanics), not visual theme styling.
3. Stable CSS/test targeting is now available through `data-we-widget` markers across container, control, and sub-widget nodes (for example menu popup/label, dialog panel, table rows/cells, tab list/content/buttons).

Rationale:

- Keeps rendering output fully theme-driven via external stylesheet overrides.
- Avoids baseline visual defaults interfering with user-provided themes (for example Bootstrap-inspired styles).
- Reduces renderer-side style duplication and attr churn during updates.
- Improves test/tool resilience by targeting widget identity (`data-we-widget`) instead of brittle DOM shape assumptions.

Remaining follow-up:

1. Keep the shared-window stylesheet model and monitor bundle size if default CSS grows.
2. Expand and document the public theme/class contract (which classes are stable API vs internal).
3. Completed: close-button icon glyph is CSS-token-driven via `--we-close-glyph` and contract-tested in core/parity close-button pages.
4. Deep keyboard contracts for dropdown/scrollspy are now part of contract dashboard gating (core + parity).

Migration note:
- Renderer-injected CSS is structural-only.
- Shared widget mechanics can live in a core external stylesheet (`smoke/web-easy-core.css`), loaded before theme stylesheets.
- Visual defaults must come from external theme stylesheets (for example `theme-external-*.css` / `theme-solar-2.css`).
- Page-specific showcase polish remains in showcase stylesheets (for example `theme-showcase-*.css`).
- Legacy visual CSS constants are retained in source as migration reference and are not injected.

Core vs Theme rule (strict):
- Core layer (`web-easy-core.css`): structure/behavior only (`display`, layout direction, open/close mechanics, positioning anchors).
- Theme layer (`theme-external-*.css`, `theme-solar-2.css`): visuals only (colors, borders, typography, shadows, spacing polish).
- Showcase layer (`theme-showcase-*.css`): page-specific layout/polish only.
- Load order must be: core -> theme -> showcase (if present).

Core utility note:
- `we-flow` is now part of the structural core layer (`.we-flow > * + *`) for uniform sibling spacing.
- Use `with-class "we-flow"` in pages/examples instead of page-specific adjacent-sibling spacing selectors when the intent is generic vertical rhythm.
- `we-menu-bar` shared row mechanics (`display/flex-wrap/align-items`) are now in core; theme styles keep only menu-bar visual density and skin.
- `we-menu-popup` shared placement sizing (`top/min-width/gap`) is now core-owned through `--we-menu-popup-*` tokens; themes set token values and keep popup skin.
- Solar2 progress utility selectors (`showcase-progress-*`, striped/animated progress fills) were migrated from showcase CSS into `theme-solar-2.css`; showcase CSS retains only section/page scaffolding.

## Theme Contract Test Architecture

Theme contracts use a dedicated runtime testing layer to keep token plumbing checks stable:

1. Contract pages inject:
   - `web-easy-core.css`
   - `theme-contract-vars.css`
2. `theme-contract-vars.css` is intentionally deterministic and token-forwarding; it is not a user-facing visual theme.
3. User-facing visual themes (`theme-external-*.css`, `theme-solar-2.css`) are validated by separate theme/external/showcase contracts and visual diff lanes.
4. Shared test helper (`smoke/theme-contract-helper.js`) centralizes iframe theme injection and basic utilities to reduce drift across contract pages.
5. Solar section parity can be run and summarized per section with:
   - `smoke/check-solar-section-parity.sh`
   - this captures section screenshots/metrics and prints per-section RMSE via `check-solar-polish-summary.mjs`.
6. Solar2 list-group clipping/alignment now has a browser contract page (`smoke/test-browser-solar2-list-group-contract.html`) included in the core contract dashboard lane.
7. Menu structural extraction is guarded by `smoke/test-browser-menu-core-structure-contract.html`.
8. Solar section parity script supports gate-only mode for quick CI/local thresholds:
   - `SOLAR_SECTIONS_GATE_ONLY=1 smoke/check-solar-section-parity.sh`

Rationale:

- Prevents theme token contracts from depending on incidental visual-theme details.
- Keeps token behavior tests reproducible while preserving independent visual-theme evolution.
- Reduces duplication and maintenance risk in contract pages.

## Behavior Contract Update (2026-03-06)

Recent contract expansion focuses on optional-argument behavior and close-reason semantics:

1. `modal` now has dedicated core/parity capsules and contracts (no longer dialog-backed smoke indirection).
2. `modal` close-reason contracts assert `cancel` / `confirm` / `escape` status transitions.
3. `carousel` contracts now cover:
   - `wrap? #f` boundary behavior
   - keyboard (`ArrowLeft`, `ArrowRight`, `Home`, `End`)
   - `autoplay? #t` progression
   - no-wrap autoplay stop-at-end.
4. `navigation-bar` contracts now cover:
   - default rendering has no collapse toggle (`expand` defaults to `'never`)
   - orientation class toggling (`is-vertical`)
   - collapsed state class (`is-collapsed`)
   - toggle `aria-expanded` transitions when `expand` is explicitly enabled (for example `'always`).
5. `toast` contracts now cover:
   - timer-based auto-hide (`duration-ms`)
   - hover pause/resume (`pause-on-hover?`).

Rationale:

- These contracts target behavior introduced by optional parameters where regressions are easy to miss in static snapshots.
- Core and parity dashboards now execute matching behavior checks, so surface differences are caught early.
- Modal close-reason checks lock down observable state transitions used by real-world flows and examples.
