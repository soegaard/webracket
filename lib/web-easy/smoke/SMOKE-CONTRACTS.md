# web-easy Smoke Contracts

Reference for contract pages, expected interaction semantics, and PASS-line expectations.

## Contract Dashboards

- `test-browser-contract-dashboard.html`
- `test-browser-dashboard.html` (includes contract pages)
- `test-browser-parity-dashboard.html` (includes parity contract pages)

## Menu Contract Semantics

Top-level menu behavior:

- only one top-level menu open at a time
- `ArrowLeft`/`ArrowRight` on menu labels moves focus between labels and wraps
- `ArrowLeft`/`ArrowRight` on menu items switches to sibling top-level menu and focuses/open it
- `Home`/`End` on menu labels jumps to first/last top-level menu
- hover while a menu is open switches open menu
- focus leaving menu container closes current menu

Popup item behavior:

- `ArrowUp`/`ArrowDown` moves focus within popup
- no wrap at popup boundaries
- `Escape` closes popup and returns focus to owning label
- type-ahead focuses matching item

## Dialog Contract Semantics

- dialog uses proper role/aria wiring
- focus trap stays within dialog while open
- `Escape` closes dialog and restores focus to opener
- cancel/confirm actions close with expected reason

## Popover Contract Semantics

- trigger toggles popover panel open/closed via aria state
- outside click (backdrop click) closes popover
- `Escape` closes popover
- focus returns to trigger when popover closes

## Dropdown Contract Semantics

- trigger/menu label toggles popup via `aria-expanded`
- `Enter` opens dropdown popup
- `Escape` closes dropdown popup
- selecting an item triggers action and closes popup

## Navigation Bar Contract Semantics

- navigation bar exposes navigation semantics (`role="navigation"`)
- button actions update selected state text deterministically

## Button Group / Toolbar Contract Semantics

- button-group exposes grouped action controls with stable class hooks
- button-toolbar contains grouped button-group clusters with predictable structure

## Card Contract Semantics

- card renders header/body/footer hooks consistently
- header/footer text updates reactively when backing observables change

## List Group Contract Semantics

- list-group renders item hooks for each entry
- selecting an item updates current/active class state

## Pagination Contract Semantics

- pagination exposes page-button hooks and stable current-page class state
- next/prev or page selection transitions the current-page marker predictably

## Alert / Badge Contract Semantics

- alert and badge expose stable severity class hooks
- severity updates reflect class and message changes deterministically

## Spinner Contract Semantics

- spinner exposes stable widget and label hooks
- label updates reflect current activity text deterministically

## Toast Contract Semantics

- toast open/close state is reflected by class and aria-hidden hooks
- severity and dismissible state transitions are reflected in rendered controls

## Collapse Contract Semantics

- collapse open/closed state is reflected by class and aria-hidden hooks
- toggle action transitions the collapse state deterministically

## Breadcrumb Contract Semantics

- breadcrumb root exposes navigation semantics (`role="navigation"`)
- exactly one breadcrumb item is marked current (`aria-current="page"`)
- clicking a non-current item switches the current marker deterministically

## Accordion Contract Semantics

- accordion trigger `aria-expanded` reflects selected/open section
- roving keyboard selection (`ArrowUp`/`ArrowDown`/`Home`/`End`) updates open section
- open/closed section state is reflected by collapse class hooks

## Offcanvas Contract Semantics

- offcanvas open/closed state is reflected by root class and `aria-hidden`
- panel side (`start`/`end`) is reflected by stable side class hooks
- backdrop/close-button actions invoke close behavior deterministically

## Close Button Contract Semantics

- close-button exposes a stable semantic hook and class (`data-we-widget`, `we-close-button`)
- close-button has role/button semantics and stable `aria-label` support

## Placeholder Contract Semantics

- placeholder exposes stable class hooks for shape variants (`text`/`rect`/`circle`)
- optional width value is reflected via a stable attribute hook

## Carousel Contract Semantics

- carousel exposes stable controls/indicator hooks
- selecting prev/next/indicator transitions current indicator class deterministically

## Scrollspy Contract Semantics

- scrollspy exposes stable nav/item hooks
- scrolling section content updates the current item class deterministically

## Dropdown Contract Semantics

- dropdown trigger exposes menu semantics (`aria-haspopup`, `aria-expanded`)
- dropdown trigger exposes a stable indicator glyph hook (`.we-dropdown .we-menu-label::after`)
- `ArrowDown` opens popup and moves focus into menu items
- type-ahead on menu items moves focus to matching items
- activation closes popup and updates selected-id text

## Tabs Contract Semantics

- disabled tabs are not activatable and are skipped by keyboard focus movement
- tab selection state and aria linkage stay synchronized (`aria-controls`, `aria-labelledby`, `aria-selected`)
- dynamic tab add/remove preserves valid selected/focused tab behavior

## A11y + Focus Order Contract Semantics

- expected roles and keyboard affordances exist for menu/tab/group/table related widgets
- focus traversal order remains stable for menu labels and tab headers

## Expected PASS Prefixes (Core)

- `PASS a11y contract:`
- `PASS keyboard contract:`
- `PASS focus-order:`
- `PASS disabled contract:`
- `PASS dialog contract:`
- `PASS menu single-open contract:`
- `PASS menu roving-focus contract:`
- `PASS menu close-reason contract:`
- `PASS dialog close-reason contract:`
- `PASS tab close-style contract:`
- `PASS menu type-ahead contract:`
- `PASS tab aria-linkage contract:`
- `PASS menu aria state contract:`
- `PASS progress contract:`
- `PASS style hook contract:`
- `PASS popover contract:`
- `PASS dropdown contract:`
- `PASS navigation-bar contract:`
- `PASS button-group contract:`
- `PASS button-toolbar contract:`
- `PASS card contract:`
- `PASS list-group contract:`
- `PASS pagination contract:`
- `PASS alert contract:`
- `PASS badge contract:`
- `PASS spinner contract:`
- `PASS toast contract:`
- `PASS collapse contract:`
- `PASS breadcrumb contract:`
- `PASS accordion contract:`
- `PASS offcanvas contract:`
- `PASS close-button contract:`
- `PASS placeholder contract:`
- `PASS carousel contract:`
- `PASS scrollspy contract:`

## Expected PASS Prefixes (Parity)

- `PASS parity a11y contract:`
- `PASS parity keyboard contract:`
- `PASS parity focus-order:`
- `PASS parity disabled contract:`
- `PASS parity dialog contract:`
- `PASS parity menu single-open contract:`
- `PASS parity menu roving-focus contract:`
- `PASS parity menu close-reason contract:`
- `PASS parity dialog close-reason contract:`
- `PASS parity tab close-style contract:`
- `PASS parity menu type-ahead contract:`
- `PASS parity tab aria-linkage contract:`
- `PASS parity menu aria state contract:`
- `PASS parity progress contract:`
- `PASS parity style hook contract:`
- `PASS parity popover contract:`
- `PASS parity dropdown contract:`
- `PASS parity navigation-bar contract:`
- `PASS parity button-group contract:`
- `PASS parity button-toolbar contract:`
- `PASS parity card contract:`
- `PASS parity list-group contract:`
- `PASS parity pagination contract:`
- `PASS parity alert contract:`
- `PASS parity badge contract:`
- `PASS parity spinner contract:`
- `PASS parity toast contract:`
- `PASS parity collapse contract:`
- `PASS parity breadcrumb contract:`
- `PASS parity accordion contract:`
- `PASS parity offcanvas contract:`
- `PASS parity close-button contract:`
- `PASS parity placeholder contract:`
- `PASS parity carousel contract:`
- `PASS parity scrollspy contract:`

## Guard Self-Test Expectation

For guard self-test command:

- command output includes `FAIL`
- includes text indicating forbidden token leakage was correctly detected
- this is expected success behavior for the guard test command itself
