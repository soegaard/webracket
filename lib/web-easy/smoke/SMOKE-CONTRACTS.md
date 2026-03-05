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

## Guard Self-Test Expectation

For guard self-test command:

- command output includes `FAIL`
- includes text indicating forbidden token leakage was correctly detected
- this is expected success behavior for the guard test command itself
