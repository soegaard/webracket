# Core Candidate Audit (External Themes)

Date: 2026-03-11  
Scope: remaining duplicated selectors across:
- `theme-external-light.css`
- `theme-external-dark.css`
- `theme-external-solar.css`

## Classification Rules

- `structural`: layout/behavior mechanics (`display`, `position`, open/close geometry, flex direction, width behavior).
- `visual`: colors, borders, shadows, typography, visual spacing polish.
- `mixed`: both structural and visual in same selector block.

## Audit Table

| Selector | Seen In | Classification | Why | Recommendation |
| --- | --- | --- | --- | --- |
| `.we-menu-bar` | light/dark/solar/solar2 | mixed | all themes share row mechanics plus per-theme skin values | first safe slice completed: shared mechanics moved to core; keep skin in themes |
| `.we-menu-popup` | light/dark/solar/solar2 | mixed | shared anchor/size mechanics + theme skin | first popup split done: core owns `top/min-width/gap` via CSS vars; themes keep visual skin |
| `.we-menu-label` | light/dark/solar | mixed | interactive element sizing + visual style | keep in themes |
| `.we-menu-label:hover` | light/dark/solar | visual | hover color/border skin | keep in themes |
| `.we-menu-label[aria-expanded='true']` | light/dark/solar | visual | open-state visual skin only (stacking now in core) | keep in themes |
| `.we-menu-item` | light/dark/solar | visual | structural part already moved to core; remaining rules are skin | keep in themes |
| `.we-menu-item:hover` | light/dark/solar | visual | hover color skin | keep in themes |
| `.we-menu-item:focus-visible` | light/dark/solar | mixed | focus color/outline + z-index clipping fix | keep in themes; possible core z-index helper later |
| `.we-input` | light/dark/solar | visual | control colors/borders/typography | keep in themes |
| `.we-input:focus-visible` | light/dark/solar | visual | focus ring/tint styling | keep in themes |
| `.we-choice` | light/dark/solar | visual | select control skin | keep in themes |
| `.we-choice:focus-visible` | light/dark/solar | visual | focus skin | keep in themes |
| `.we-button` | light/dark/solar | visual | button skin | keep in themes |
| `.we-button:*` variants | light/dark/solar | visual | hover/active/focus/variant skin | keep in themes |
| `.we-page-btn:focus-visible` | light/dark/solar | visual | focus skin | keep in themes |
| `.we-dialog-panel` | light/dark/solar | visual | dialog surface skin by theme | keep in themes |
| `.we-dialog-panel:focus-visible` | light/dark (solar separate style) | visual | focus skin | keep in themes |
| `.we-tab-list` | light/dark (+solar has its own system) | mixed | structure (`display/align`) + border/padding visual | structure already partially in core; keep remainder in themes |
| `.we-tab-btn` | light/dark/solar | mixed | tab sizing geometry + visual skin | keep in themes |
| `.we-tab-btn + .we-tab-btn` | light/dark/solar | mixed | seam join behavior (visual geometry) | keep in themes |
| `.we-tab-btn.is-selected` | light/dark (+solar uses aria-selected) | visual | active tab skin | keep in themes |
| `.we-tab-content` | light/dark/solar | visual | panel skin | keep in themes |
| `.we-alert` / success/warn/error | light/dark/solar | visual | semantic color skins | keep in themes |
| `.we-toast` / success/warn/error/close | light/dark/solar | visual | toast skins | keep in themes |
| `.we-accordion` | light/dark/solar | mixed | layout stack + spacing + some skin context | `display/flex-direction` extracted to core; keep theme `gap` and skin |
| `.we-accordion-section` | light/dark/solar | visual | panel borders/background | keep in themes |
| `.we-accordion-trigger` | light/dark/solar | mixed | structural button layout + visual styling | structural subset extracted to core; keep visual skin in themes |
| `.we-accordion-trigger::after` | light/dark/solar | mixed | indicator behavior + color | transition-property/timing + display/margin extracted to core; keep color/content/duration in themes |
| `.we-accordion-trigger.is-open` / `::after` | light/dark/solar | visual | open-state visual styling | keep in themes |

## Completed Extractions

1. `.we-accordion` container mechanics (`display/flex-direction`) moved to core.
2. `.we-accordion-trigger` mechanics (`width/display/align-items/justify-content/text-align`) moved to core.
3. `.we-accordion-trigger::after` shared mechanics (`display`, `margin-left`, `transition-property`, `transition-timing-function`) moved to core.
4. `.we-flow > * + *` vertical rhythm helper moved to core; Solar2 showcase now uses `we-flow` instead of page-only sibling-spacing selectors.
5. `.we-menu-bar` shared row mechanics (`display/flex-wrap/align-items`) moved to core; theme files now own only visual density/skin values for menu bars.
6. `.we-menu-popup` shared anchor sizing moved to core via tokens:
   - core: `top`, `min-width`, `gap` with `--we-menu-popup-*` vars
   - themes: per-theme token values + popup visual skin only

## Concrete Menu Split Plan

Goal: split structural menu mechanics from visual skin without changing behavior.

1. Extracted now (safe):
   - `.we-menu-bar` row mechanics in core:
     - `display`, `flex-wrap`, `align-items`
2. Keep in themes (visual density/skin):
   - `.we-menu-bar` `gap`, `padding`, `border`, `border-radius`, `background`
3. Next candidate (pending decision):
   - `.we-menu-popup` left/right anchoring and optional placement helpers
   - blocked on API choice for popup placement semantics (for example right-aligned menus).
4. Guardrail:
   - any future extraction must preserve dashboard contracts and Solar parity RMSE thresholds.

## Safe Next Extraction Candidates

These are the low-risk remaining candidates that are plausibly structural:

1. Optional utility for menu item clipping fix  
   - Could add core helper class for raised focus layering if we want to remove repeated `position:relative; z-index:1` from theme focus blocks.

2. Potential `we-menu-bar` partial split  
   - Only if we decide `display/flex-wrap/align-items` are truly global behavior and not theme density decisions.

3. Potential `we-menu-popup` partial split  
   - Only after deciding whether `top/gap/min-width` belong to structure or theme visual density.

## What Not To Extract Now

- Anything driven by color or border semantics (`--we-*` token outputs).
- Tab seam/join details (`margin-left:-1px`, active border-bottom tricks) because they are visual-style dependent.
- Menu popup offsets (`top`, `gap`, `min-width`) until we decide whether they are part of visual density or global behavior.
