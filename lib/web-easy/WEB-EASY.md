# WEB-EASY

This document captures internal design decisions for `lib/web-easy/`.

## API Layers

The intended core-vs-library split is described in [API-LAYERS.md](API-LAYERS.md).

## Recent Changes

- Began making the core-vs-library split explicit:
  - uppercase HTML-like constructors are the primitive/core layer,
  - lowercase convenience constructors are the higher-level component layer.
- Extended the main text-bearing primitive family in place so they preserve historical text content while also accepting child views:
  - `Button`, `A`, `Label`, `H1` .. `H6`
  - `P`, `Span`, `Strong`, `Em`, `Code`, `Pre`, `Small`
  - `B`, `I`, `U`, `S`, `Mark`, `Sub`, `Sup`, `Kbd`, `Samp`, `Var`
  - `Q`, `Cite`, `Dfn`, `Abbr`, `Time`, `Data`, `Del`, `Ins`
- Extended primitive `Option` and `Legend` so they accept either a single text-like value or child views.
- Added internal `define/element` support for broad primitive HTML constructor generation.
- Expanded uppercase primitive element coverage (leaf + with-children families) with validated keyword attrs.
- Added generic primitive DOM event keywords for bubbling mouse and pointer events
  (for example `#:on-mouseup`, `#:on-pointerdown`, `#:on-click`), with raw browser
  event payloads passed to callbacks.
- Expanded primitive DOM event keywords with keyboard, focus, and form/input
  families such as `#:on-keydown`, `#:on-focus`, `#:on-input`, and `#:on-submit`.
- Expanded primitive DOM event keywords further with wheel/scroll, drag/drop,
  and touch families such as `#:on-wheel`, `#:on-dragover`, and `#:on-touchstart`.
- Expanded primitive DOM event keywords again with media/load/error and
  animation/transition families such as `#:on-load`, `#:on-error`,
  `#:on-animationstart`, and `#:on-transitionend`.
- Added browser-only event helper wrappers on top of the raw DOM event FFI,
  for example `prevent-default!`, `mouse-event-offset-x`, and
  `keyboard-event-key`.
- Added `Base` constraints:
  - constructor requires at least one of `#:href` or `#:target`,
  - conservative runtime ordering check for direct `window` children.
- Consolidated HTML primitive tests with helper-based rejection/assertion patterns.
- Removed obsolete renderer branches and renderer-only helpers for `tab-panel`, `accordion`, and `card` after their view-level primitive-composition migrations.

## `define/element` (internal)

`define/element` is implemented in:

- `lib/web-easy/define-element.rkt`

and included by:

- `lib/web-easy/main.rkt`
- `lib/web-easy/main-browser.rkt`

with:

```racket
(include/reader "define-element.rkt" read-syntax/skip-first-line)
```

### Constructor shape

Current shape:

```racket
(define/element Name Base fixed-positional ...)
```

Example:

```racket
(define/element H1 html-element 'h1)
```

### Attribute validation

For `html-element` wrappers:

- attribute keywords are validated against
  - global attrs (`"*"` row),
  - element-specific attrs,
  - wildcard prefixes `data-*` and `aria-*`.
- unknown attrs are rejected early.

Validation is based on generated data:

- `lib/web-easy/spec/html-element-attributes.sexp`

### Wildcard Attr Matching Rules

`define/element` supports wildcard prefixes:

- `data-*`
- `aria-*`

Matching is prefix-based and requires the `-` separator.

Examples:

- `#:data-testid` allowed
- `#:aria-label` allowed
- `#:datatest` rejected
- `#:arialabel` rejected

### `#:attrs` escape hatch

Constructors also support `#:attrs` (list of attr pairs), merged with keyword attrs.

- `#:attrs #f` is allowed.
- keyword attrs and `#:attrs` are combined.

### Class merge semantics

`#:class` keeps web-easy merge/dedupe behavior:

- classes from base attrs, `#:class`, and `#:attrs` are merged,
- duplicate class tokens are removed,
- order is stable by first occurrence.

### Reactive attrs (primitive html-element)

Primitive `html-element` supports observable attrs.

- scalar attrs update reactively,
- `data-*` and `aria-*` update reactively,
- `class` merge updates reactively,
- `style` updates reactively,
- tag can be observable (used by semantic `heading` delegation).

### Primitive DOM event keywords

Primitive HTML constructors now accept a Phase 1 set of generic bubbling DOM
event keywords:

- mouse: `#:on-click`, `#:on-doubleclick`, `#:on-mousedown`, `#:on-mousemove`,
  `#:on-mouseup`, `#:on-mouseenter`, `#:on-mouseleave`, `#:on-mouseover`,
  `#:on-mouseout`, `#:on-contextmenu`
- pointer: `#:on-pointerdown`, `#:on-pointermove`, `#:on-pointerup`,
  `#:on-pointerenter`, `#:on-pointerleave`, `#:on-pointerover`,
  `#:on-pointerout`, `#:on-pointercancel`
- keyboard: `#:on-keydown`, `#:on-keyup`
- focus: `#:on-focus`, `#:on-blur`, `#:on-focusin`, `#:on-focusout`
- form/input: `#:on-input`, `#:on-change`, `#:on-beforeinput`,
  `#:on-submit`, `#:on-reset`, `#:on-invalid`
- wheel/scroll: `#:on-wheel`, `#:on-scroll`
- drag/drop: `#:on-drag`, `#:on-dragstart`, `#:on-dragend`,
  `#:on-dragenter`, `#:on-dragleave`, `#:on-dragover`, `#:on-drop`
- touch: `#:on-touchstart`, `#:on-touchmove`, `#:on-touchend`,
  `#:on-touchcancel`
- media/load/error: `#:on-load`, `#:on-error`, `#:on-abort`
- animation/transition: `#:on-animationstart`, `#:on-animationend`,
  `#:on-animationiteration`, `#:on-transitionend`

These keywords are part of the primitive/core layer, not the higher-level
component layer.
Callbacks receive the raw browser event object.

### Special text-only primitives

Some primitives intentionally remain text-only even after the broader primitive widening pass:

- `Title`
- `Style`
- `Textarea`

Current direction:

- `Title` and `Style` stay text-only and should accept plain strings or observables.
- If an observable update for `Title` or `Style` becomes a non-string and non-`#f` value, the update should be ignored and a console error should be logged.
- `Textarea` stays a value/text primitive rather than accepting child views.

### Procedure-valued attrs

Policy:

- construction-time procedure-valued attrs are rejected,
- observable updates that become procedure-valued are ignored,
- a warning is emitted through `current-web-easy-warning-handler`,
- last valid value is kept.

You can replace the warning sink with:

- `set-current-web-easy-warning-handler!`
- `call-with-web-easy-warning-handler`

This keeps attr channels data-only and avoids runtime crashes from invalid updates.

### Implemented constraints

Current constructor/runtime constraints implemented for primitive HTML elements:

1. `Base` keyword requirement:
   `Base` must be called with at least one of `#:href` or `#:target`.
2. `Base` ordering check (conservative phase):
   in direct `window` children, `Base` must appear before URL-bearing primitive elements.
   URL-bearing detection currently treats empty-string URL attrs as absent.

## `define/component` (internal)

`define/component` lives in:

- `lib/web-easy/define-element.rkt`

Current conventions:

1. Positional arguments use `#:positional`:
   - required positional entries: `[id]`
   - optional positional entries with defaults: `[id default]`
   - required entries must come before defaulted entries.
2. `#:positional-count` is optional when `#:positional` is present:
   - inferred arity is `required..total`,
   - if provided explicitly, it must match inferred arity.
3. Root attrs use identifier-style `#:root-attrs`:
   - use `#:root-attrs attrs/final`,
   - define `(define attrs/final (list ...))` in the component body,
   - macro rewrites this binding to merge forwarded/global attrs through generated root-attrs helper.
4. Body style:
   - prefer plain body-level `define`s,
   - avoid `let*`/`let` wrappers unless lexical scoping is required.

## `observable-element-children` internal hook

`observable-element-children` has an internal optional `#:after-render` hook.

- Purpose:
  - allow component-specific backend wiring after dynamic child rebuilds,
  - keep renderer generic (no widget-name special-casing).
- Current users:
  - `carousel` (autoplay timeout bridge),
  - `scrollspy` (scroll observer + scroll-into-view bridge).
  - `list-view` (keyed child reuse/reorder without a dedicated renderer branch).
- Current internal API includes:
  - `dom-node-attr-ref`, `find-node-by-widget`, `dom-node-children`,
  - `dom-node-on-click`, `set-dom-node-on-click!`,
  - `backend-replace-children!`, `build-node`,
  - `backend-set-timeout!`, `backend-clear-timeout!`,
  - `backend-scrollspy-observe-scroll!`, `backend-scrollspy-scroll-into-view!`,
  - `backend-scrollspy-active-id`.
- Contract status:
  - internal-only,
  - not part of public API/docs stability guarantees.

### Primitive Composition Status

All built-in semantic/compound constructors are now view-level primitive compositions built with `define/component`.
Only low-level constructors remain on `define/key`:

- `html-element`
- `html-element-children`
- internal `observable-element-children`

- They no longer require dedicated renderer kind branches.
- Root semantics are expressed through primitive attrs (`role`, `aria-*`, `on-change-action`, widget classes).
- Close callbacks are bridged from primitive events and support both callback shapes:
  - arity-0: `(lambda () ...)`
  - arity-1: `(lambda (reason) ...)`, where reason is one of `'escape`, `'button`, or `'backdrop` (offcanvas).

Example style:

```racket
(define/component spacer
  #:root-tag 'span
  #:positional ([grow 1])
  #:component-keywords ([#:grow grow-kw #f])
  #:root-attrs attrs/final
  (define final-grow (if (eq? grow-kw #f) grow grow-kw))
  (define @grow (observable-or-const final-grow))
  (define @style (~> @grow (lambda (grow0) ...)))
  (define attrs/final
    (list (cons 'data-we-widget "spacer")
          (cons 'class "we-spacer")
          (cons 'style @style)))
  (Span "" #:attrs attrs/final))
```

## Roadmap

Near-term directions:

1. Event attrs policy:
   keep primitive attr channels data-only for now, and decide if/when to introduce explicit event hooks.
2. Warning sink evolution:
   current warning-handler hook is intentionally simple; we can later layer structured logging/categories on top.
3. Spec refresh flow:
   keep the generated HTML attribute snapshot up to date and document refresh cadence/tool invocation.
   Refresh command:
   `node tools/fetch-html-element-attributes.mjs --version 3.1.0`
4. Base ordering validation scope:
   current check is conservative (direct `window` children only); a future enhancement is a full document-order traversal so `<base>` ordering is validated across the whole rendered view tree.
   TODO: implement full-tree (document-order) validation mode and keep conservative mode as a fast-path fallback.

## Regression Notes

### Include/Reader Error Binding (Resolved)

The include/reader error-binding regression is fixed.

Root cause:

- `collect-assignable-variables` reset the running mutable set to `empty-set`
  when seeing `#%require` / `#%provide`, so mutable bindings discovered earlier
  (including `error.59`) were dropped.

Fix:

- Preserve threaded accumulator `xs` in `#%require` and `#%provide` clauses.

Regression guard:

- `lib/web-easy/test/test-error-binding-regression.rkt`

Validation:

- `racket ../../../webracket.rkt -r test-error-binding-regression.rkt` returns `#<void>`.

### Dialog Initial Focus in Browser Backend (Resolved)

Parity dialog pages expecting initial focus inside the dialog now pass.

Fix:

- defer dialog focus-on-open by one timeout tick;
- clear pending deferred focus timer on close.

### Solar2 List-Group Contract Alignment (Resolved)

Solar2 list-group contract pages now pass with unclipped rows and expected overflow behavior.

Fixes:

- set Solar showcase default theme to `solar2` in `example-browser-solar-showcase.rkt`;
- ensure list-group overflow is visible in renderer/style path used by Solar2 contract pages.

## Uppercase With-Children (Decision)

First target elements:

- `Div`
- `Section`

Proposed constructor shape:

```racket
(Div child ... [#:attrs attrs] [#:* html-attrs])
(Section child ... [#:attrs attrs] [#:* html-attrs])
```

Notes:

- these are uppercase primitive HTML constructors with children;
- children are variadic view arguments;
- HTML attributes follow the same validation model as current leaf uppercase elements
  (global + element-specific + `data-*`/`aria-*`, plus `#:attrs` merge behavior);
- class merge/reactive behavior should match existing primitive HTML element behavior.
