# WEB-EASY

This document captures internal design decisions for `lib/web-easy/`.

## Recent Changes

- Added internal `define/element` support for broad primitive HTML constructor generation.
- Expanded uppercase primitive element coverage (leaf + with-children families) with validated keyword attrs.
- Added `Base` constraints:
  - constructor requires at least one of `#:href` or `#:target`,
  - conservative runtime ordering check for direct `window` children.
- Consolidated HTML primitive tests with helper-based rejection/assertion patterns.

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
