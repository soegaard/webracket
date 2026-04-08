# DESIGN: Building Browser-Backed APIs in WebRacket

## 1. Purpose

This guide captures the pattern that worked well for WebSocket and should be reused for similar browser-backed APIs in WebRacket.

The basic shape is:

1. add a small raw FFI surface in `ffi/*.ffi`,
2. build a Rackety wrapper library on top,
3. document the raw bridge and the public library separately.

The goal is not just to explain WebSocket, but to give a repeatable recipe for future DOM, media, event, or browser-object APIs.

## 2. Low-Level Bindings

### 2.1 Keep the raw surface small

The low-level layer should mirror the browser API closely, but only for the operations you really need.

For a single-object API, that often means:
- constructor
- `send`
- `close`
- a few read-only state accessors
- no extra convenience behavior

Prefer a narrow raw surface over a large one. It is easier to test, easier to document, and easier to wrap later.

### 2.2 Use `js-` names for raw bindings

Raw bindings should clearly look like interop primitives:
- `js-websocket-new`
- `js-websocket-send`
- `js-websocket-close`
- `js-websocket-ready-state-number`

That naming makes the boundary obvious:
- `js-*` means “low-level browser interop”
- wrapper names like `websocket-*` mean “checked, Rackety API”

If the API is a family of browser objects rather than a single object, keep the family structure visible in the names. For example, Audio naturally splits into `AudioContext`, `AudioNode`, `AudioParam`, `AudioBuffer`, and node-specific accessors. Make the naming reflect that shape instead of flattening everything into one undifferentiated list.

### 2.3 Choose return conventions deliberately

Do not pick a return type just because the browser API returns *something*.

Use the low-level return convention that matches the browser behavior:
- `extern/raw` when the caller needs the exact JS value unchanged
- `extern` when `null` should become `#f`
- `extern/undefined` when `undefined` should become `#f`
- `extern/nullish` when either `null` or `undefined` means absence
- `value` when the result should enter the normal JS-to-WebRacket conversion bridge

If the API is naturally nullable, encode that at the raw layer instead of forcing the wrapper to guess.

If the browser returns an object with identity or behavior that is worth
preserving on the WebRacket side, prefer a checked wrapper struct instead of
exposing the raw `external/raw` value directly. Keep a raw accessor such as
`*-raw` only when callers genuinely need the browser object itself.

If multiple wrapper libraries need to share the same checked structs or
unwrap helpers, put those shared definitions in
`stdlib/shared-library-structs.rkt` and have the compiler include that file
before the main program. In other words: use
`stdlib/shared-library-structs.rkt` for shared wrapper types, not compiler
changes or nested wrapper includes.

### 2.4 Keep optional arguments explicit at the raw layer

Low-level bindings may use `(void)` to mean “argument omitted” when that matches the browser shape.

That is fine at the FFI layer, but do not leak that convention into the public library unless there is a strong reason.

In the public Rackety API, prefer `#f` for omitted optional arguments. If a
browser call needs a literal `#f` value, the wrapper may accept a thunk such as
`(lambda () #f)` and force it only when the real `#f` value is needed.

### 2.5 Add focused tests for the raw surface

Test the raw bindings with a mocked browser object rather than a live network service or real page interaction.

For the raw surface, the useful checks are:
- constructor forwards URL and protocol arguments correctly
- `send` and `close` call through with the expected values
- property getters return the expected low-level values
- the FFI compiles cleanly in the browser backend

If a value-shape is awkward or browser-specific, test that awkwardness at the raw layer once, then hide it in the wrapper.

### 2.6 Guard browser feature detection explicitly

Some browser APIs are not available in every environment, and some constructors only exist in the browser but not in tests or alternate runtimes.

For those APIs, wrapper predicates should check that the constructor exists before using `js-instanceof`. That keeps the wrapper from failing with a missing-global error when the environment does not expose the browser object.

This matters more for object-family APIs like Audio than for simple single-object APIs like WebSocket.

## 3. Rackety Library

### 3.1 Wrap the raw bindings with checked functions

The public library should export `*-` functions that feel like ordinary Racket code.

For WebSocket, that means:
- `websocket-new`
- `websocket-send`
- `websocket-close`
- `websocket-url`
- `websocket-ready-state`
- event helpers

The wrapper layer should validate arguments before calling the `js-*` primitive.

### 3.2 Prefer Racket defaults over browser sentinels

The wrapper should translate browser conventions into Racket-friendly defaults.

Examples that work well for a browser wrapper:
- `websocket-new` is variadic for protocol names instead of forcing a sentinel like `(void)`
- `websocket-close` defaults the close code to `1000`
- event-handler setters accept `#f` to clear a handler
- listener helpers accept positional options instead of requiring a keyword API

The rule of thumb is:
- keep the raw browser shape in `js-*`
- give Racket users a nicer shape in `websocket-*`

When a browser API naturally returns a collection-like object, prefer a
wrapper type that matches the browser shape and still feels like Racket. For
example:
- map-like browser values can become a checked struct plus `get`, `has?`,
  `keys`, `values`, and `for-each` helpers
- iterator-like browser values can become a checked `iterator` struct plus a
  small set of conversion and traversal helpers such as `iterator->vector`

Name the helper to match the WebRacket result, not just the browser source
shape. If the wrapper returns a vector, call it `*-vector`, not `*-array`.

### 3.3 Normalize awkward browser values in the wrapper

The wrapper should be where browser-specific oddities disappear.

Good examples:
- convert protocol symbols to strings
- when a browser method expects a string argument, accept both strings and
  symbols in the wrapper and normalize symbols to strings
- map the numeric ready-state to symbols like `'connecting`, `'open`, `'closing`, and `'closed`
- provide both the raw number and the symbolic version if both are useful
- give event handlers a clear “set handler / clear handler” story
- use exact integer contracts for browser counters and real-valued contracts
  for high-resolution timestamps
- wrap browser event-count or memory-info objects in checked structs rather
  than exposing raw externals

If the browser API has a raw shape that is technically correct but awkward in Racket, hide that awkwardness here.

For optional callback receivers or `this`-style arguments, keep the public
contract Racket-friendly and document the convention explicitly. In our
wrappers, `#f` means omitted, and a thunk can be used when a literal `#f`
value is needed.

When a wrapper returns another browser object that is shared across multiple
APIs, define the wrapper type once in `stdlib/shared-library-structs.rkt`
and reuse it from the public libraries. That keeps the public API consistent
without requiring module-style sharing in the compiler.

### 3.4 Add event support explicitly

Browser APIs often become useful only once events are wrapped cleanly.

For event-driven APIs, consider two layers:
- property-style handler setters such as `websocket-onmessage!`
- listener helpers such as `websocket-add-event-listener!`

If listener removal needs an identity token or callback wrapper, keep that bookkeeping in the wrapper library, not in example code.

For event handlers, prefer a browser-native clearing convention in the wrapper. In practice that means `#f` should clear the handler slot, and the wrapper should translate that into the JS sentinel the browser expects. For many browser properties, `undefined` is a better clearing value than `null`.

### 3.5 Keep wrapper code small and boring

The wrapper library should mostly be:
- checks
- argument normalization
- direct forwarding
- small policy decisions

Avoid reimplementing the browser API in Racket. The wrapper should make the raw API pleasant, not invent a second API.

When a browser helper takes a callback, make the callback arity visible in the
public contract and explain the arguments in the docs. Spell out how boolean
results are interpreted, especially when the callback result is treated as
true unless it is `#f`.

### 3.6 Promise-returning methods can start raw

Some browser APIs expose methods that return promises. You do not need to design a Promise abstraction first in order to ship a useful first version of the API.

If the immediate goal is to expose the browser surface faithfully, it is fine to leave those methods as raw results at first and add Promise-aware helpers later if they prove useful.

## 4. Documentation

### 4.1 Split low-level docs from public docs

Write two documents:
- a low-level reference for the raw `js-*` FFI bindings
- a public library chapter for the checked `websocket-*` wrapper

Do not mix the two. The low-level page should explain the bridge and the raw bindings. The library page should explain what users should actually call.

### 4.2 Explain the API in user terms

The library docs should assume the reader may not know the browser API yet.

The public chapter works best when it includes:
- a short introduction to what WebSockets are
- a quick start example
- a use-case list
- a reference grouped by task:
  - construction and state
  - sending and closing
  - event handlers
  - event listeners

That structure should be reused for other APIs whenever possible.

For browser-object families that are more specialized, make the introduction
explain the browser concept in plain language before diving into the API
surface. A reader should be able to understand a page like `Performance`,
`Iterator`, or `Window` without already knowing the browser object.

If the public chapter includes helper methods with callbacks, mention the
callback argument shapes in the signature and repeat the receiver/default
convention on each relevant entry so each section stands on its own.

### 4.3 Use the Scribble banner pattern consistently

The docs converge on a useful visual convention:
- `include-lib` banner for the wrapper library
- compile-option banner for the build flag
- MDN banner under each entry

Keep the banners in the entry body, and keep the signature and prose readable before the banner appears.

For per-entry browser API docs, use the shared `mdn-bar` helper so the
inline MDN banner has a consistent look across wrapper chapters like
`audio`, `websocket`, and `console`.

The banner pattern helps the reader answer three questions quickly:
- how do I require this?
- how do I compile it?
- what is this API on the browser side?

### 4.4 Make cross references canonical

Scribble warnings are easiest to avoid when every cross-reference target has
one obvious home.

Use these rules for new APIs and doc refactors:
- Give every appendix, section, or subsection that is meant to be linked a
  unique explicit Scribble tag with `#:tag`.
- Put raw bridge accessors in one canonical appendix instead of repeating them
  across wrapper chapters.
- Keep shared docs-only label modules small and chapter-local; use them for
  `for-label` resolution, but keep `@declare-exporting` on the real
  implementation chapter.
- Document each shared wrapper type in the chapter where users expect to find
  it. For example, `DOMRectList` belongs with `DOMRect`, and collection types
  that belong to `Element` should be documented in the `Element` chapter.
- Do not document the same identifier in multiple chapters unless one chapter
  is clearly the canonical owner and the others only reference it.

The practical goal is simple: a reader should be able to click the obvious
name and land in one stable place, with no duplicate Scribble anchors and no
missing-tag warnings.

### 4.5 Document low-level bridge behavior precisely

When documenting an API made of several browser object families, group the docs the same way the implementation is grouped. For Audio, that means one reference structure for `AudioContext`, another for `AudioNode`/`AudioParam`, and separate coverage for node-specific accessors and event helpers.

When a helper returns another browser iterator or map-like object, document the
wrapper type and the conversion helpers together so the reader sees the full
workflow in one place.

### 4.5 Keep examples aligned with the implementation

Examples in the docs should use the same names and conventions as the real library.

For a concrete API, that means:
- examples use `include-lib websocket`
- wrapper examples use `websocket-*`
- the low-level reference uses `js-websocket-*`

The docs should never force readers to think in terms of the internal raw API unless they are on the raw reference page.

If an example uses a helper that returns a vector, show a vector in the
example. If an example uses a wrapped iterator, show the iterator wrapper and
the conversion helper that consumes it.

### 4.6 JS interop conventions

Use the JS bridge helpers that match the shape of the value you are working
with.

| Need | Use | Example | Notes |
|---|---|---|---|
| Named JS property access | `js-ref` | `(js-ref obj "width")` | Best for ordinary properties and getters. |
| Numeric/indexed access on array-like JS values | `js-index` | `(js-index arr i)` | Preferred over stringifying the index. |
| JS method call that returns a converted WebRacket value | `js-send` | `(js-send obj "slice" (vector 1))` | Use when the result should be converted. |
| JS method call that returns a raw extern value | `js-send/extern` | `(js-send/extern obj "createImageData" (vector 2 3))` | Use when you need the exact JS value back. |
| JS method call with an enforced result type | `js-send/boolean`, `js-send/value`, etc. | `(js-send/boolean arr "includes" (vector 2))` | Use the most specific helper that matches the expected result. |
| JS Arrays and Array-specific operations | `js-array-*` helpers | `js-array-ref`, `js-array-length`, `js-array-map` | Use only for real JS `Array` values or Array-centric operations. |
| Browser object fields and named properties | `js-ref` / `js-set!` | `crossOrigin`, `srcObject`, `value`, `length` | Keep these as property access, not method calls. |

Do not:
- fake indexed access with `(js-ref arr (number->string i))`
- use `js-array-*` helpers for DOM collections or non-Array browser objects unless you really want Array semantics
- use `js-send` for a property lookup
- use `js-ref` when the value is really an index access

Rules of thumb:
- Use `js-index` for anything that is really “element at position `i`”.
- Use `js-ref` for named properties.
- Use `js-send` for method calls, not property lookups.
- If you need a raw external value, prefer the `/extern` variant rather than converting and re-wrapping.

The general rule is: match the helper to the JS shape you actually have. If the value is a named property, use `js-ref`. If it is indexed, use `js-index`. If it is a method call, use `js-send` or one of its typed variants.

When a property access shows up repeatedly in a public wrapper, prefer a dedicated FFI binding over a generic `js-ref` or `js-set!` helper. Keep the generic helpers for one-off shape checks, unusually dynamic code, or cases where the browser object really is being treated as ad hoc JS.

### 4.7 Use docs-only label modules when Scribble needs fake bindings

If a library chapter shows example code in `@racketblock`, Scribble can
only turn identifiers into links when it knows those identifiers as
documented bindings. When the implementation lives outside ordinary
Racket modules, or when the documented surface is generated, add a
docs-only `*-labels.rkt` module that provides fake bindings for the
names used in the examples.

Those bindings can all be `any/c`; they exist only so Scribble has
something to link to. In the Scribble file, require the label module
with `for-label` and add `@declare-exporting` with the same module path.
Then identifiers inside `@racketblock` will link automatically to the
reference entries.

This pattern works well for both checked wrapper chapters and raw FFI
chapters. It keeps examples readable while still making the docs
navigable.

### 4.8 Add the site wiring to the checklist

New browser APIs are not complete until the docs are visible in the site build and publish flow.

For a new API, check that you have:
- the raw FFI document
- the public library document
- the site or publish hook that makes the docs reachable
- a mock-based test that exercises the raw bridge
- a mock-based test that exercises the public wrapper
- any wrapper structs or iterator helpers documented in the manual
- any special docs-only label modules needed for Scribble linking

## 5. Suggested Workflow for a New API

When adding another browser-backed API, follow this order:

1. Implement the smallest useful `ffi/*.ffi` surface.
2. Decide which results should be `value`, `extern`, `extern/raw`, or a nullable variant.
3. Add a checked wrapper library with `include-lib`.
4. Add event helpers or other ergonomic wrappers only after the core operations work.
5. Introduce checked wrapper structs for browser objects that should not be
   exposed as raw externals.
6. Use `#f` for omitted optional arguments in the public API, and document the
   thunk convention when a literal `#f` must be passed through.
7. Write the low-level reference first, then the public library docs.
8. Add an intro, quick start, use cases, and MDN-linked per-entry documentation.
9. Make callback arities, truthiness conventions, and conversion helpers
   explicit in the docs.
10. Validate with mock-based tests before relying on live browser behavior.
11. Make sure the site/publish wiring exposes the new pages.

## 6. WebSocket as the Template

WebSocket is a good model because it has:
- a clear raw browser object
- a small core operation set
- important event-driven behavior
- a meaningful ergonomic wrapper layer
- a clean docs split between raw interop and user-facing API

If a future API looks similar, reuse this pattern rather than inventing a new one.
