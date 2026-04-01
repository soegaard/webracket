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

For WebSocket, that meant:
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

### 2.3 Choose return conventions deliberately

Do not pick a return type just because the browser API returns *something*.

Use the low-level return convention that matches the browser behavior:
- `extern/raw` when the caller needs the exact JS value unchanged
- `extern` when `null` should become `#f`
- `extern/undefined` when `undefined` should become `#f`
- `extern/nullish` when either `null` or `undefined` means absence
- `value` when the result should enter the normal JS-to-WebRacket conversion bridge

If the API is naturally nullable, encode that at the raw layer instead of forcing the wrapper to guess.

### 2.4 Keep optional arguments explicit at the raw layer

Low-level bindings may use `(void)` to mean “argument omitted” when that matches the browser shape.

That is fine at the FFI layer, but do not leak that convention into the public library unless there is a strong reason.

### 2.5 Add focused tests for the raw surface

Test the raw bindings with a mocked browser object rather than a live network service or real page interaction.

For WebSocket, the useful checks were:
- constructor forwards URL and protocol arguments correctly
- `send` and `close` call through with the expected values
- property getters return the expected low-level values
- the FFI compiles cleanly in the browser backend

If a value-shape is awkward or browser-specific, test that awkwardness at the raw layer once, then hide it in the wrapper.

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

Examples that worked well for WebSocket:
- `websocket-new` is variadic for protocol names instead of forcing a sentinel like `(void)`
- `websocket-close` defaults the close code to `1000`
- event-handler setters accept `#f` to clear a handler
- listener helpers accept positional options instead of requiring a keyword API

The rule of thumb is:
- keep the raw browser shape in `js-*`
- give Racket users a nicer shape in `websocket-*`

### 3.3 Normalize awkward browser values in the wrapper

The wrapper should be where browser-specific oddities disappear.

Good examples:
- convert protocol symbols to strings
- map the numeric ready-state to symbols like `'connecting`, `'open`, `'closing`, and `'closed`
- provide both the raw number and the symbolic version if both are useful
- give event handlers a clear “set handler / clear handler” story

If the browser API has a raw shape that is technically correct but awkward in Racket, hide that awkwardness here.

### 3.4 Add event support explicitly

Browser APIs often become useful only once events are wrapped cleanly.

For event-driven APIs, consider two layers:
- property-style handler setters such as `websocket-onmessage!`
- listener helpers such as `websocket-add-event-listener!`

If listener removal needs an identity token or callback wrapper, keep that bookkeeping in the wrapper library, not in example code.

### 3.5 Keep wrapper code small and boring

The wrapper library should mostly be:
- checks
- argument normalization
- direct forwarding
- small policy decisions

Avoid reimplementing the browser API in Racket. The wrapper should make the raw API pleasant, not invent a second API.

## 4. Documentation

### 4.1 Split low-level docs from public docs

Write two documents:
- a low-level reference for the raw `js-*` FFI bindings
- a public library chapter for the checked `websocket-*` wrapper

Do not mix the two. The low-level page should explain the bridge and the raw bindings. The library page should explain what users should actually call.

### 4.2 Explain the API in user terms

The library docs should assume the reader may not know the browser API yet.

The WebSocket chapter worked well because it included:
- a short introduction to what WebSockets are
- a quick start example
- a use-case list
- a reference grouped by task:
  - construction and state
  - sending and closing
  - event handlers
  - event listeners

That structure should be reused for other APIs whenever possible.

### 4.3 Use the Scribble banner pattern consistently

The WebSocket docs converged on a useful visual convention:
- `include-lib` banner for the wrapper library
- compile-option banner for the build flag
- MDN banner under each entry

Keep the banners in the entry body, and keep the signature and prose readable before the banner appears.

The banner pattern helps the reader answer three questions quickly:
- how do I require this?
- how do I compile it?
- what is this API on the browser side?

### 4.4 Document low-level bridge behavior precisely

### 4.5 Keep examples aligned with the implementation

Examples in the docs should use the same names and conventions as the real library.

For WebSocket, that meant:
- examples use `include-lib websocket`
- wrapper examples use `websocket-*`
- the low-level reference uses `js-websocket-*`

The docs should never force readers to think in terms of the internal raw API unless they are on the raw reference page.

## 5. Suggested Workflow for a New API

When adding another browser-backed API, follow this order:

1. Implement the smallest useful `ffi/*.ffi` surface.
2. Decide which results should be `value`, `extern`, `extern/raw`, or a nullable variant.
3. Add a checked wrapper library with `include-lib`.
4. Add event helpers or other ergonomic wrappers only after the core operations work.
5. Write the low-level reference first, then the public library docs.
6. Add an intro, quick start, use cases, and MDN-linked per-entry documentation.
7. Validate with mock-based tests before relying on live browser behavior.

## 6. WebSocket as the Template

WebSocket is a good model because it has:
- a clear raw browser object
- a small core operation set
- important event-driven behavior
- a meaningful ergonomic wrapper layer
- a clean docs split between raw interop and user-facing API

If a future API looks similar, reuse this pattern rather than inventing a new one.
