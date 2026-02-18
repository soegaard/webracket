# Reference: `dom.ffi` 

## Chapter 1 — Introduction

This document describes the browser DOM FFI exported by `ffi/dom.ffi` in WebRacket.

What this API gives you:
- Window/document access and DOM tree operations
- Event construction and listener wiring
- Canvas 2D drawing bindings
- HTML media and image bindings

Assumption in examples: the program is compiled with `--ffi dom`.

All function names are linked to MDN pages for the corresponding Web API interface/member.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Value and Boolean Conventions](#22-value-and-boolean-conventions)
- [2.3 Optional and Variadic Arguments](#23-optional-and-variadic-arguments)
- [2.4 Common Setup Helpers](#24-common-setup-helpers)
- [2.5 Quick Navigation by Goal](#25-quick-navigation-by-goal)
- [2.6 Optional Argument Matrix](#26-optional-argument-matrix)
- [2.7 Callback Signatures](#27-callback-signatures)
- [2.8 Environment and Availability Notes](#28-environment-and-availability-notes)
- [Chapter 3 — Window and Global Browser State](#chapter-3--window-and-global-browser-state)
- [3.1 Window Module](#31-window-module)
- [3.2 Performance Module](#32-performance-module)
- [Chapter 4 — DOM Tree and Selectors](#chapter-4--dom-tree-and-selectors)
- [4.1 Document Module](#41-document-module)
- [4.2 Element Module](#42-element-module)
- [Chapter 5 — Events](#chapter-5--events)
- [5.1 EventTarget Module](#51-eventtarget-module)
- [5.2 Event Module](#52-event-module)
- [Chapter 6 — Canvas](#chapter-6--canvas)
- [6.1 HTMLCanvasElement Module](#61-htmlcanvaselement-module)
- [6.2 CanvasRenderingContext2D Module](#62-canvasrenderingcontext2d-module)
- [Chapter 7 — Media and Images](#chapter-7--media-and-images)
- [7.1 HTMLMediaElement Module](#71-htmlmediaelement-module)
- [7.2 HTMLImageElement Module](#72-htmlimageelement-module)
- [Chapter 8 — Mini Workflows](#chapter-8--mini-workflows)
- [Build and insert a DOM node](#build-and-insert-a-dom-node)
- [Query and patch attributes](#query-and-patch-attributes)
- [Attach an event listener](#attach-an-event-listener)
- [Draw on a canvas](#draw-on-a-canvas)
- [Play media](#play-media)
- [Scroll and viewport control](#scroll-and-viewport-control)
- [Chapter 9 — Coverage Checklist](#chapter-9--coverage-checklist)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | Raw JavaScript value/object reference (no value conversion). |
| `(value)` | WebRacket value converted through the FFI value bridge. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(i32)` | 32-bit integer; often used as JS boolean (`0` / non-zero). |
| `(u32)` | Unsigned 32-bit integer. |
| `(f64)` | Double-precision floating-point number. |
| `()` | No arguments (input) or no value/void (output). |

### 2.2 Value and Boolean Conventions

- Many DOM booleans are exposed as `(i32)` for historical compatibility in this FFI module.
- Convention in this file: pass `0` for false and `1` for true where an `(i32)` flag is expected.
- Functions returning `(extern)` can return JavaScript `null`; check with `js-nullish?` from `ffi/standard.ffi` when needed.
- `Side effects?` in function tables indicates whether the call is expected to mutate browser/DOM/canvas state.

### 2.3 Optional and Variadic Arguments

- For optional JS arguments, this FFI commonly uses explicit `(value)` arguments. Pass `(void)` when omitted.
- For variadic JS methods, this FFI usually provides a fixed-arity subset.

### 2.4 Common Setup Helpers

```racket
(define win (js-window-window))
(define doc (js-window-document))
(define body (js-document-body))
(define canvas (js-create-element "canvas"))
```

### 2.5 Quick Navigation by Goal

| Goal | Start with | Typical next calls |
|---|---|---|
| Create/query DOM nodes | `js-create-element`, `js-get-element-by-id`, `js-query-selector` | `js-set-attribute!`, `js-append-child!`, `js-add-event-listener!` |
| Edit attributes/classes/styles | `js-set-attribute!`, `js-toggle-attribute!`, `js-set-style!` | `js-get-attribute`, `js-remove-attribute!`, `js-computed-style-map` |
| Wire events | `js-add-event-listener!` | `js-remove-event-listener!`, `js-dispatch-event!`, `js-event-target` |
| Draw on canvas | `js-get-context`, `js-canvas2d-fill-rect` | `js-canvas2d-begin-path`, `js-canvas2d-line-to`, `js-canvas2d-stroke` |
| Use media elements | `js-play`, `js-pause`, `js-load` | `js-current-time`, `js-set-current-time!`, `js-duration` |
| Handle scrolling/layout | `js-window-scroll-to`, `js-scroll-into-view!`, `js-get-bounding-client-rect` | `js-window-inner-width`, `js-window-page-y-offset`, `js-window-scroll-y` |

### 2.6 Optional Argument Matrix

| Binding | Optional arguments | Pass `(void)` for | JavaScript default behavior |
|---|---|---|---|
| `js-window-create-image-bitmap` | crop/options fields | missing optional args | browser default image bitmap options |
| `js-window-open` | target, windowFeatures, replace | missing optional args | browser chooses defaults |
| `js-window-post-message` | transfer/options | missing optional args | default structured-clone transfer behavior |
| `js-event-new` | event-init dict | missing init dict | event fields use default values |
| `js-animate` | options | missing options | browser animation defaults |
| `js-toggle-attribute!` | force | missing force (if wrapper supports) | toggle behavior based on current state |
| `js-canvas2d-clip` | path/fill-rule variants | missing optional args | current path with default rule |
| `js-canvas2d-fill` | path/fill-rule variants | missing optional args | current path with default fill rule |
| `js-canvas2d-fill-text` | max-width | missing max-width | no width constraint |
| `js-canvas2d-get-image-data` | settings/options | missing optional args | default pixel extraction behavior |
| `js-canvas2d-is-point-in-path` | path/fill-rule variants | missing optional args | current path/default fill rule |
| `js-canvas2d-is-point-in-stroke` | path variant | missing optional args | current path |
| `js-canvas2d-put-image-data` | dirty rectangle fields | missing optional args | full image data write |
| `js-canvas2d-round-rect` | radii variants | missing optional args | default corner radius behavior |
| `js-after!`, `js-append!`, `js-before!`, `js-prepend!` | variadic nodes/strings | wrapper accepts one node/string | JS supports variadic inserts |
| `js-replace-children!`, `js-replace-with!` | variadic nodes/strings | wrapper accepts one node | JS supports variadic replacements |
| `js-scroll!`, `js-scroll-by!`, `js-scroll-to!` | options object form | not supported by this wrapper | coordinate form only |
| `js-scroll-into-view!` | options object form | not supported by this wrapper | boolean form only |

### 2.7 Callback Signatures

- Event listeners (`js-add-event-listener!`):
  - `(lambda (event) ...)`
- Timers and frame callbacks (`js-window-set-timeout`, `js-window-set-interval`, `js-window-request-animation-frame`, `js-window-request-idle-callback`):
  - `(lambda args ...)`
  - `requestAnimationFrame` callback receives a timestamp.
- Web Animations (`js-animate` options object callbacks):
  - pass JavaScript-callable values where the platform expects callbacks.

### 2.8 Environment and Availability Notes

- These bindings are browser-oriented and assume a DOM-capable runtime.
- Some APIs may be unavailable depending on browser support and context:
  - secure-context gated APIs (for example parts of `crypto`/storage capabilities)
  - gesture-gated APIs (`play`, fullscreen/pointer-lock related calls)
  - permission-gated APIs (camera/microphone-related media flows)

## Chapter 3 — Window and Global Browser State

### 3.1 Window Module

Module source in `ffi/dom.ffi`: `#:module "window"`  
MDN root: [Window](https://developer.mozilla.org/en-US/docs/Web/API/Window)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-window-window`](https://developer.mozilla.org/en-US/docs/Web/API/Window) | `()` | `(extern)` | no | no | yes | `(js-window-window)` | read `window` as `extern`. |
| [`js-window-self`](https://developer.mozilla.org/en-US/docs/Web/API/Window) | `()` | `(extern)` | no | no | yes | `(js-window-self)` | read `self` as `extern`. |
| [`js-window-document`](https://developer.mozilla.org/en-US/docs/Web/API/Window) | `()` | `(extern)` | no | no | yes | `(js-window-document)` | read `document` as `extern`. |
| [`js-window-name`](https://developer.mozilla.org/en-US/docs/Web/API/Window/name) | `()` | `(string)` | no | no | no | `(js-window-name)` | read `name` as `string`. |
| [`js-set-window-name!`](https://developer.mozilla.org/en-US/docs/Web/API/Window/name) | `(string)` | `()` | yes | no | no | `(js-set-window-name! "x")` | set `name` on a Window value. |
| [`js-window-location`](https://developer.mozilla.org/en-US/docs/Web/API/Window/location) | `()` | `(extern)` | no | no | yes | `(js-window-location)` | read `location` as `extern`. |
| [`js-set-window-location!`](https://developer.mozilla.org/en-US/docs/Web/API/Window/location) | `(value)` | `()` | yes | no | no | `(js-set-window-location! (void))` | set `location` on a Window value. |
| [`js-window-custom-elements`](https://developer.mozilla.org/en-US/docs/Web/API/Window/customElements) | `()` | `(extern)` | no | no | yes | `(js-window-custom-elements)` | read `custom-elements` as `extern`. |
| [`js-window-history`](https://developer.mozilla.org/en-US/docs/Web/API/Window/history) | `()` | `(extern)` | no | no | yes | `(js-window-history)` | read `history` as `extern`. |
| [`js-window-locationbar`](https://developer.mozilla.org/en-US/docs/Web/API/Window/locationbar) | `()` | `(extern)` | no | no | yes | `(js-window-locationbar)` | read `locationbar` as `extern`. |
| [`js-window-menubar`](https://developer.mozilla.org/en-US/docs/Web/API/Window/menubar) | `()` | `(extern)` | no | no | yes | `(js-window-menubar)` | read `menubar` as `extern`. |
| [`js-window-personalbar`](https://developer.mozilla.org/en-US/docs/Web/API/Window/personalbar) | `()` | `(extern)` | no | no | yes | `(js-window-personalbar)` | read `personalbar` as `extern`. |
| [`js-window-scrollbars`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollbars) | `()` | `(extern)` | no | no | yes | `(js-window-scrollbars)` | read `scrollbars` as `extern`. |
| [`js-window-statusbar`](https://developer.mozilla.org/en-US/docs/Web/API/Window/statusbar) | `()` | `(extern)` | no | no | yes | `(js-window-statusbar)` | read `statusbar` as `extern`. |
| [`js-window-toolbar`](https://developer.mozilla.org/en-US/docs/Web/API/Window/toolbar) | `()` | `(extern)` | no | no | yes | `(js-window-toolbar)` | read `toolbar` as `extern`. |
| [`js-window-status`](https://developer.mozilla.org/en-US/docs/Web/API/Window/status) | `()` | `(string)` | no | no | no | `(js-window-status)` | read `status` as `string`. |
| [`js-set-window-status!`](https://developer.mozilla.org/en-US/docs/Web/API/Window/status) | `(string)` | `()` | yes | no | no | `(js-set-window-status! "x")` | set `status` on a Window value. |
| [`js-window-closed`](https://developer.mozilla.org/en-US/docs/Web/API/Window/closed) | `()` | `(i32)` | no | no | no | `(js-window-closed)` | read `closed` as `i32`. |
| [`js-window-frames`](https://developer.mozilla.org/en-US/docs/Web/API/Window/frames) | `()` | `(extern)` | no | no | yes | `(js-window-frames)` | read `frames` as `extern`. |
| [`js-window-length`](https://developer.mozilla.org/en-US/docs/Web/API/Window/length) | `()` | `(u32)` | no | no | no | `(js-window-length)` | read `length` as `u32`. |
| [`js-window-opener`](https://developer.mozilla.org/en-US/docs/Web/API/Window/opener) | `()` | `(extern)` | no | no | yes | `(js-window-opener)` | read `opener` as `extern`. |
| [`js-set-window-opener!`](https://developer.mozilla.org/en-US/docs/Web/API/Window/opener) | `(extern)` | `()` | yes | no | no | `(js-set-window-opener! obj)` | set `opener` on a Window value. |
| [`js-window-parent`](https://developer.mozilla.org/en-US/docs/Web/API/Window/parent) | `()` | `(extern)` | no | no | yes | `(js-window-parent)` | read `parent` as `extern`. |
| [`js-window-top`](https://developer.mozilla.org/en-US/docs/Web/API/Window/top) | `()` | `(extern)` | no | no | yes | `(js-window-top)` | read `top` as `extern`. |
| [`js-window-visual-viewport`](https://developer.mozilla.org/en-US/docs/Web/API/Window/visualViewport) | `()` | `(extern)` | no | no | yes | `(js-window-visual-viewport)` | read `visual-viewport` as `extern`. |
| [`js-window-navigator`](https://developer.mozilla.org/en-US/docs/Web/API/Window/navigator) | `()` | `(extern)` | no | no | yes | `(js-window-navigator)` | read `navigator` as `extern`. |
| [`js-window-origin`](https://developer.mozilla.org/en-US/docs/Web/API/Window/origin) | `()` | `(string)` | no | no | no | `(js-window-origin)` | read `origin` as `string`. |
| [`js-window-crypto`](https://developer.mozilla.org/en-US/docs/Web/API/Window/crypto) | `()` | `(extern)` | no | no | yes | `(js-window-crypto)` | read `crypto` as `extern`. |
| [`js-window-device-pixel-ratio`](https://developer.mozilla.org/en-US/docs/Web/API/Window/devicePixelRatio) | `()` | `(f64)` | no | no | no | `(js-window-device-pixel-ratio)` | read `device-pixel-ratio` as `f64`. |
| [`js-window-event`](https://developer.mozilla.org/en-US/docs/Web/API/Window/event) | `()` | `(extern)` | no | no | yes | `(js-window-event)` | read `event` as `extern`. |
| [`js-window-frame-element`](https://developer.mozilla.org/en-US/docs/Web/API/Window/frameElement) | `()` | `(extern)` | no | no | yes | `(js-window-frame-element)` | read `frame-element` as `extern`. |
| [`js-window-inner-height`](https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight) | `()` | `(f64)` | no | no | no | `(js-window-inner-height)` | read `inner-height` as `f64`. |
| [`js-window-inner-width`](https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth) | `()` | `(f64)` | no | no | no | `(js-window-inner-width)` | read `inner-width` as `f64`. |
| [`js-window-outer-height`](https://developer.mozilla.org/en-US/docs/Web/API/Window/outerHeight) | `()` | `(f64)` | no | no | no | `(js-window-outer-height)` | read `outer-height` as `f64`. |
| [`js-window-outer-width`](https://developer.mozilla.org/en-US/docs/Web/API/Window/outerWidth) | `()` | `(f64)` | no | no | no | `(js-window-outer-width)` | read `outer-width` as `f64`. |
| [`js-window-screen-x`](https://developer.mozilla.org/en-US/docs/Web/API/Window/screenX) | `()` | `(f64)` | no | no | no | `(js-window-screen-x)` | read `screen-x` as `f64`. |
| [`js-window-screen-y`](https://developer.mozilla.org/en-US/docs/Web/API/Window/screenY) | `()` | `(f64)` | no | no | no | `(js-window-screen-y)` | read `screen-y` as `f64`. |
| [`js-window-screen-left`](https://developer.mozilla.org/en-US/docs/Web/API/Window/screenLeft) | `()` | `(f64)` | no | no | no | `(js-window-screen-left)` | read `screen-left` as `f64`. |
| [`js-window-screen-top`](https://developer.mozilla.org/en-US/docs/Web/API/Window/screenTop) | `()` | `(f64)` | no | no | no | `(js-window-screen-top)` | read `screen-top` as `f64`. |
| [`js-window-page-x-offset`](https://developer.mozilla.org/en-US/docs/Web/API/Window/pageXOffset) | `()` | `(f64)` | no | no | no | `(js-window-page-x-offset)` | read `page-x-offset` as `f64`. |
| [`js-window-page-y-offset`](https://developer.mozilla.org/en-US/docs/Web/API/Window/pageYOffset) | `()` | `(f64)` | no | no | no | `(js-window-page-y-offset)` | read `page-y-offset` as `f64`. |
| [`js-window-scroll-x`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollX) | `()` | `(f64)` | no | no | no | `(js-window-scroll-x)` | read `scroll-x` as `f64`. |
| [`js-window-scroll-y`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollY) | `()` | `(f64)` | no | no | no | `(js-window-scroll-y)` | read `scroll-y` as `f64`. |
| [`js-window-screen`](https://developer.mozilla.org/en-US/docs/Web/API/Window/screen) | `()` | `(extern)` | no | no | yes | `(js-window-screen)` | read `screen` as `extern`. |
| [`js-window-local-storage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) | `()` | `(extern)` | no | no | yes | `(js-window-local-storage)` | read `local-storage` as `extern`. |
| [`js-window-session-storage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage) | `()` | `(extern)` | no | no | yes | `(js-window-session-storage)` | read `session-storage` as `extern`. |
| [`js-window-performance`](https://developer.mozilla.org/en-US/docs/Web/API/Window/performance) | `()` | `(extern)` | no | no | yes | `(js-window-performance)` | read `performance` as `extern`. |
| [`js-window-indexed-db`](https://developer.mozilla.org/en-US/docs/Web/API/Window/indexedDB) | `()` | `(extern)` | no | no | yes | `(js-window-indexed-db)` | read `indexed-db` as `extern`. |
| [`js-window-is-secure-context`](https://developer.mozilla.org/en-US/docs/Web/API/Window/isSecureContext) | `()` | `(i32)` | no | no | no | `(js-window-is-secure-context)` | read `is-secure-context` as `i32`. |
| [`js-window-cross-origin-isolated`](https://developer.mozilla.org/en-US/docs/Web/API/Window/crossOriginIsolated) | `()` | `(i32)` | no | no | no | `(js-window-cross-origin-isolated)` | read `cross-origin-isolated` as `i32`. |
| [`js-window-caches`](https://developer.mozilla.org/en-US/docs/Web/API/Window/caches) | `()` | `(extern)` | no | no | yes | `(js-window-caches)` | read `caches` as `extern`. |
| [`js-window-speech-synthesis`](https://developer.mozilla.org/en-US/docs/Web/API/Window/speechSynthesis) | `()` | `(extern)` | no | no | yes | `(js-window-speech-synthesis)` | read `speech-synthesis` as `extern`. |
| [`js-window-style-media`](https://developer.mozilla.org/en-US/docs/Web/API/Window/styleMedia) | `()` | `(extern)` | no | no | yes | `(js-window-style-media)` | read `style-media` as `extern`. |
| [`js-window-alert`](https://developer.mozilla.org/en-US/docs/Web/API/Window/alert) | `(string)` | `()` | yes | no | no | `(js-window-alert "x")` | call `alert` for side effects. |
| [`js-window-atob`](https://developer.mozilla.org/en-US/docs/Web/API/Window/atob) | `(string)` | `(string)` | no | no | no | `(js-window-atob "x")` | call `atob` and use the `string` result. |
| [`js-window-btoa`](https://developer.mozilla.org/en-US/docs/Web/API/Window/btoa) | `(string)` | `(string)` | no | no | no | `(js-window-btoa "x")` | call `btoa` and use the `string` result. |
| [`js-window-blur`](https://developer.mozilla.org/en-US/docs/Web/API/Window/blur) | `()` | `()` | yes | no | no | `(js-window-blur)` | call `blur` for side effects. |
| [`js-window-cancel-animation-frame`](https://developer.mozilla.org/en-US/docs/Web/API/Window/cancelAnimationFrame) | `(u32)` | `()` | yes | no | no | `(js-window-cancel-animation-frame 0)` | call `cancel-animation-frame` for side effects. |
| [`js-window-cancel-idle-callback`](https://developer.mozilla.org/en-US/docs/Web/API/Window/cancelIdleCallback) | `(u32)` | `()` | yes | yes | no | `(js-window-cancel-idle-callback 0)` | call `cancel-idle-callback` for side effects. |
| [`js-window-clear-interval`](https://developer.mozilla.org/en-US/docs/Web/API/Window/clearInterval) | `(u32)` | `()` | yes | no | no | `(js-window-clear-interval 0)` | call `clear-interval` for side effects. |
| [`js-window-clear-timeout`](https://developer.mozilla.org/en-US/docs/Web/API/Window/clearTimeout) | `(u32)` | `()` | yes | no | no | `(js-window-clear-timeout 0)` | call `clear-timeout` for side effects. |
| [`js-window-close`](https://developer.mozilla.org/en-US/docs/Web/API/Window/close) | `()` | `()` | yes | no | no | `(js-window-close)` | call `close` for side effects. |
| [`js-window-confirm`](https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm) | `(string)` | `(i32)` | no | no | no | `(js-window-confirm "x")` | call `confirm` and use the `i32` result. |
| [`js-window-create-image-bitmap`](https://developer.mozilla.org/en-US/docs/Web/API/Window/createImageBitmap) | `(extern value value value value value)` | `(extern)` | no | no | yes | `(js-window-create-image-bitmap obj (void) (void) (void) (void) (void))` | call `create-image-bitmap` and keep the raw JS result. |
| [`js-window-fetch`](https://developer.mozilla.org/en-US/docs/Web/API/Window/fetch) | `(value value)` | `(extern)` | yes | no | yes | `(js-window-fetch (void) (void))` | call `fetch` and keep the raw JS result. |
| [`js-window-focus`](https://developer.mozilla.org/en-US/docs/Web/API/Window/focus) | `()` | `()` | yes | no | no | `(js-window-focus)` | call `focus` for side effects. |
| [`js-window-get-computed-style`](https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle) | `(extern value)` | `(extern)` | no | no | yes | `(js-window-get-computed-style obj (void))` | call `get-computed-style` and keep the raw JS result. |
| [`js-window-get-selection`](https://developer.mozilla.org/en-US/docs/Web/API/Window/getSelection) | `()` | `(extern)` | no | no | yes | `(js-window-get-selection)` | call `get-selection` and keep the raw JS result. |
| [`js-window-match-media`](https://developer.mozilla.org/en-US/docs/Web/API/Window/matchMedia) | `(string)` | `(extern)` | no | no | yes | `(js-window-match-media "x")` | call `match-media` and keep the raw JS result. |
| [`js-window-move-by`](https://developer.mozilla.org/en-US/docs/Web/API/Window/moveBy) | `(f64 f64)` | `()` | yes | no | no | `(js-window-move-by 0.0 0.0)` | call `move-by` for side effects. |
| [`js-window-move-to`](https://developer.mozilla.org/en-US/docs/Web/API/Window/moveTo) | `(f64 f64)` | `()` | yes | no | no | `(js-window-move-to 0.0 0.0)` | call `move-to` for side effects. |
| [`js-window-open`](https://developer.mozilla.org/en-US/docs/Web/API/Window/open) | `(string value value value)` | `(extern)` | yes | no | yes | `(js-window-open "x" (void) (void) (void))` | call `open` and keep the raw JS result. |
| [`js-window-post-message`](https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage) | `(value string value)` | `()` | yes | no | no | `(js-window-post-message (void) "x" (void))` | call `post-message` for side effects. |
| [`js-window-print`](https://developer.mozilla.org/en-US/docs/Web/API/Window/print) | `()` | `()` | yes | no | no | `(js-window-print)` | call `print` for side effects. |
| [`js-window-prompt`](https://developer.mozilla.org/en-US/docs/Web/API/Window/prompt) | `(string value)` | `(value)` | no | no | yes | `(js-window-prompt "x" (void))` | call `prompt` with JS↔WebRacket value conversion. |
| [`js-window-request-animation-frame`](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame) | `(extern)` | `(u32)` | no | yes | no | `(js-window-request-animation-frame obj)` | call `request-animation-frame` and use the `u32` result. |
| [`js-window-request-idle-callback`](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestIdleCallback) | `(extern value)` | `(u32)` | no | yes | no | `(js-window-request-idle-callback obj (void))` | call `request-idle-callback` and use the `u32` result. |
| [`js-window-resize-by`](https://developer.mozilla.org/en-US/docs/Web/API/Window/resizeBy) | `(f64 f64)` | `()` | yes | no | no | `(js-window-resize-by 0.0 0.0)` | call `resize-by` for side effects. |
| [`js-window-resize-to`](https://developer.mozilla.org/en-US/docs/Web/API/Window/resizeTo) | `(f64 f64)` | `()` | yes | no | no | `(js-window-resize-to 0.0 0.0)` | call `resize-to` for side effects. |
| [`js-window-scroll`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scroll) | `(f64 f64 value)` | `()` | yes | no | no | `(js-window-scroll 0.0 0.0 (void))` | call `scroll` for side effects. |
| [`js-window-scroll-by`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollBy) | `(f64 f64 value)` | `()` | yes | no | no | `(js-window-scroll-by 0.0 0.0 (void))` | call `scroll-by` for side effects. |
| [`js-window-scroll-to`](https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollTo) | `(f64 f64 value)` | `()` | yes | no | no | `(js-window-scroll-to 0.0 0.0 (void))` | call `scroll-to` for side effects. |
| [`js-window-set-interval`](https://developer.mozilla.org/en-US/docs/Web/API/Window/setInterval) | `(extern f64)` | `(u32)` | no | yes | no | `(js-window-set-interval obj 0.0)` | call `set-interval` and use the `u32` result. |
| [`js-window-set-timeout`](https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout) | `(extern)` | `(u32)` | no | yes | no | `(js-window-set-timeout obj)` | call `set-timeout` and use the `u32` result. |
| [`js-window-set-timeout/delay`](https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout) | `(extern f64)` | `(u32)` | no | yes | no | `(js-window-set-timeout/delay obj 0.0)` | call `set-timeout/delay` and use the `u32` result. |
| [`js-window-stop`](https://developer.mozilla.org/en-US/docs/Web/API/Window/stop) | `()` | `()` | yes | no | no | `(js-window-stop)` | call `stop` for side effects. |
| [`js-window-structured-clone`](https://developer.mozilla.org/en-US/docs/Web/API/Window/structuredClone) | `(value value)` | `(value)` | no | no | yes | `(js-window-structured-clone (void) (void))` | call `structured-clone` with JS↔WebRacket value conversion. |
| [`js-window-queue-microtask`](https://developer.mozilla.org/en-US/docs/Web/API/Window/queueMicrotask) | `(extern)` | `()` | yes | yes | no | `(js-window-queue-microtask obj)` | call `queue-microtask` for side effects. |
| [`js-window-report-error`](https://developer.mozilla.org/en-US/docs/Web/API/Window/reportError) | `(value)` | `()` | yes | no | no | `(js-window-report-error (void))` | call `report-error` for side effects. |

Note: Promise-like externals in this table: [`js-window-create-image-bitmap`](https://developer.mozilla.org/en-US/docs/Web/API/Window/createImageBitmap), [`js-window-fetch`](https://developer.mozilla.org/en-US/docs/Web/API/Window/fetch).

### 3.2 Performance Module

Module source in `ffi/dom.ffi`: `#:module "performance"`  
MDN root: [Performance](https://developer.mozilla.org/en-US/docs/Web/API/Performance)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-performance-now`](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now) | `()` | `(f64)` | no | no | no | `(js-performance-now)` | read `now` as `f64`. |

Note: Promise-like externals in this table: none.

## Chapter 4 — DOM Tree and Selectors

### 4.1 Document Module

Module source in `ffi/dom.ffi`: `#:module "document"`  
MDN root: [Document](https://developer.mozilla.org/en-US/docs/Web/API/Document)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-document`](https://developer.mozilla.org/en-US/docs/Web/API/Document) | `()` | `(extern)` | no | no | yes | `(js-document)` | read `document` as `extern`. |
| [`js-document-head`](https://developer.mozilla.org/en-US/docs/Web/API/Document/head) | `()` | `(extern)` | no | no | yes | `(js-document-head)` | read `head` as `extern`. |
| [`js-document-body`](https://developer.mozilla.org/en-US/docs/Web/API/Document/body) | `()` | `(extern)` | no | no | yes | `(js-document-body)` | read `body` as `extern`. |
| [`js-document-element`](https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement) | `()` | `(extern)` | no | no | yes | `(js-document-element)` | read `document-element` as `extern`. |
| [`js-create-element`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement) | `(string)` | `(extern)` | no | no | yes | `(js-create-element "x")` | call `create-element` and keep the raw JS result. |
| [`js-create-text-node`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode) | `(string)` | `(extern)` | no | no | yes | `(js-create-text-node "x")` | call `create-text-node` and keep the raw JS result. |
| [`js-adopt-node`](https://developer.mozilla.org/en-US/docs/Web/API/Document/adoptNode) | `(extern)` | `(extern)` | no | no | yes | `(js-adopt-node obj)` | call `adopt-node` and keep the raw JS result. |
| [`js-caret-range-from-point`](https://developer.mozilla.org/en-US/docs/Web/API/Document/clear) | `()` | `()` | yes | no | no | `(js-caret-range-from-point)` | call `clear` for side effects. |
| [`js-close`](https://developer.mozilla.org/en-US/docs/Web/API/Document/close) | `()` | `()` | yes | no | no | `(js-close)` | call `close` for side effects. |
| [`js-create-attribute`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createAttribute) | `(string)` | `(extern)` | no | no | yes | `(js-create-attribute "x")` | call `create-attribute` and keep the raw JS result. |
| [`js-create-attribute-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createAttributeNS) | `(string string)` | `(extern)` | no | no | yes | `(js-create-attribute-ns "x" "x")` | call `create-attribute-ns` and keep the raw JS result. |
| [`js-create-cdata-section`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createCdataSection) | `(string)` | `(extern)` | no | no | yes | `(js-create-cdata-section "x")` | call `create-cdata-section` and keep the raw JS result. |
| [`js-create-comment`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createComment) | `(string)` | `(extern)` | no | no | yes | `(js-create-comment "x")` | call `create-comment` and keep the raw JS result. |
| [`js-create-document-fragment`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createDocumentFragment) | `()` | `(extern)` | no | no | yes | `(js-create-document-fragment)` | read `create-document-fragment` as `extern`. |
| [`js-create-element-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createEvent) | `(string)` | `(extern)` | no | no | yes | `(js-create-element-ns "x")` | call `create-event` and keep the raw JS result. |
| [`js-create-expression`](https://developer.mozilla.org/en-US/docs/Web/API/XPathEvaluator/createExpression) | `(string extern)` | `(extern)` | no | no | yes | `(js-create-expression "//*" obj)` | call `create-expression` and keep the raw JS result. |
| [`js-create-node-iterator`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createNodeIterator) | `(extern u32 extern)` | `(extern)` | no | no | yes | `(js-create-node-iterator obj 0 obj3)` | call `create-node-iterator` and keep the raw JS result. |
| [`js-create-processing-instruction`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createProcessingInstruction) | `(string string)` | `(extern)` | no | no | yes | `(js-create-processing-instruction "x" "x")` | call `create-processing-instruction` and keep the raw JS result. |
| [`js-create-range`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createRange) | `()` | `(extern)` | no | no | yes | `(js-create-range)` | read `create-range` as `extern`. |
| [`js-create-tree-walker`](https://developer.mozilla.org/en-US/docs/Web/API/Document/createTreeWalker) | `(extern u32 extern)` | `(extern)` | no | no | yes | `(js-create-tree-walker obj 0 obj3)` | call `create-tree-walker` and keep the raw JS result. |
| [`js-element-from-point`](https://developer.mozilla.org/en-US/docs/Web/API/Document/elementFromPoint) | `(f64 f64)` | `(extern)` | no | no | yes | `(js-element-from-point 0.0 0.0)` | call `element-from-point` and keep the raw JS result. |
| [`js-elements-from-point`](https://developer.mozilla.org/en-US/docs/Web/API/Document/elementsFromPoint) | `(f64 f64)` | `(extern)` | no | no | yes | `(js-elements-from-point 0.0 0.0)` | call `elements-from-point` and keep the raw JS result. |
| [`js-enable-style-sheets-for-set`](https://developer.mozilla.org/en-US/docs/Web/API/Document/enableStyleSheetsForSet) | `(string)` | `()` | yes | no | no | `(js-enable-style-sheets-for-set "x")` | call `enable-style-sheets-for-set` for side effects. |
| [`js-evaluate`](https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand) | `(string i32 string)` | `(i32)` | no | no | no | `(js-evaluate "x" 1 "x")` | call `exec-command` and use the `i32` result. |
| [`js-exit-fullscreen`](https://developer.mozilla.org/en-US/docs/Web/API/Document/exitFullscreen) | `()` | `(extern)` | yes | no | yes | `(js-exit-fullscreen)` | read `exit-fullscreen` as `extern`. |
| [`js-exit-picture-in-picture`](https://developer.mozilla.org/en-US/docs/Web/API/Document/exitPictureInPicture) | `()` | `(extern)` | yes | no | yes | `(js-exit-picture-in-picture)` | read `exit-picture-in-picture` as `extern`. |
| [`js-exit-pointer-lock`](https://developer.mozilla.org/en-US/docs/Web/API/Document/exitPointerLock) | `()` | `()` | yes | no | no | `(js-exit-pointer-lock)` | call `exit-pointer-lock` for side effects. |
| [`js-get-element-by-id`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById) | `(string)` | `(extern)` | no | no | yes | `(js-get-element-by-id "root")` | call `get-element-by-id` and keep the raw JS result. |
| [`js-get-elements-by-class-name`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementsByClassName) | `(string)` | `(extern)` | no | no | yes | `(js-get-elements-by-class-name "x")` | call `get-elements-by-class-name` and keep the raw JS result. |
| [`js-get-elements-by-name`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementsByName) | `(string)` | `(extern)` | no | no | yes | `(js-get-elements-by-name "x")` | call `get-elements-by-name` and keep the raw JS result. |
| [`js-get-elements-by-tag-name`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementsByTagName) | `(string)` | `(extern)` | no | no | yes | `(js-get-elements-by-tag-name "x")` | call `get-elements-by-tag-name` and keep the raw JS result. |
| [`js-get-elements-by-tag-name-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementsByTagNameNS) | `(string string)` | `(extern)` | no | no | yes | `(js-get-elements-by-tag-name-ns "x" "x")` | call `get-elements-by-tag-name-ns` and keep the raw JS result. |
| [`js-get-selection`](https://developer.mozilla.org/en-US/docs/Web/API/Document/getSelection) | `()` | `(extern)` | no | no | yes | `(js-get-selection)` | read `get-selection` as `extern`. |
| [`js-has-focus`](https://developer.mozilla.org/en-US/docs/Web/API/Document/hasFocus) | `()` | `(i32)` | no | no | no | `(js-has-focus)` | read `has-focus` as `i32`. |
| [`js-import-node`](https://developer.mozilla.org/en-US/docs/Web/API/Document/importNode) | `(extern i32)` | `(extern)` | no | no | yes | `(js-import-node obj 1)` | call `import-node` and keep the raw JS result. |
| [`js-open`](https://developer.mozilla.org/en-US/docs/Web/API/Document/queryCommandValue) | `(string)` | `(extern)` | no | no | yes | `(js-open "x")` | call `query-command-value` and keep the raw JS result. |
| [`js-query-selector`](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector) | `(string)` | `(extern)` | no | no | yes | `(js-query-selector ".item")` | call `query-selector` and keep the raw JS result. |
| [`js-query-selector-all`](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelectorAll) | `(string)` | `(extern)` | no | no | yes | `(js-query-selector-all ".item")` | call `query-selector-all` and keep the raw JS result. |
| [`js-release-events`](https://developer.mozilla.org/en-US/docs/Web/API/Document/writeln) | `(string)` | `()` | yes | no | no | `(js-release-events "x")` | call `writeln` for side effects. |

Note: Promise-like externals in this table: [`js-exit-fullscreen`](https://developer.mozilla.org/en-US/docs/Web/API/Document/exitFullscreen), [`js-exit-picture-in-picture`](https://developer.mozilla.org/en-US/docs/Web/API/Document/exitPictureInPicture).

### 4.2 Element Module

Module source in `ffi/dom.ffi`: `#:module "element"`  
MDN root: [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-append-child!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/appendChild) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-append-child! obj obj2)` | call `append-child` and keep the raw JS result. |
| [`js-set-attribute!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute) | `(extern string string)` | `()` | yes | no | no | `(js-set-attribute! el "x" "x")` | set `attribute` on a Element value. |
| [`js-after!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/after) | `(extern extern)` | `()` | yes | no | no | `(js-after! obj obj2)` | call `after` for side effects. |
| [`js-animate`](https://developer.mozilla.org/en-US/docs/Web/API/Element/animate) | `(extern extern extern)` | `(extern)` | no | yes | yes | `(js-animate obj obj2 obj3)` | call `animate` and keep the raw JS result. |
| [`js-append!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/append) | `(extern extern)` | `()` | yes | no | no | `(js-append! obj obj2)` | call `append` for side effects. |
| [`js-attach-shadow!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/attachShadow) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-attach-shadow! obj obj2)` | call `attach-shadow` and keep the raw JS result. |
| [`js-before!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/before) | `(extern extern)` | `()` | yes | no | no | `(js-before! obj obj2)` | call `before` for side effects. |
| [`js-closest`](https://developer.mozilla.org/en-US/docs/Web/API/Element/closest) | `(extern string)` | `(extern)` | no | no | yes | `(js-closest obj "x")` | call `closest` and keep the raw JS result. |
| [`js-computed-style-map`](https://developer.mozilla.org/en-US/docs/Web/API/Element/computedStyleMap) | `(extern)` | `(extern)` | no | no | yes | `(js-computed-style-map obj)` | call `computed-style-map` and keep the raw JS result. |
| [`js-get-animations`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAnimations) | `(extern)` | `(extern)` | no | no | yes | `(js-get-animations obj)` | call `get-animations` and keep the raw JS result. |
| [`js-get-attribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute) | `(extern string)` | `(extern)` | no | no | yes | `(js-get-attribute el "x")` | call `get-attribute` and keep the raw JS result. |
| [`js-get-attribute-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNS) | `(extern string string)` | `(extern)` | no | no | yes | `(js-get-attribute-ns el "x" "x")` | call `get-attribute-ns` and keep the raw JS result. |
| [`js-get-attribute-names`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNames) | `(extern)` | `(extern)` | no | no | yes | `(js-get-attribute-names el)` | call `get-attribute-names` and keep the raw JS result. |
| [`js-get-attribute-node`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNode) | `(extern string)` | `(extern)` | no | no | yes | `(js-get-attribute-node el "x")` | call `get-attribute-node` and keep the raw JS result. |
| [`js-get-attribute-node-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNodeNS) | `(extern string string)` | `(extern)` | no | no | yes | `(js-get-attribute-node-ns el "x" "x")` | call `get-attribute-node-ns` and keep the raw JS result. |
| [`js-get-bounding-client-rect`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect) | `(extern)` | `(extern)` | no | no | yes | `(js-get-bounding-client-rect obj)` | call `get-bounding-client-rect` and keep the raw JS result. |
| [`js-get-client-rects`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getClientRects) | `(extern)` | `(extern)` | no | no | yes | `(js-get-client-rects obj)` | call `get-client-rects` and keep the raw JS result. |
| [`js-element-get-elements-by-class-name`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByClassName) | `(extern string)` | `(extern)` | no | no | yes | `(js-element-get-elements-by-class-name el "x")` | call `get-elements-by-class-name` and keep the raw JS result. |
| [`js-element-get-elements-by-tag-name`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByTagName) | `(extern string)` | `(extern)` | no | no | yes | `(js-element-get-elements-by-tag-name el "x")` | call `get-elements-by-tag-name` and keep the raw JS result. |
| [`js-element-get-elements-by-tag-name-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByTagNameNS) | `(extern string string)` | `(extern)` | no | no | yes | `(js-element-get-elements-by-tag-name-ns el "x" "x")` | call `get-elements-by-tag-name-ns` and keep the raw JS result. |
| [`js-has-attribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/hasAttribute) | `(extern string)` | `(i32)` | no | no | no | `(js-has-attribute el "x")` | call `has-attribute` and use the `i32` result. |
| [`js-has-attribute-ns`](https://developer.mozilla.org/en-US/docs/Web/API/Element/hasAttributeNS) | `(extern string string)` | `(i32)` | no | no | no | `(js-has-attribute-ns el "x" "x")` | call `has-attribute-ns` and use the `i32` result. |
| [`js-has-attributes`](https://developer.mozilla.org/en-US/docs/Web/API/Element/hasAttributes) | `(extern)` | `(i32)` | no | no | no | `(js-has-attributes el)` | call `has-attributes` and use the `i32` result. |
| [`js-has-pointer-capture`](https://developer.mozilla.org/en-US/docs/Web/API/Element/hasPointerCapture) | `(extern i32)` | `(i32)` | no | no | no | `(js-has-pointer-capture obj 1)` | call `has-pointer-capture` and use the `i32` result. |
| [`js-insert-adjacent-element!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentElement) | `(extern string extern)` | `(extern)` | yes | no | yes | `(js-insert-adjacent-element! obj "x" obj3)` | call `insert-adjacent-element` and keep the raw JS result. |
| [`js-insert-adjacent-html!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHtml) | `(extern string string)` | `()` | yes | no | no | `(js-insert-adjacent-html! obj "x" "x")` | call `insert-adjacent-html` for side effects. |
| [`js-insert-adjacent-text!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentText) | `(extern string string)` | `()` | yes | no | no | `(js-insert-adjacent-text! obj "x" "x")` | call `insert-adjacent-text` for side effects. |
| [`js-matches`](https://developer.mozilla.org/en-US/docs/Web/API/Element/matches) | `(extern string)` | `(i32)` | no | no | no | `(js-matches obj "x")` | call `matches` and use the `i32` result. |
| [`js-prepend!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/prepend) | `(extern extern)` | `()` | yes | no | no | `(js-prepend! obj obj2)` | call `prepend` for side effects. |
| [`js-element-query-selector`](https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelector) | `(extern string)` | `(extern)` | no | no | yes | `(js-element-query-selector el ".item")` | call `query-selector` and keep the raw JS result. |
| [`js-element-query-selector-all`](https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll) | `(extern string)` | `(extern)` | no | no | yes | `(js-element-query-selector-all el ".item")` | call `query-selector-all` and keep the raw JS result. |
| [`js-release-pointer-capture!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/releasePointerCapture) | `(extern i32)` | `()` | yes | no | no | `(js-release-pointer-capture! obj 1)` | call `release-pointer-capture` for side effects. |
| [`js-remove!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/remove) | `(extern)` | `()` | yes | no | no | `(js-remove! obj)` | call `remove` for side effects. |
| [`js-remove-attribute!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute) | `(extern string)` | `()` | yes | no | no | `(js-remove-attribute! el "x")` | call `remove-attribute` for side effects. |
| [`js-remove-attribute-ns!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttributeNS) | `(extern string string)` | `()` | yes | no | no | `(js-remove-attribute-ns! el "x" "x")` | call `remove-attribute-ns` for side effects. |
| [`js-remove-attribute-node!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttributeNode) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-remove-attribute-node! el obj2)` | call `remove-attribute-node` and keep the raw JS result. |
| [`js-replace-children!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/replaceChildren) | `(extern extern)` | `()` | yes | no | no | `(js-replace-children! obj obj2)` | call `replace-children` for side effects. |
| [`js-replace-with!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/replaceWith) | `(extern extern)` | `()` | yes | no | no | `(js-replace-with! obj obj2)` | call `replace-with` for side effects. |
| [`js-request-fullscreen`](https://developer.mozilla.org/en-US/docs/Web/API/Element/requestFullscreen) | `(extern)` | `(extern)` | no | no | yes | `(js-request-fullscreen obj)` | call `request-fullscreen` and keep the raw JS result. |
| [`js-request-pointer-lock`](https://developer.mozilla.org/en-US/docs/Web/API/Element/requestPointerLock) | `(extern)` | `()` | yes | no | no | `(js-request-pointer-lock obj)` | call `request-pointer-lock` for side effects. |
| [`js-scroll!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scroll) | `(extern f64 f64)` | `()` | yes | no | no | `(js-scroll! obj 0.0 0.0)` | call `scroll` for side effects. |
| [`js-scroll-by!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollBy) | `(extern f64 f64)` | `()` | yes | no | no | `(js-scroll-by! obj 0.0 0.0)` | call `scroll-by` for side effects. |
| [`js-scroll-into-view!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView) | `(extern i32)` | `()` | yes | no | no | `(js-scroll-into-view! obj 1)` | call `scroll-into-view` for side effects. |
| [`js-scroll-to!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTo) | `(extern f64 f64)` | `()` | yes | no | no | `(js-scroll-to! obj 0.0 0.0)` | call `scroll-to` for side effects. |
| [`js-set-attribute-ns!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNS) | `(extern string string string)` | `()` | yes | no | no | `(js-set-attribute-ns! el "x" "x" "x")` | set `attribute-ns` on a Element value. |
| [`js-set-attribute-node!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNode) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-set-attribute-node! el obj2)` | set `attribute-node` on a Element value. |
| [`js-set-attribute-node-ns!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNodeNS) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-set-attribute-node-ns! el obj2)` | set `attribute-node-ns` on a Element value. |
| [`js-set-pointer-capture!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setPointerCapture) | `(extern i32)` | `()` | yes | no | no | `(js-set-pointer-capture! obj 1)` | set `pointer-capture` on a Element value. |
| [`js-toggle-attribute!`](https://developer.mozilla.org/en-US/docs/Web/API/Element/toggleAttribute) | `(extern string i32)` | `(i32)` | yes | no | no | `(js-toggle-attribute! el "x" 1)` | call `toggle-attribute` and use the `i32` result. |

Note: Promise-like externals in this table: [`js-request-fullscreen`](https://developer.mozilla.org/en-US/docs/Web/API/Element/requestFullscreen).

## Chapter 5 — Events

### 5.1 EventTarget Module

Module source in `ffi/dom.ffi`: `#:module "event-target"`  
MDN root: [EventTarget](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-add-event-listener!`](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener) | `(extern string extern)` | `()` | yes | yes | no | `(js-add-event-listener! obj "click" obj3)` | call `add-event-listener` for side effects. |

Note: Promise-like externals in this table: none.

### 5.2 Event Module

Module source in `ffi/dom.ffi`: `#:module "event"`  
MDN root: [Event](https://developer.mozilla.org/en-US/docs/Web/API/Event)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-event-new`](https://developer.mozilla.org/en-US/docs/Web/API/Event/Event) | `(string value)` | `(extern)` | no | no | yes | `(js-event-new "click" (void))` | call `new` and keep the raw JS result. |
| [`js-event-type`](https://developer.mozilla.org/en-US/docs/Web/API/Event/type) | `(extern)` | `(string)` | no | no | no | `(js-event-type evt)` | read `type` as `string`. |
| [`js-event-target`](https://developer.mozilla.org/en-US/docs/Web/API/Event/target) | `(extern)` | `(extern)` | no | no | yes | `(js-event-target evt)` | read `target` as `extern`. |
| [`js-event-current-target`](https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget) | `(extern)` | `(extern)` | no | no | yes | `(js-event-current-target evt)` | read `current-target` as `extern`. |
| [`js-event-event-phase`](https://developer.mozilla.org/en-US/docs/Web/API/Event/eventPhase) | `(extern)` | `(u32)` | no | no | no | `(js-event-event-phase evt)` | read `event-phase` as `u32`. |
| [`js-event-bubbles`](https://developer.mozilla.org/en-US/docs/Web/API/Event/bubbles) | `(extern)` | `(i32)` | no | no | no | `(js-event-bubbles evt)` | read `bubbles` as `i32`. |
| [`js-event-cancelable`](https://developer.mozilla.org/en-US/docs/Web/API/Event/cancelable) | `(extern)` | `(i32)` | no | no | no | `(js-event-cancelable evt)` | read `cancelable` as `i32`. |
| [`js-event-default-prevented`](https://developer.mozilla.org/en-US/docs/Web/API/Event/defaultPrevented) | `(extern)` | `(i32)` | no | no | no | `(js-event-default-prevented evt)` | read `default-prevented` as `i32`. |
| [`js-event-composed`](https://developer.mozilla.org/en-US/docs/Web/API/Event/composed) | `(extern)` | `(i32)` | no | no | no | `(js-event-composed evt)` | read `composed` as `i32`. |
| [`js-event-is-trusted`](https://developer.mozilla.org/en-US/docs/Web/API/Event/isTrusted) | `(extern)` | `(i32)` | no | no | no | `(js-event-is-trusted evt)` | read `is-trusted` as `i32`. |
| [`js-event-time-stamp`](https://developer.mozilla.org/en-US/docs/Web/API/Event/timeStamp) | `(extern)` | `(f64)` | no | no | no | `(js-event-time-stamp evt)` | read `time-stamp` as `f64`. |
| [`js-event-composed-path`](https://developer.mozilla.org/en-US/docs/Web/API/Event/composedPath) | `(extern)` | `(extern)` | no | no | yes | `(js-event-composed-path evt)` | call `composed-path` and keep the raw JS result. |
| [`js-event-prevent-default`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault) | `(extern)` | `()` | yes | no | no | `(js-event-prevent-default evt)` | call `prevent-default` for side effects. |
| [`js-event-stop-propagation`](https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation) | `(extern)` | `()` | yes | no | no | `(js-event-stop-propagation evt)` | call `stop-propagation` for side effects. |
| [`js-event-stop-immediate-propagation`](https://developer.mozilla.org/en-US/docs/Web/API/Event/stopImmediatePropagation) | `(extern)` | `()` | yes | no | no | `(js-event-stop-immediate-propagation evt)` | call `stop-immediate-propagation` for side effects. |

Note: Promise-like externals in this table: none.

## Chapter 6 — Canvas

### 6.1 HTMLCanvasElement Module

Module source in `ffi/dom.ffi`: `#:module "canvas"`  
MDN root: [HTMLCanvasElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-canvas-capture-stream`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/captureStream) | `(extern f64)` | `(extern)` | no | no | yes | `(js-canvas-capture-stream canvas 0.0)` | call `capture-stream` and keep the raw JS result. |
| [`js-canvas-get-context`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext) | `(extern string extern)` | `(extern)` | no | no | yes | `(js-canvas-get-context canvas "x" obj3)` | call `get-context` and keep the raw JS result. |
| [`js-canvas-height`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/height) | `(extern)` | `(u32)` | no | no | no | `(js-canvas-height canvas)` | call `height` and use the `u32` result. |
| [`js-set-canvas-height!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/height) | `(extern u32)` | `()` | yes | no | no | `(js-set-canvas-height! canvas 0)` | set `height` on a HTMLCanvasElement value. |
| [`js-canvas-to-blob`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/toBlob) | `(extern extern string f64)` | `()` | yes | no | no | `(js-canvas-to-blob canvas obj2 "x" 0.0)` | call `to-blob` for side effects. |
| [`js-canvas-to-data-url`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/toDataUrl) | `(extern string f64)` | `(string)` | no | no | no | `(js-canvas-to-data-url canvas "x" 0.0)` | call `to-data-url` and use the `string` result. |
| [`js-canvas-transfer-control-to-offscreen`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/transferControlToOffscreen) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas-transfer-control-to-offscreen canvas)` | call `transfer-control-to-offscreen` and keep the raw JS result. |
| [`js-canvas-width`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/width) | `(extern)` | `(u32)` | no | no | no | `(js-canvas-width canvas)` | call `width` and use the `u32` result. |
| [`js-set-canvas-width!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/width) | `(extern u32)` | `()` | yes | no | no | `(js-set-canvas-width! canvas 0)` | set `width` on a HTMLCanvasElement value. |

Note: Promise-like externals in this table: none.

### 6.2 CanvasRenderingContext2D Module

Module source in `ffi/dom.ffi`: `#:module "canvas-rendering-context-2d"`  
MDN root: [CanvasRenderingContext2D](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-canvas2d-canvas`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/canvas) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas2d-canvas ctx)` | call `canvas` and keep the raw JS result. |
| [`js-canvas2d-direction`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/direction) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-direction ctx)` | call `direction` and use the `string` result. |
| [`js-set-canvas2d-direction!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/direction) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-direction! ctx "x")` | set `direction` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-fill-style`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillStyle) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas2d-fill-style ctx)` | call `fill-style` and keep the raw JS result. |
| [`js-set-canvas2d-fill-style!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillStyle) | `(extern value)` | `()` | yes | no | no | `(js-set-canvas2d-fill-style! ctx (void))` | set `fill-style` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-filter`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-filter ctx)` | call `filter` and use the `string` result. |
| [`js-set-canvas2d-filter!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-filter! ctx "x")` | set `filter` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/font) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font ctx)` | call `font` and use the `string` result. |
| [`js-set-canvas2d-font!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/font) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font! ctx "x")` | set `font` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-global-alpha`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalAlpha) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-global-alpha ctx)` | call `global-alpha` and use the `f64` result. |
| [`js-set-canvas2d-global-alpha!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalAlpha) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-global-alpha! ctx 0.0)` | set `global-alpha` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-global-composite-operation`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-global-composite-operation ctx)` | call `global-composite-operation` and use the `string` result. |
| [`js-set-canvas2d-global-composite-operation!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-global-composite-operation! ctx "x")` | set `global-composite-operation` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-image-smoothing-enabled`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/imageSmoothingEnabled) | `(extern)` | `(i32)` | no | no | no | `(js-canvas2d-image-smoothing-enabled ctx)` | call `image-smoothing-enabled` and use the `i32` result. |
| [`js-set-canvas2d-image-smoothing-enabled!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/imageSmoothingEnabled) | `(extern i32)` | `()` | yes | no | no | `(js-set-canvas2d-image-smoothing-enabled! ctx 1)` | set `image-smoothing-enabled` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-image-smoothing-quality`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/imageSmoothingQuality) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-image-smoothing-quality ctx)` | call `image-smoothing-quality` and use the `string` result. |
| [`js-set-canvas2d-image-smoothing-quality!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/imageSmoothingQuality) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-image-smoothing-quality! ctx "x")` | set `image-smoothing-quality` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-line-cap`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineCap) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-line-cap ctx)` | call `line-cap` and use the `string` result. |
| [`js-set-canvas2d-line-cap!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineCap) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-line-cap! ctx "x")` | set `line-cap` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-line-dash-offset`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-line-dash-offset ctx)` | call `line-dash-offset` and use the `f64` result. |
| [`js-set-canvas2d-line-dash-offset!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-line-dash-offset! ctx 0.0)` | set `line-dash-offset` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-line-join`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-line-join ctx)` | call `line-join` and use the `string` result. |
| [`js-set-canvas2d-line-join!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-line-join! ctx "x")` | set `line-join` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-line-width`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineWidth) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-line-width ctx)` | call `line-width` and use the `f64` result. |
| [`js-set-canvas2d-line-width!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineWidth) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-line-width! ctx 0.0)` | set `line-width` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-miter-limit`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/miterLimit) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-miter-limit ctx)` | call `miter-limit` and use the `f64` result. |
| [`js-set-canvas2d-miter-limit!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/miterLimit) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-miter-limit! ctx 0.0)` | set `miter-limit` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-shadow-blur`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowBlur) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-shadow-blur ctx)` | call `shadow-blur` and use the `f64` result. |
| [`js-set-canvas2d-shadow-blur!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowBlur) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-shadow-blur! ctx 0.0)` | set `shadow-blur` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-shadow-color`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowColor) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-shadow-color ctx)` | call `shadow-color` and use the `string` result. |
| [`js-set-canvas2d-shadow-color!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowColor) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-shadow-color! ctx "x")` | set `shadow-color` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-shadow-offset-x`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowOffsetX) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-shadow-offset-x ctx)` | call `shadow-offset-x` and use the `f64` result. |
| [`js-set-canvas2d-shadow-offset-x!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowOffsetX) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-shadow-offset-x! ctx 0.0)` | set `shadow-offset-x` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-shadow-offset-y`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowOffsetY) | `(extern)` | `(f64)` | no | no | no | `(js-canvas2d-shadow-offset-y ctx)` | call `shadow-offset-y` and use the `f64` result. |
| [`js-set-canvas2d-shadow-offset-y!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/shadowOffsetY) | `(extern f64)` | `()` | yes | no | no | `(js-set-canvas2d-shadow-offset-y! ctx 0.0)` | set `shadow-offset-y` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-stroke-style`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeStyle) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas2d-stroke-style ctx)` | call `stroke-style` and keep the raw JS result. |
| [`js-set-canvas2d-stroke-style!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeStyle) | `(extern value)` | `()` | yes | no | no | `(js-set-canvas2d-stroke-style! ctx (void))` | set `stroke-style` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-text-align`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textAlign) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-text-align ctx)` | call `text-align` and use the `string` result. |
| [`js-set-canvas2d-text-align!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textAlign) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-text-align! ctx "x")` | set `text-align` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-text-baseline`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textBaseline) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-text-baseline ctx)` | call `text-baseline` and use the `string` result. |
| [`js-set-canvas2d-text-baseline!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textBaseline) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-text-baseline! ctx "x")` | set `text-baseline` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-text-rendering`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textRendering) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-text-rendering ctx)` | call `text-rendering` and use the `string` result. |
| [`js-set-canvas2d-text-rendering!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textRendering) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-text-rendering! ctx "x")` | set `text-rendering` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font-kerning`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontKerning) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font-kerning ctx)` | call `font-kerning` and use the `string` result. |
| [`js-set-canvas2d-font-kerning!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontKerning) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font-kerning! ctx "x")` | set `font-kerning` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font-stretch`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontStretch) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font-stretch ctx)` | call `font-stretch` and use the `string` result. |
| [`js-set-canvas2d-font-stretch!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontStretch) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font-stretch! ctx "x")` | set `font-stretch` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font-variant-caps`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantCaps) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font-variant-caps ctx)` | call `font-variant-caps` and use the `string` result. |
| [`js-set-canvas2d-font-variant-caps!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantCaps) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font-variant-caps! ctx "x")` | set `font-variant-caps` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font-variant-ligatures`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantLigatures) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font-variant-ligatures ctx)` | call `font-variant-ligatures` and use the `string` result. |
| [`js-set-canvas2d-font-variant-ligatures!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantLigatures) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font-variant-ligatures! ctx "x")` | set `font-variant-ligatures` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-font-variant-numeric`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantNumeric) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-font-variant-numeric ctx)` | call `font-variant-numeric` and use the `string` result. |
| [`js-set-canvas2d-font-variant-numeric!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fontVariantNumeric) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-font-variant-numeric! ctx "x")` | set `font-variant-numeric` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-letter-spacing`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/letterSpacing) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-letter-spacing ctx)` | call `letter-spacing` and use the `string` result. |
| [`js-set-canvas2d-letter-spacing!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/letterSpacing) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-letter-spacing! ctx "x")` | set `letter-spacing` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-word-spacing`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/wordSpacing) | `(extern)` | `(string)` | no | no | no | `(js-canvas2d-word-spacing ctx)` | call `word-spacing` and use the `string` result. |
| [`js-set-canvas2d-word-spacing!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/wordSpacing) | `(extern string)` | `()` | yes | no | no | `(js-set-canvas2d-word-spacing! ctx "x")` | set `word-spacing` on a CanvasRenderingContext2D value. |
| [`js-canvas2d-arc`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/arc) | `(extern f64 f64 f64 f64 f64 i32)` | `()` | yes | no | no | `(js-canvas2d-arc ctx 0.0 0.0 0.0 0.0 0.0 1)` | call `arc` for side effects. |
| [`js-canvas2d-arc-to`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/arcTo) | `(extern f64 f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-arc-to ctx 0.0 0.0 0.0 0.0 0.0)` | call `arc-to` for side effects. |
| [`js-canvas2d-begin-path`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/beginPath) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-begin-path ctx)` | call `begin-path` for side effects. |
| [`js-canvas2d-bezier-curve-to`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/bezierCurveTo) | `(extern f64 f64 f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-bezier-curve-to ctx 0.0 0.0 0.0 0.0 0.0 0.0)` | call `bezier-curve-to` for side effects. |
| [`js-canvas2d-clear-rect`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/clearRect) | `(extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-clear-rect ctx 0.0 0.0 0.0 0.0)` | call `clear-rect` for side effects. |
| [`js-canvas2d-clip`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/clip) | `(extern value value)` | `()` | yes | no | no | `(js-canvas2d-clip ctx (void) (void))` | call `clip` for side effects. |
| [`js-canvas2d-close-path`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/closePath) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-close-path ctx)` | call `close-path` for side effects. |
| [`js-canvas2d-create-image-data`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createImageData) | `(extern f64 f64)` | `(extern)` | no | no | yes | `(js-canvas2d-create-image-data ctx 0.0 0.0)` | call `create-image-data` and keep the raw JS result. |
| [`js-canvas2d-create-image-data-from`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createImageData) | `(extern extern)` | `(extern)` | no | no | yes | `(js-canvas2d-create-image-data-from ctx obj2)` | call `create-image-data-from` and keep the raw JS result. |
| [`js-canvas2d-create-linear-gradient`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createLinearGradient) | `(extern f64 f64 f64 f64)` | `(extern)` | no | no | yes | `(js-canvas2d-create-linear-gradient ctx 0.0 0.0 0.0 0.0)` | call `create-linear-gradient` and keep the raw JS result. |
| [`js-canvas2d-create-pattern`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createPattern) | `(extern extern string)` | `(extern)` | no | no | yes | `(js-canvas2d-create-pattern ctx obj2 "x")` | call `create-pattern` and keep the raw JS result. |
| [`js-canvas2d-create-radial-gradient`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createRadialGradient) | `(extern f64 f64 f64 f64 f64 f64)` | `(extern)` | no | no | yes | `(js-canvas2d-create-radial-gradient ctx 0.0 0.0 0.0 0.0 0.0 0.0)` | call `create-radial-gradient` and keep the raw JS result. |
| [`js-canvas2d-create-conic-gradient`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createConicGradient) | `(extern f64 f64 f64)` | `(extern)` | no | no | yes | `(js-canvas2d-create-conic-gradient ctx 0.0 0.0 0.0)` | call `create-conic-gradient` and keep the raw JS result. |
| [`js-canvas2d-draw-focus-if-needed!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawFocusIfNeeded) | `(extern extern)` | `()` | yes | no | no | `(js-canvas2d-draw-focus-if-needed! ctx obj2)` | call `draw-focus-if-needed` for side effects. |
| [`js-canvas2d-draw-focus-if-needed-path!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawFocusIfNeeded) | `(extern extern extern)` | `()` | yes | no | no | `(js-canvas2d-draw-focus-if-needed-path! ctx obj2 obj3)` | call `draw-focus-if-needed-path` for side effects. |
| [`js-canvas2d-draw-image`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawImage) | `(extern extern f64 f64)` | `()` | yes | no | no | `(js-canvas2d-draw-image ctx obj2 0.0 0.0)` | call `draw-image` for side effects. |
| [`js-canvas2d-draw-image-5`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawImage) | `(extern extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-draw-image-5 ctx obj2 0.0 0.0 0.0 0.0)` | call `draw-image` for side effects. |
| [`js-canvas2d-draw-image-9`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawImage) | `(extern extern f64 f64 f64 f64 f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-draw-image-9 ctx obj2 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)` | call `draw-image` for side effects. |
| [`js-canvas2d-ellipse`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/ellipse) | `(extern f64 f64 f64 f64 f64 f64 f64 i32)` | `()` | yes | no | no | `(js-canvas2d-ellipse ctx 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1)` | call `ellipse` for side effects. |
| [`js-canvas2d-fill`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fill) | `(extern value value)` | `()` | yes | no | no | `(js-canvas2d-fill ctx (void) (void))` | call `fill` for side effects. |
| [`js-canvas2d-fill-rect`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillRect) | `(extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-fill-rect ctx 0.0 0.0 0.0 0.0)` | call `fill-rect` for side effects. |
| [`js-canvas2d-fill-text`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillText) | `(extern string f64 f64 value)` | `()` | yes | no | no | `(js-canvas2d-fill-text ctx "x" 0.0 0.0 (void))` | call `fill-text` for side effects. |
| [`js-canvas2d-get-image-data`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getImageData) | `(extern f64 f64 f64 f64 value)` | `(extern)` | no | no | yes | `(js-canvas2d-get-image-data ctx 0.0 0.0 0.0 0.0 (void))` | call `get-image-data` and keep the raw JS result. |
| [`js-canvas2d-get-line-dash`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getLineDash) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas2d-get-line-dash ctx)` | call `get-line-dash` and keep the raw JS result. |
| [`js-canvas2d-get-transform`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getTransform) | `(extern)` | `(extern)` | no | no | yes | `(js-canvas2d-get-transform ctx)` | call `get-transform` and keep the raw JS result. |
| [`js-canvas2d-is-point-in-path`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/isPointInPath) | `(extern value f64 f64 value)` | `(i32)` | no | no | no | `(js-canvas2d-is-point-in-path ctx (void) 0.0 0.0 (void))` | call `is-point-in-path` and use the `i32` result. |
| [`js-canvas2d-is-point-in-stroke`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/isPointInStroke) | `(extern value f64 f64)` | `(i32)` | no | no | no | `(js-canvas2d-is-point-in-stroke ctx (void) 0.0 0.0)` | call `is-point-in-stroke` and use the `i32` result. |
| [`js-canvas2d-line-to`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineTo) | `(extern f64 f64)` | `()` | yes | no | no | `(js-canvas2d-line-to ctx 0.0 0.0)` | call `line-to` for side effects. |
| [`js-canvas2d-measure-text`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/measureText) | `(extern string)` | `(extern)` | no | no | yes | `(js-canvas2d-measure-text ctx "x")` | call `measure-text` and keep the raw JS result. |
| [`js-canvas2d-move-to`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/moveTo) | `(extern f64 f64)` | `()` | yes | no | no | `(js-canvas2d-move-to ctx 0.0 0.0)` | call `move-to` for side effects. |
| [`js-canvas2d-put-image-data`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/putImageData) | `(extern extern f64 f64 value value value value)` | `()` | yes | no | no | `(js-canvas2d-put-image-data ctx obj2 0.0 0.0 (void) (void) (void) (void))` | call `put-image-data` for side effects. |
| [`js-canvas2d-quadratic-curve-to`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/quadraticCurveTo) | `(extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-quadratic-curve-to ctx 0.0 0.0 0.0 0.0)` | call `quadratic-curve-to` for side effects. |
| [`js-canvas2d-rect`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/rect) | `(extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-rect ctx 0.0 0.0 0.0 0.0)` | call `rect` for side effects. |
| [`js-canvas2d-reset`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/reset) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-reset ctx)` | call `reset` for side effects. |
| [`js-canvas2d-reset-transform`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/resetTransform) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-reset-transform ctx)` | call `reset-transform` for side effects. |
| [`js-canvas2d-restore`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/restore) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-restore ctx)` | call `restore` for side effects. |
| [`js-canvas2d-rotate`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/rotate) | `(extern f64)` | `()` | yes | no | no | `(js-canvas2d-rotate ctx 0.0)` | call `rotate` for side effects. |
| [`js-canvas2d-round-rect`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/roundRect) | `(extern f64 f64 f64 f64 value)` | `()` | yes | no | no | `(js-canvas2d-round-rect ctx 0.0 0.0 0.0 0.0 (void))` | call `round-rect` for side effects. |
| [`js-canvas2d-save`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/save) | `(extern)` | `()` | yes | no | no | `(js-canvas2d-save ctx)` | call `save` for side effects. |
| [`js-canvas2d-scale`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/scale) | `(extern f64 f64)` | `()` | yes | no | no | `(js-canvas2d-scale ctx 0.0 0.0)` | call `scale` for side effects. |
| [`js-canvas2d-set-line-dash`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash) | `(extern extern)` | `()` | yes | no | no | `(js-canvas2d-set-line-dash ctx obj2)` | call `set-line-dash` for side effects. |
| [`js-canvas2d-set-transform!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setTransform) | `(extern f64 f64 f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-set-transform! ctx 0.0 0.0 0.0 0.0 0.0 0.0)` | call `transform` for side effects. |
| [`js-canvas2d-set-transform-matrix!`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setTransform) | `(extern extern)` | `()` | yes | no | no | `(js-canvas2d-set-transform-matrix! ctx obj2)` | call `transform-matrix` for side effects. |
| [`js-canvas2d-stroke`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/stroke) | `(extern value)` | `()` | yes | no | no | `(js-canvas2d-stroke ctx (void))` | call `stroke` for side effects. |
| [`js-canvas2d-stroke-rect`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeRect) | `(extern f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-stroke-rect ctx 0.0 0.0 0.0 0.0)` | call `stroke-rect` for side effects. |
| [`js-canvas2d-stroke-text`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeText) | `(extern string f64 f64 value)` | `()` | yes | no | no | `(js-canvas2d-stroke-text ctx "x" 0.0 0.0 (void))` | call `stroke-text` for side effects. |
| [`js-canvas2d-transform`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/transform) | `(extern f64 f64 f64 f64 f64 f64)` | `()` | yes | no | no | `(js-canvas2d-transform ctx 0.0 0.0 0.0 0.0 0.0 0.0)` | call `transform` for side effects. |
| [`js-canvas2d-translate`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/translate) | `(extern f64 f64)` | `()` | yes | no | no | `(js-canvas2d-translate ctx 0.0 0.0)` | call `translate` for side effects. |

Note: Promise-like externals in this table: none.

## Chapter 7 — Media and Images

### 7.1 HTMLMediaElement Module

Module source in `ffi/dom.ffi`: `#:module "media"`  
MDN root: [HTMLMediaElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-media-audio-tracks`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/audioTracks) | `(extern)` | `(extern)` | no | no | yes | `(js-media-audio-tracks media)` | read `audio-tracks` as `extern`. |
| [`js-media-autoplay`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay) | `(extern)` | `(i32)` | no | no | no | `(js-media-autoplay media)` | read `autoplay` as `i32`. |
| [`js-set-media-autoplay!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-autoplay! media 1)` | set `autoplay` on a HTMLMediaElement value. |
| [`js-media-buffered`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/buffered) | `(extern)` | `(extern)` | no | no | yes | `(js-media-buffered media)` | read `buffered` as `extern`. |
| [`js-media-controls`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls) | `(extern)` | `(i32)` | no | no | no | `(js-media-controls media)` | read `controls` as `i32`. |
| [`js-set-media-controls!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-controls! media 1)` | set `controls` on a HTMLMediaElement value. |
| [`js-media-controls-list`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controlsList) | `(extern)` | `(extern)` | no | no | yes | `(js-media-controls-list media)` | read `controls-list` as `extern`. |
| [`js-media-cross-origin`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/crossOrigin) | `(extern)` | `(value)` | no | no | yes | `(js-media-cross-origin media)` | read `cross-origin` as `value`. |
| [`js-set-media-cross-origin!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/crossOrigin) | `(extern value)` | `()` | yes | no | no | `(js-set-media-cross-origin! media (void))` | set `cross-origin` on a HTMLMediaElement value. |
| [`js-media-current-src`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentSrc) | `(extern)` | `(string)` | no | no | no | `(js-media-current-src media)` | read `current-src` as `string`. |
| [`js-media-current-time`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime) | `(extern)` | `(f64)` | no | no | no | `(js-media-current-time media)` | read `current-time` as `f64`. |
| [`js-set-media-current-time!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime) | `(extern f64)` | `()` | yes | no | no | `(js-set-media-current-time! media 0.0)` | set `current-time` on a HTMLMediaElement value. |
| [`js-media-default-muted`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultMuted) | `(extern)` | `(i32)` | no | no | no | `(js-media-default-muted media)` | read `default-muted` as `i32`. |
| [`js-set-media-default-muted!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultMuted) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-default-muted! media 1)` | set `default-muted` on a HTMLMediaElement value. |
| [`js-media-default-playback-rate`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultPlaybackRate) | `(extern)` | `(f64)` | no | no | no | `(js-media-default-playback-rate media)` | read `default-playback-rate` as `f64`. |
| [`js-set-media-default-playback-rate!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultPlaybackRate) | `(extern f64)` | `()` | yes | no | no | `(js-set-media-default-playback-rate! media 0.0)` | set `default-playback-rate` on a HTMLMediaElement value. |
| [`js-media-disable-remote-playback`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/disableRemotePlayback) | `(extern)` | `(i32)` | no | no | no | `(js-media-disable-remote-playback media)` | read `disable-remote-playback` as `i32`. |
| [`js-set-media-disable-remote-playback!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/disableRemotePlayback) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-disable-remote-playback! media 1)` | set `disable-remote-playback` on a HTMLMediaElement value. |
| [`js-media-duration`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/duration) | `(extern)` | `(f64)` | no | no | no | `(js-media-duration media)` | read `duration` as `f64`. |
| [`js-media-ended`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/ended) | `(extern)` | `(i32)` | no | no | no | `(js-media-ended media)` | read `ended` as `i32`. |
| [`js-media-error`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/error) | `(extern)` | `(extern)` | no | no | yes | `(js-media-error media)` | read `error` as `extern`. |
| [`js-media-loop`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/loop) | `(extern)` | `(i32)` | no | no | no | `(js-media-loop media)` | read `loop` as `i32`. |
| [`js-set-media-loop!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/loop) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-loop! media 1)` | set `loop` on a HTMLMediaElement value. |
| [`js-media-media-keys`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaKeys) | `(extern)` | `(extern)` | no | no | yes | `(js-media-media-keys media)` | read `media-keys` as `extern`. |
| [`js-media-media-group`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup) | `(extern)` | `(string)` | no | no | no | `(js-media-media-group media)` | read `media-group` as `string`. |
| [`js-set-media-media-group!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup) | `(extern string)` | `()` | yes | no | no | `(js-set-media-media-group! media "x")` | set `media-group` on a HTMLMediaElement value. |
| [`js-media-muted`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/muted) | `(extern)` | `(i32)` | no | no | no | `(js-media-muted media)` | read `muted` as `i32`. |
| [`js-set-media-muted!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/muted) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-muted! media 1)` | set `muted` on a HTMLMediaElement value. |
| [`js-media-network-state`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/networkState) | `(extern)` | `(u32)` | no | no | no | `(js-media-network-state media)` | read `network-state` as `u32`. |
| [`js-media-paused`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/paused) | `(extern)` | `(i32)` | no | no | no | `(js-media-paused media)` | read `paused` as `i32`. |
| [`js-media-playback-rate`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/playbackRate) | `(extern)` | `(f64)` | no | no | no | `(js-media-playback-rate media)` | read `playback-rate` as `f64`. |
| [`js-set-media-playback-rate!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/playbackRate) | `(extern f64)` | `()` | yes | no | no | `(js-set-media-playback-rate! media 0.0)` | set `playback-rate` on a HTMLMediaElement value. |
| [`js-media-played`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/played) | `(extern)` | `(extern)` | no | no | yes | `(js-media-played media)` | read `played` as `extern`. |
| [`js-media-preload`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload) | `(extern)` | `(string)` | no | no | no | `(js-media-preload media)` | read `preload` as `string`. |
| [`js-set-media-preload!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload) | `(extern string)` | `()` | yes | no | no | `(js-set-media-preload! media "x")` | set `preload` on a HTMLMediaElement value. |
| [`js-media-preserves-pitch`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preservesPitch) | `(extern)` | `(i32)` | no | no | no | `(js-media-preserves-pitch media)` | read `preserves-pitch` as `i32`. |
| [`js-set-media-preserves-pitch!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preservesPitch) | `(extern i32)` | `()` | yes | no | no | `(js-set-media-preserves-pitch! media 1)` | set `preserves-pitch` on a HTMLMediaElement value. |
| [`js-media-ready-state`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/readyState) | `(extern)` | `(u32)` | no | no | no | `(js-media-ready-state media)` | read `ready-state` as `u32`. |
| [`js-media-seekable`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/seekable) | `(extern)` | `(extern)` | no | no | yes | `(js-media-seekable media)` | read `seekable` as `extern`. |
| [`js-media-seeking`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/seeking) | `(extern)` | `(i32)` | no | no | no | `(js-media-seeking media)` | read `seeking` as `i32`. |
| [`js-media-sink-id`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/sinkId) | `(extern)` | `(string)` | no | no | no | `(js-media-sink-id media)` | read `sink-id` as `string`. |
| [`js-media-src`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src) | `(extern)` | `(string)` | no | no | no | `(js-media-src media)` | read `src` as `string`. |
| [`js-set-media-src!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src) | `(extern string)` | `()` | yes | no | no | `(js-set-media-src! media "/img.png")` | set `src` on a HTMLMediaElement value. |
| [`js-media-src-object`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/srcObject) | `(extern)` | `(extern)` | no | no | yes | `(js-media-src-object media)` | read `src-object` as `extern`. |
| [`js-set-media-src-object!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/srcObject) | `(extern extern)` | `()` | yes | no | no | `(js-set-media-src-object! media obj2)` | set `src-object` on a HTMLMediaElement value. |
| [`js-media-text-tracks`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/textTracks) | `(extern)` | `(extern)` | no | no | yes | `(js-media-text-tracks media)` | read `text-tracks` as `extern`. |
| [`js-media-video-tracks`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/videoTracks) | `(extern)` | `(extern)` | no | no | yes | `(js-media-video-tracks media)` | read `video-tracks` as `extern`. |
| [`js-media-volume`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/volume) | `(extern)` | `(f64)` | no | no | no | `(js-media-volume media)` | read `volume` as `f64`. |
| [`js-set-media-volume!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/volume) | `(extern f64)` | `()` | yes | no | no | `(js-set-media-volume! media 0.0)` | set `volume` on a HTMLMediaElement value. |
| [`js-media-add-text-track!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/addTextTrack) | `(extern string value value)` | `(extern)` | yes | no | yes | `(js-media-add-text-track! media "x" (void) (void))` | call `add-text-track` and keep the raw JS result. |
| [`js-media-can-play-type`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/canPlayType) | `(extern string)` | `(string)` | no | no | no | `(js-media-can-play-type media "x")` | call `can-play-type` and use the `string` result. |
| [`js-media-capture-stream`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/captureStream) | `(extern value)` | `(extern)` | no | no | yes | `(js-media-capture-stream media (void))` | call `capture-stream` and keep the raw JS result. |
| [`js-media-fast-seek!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/fastSeek) | `(extern f64)` | `()` | yes | no | no | `(js-media-fast-seek! media 0.0)` | call `fast-seek` for side effects. |
| [`js-media-load!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/load) | `(extern)` | `()` | yes | no | no | `(js-media-load! media)` | call `load` for side effects. |
| [`js-media-pause`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/pause) | `(extern)` | `()` | yes | no | no | `(js-media-pause media)` | call `pause` for side effects. |
| [`js-media-play`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/play) | `(extern)` | `(extern)` | no | no | yes | `(js-media-play media)` | call `play` and keep the raw JS result. |
| [`js-media-set-media-keys!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/setMediaKeys) | `(extern extern)` | `(extern)` | yes | no | yes | `(js-media-set-media-keys! media obj2)` | call `media-keys` and keep the raw JS result. |
| [`js-media-set-sink-id!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/setSinkId) | `(extern string)` | `(extern)` | yes | no | yes | `(js-media-set-sink-id! media "root")` | call `sink-id` and keep the raw JS result. |

Note: Promise-like externals in this table: none.

### 7.2 HTMLImageElement Module

Module source in `ffi/dom.ffi`: `#:module "image"`  
MDN root: [HTMLImageElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement)

| Function | Input types | Output type | Side effects? | Callback? | Nullable return? | Example | Use when |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [`js-image-new`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/Image) | `(value value)` | `(extern)` | no | no | yes | `(js-image-new (void) (void))` | call `new` and keep the raw JS result. |
| [`js-image-alt`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/alt) | `(extern)` | `(string)` | no | no | no | `(js-image-alt img)` | read `alt` as `string`. |
| [`js-set-image-alt!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/alt) | `(extern string)` | `()` | yes | no | no | `(js-set-image-alt! img "x")` | set `alt` on a HTMLImageElement value. |
| [`js-image-src`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/src) | `(extern)` | `(string)` | no | no | no | `(js-image-src img)` | read `src` as `string`. |
| [`js-set-image-src!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/src) | `(extern string)` | `()` | yes | no | no | `(js-set-image-src! img "/img.png")` | set `src` on a HTMLImageElement value. |
| [`js-image-srcset`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/srcset) | `(extern)` | `(string)` | no | no | no | `(js-image-srcset img)` | read `srcset` as `string`. |
| [`js-set-image-srcset!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/srcset) | `(extern string)` | `()` | yes | no | no | `(js-set-image-srcset! img "/img.png")` | set `srcset` on a HTMLImageElement value. |
| [`js-image-sizes`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/sizes) | `(extern)` | `(string)` | no | no | no | `(js-image-sizes img)` | read `sizes` as `string`. |
| [`js-set-image-sizes!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/sizes) | `(extern string)` | `()` | yes | no | no | `(js-set-image-sizes! img "x")` | set `sizes` on a HTMLImageElement value. |
| [`js-image-cross-origin`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/crossOrigin) | `(extern)` | `(value)` | no | no | yes | `(js-image-cross-origin img)` | read `cross-origin` as `value`. |
| [`js-set-image-cross-origin!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/crossOrigin) | `(extern value)` | `()` | yes | no | no | `(js-set-image-cross-origin! img (void))` | set `cross-origin` on a HTMLImageElement value. |
| [`js-image-use-map`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/useMap) | `(extern)` | `(string)` | no | no | no | `(js-image-use-map img)` | read `use-map` as `string`. |
| [`js-set-image-use-map!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/useMap) | `(extern string)` | `()` | yes | no | no | `(js-set-image-use-map! img "x")` | set `use-map` on a HTMLImageElement value. |
| [`js-image-is-map`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/isMap) | `(extern)` | `(i32)` | no | no | no | `(js-image-is-map img)` | read `is-map` as `i32`. |
| [`js-set-image-is-map!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/isMap) | `(extern i32)` | `()` | yes | no | no | `(js-set-image-is-map! img 1)` | set `is-map` on a HTMLImageElement value. |
| [`js-image-width`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/width) | `(extern)` | `(u32)` | no | no | no | `(js-image-width img)` | read `width` as `u32`. |
| [`js-set-image-width!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/width) | `(extern u32)` | `()` | yes | no | no | `(js-set-image-width! img 0)` | set `width` on a HTMLImageElement value. |
| [`js-image-height`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/height) | `(extern)` | `(u32)` | no | no | no | `(js-image-height img)` | read `height` as `u32`. |
| [`js-set-image-height!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/height) | `(extern u32)` | `()` | yes | no | no | `(js-set-image-height! img 0)` | set `height` on a HTMLImageElement value. |
| [`js-image-natural-width`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/naturalWidth) | `(extern)` | `(u32)` | no | no | no | `(js-image-natural-width img)` | read `natural-width` as `u32`. |
| [`js-image-natural-height`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/naturalHeight) | `(extern)` | `(u32)` | no | no | no | `(js-image-natural-height img)` | read `natural-height` as `u32`. |
| [`js-image-complete`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/complete) | `(extern)` | `(i32)` | no | no | no | `(js-image-complete img)` | read `complete` as `i32`. |
| [`js-image-current-src`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/currentSrc) | `(extern)` | `(string)` | no | no | no | `(js-image-current-src img)` | read `current-src` as `string`. |
| [`js-image-decoding`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decoding) | `(extern)` | `(string)` | no | no | no | `(js-image-decoding img)` | read `decoding` as `string`. |
| [`js-set-image-decoding!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decoding) | `(extern string)` | `()` | yes | no | no | `(js-set-image-decoding! img "x")` | set `decoding` on a HTMLImageElement value. |
| [`js-image-fetch-priority`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/fetchPriority) | `(extern)` | `(string)` | no | no | no | `(js-image-fetch-priority img)` | read `fetch-priority` as `string`. |
| [`js-set-image-fetch-priority!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/fetchPriority) | `(extern string)` | `()` | yes | no | no | `(js-set-image-fetch-priority! img "x")` | set `fetch-priority` on a HTMLImageElement value. |
| [`js-image-loading`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/loading) | `(extern)` | `(string)` | no | no | no | `(js-image-loading img)` | read `loading` as `string`. |
| [`js-set-image-loading!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/loading) | `(extern string)` | `()` | yes | no | no | `(js-set-image-loading! img "x")` | set `loading` on a HTMLImageElement value. |
| [`js-image-referrer-policy`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/referrerPolicy) | `(extern)` | `(string)` | no | no | no | `(js-image-referrer-policy img)` | read `referrer-policy` as `string`. |
| [`js-set-image-referrer-policy!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/referrerPolicy) | `(extern string)` | `()` | yes | no | no | `(js-set-image-referrer-policy! img "x")` | set `referrer-policy` on a HTMLImageElement value. |
| [`js-image-name`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/name) | `(extern)` | `(string)` | no | no | no | `(js-image-name img)` | read `name` as `string`. |
| [`js-set-image-name!`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/name) | `(extern string)` | `()` | yes | no | no | `(js-set-image-name! img "x")` | set `name` on a HTMLImageElement value. |
| [`js-image-x`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/x) | `(extern)` | `(i32)` | no | no | no | `(js-image-x img)` | read `x` as `i32`. |
| [`js-image-y`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/y) | `(extern)` | `(i32)` | no | no | no | `(js-image-y img)` | read `y` as `i32`. |
| [`js-image-decode`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decode) | `(extern)` | `(extern)` | no | no | yes | `(js-image-decode img)` | call `decode` and keep the promise-like JS result. |

Note: Promise-like externals in this table: [`js-image-decode`](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decode).

## Chapter 8 — Mini Workflows

### Build and insert a DOM node

```racket
(define doc (js-window-document))
(define body (js-document-body))
(define p (js-create-element "p"))
(js-set-attribute! p "class" "notice")
(js-append-child! p (js-create-text-node "Hello from WebRacket"))
(js-append-child! body p)
```

### Query and patch attributes

```racket
(define card (js-query-selector ".card"))
(define old-role (js-get-attribute card "role"))
(js-set-attribute! card "role" "region")
(js-toggle-attribute! card "data-active" 1)
old-role
```

### Attach an event listener

```racket
(define btn (js-get-element-by-id "run-btn"))
(define handler (lambda (_ev) (js-window-alert "clicked")))
(js-add-event-listener! btn "click" handler)
```

### Draw on a canvas

```racket
(define canvas (js-get-element-by-id "c"))
(define ctx (js-canvas-get-context canvas "2d" (void)))
(js-set-canvas2d-fill-style! ctx "#44d1ff")
(js-canvas2d-fill-rect ctx 10.0 10.0 120.0 50.0)
```

### Play media

```racket
(define media (js-get-element-by-id "player"))
(js-set-media-src! media "/audio/theme.ogg")
(js-set-media-volume! media 0.5)
(js-media-play media)
```

### Scroll and viewport control

```racket
(js-window-scroll-to 0.0 500.0 (void))
(define y (js-window-page-y-offset))
(define h (js-window-inner-height))
(list y h)
```

## Chapter 9 — Coverage Checklist

- Total documented functions: **402**
- `window`: 89 functions
- `performance`: 1 function
- `document`: 39 functions
- `event-target`: 1 function
- `event`: 15 functions
- `element`: 49 functions
- `canvas`: 9 functions
- `canvas-rendering-context-2d`: 105 functions
- `media`: 58 functions
- `image`: 36 functions

Deprecated forms that are commented out in `ffi/dom.ffi` are intentionally excluded.

