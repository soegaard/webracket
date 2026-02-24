# Reference: `xtermjs.ffi` 

## Chapter 1 — Introduction

This document describes the xterm.js bindings exported by `ffi/xtermjs.ffi` in WebRacket.

xterm.js is a browser terminal emulator for building terminal-like UIs in web applications. This FFI exposes terminal construction, terminal control/events, and the FitAddon API for responsive sizing.

Assumption in examples: the program is compiled with `--ffi xtermjs`.

All function names are linked to xterm.js API documentation.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Optional Argument and Return Conventions](#22-optional-argument-and-return-conventions)
- [2.3 Common Setup Helpers](#23-common-setup-helpers)
- [Chapter 3 — Terminal Construction](#chapter-3--terminal-construction)
- [Chapter 4 — Terminal Properties](#chapter-4--terminal-properties)
- [Chapter 5 — Terminal Events](#chapter-5--terminal-events)
- [Chapter 6 — Terminal Methods](#chapter-6--terminal-methods)
- [Chapter 7 — FitAddon](#chapter-7--fitaddon)
- [7.1 Construction](#71-construction)
- [7.2 Lifecycle](#72-lifecycle)
- [7.3 Behavior](#73-behavior)
- [Chapter 8 — Mini Workflows](#chapter-8--mini-workflows)
- [Open, Write, and Read Selection](#open-write-and-read-selection)
- [Attach and Use FitAddon](#attach-and-use-fitaddon)
- [Resize and Scroll Control](#resize-and-scroll-control)
- [Chapter 9 — Coverage Checklist](#chapter-9--coverage-checklist)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript object/reference (typically used for input parameters). |
| `(extern/raw)` | Raw JavaScript return object/reference (no `null`/`undefined` mapping). |
| `(value)` | FFI value conversion boundary; can be JS object or primitive via WebRacket conversion rules. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(i32)` | Signed 32-bit integer. Used for indexes/counts and some boolean-like returns (`1`/`0`). |
| `(u32)` | Unsigned 32-bit integer. Used for dimensions such as cols/rows. |
| `()` | No arguments (input) or no value/void (output). |

### 2.2 Optional Argument and Return Conventions

- For `xterm-terminal-new`, pass `(void)` to use built-in default options.
- For `xterm-terminal-input`, pass `(void)` for `was-user-input?` to use default behavior (`true`).
- For `xterm-terminal-register-marker`, pass `(void)` for `cursor-y-offset` to use default offset (`0`).
- For `xterm-terminal-write` and `xterm-terminal-writeln`, pass `(void)` as callback for no callback.
- `xterm-terminal-element` and `xterm-terminal-textarea` return `(value)` and may be `(void)` until `open` has been called.
- `xterm-fit-addon-propose-dimensions` returns `(value)` and may be `(void)` when dimensions cannot be computed.

### 2.3 Common Setup Helpers

```racket
(define term (xterm-terminal-new (void)))
(define host-el (js-ref/document-by-id "terminal"))
(define fit (xterm-fit-addon-new))
(define options (js-eval "({ cursorBlink: true })"))
(define handler (js-eval "(ev) => true"))
```

## Chapter 3 — Terminal Construction

xterm.js Terminal API: [Terminal](https://xtermjs.org/docs/api/terminal/classes/terminal/)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-terminal-new`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(value)` | `(extern/raw)` | `(xterm-terminal-new options)` | create a new Terminal instance. |

## Chapter 4 — Terminal Properties

xterm.js Terminal API: [Terminal](https://xtermjs.org/docs/api/terminal/classes/terminal/)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-terminal-buffer`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-buffer term)` | access to the normal and alt buffers. |
| [`xterm-terminal-cols`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(u32)` | `(xterm-terminal-cols term)` | number of columns in the viewport. |
| [`xterm-terminal-rows`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(u32)` | `(xterm-terminal-rows term)` | number of rows in the viewport. |
| [`xterm-terminal-element`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(value)` | `(xterm-terminal-element term)` | root element that hosts the terminal. |
| [`xterm-terminal-textarea`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(value)` | `(xterm-terminal-textarea term)` | textarea element that receives keyboard input. |
| [`xterm-terminal-markers`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-markers term)` | registered markers associated with the buffer. |
| [`xterm-terminal-modes`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-modes term)` | terminal modes currently enabled. |
| [`xterm-terminal-options`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-options term)` | terminal options getter. |
| [`xterm-set-terminal-options!`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern value)` | `()` | `(xterm-set-terminal-options! term options)` | terminal options setter. |
| [`xterm-terminal-parser`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-parser term)` | parser interface for custom escape handlers. |
| [`xterm-terminal-unicode`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-unicode term)` | unicode handling interface. |

## Chapter 5 — Terminal Events

xterm.js Terminal API: [Terminal](https://xtermjs.org/docs/api/terminal/classes/terminal/)

All `on-*` bindings return event emitter objects as `(extern/raw)`; attach listeners via your JS interop layer.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-terminal-on-bell`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-bell term)` | event fired when the bell rings. |
| [`xterm-terminal-on-binary`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-binary term)` | event fired for binary data. |
| [`xterm-terminal-on-cursor-move`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-cursor-move term)` | event fired when the cursor moves. |
| [`xterm-terminal-on-data`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-data term)` | event fired when data is emitted by the terminal. |
| [`xterm-terminal-on-key`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-key term)` | event fired when a key is pressed. |
| [`xterm-terminal-on-line-feed`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-line-feed term)` | event fired when a line feed is added. |
| [`xterm-terminal-on-render`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-render term)` | event fired when rows are rendered. |
| [`xterm-terminal-on-resize`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-resize term)` | event fired when the terminal is resized. |
| [`xterm-terminal-on-scroll`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-scroll term)` | event fired when the viewport scrolls. |
| [`xterm-terminal-on-selection-change`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-selection-change term)` | event fired when the selection changes. |
| [`xterm-terminal-on-title-change`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-title-change term)` | event fired when the terminal title changes. |
| [`xterm-terminal-on-write-parsed`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(extern/raw)` | `(xterm-terminal-on-write-parsed term)` | event fired after write data was parsed. |

## Chapter 6 — Terminal Methods

xterm.js Terminal API: [Terminal](https://xtermjs.org/docs/api/terminal/classes/terminal/)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-terminal-attach-custom-key-event-handler`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `()` | `(xterm-terminal-attach-custom-key-event-handler term handler)` | register a custom key event handler. |
| [`xterm-terminal-attach-custom-wheel-event-handler`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `()` | `(xterm-terminal-attach-custom-wheel-event-handler term handler)` | register a custom wheel event handler. |
| [`xterm-terminal-blur`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-blur term)` | remove focus from the terminal. |
| [`xterm-terminal-focus`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-focus term)` | focus the terminal. |
| [`xterm-terminal-dispose`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-dispose term)` | dispose of the terminal instance. |
| [`xterm-terminal-clear`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-clear term)` | clear the entire buffer. |
| [`xterm-terminal-clear-selection`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-clear-selection term)` | clear the active selection. |
| [`xterm-terminal-clear-texture-atlas`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-clear-texture-atlas term)` | clear the texture atlas. |
| [`xterm-terminal-deregister-character-joiner`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32)` | `()` | `(xterm-terminal-deregister-character-joiner term 1)` | deregister a previously registered character joiner. |
| [`xterm-terminal-get-selection`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(string)` | `(xterm-terminal-get-selection term)` | retrieve the current selection as a string. |
| [`xterm-terminal-get-selection-position`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(value)` | `(xterm-terminal-get-selection-position term)` | retrieve the current selection range. |
| [`xterm-terminal-has-selection`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `(i32)` | `(xterm-terminal-has-selection term)` | test whether a selection is active. |
| [`xterm-terminal-input`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern string value)` | `()` | `(xterm-terminal-input term "hello" (void))` | forward input data to the application. |
| [`xterm-terminal-load-addon`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `()` | `(xterm-terminal-load-addon term fit)` | load an addon instance. |
| [`xterm-terminal-open`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `()` | `(xterm-terminal-open term host-el)` | open the terminal inside the provided parent element. |
| [`xterm-terminal-paste`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern string)` | `()` | `(xterm-terminal-paste term "hello")` | paste text into the terminal. |
| [`xterm-terminal-refresh`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32 i32)` | `()` | `(xterm-terminal-refresh term 1 1)` | refresh the viewport between the given rows. |
| [`xterm-terminal-register-character-joiner`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `(i32)` | `(xterm-terminal-register-character-joiner term handler)` | register a character joiner and receive its identifier. |
| [`xterm-terminal-register-decoration`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `(extern/raw)` | `(xterm-terminal-register-decoration term options)` | register a decoration on the terminal. |
| [`xterm-terminal-register-link-provider`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern extern)` | `(extern/raw)` | `(xterm-terminal-register-link-provider term provider)` | register a custom link provider. |
| [`xterm-terminal-register-marker`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern value)` | `(extern/raw)` | `(xterm-terminal-register-marker term (void))` | register a marker relative to the cursor. |
| [`xterm-terminal-reset`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-reset term)` | perform a full terminal reset. |
| [`xterm-terminal-resize`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32 i32)` | `()` | `(xterm-terminal-resize term 1 1)` | resize the terminal viewport. |
| [`xterm-terminal-scroll-lines`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32)` | `()` | `(xterm-terminal-scroll-lines term 1)` | scroll by a number of lines. |
| [`xterm-terminal-scroll-pages`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32)` | `()` | `(xterm-terminal-scroll-pages term 1)` | scroll by a number of pages. |
| [`xterm-terminal-scroll-to-bottom`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-scroll-to-bottom term)` | scroll to the bottom of the buffer. |
| [`xterm-terminal-scroll-to-line`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32)` | `()` | `(xterm-terminal-scroll-to-line term 1)` | scroll to a specific line. |
| [`xterm-terminal-scroll-to-top`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-scroll-to-top term)` | scroll to the top of the buffer. |
| [`xterm-terminal-select`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32 i32 i32)` | `()` | `(xterm-terminal-select term 1 1 1)` | select text starting at a column and row for the given length. |
| [`xterm-terminal-select-all`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern)` | `()` | `(xterm-terminal-select-all term)` | select all text in the buffer. |
| [`xterm-terminal-select-lines`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern i32 i32)` | `()` | `(xterm-terminal-select-lines term 1 1)` | select text between two buffer lines. |
| [`xterm-terminal-write`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern value value)` | `()` | `(xterm-terminal-write term "hello" (void))` | write data to the terminal. |
| [`xterm-terminal-writeln`](https://xtermjs.org/docs/api/terminal/classes/terminal/) | `(extern value value)` | `()` | `(xterm-terminal-writeln term "hello" (void))` | write data followed by a newline. |

## Chapter 7 — FitAddon

xterm.js FitAddon API: [FitAddon](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/)

### 7.1 Construction

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-fit-addon-new`](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/) | `()` | `(extern/raw)` | `(xterm-fit-addon-new)` | create a new FitAddon instance. |

### 7.2 Lifecycle

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-fit-addon-activate`](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/) | `(extern extern)` | `()` | `(xterm-fit-addon-activate fit term)` | activate the addon for the provided terminal instance. |
| [`xterm-fit-addon-dispose`](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/) | `(extern)` | `()` | `(xterm-fit-addon-dispose fit)` | dispose of the addon and release any associated resources. |

### 7.3 Behavior

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`xterm-fit-addon-fit`](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/) | `(extern)` | `()` | `(xterm-fit-addon-fit fit)` | recalculate and apply the best fit for the terminal within its container. |
| [`xterm-fit-addon-propose-dimensions`](https://xtermjs.org/docs/api/addons/addon-fit/classes/fitaddon/) | `(extern)` | `(value)` | `(xterm-fit-addon-propose-dimensions fit)` | propose the optimal terminal dimensions for the current container size. |

## Chapter 8 — Mini Workflows

### Open, Write, and Read Selection

```racket
(define term (xterm-terminal-new (void)))
(xterm-terminal-open term host-el)
(xterm-terminal-write term "hello" (void))
(xterm-terminal-select-all term)
(xterm-terminal-get-selection term)
```

### Attach and Use FitAddon

```racket
(define term (xterm-terminal-new (void)))
(define fit (xterm-fit-addon-new))
(xterm-terminal-load-addon term fit)
(xterm-fit-addon-fit fit)
```

### Resize and Scroll Control

```racket
(xterm-terminal-resize term 120 40)
(xterm-terminal-scroll-to-bottom term)
(xterm-terminal-refresh term 0 39)
```

## Chapter 9 — Coverage Checklist

- This document covers **62** functions from `ffi/xtermjs.ffi`.
- Total documented functions: **62**
- `terminal construction`: 1 function
- `terminal properties`: 11 functions
- `terminal events`: 12 functions
- `terminal methods`: 33 functions
- `fit addon construction`: 1 function
- `fit addon lifecycle`: 2 functions
- `fit addon behavior`: 2 functions
