# Reference: `jsxgraph.ffi`

## Chapter 1 — Introduction

This document describes the JSXGraph bindings exported by `ffi/jsxgraph.ffi` in WebRacket.

JSXGraph is a browser-based JavaScript library for interactive mathematics and visualization. In WebRacket, `ffi/jsxgraph.ffi` gives direct access to selected board and point APIs.

Current scope:
- Board lifecycle and element creation
- Point predicates
- Point attributes (getters/setters)
- Point methods for hit-testing, style updates, and renderer updates

Assumption in examples: the program is compiled with `--ffi jsxgraph`.

All function names are linked to JSXGraph API documentation.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Boolean and Numeric Conventions](#22-boolean-and-numeric-conventions)
- [2.3 Common Setup Helpers](#23-common-setup-helpers)
- [Chapter 3 — Predicates](#chapter-3--predicates)
- [Chapter 4 — Point Properties (Getters)](#chapter-4--point-properties-getters)
- [Chapter 5 — Point Properties (Setters)](#chapter-5--point-properties-setters)
- [Chapter 6 — Point Methods](#chapter-6--point-methods)
- [Chapter 7 — Board API](#chapter-7--board-api)
- [7.1 Construction](#71-construction)
- [7.2 Lifecycle and Updates](#72-lifecycle-and-updates)
- [Chapter 8 — Mini Workflows](#chapter-8--mini-workflows)
- [Configure Point Snapping](#configure-point-snapping)
- [Hit-Testing and Projection](#hit-testing-and-projection)
- [Style and Renderer Refresh](#style-and-renderer-refresh)
- [Minimal Geometry Constructors](#minimal-geometry-constructors)
- [Chapter 9 — Coverage Checklist](#chapter-9--coverage-checklist)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript object/reference (typically used for input parameters). |
| `(extern/raw)` | Raw JavaScript return object/reference (no `null`/`undefined` mapping). |
| `(value)` | WebRacket value converted through the FFI value bridge. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(i32)` | 32-bit integer. In this API it is often used as boolean flag (`0`/`1`). |
| `(f64)` | Double-precision floating-point number. |
| `()` | No arguments (input) or no value / void (output). |

### 2.2 Boolean and Numeric Conventions

- Several point flags are `(i32)` instead of `(boolean)`. Use `0` for false and `1` for true.
- Coordinates and distances are `(f64)`.
- `jsx-point-style` and infobox digit settings are `(i32)`.

### 2.3 Common Setup Helpers

```racket
(define board (js-eval "window.board"))
(define pt    (js-eval "window.pt"))
(define pt2   (js-eval "window.pt2"))
(define el1   (js-eval "window.el1"))
(define el2   (js-eval "window.el2"))
(define attrs  (js-object (vector)))
```

## Chapter 3 — Predicates

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-point?`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(boolean)` | `(jsx-point? pt)` | validate an external object is a JSXGraph point. |

## Chapter 4 — Point Properties (Getters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-point-attractor-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-attractor-distance pt)` | read the attractor capture distance. |
| [`jsx-point-attractors`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(jsx-point-attractors pt)` | inspect the point attractor list. |
| [`jsx-point-attractor-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(jsx-point-attractor-unit pt)` | read the unit used for attractor distance. |
| [`jsx-point-attract-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-attract-to-grid pt)` | check whether attract-to-grid is enabled. |
| [`jsx-point-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(jsx-point-face pt)` | read the point marker face. |
| [`jsx-point-ignored-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(jsx-point-ignored-snap-to-points pt)` | read the ignored points for snap-to-points. |
| [`jsx-point-infobox-digits`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-infobox-digits pt)` | read the infobox precision setting. |
| [`jsx-point-show-infobox`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-show-infobox pt)` | check whether the infobox display is enabled. |
| [`jsx-point-size`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-size pt)` | read the point marker size. |
| [`jsx-point-size-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(jsx-point-size-unit pt)` | read the unit for point size. |
| [`jsx-point-snap-size-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-snap-size-x pt)` | read the x-axis snap step. |
| [`jsx-point-snap-size-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-snap-size-y pt)` | read the y-axis snap step. |
| [`jsx-point-snap-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-snap-to-grid pt)` | check whether snap-to-grid is enabled. |
| [`jsx-point-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-snap-to-points pt)` | check whether snap-to-points is enabled. |
| [`jsx-point-snatch-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-snatch-distance pt)` | read the maximum snapping distance. |
| [`jsx-point-style`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-style pt)` | read the numeric point style index. |
| [`jsx-point-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-x pt)` | read the point x-coordinate. |
| [`jsx-point-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(jsx-point-y pt)` | read the point y-coordinate. |
| [`jsx-point-zoom`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(jsx-point-zoom pt)` | check whether point size scales with zoom. |

## Chapter 5 — Point Properties (Setters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-set-point-attractor-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(jsx-set-point-attractor-distance! pt 12.0)` | set the attractor capture distance. |
| [`jsx-set-point-attractors!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(jsx-set-point-attractors! pt attractors)` | replace the point attractor list. |
| [`jsx-set-point-attractor-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(jsx-set-point-attractor-unit! pt "px")` | set the unit for attractor distance. |
| [`jsx-set-point-attract-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-attract-to-grid! pt 1)` | enable or disable attract-to-grid. |
| [`jsx-set-point-face!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(jsx-set-point-face! pt "circle")` | change the marker face. |
| [`jsx-set-point-ignored-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(jsx-set-point-ignored-snap-to-points! pt points)` | set the points ignored during snap-to-points. |
| [`jsx-set-point-infobox-digits!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-infobox-digits! pt 3)` | set the infobox precision. |
| [`jsx-set-point-show-infobox!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-show-infobox! pt 1)` | show or hide the infobox. |
| [`jsx-set-point-size!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(jsx-set-point-size! pt 4.0)` | set the point marker size. |
| [`jsx-set-point-size-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(jsx-set-point-size-unit! pt "px")` | set the unit for point size. |
| [`jsx-set-point-snap-size-x!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(jsx-set-point-snap-size-x! pt 0.5)` | set the x-axis snap step. |
| [`jsx-set-point-snap-size-y!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(jsx-set-point-snap-size-y! pt 0.5)` | set the y-axis snap step. |
| [`jsx-set-point-snap-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-snap-to-grid! pt 1)` | enable or disable snap-to-grid. |
| [`jsx-set-point-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-snap-to-points! pt 1)` | enable or disable snap-to-points. |
| [`jsx-set-point-snatch-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(jsx-set-point-snatch-distance! pt 12.0)` | set the maximum snapping distance. |
| [`jsx-set-point-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-style! pt 2)` | set the numeric point style index. |
| [`jsx-set-point-zoom!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-set-point-zoom! pt 1)` | enable or disable zoom scaling of point size. |

## Chapter 6 — Point Methods

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-point-has-point`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64 f64)` | `(i32)` | `(jsx-point-has-point pt 100.0 120.0)` | hit-test screen coordinates against the point marker. |
| [`jsx-point-is-on`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern f64)` | `(i32)` | `(jsx-point-is-on pt el1 0.1)` | test whether a point lies on another element within tolerance. |
| [`jsx-point-make-intersection!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern extern i32 i32)` | `()` | `(jsx-point-make-intersection! pt el1 el2 0 0)` | turn a point into an intersection definition. |
| [`jsx-point-normalize-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `(string)` | `(jsx-point-normalize-face pt "x")` | normalize or validate a face token. |
| [`jsx-point-set-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-point-set-style! pt 2)` | invoke the point style setter method directly. |
| [`jsx-point-update!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(jsx-point-update! pt 1)` | recompute the point position or state. |
| [`jsx-point-update-renderer!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `()` | `(jsx-point-update-renderer! pt)` | trigger a renderer refresh for the point. |
| [`jsx-point-update-transform!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `(extern/raw)` | `(jsx-point-update-transform! pt 1)` | apply transformations and return the transformed base element. |

## Chapter 7 — Board API

Reference roots:
- [JXG.JSXGraph](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html)
- [JXG.Board](https://jsxgraph.org/docs/symbols/JXG.Board.html)

### 7.1 Construction

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-init-board`](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html) | `(string value)` | `(extern/raw)` | `(jsx-init-board "box" attrs)` | initialize a JSXGraph board in a DOM container. |
| [`jsx-board-create`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern string value value)` | `(extern/raw)` | `(jsx-board-create board "point" parents attrs)` | create a generic JSXGraph element on an existing board. |
| [`jsx-board-create-point`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-point board #[-3 1] attrs)` | create a point using the board helper. |
| [`jsx-board-create-line`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-line board #[p q] attrs)` | create a line from parent points or coordinates. |
| [`jsx-board-create-segment`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-segment board #[p q] attrs)` | create a finite segment from parent points or coordinates. |
| [`jsx-board-create-circle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-circle board #[p q] attrs)` | create a circle from parent geometry. |
| [`jsx-board-create-intersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-intersection board #[l1 l2 0] attrs)` | create an intersection point from parent elements. |
| [`jsx-board-create-text`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(jsx-board-create-text board #[-5 5 "A"] attrs)` | create a text label or annotation on the board. |

### 7.2 Lifecycle and Updates

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`jsx-board-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(jsx-board-update! board)` | update the board state and redraw as needed. |
| [`jsx-board-full-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(jsx-board-full-update! board)` | force a full board update pass. |
| [`jsx-board-remove-object!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern extern)` | `()` | `(jsx-board-remove-object! board obj)` | remove a previously created object from the board. |
| [`jsx-board-suspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(jsx-board-suspend-update! board)` | suspend automatic redraws while batching changes. |
| [`jsx-board-unsuspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(jsx-board-unsuspend-update! board)` | resume automatic redraws after batching changes. |

## Chapter 8 — Mini Workflows

### Configure Point Snapping

```racket
(jsx-set-point-snap-to-grid! pt 1)
(jsx-set-point-snap-size-x! pt 0.25)
(jsx-set-point-snap-size-y! pt 0.25)
(jsx-set-point-snatch-distance! pt 6.0)
```

### Hit-Testing and Projection

```racket
(define hits? (jsx-point-has-point pt 120.0 80.0))
(define on-curve? (jsx-point-is-on pt el1 0.05))
(list hits? on-curve?)
```

### Style and Renderer Refresh

```racket
(jsx-set-point-face! pt "cross")
(jsx-set-point-size! pt 5.0)
(jsx-point-update! pt 1)
(jsx-point-update-renderer! pt)
```

### Minimal Geometry Constructors

```racket
(define p (jsx-board-create-point board #[-3 1] (js-object (vector (vector "name" "P")))))
(define q (jsx-board-create-point board #[2 2] (js-object (vector (vector "name" "Q")))))
(define l (jsx-board-create-line board #[p q] (js-object (vector))))
(define s (jsx-board-create-segment board #[p q] (js-object (vector))))
(define c (jsx-board-create-circle board #[p q] (js-object (vector))))
(define t (jsx-board-create-text board #[-5 5 "PQ"] (js-object (vector))))
```

## Chapter 9 — Coverage Checklist

- This document covers **58** functions from `ffi/jsxgraph.ffi`.
- Total documented functions: **58**
- `board api`: 13 functions
- `predicates`: 1 function
- `point getters`: 19 functions
- `point setters`: 17 functions
- `point methods`: 8 functions
