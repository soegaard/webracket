# Reference: `jsxgraph.ffi`

## Chapter 1 — Introduction

This document describes the JSXGraph bindings exported by `ffi/jsxgraph.ffi` in WebRacket.

JSXGraph is a browser-based JavaScript library for interactive mathematics and visualization. In WebRacket, `ffi/jsxgraph.ffi` gives direct access to selected board and point APIs.

Current scope:
- Board lifecycle and element creation
- Board properties and object lists
- GeometryElement bridge helpers
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
- [7.3 Board Properties](#73-board-properties)
- [Chapter 8 — GeometryElement Bridge](#chapter-8--geometryelement-bridge)
- [Chapter 9 — Line Bridge](#chapter-9--line-bridge)
- [Chapter 10 — Circle Bridge](#chapter-10--circle-bridge)
- [Chapter 11 — Curve Bridge](#chapter-11--curve-bridge)
- [Chapter 12 — Mini Workflows](#chapter-12--mini-workflows)
- [Configure Point Snapping](#configure-point-snapping)
- [Hit-Testing and Projection](#hit-testing-and-projection)
- [Style and Renderer Refresh](#style-and-renderer-refresh)
- [Minimal Geometry Constructors](#minimal-geometry-constructors)
- [Chapter 13 — Coverage Checklist](#chapter-13--coverage-checklist)

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
- `js-jsx-point-style` and infobox digit settings are `(i32)`.

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
| [`js-jsx-point?`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(boolean)` | `(js-jsx-point? pt)` | validate an external object is a JSXGraph point. |

## Chapter 4 — Point Properties (Getters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-point-attractor-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-attractor-distance pt)` | read the attractor capture distance. |
| [`js-jsx-point-attractors`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(js-jsx-point-attractors pt)` | inspect the point attractor list. |
| [`js-jsx-point-attractor-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-attractor-unit pt)` | read the unit used for attractor distance. |
| [`js-jsx-point-attract-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-attract-to-grid pt)` | check whether attract-to-grid is enabled. |
| [`js-jsx-point-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-face pt)` | read the point marker face. |
| [`js-jsx-point-ignored-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(js-jsx-point-ignored-snap-to-points pt)` | read the ignored points for snap-to-points. |
| [`js-jsx-point-infobox-digits`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-infobox-digits pt)` | read the infobox precision setting. |
| [`js-jsx-point-show-infobox`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-show-infobox pt)` | check whether the infobox display is enabled. |
| [`js-jsx-point-size`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-size pt)` | read the point marker size. |
| [`js-jsx-point-size-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-size-unit pt)` | read the unit for point size. |
| [`js-jsx-point-snap-size-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snap-size-x pt)` | read the x-axis snap step. |
| [`js-jsx-point-snap-size-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snap-size-y pt)` | read the y-axis snap step. |
| [`js-jsx-point-snap-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-snap-to-grid pt)` | check whether snap-to-grid is enabled. |
| [`js-jsx-point-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-snap-to-points pt)` | check whether snap-to-points is enabled. |
| [`js-jsx-point-snatch-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snatch-distance pt)` | read the maximum snapping distance. |
| [`js-jsx-point-style`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-style pt)` | read the numeric point style index. |
| [`js-jsx-point-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-x pt)` | read the point x-coordinate. |
| [`js-jsx-point-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-y pt)` | read the point y-coordinate. |
| [`js-jsx-point-zoom`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-zoom pt)` | check whether point size scales with zoom. |

## Chapter 5 — Point Properties (Setters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-set-point-attractor-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-attractor-distance! pt 12.0)` | set the attractor capture distance. |
| [`js-jsx-set-point-attractors!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(js-jsx-set-point-attractors! pt attractors)` | replace the point attractor list. |
| [`js-jsx-set-point-attractor-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-attractor-unit! pt "px")` | set the unit for attractor distance. |
| [`js-jsx-set-point-attract-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-attract-to-grid! pt 1)` | enable or disable attract-to-grid. |
| [`js-jsx-set-point-face!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-face! pt "circle")` | change the marker face. |
| [`js-jsx-set-point-ignored-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(js-jsx-set-point-ignored-snap-to-points! pt points)` | set the points ignored during snap-to-points. |
| [`js-jsx-set-point-infobox-digits!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-infobox-digits! pt 3)` | set the infobox precision. |
| [`js-jsx-set-point-show-infobox!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-show-infobox! pt 1)` | show or hide the infobox. |
| [`js-jsx-set-point-size!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-size! pt 4.0)` | set the point marker size. |
| [`js-jsx-set-point-size-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-size-unit! pt "px")` | set the unit for point size. |
| [`js-jsx-set-point-snap-size-x!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snap-size-x! pt 0.5)` | set the x-axis snap step. |
| [`js-jsx-set-point-snap-size-y!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snap-size-y! pt 0.5)` | set the y-axis snap step. |
| [`js-jsx-set-point-snap-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-snap-to-grid! pt 1)` | enable or disable snap-to-grid. |
| [`js-jsx-set-point-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-snap-to-points! pt 1)` | enable or disable snap-to-points. |
| [`js-jsx-set-point-snatch-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snatch-distance! pt 12.0)` | set the maximum snapping distance. |
| [`js-jsx-set-point-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-style! pt 2)` | set the numeric point style index. |
| [`js-jsx-set-point-zoom!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-zoom! pt 1)` | enable or disable zoom scaling of point size. |

## Chapter 6 — Point Methods

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-point-has-point`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64 f64)` | `(i32)` | `(js-jsx-point-has-point pt 100.0 120.0)` | hit-test screen coordinates against the point marker. |
| [`js-jsx-point-is-on`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern f64)` | `(i32)` | `(js-jsx-point-is-on pt el1 0.1)` | test whether a point lies on another element within tolerance. |
| [`js-jsx-point-make-intersection!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern extern i32 i32)` | `()` | `(js-jsx-point-make-intersection! pt el1 el2 0 0)` | turn a point into an intersection definition. |
| [`js-jsx-point-normalize-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `(string)` | `(js-jsx-point-normalize-face pt "x")` | normalize or validate a face token. |
| [`js-jsx-point-set-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-point-set-style! pt 2)` | invoke the point style setter method directly. |
| [`js-jsx-point-update!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-point-update! pt 1)` | recompute the point position or state. |
| [`js-jsx-point-update-renderer!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `()` | `(js-jsx-point-update-renderer! pt)` | trigger a renderer refresh for the point. |
| [`js-jsx-point-update-transform!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `(extern/raw)` | `(js-jsx-point-update-transform! pt 1)` | apply transformations and return the transformed base element. |

## Chapter 7 — Board API

Reference roots:
- [JXG.JSXGraph](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html)
- [JXG.Board](https://jsxgraph.org/docs/symbols/JXG.Board.html)

### 7.1 Construction

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-init-board`](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html) | `(string value)` | `(extern/raw)` | `(js-jsx-init-board "box" attrs)` | initialize a JSXGraph board in a DOM container. |
| [`js-jsx-board-create`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern string value value)` | `(extern/raw)` | `(js-jsx-board-create board "point" parents attrs)` | create a generic JSXGraph element on an existing board. |
| [`js-jsx-board-create-point`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-point board #[-3 1] attrs)` | create a point using the board helper. |
| [`js-jsx-board-create-line`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-line board #[p q] attrs)` | create a line from parent points or coordinates. |
| [`js-jsx-board-create-segment`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-segment board #[p q] attrs)` | create a finite segment from parent points or coordinates. |
| [`js-jsx-board-create-circle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circle board #[p q] attrs)` | create a circle from parent geometry. |
| [`js-jsx-board-create-curve`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curve board #[f g] attrs)` | create a curve from parent data or functions. |
| [`js-jsx-board-create-intersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-intersection board #[l1 l2 0] attrs)` | create an intersection point from parent elements. |
| [`js-jsx-board-create-text`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-text board #[-5 5 "A"] attrs)` | create a text label or annotation on the board. |

### 7.2 Lifecycle and Updates

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-board-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-update! board)` | update the board state and redraw as needed. |
| [`js-jsx-board-full-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-full-update! board)` | force a full board update pass. |
| [`js-jsx-board-remove-object!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern extern)` | `()` | `(js-jsx-board-remove-object! board obj)` | remove a previously created object from the board. |
| [`js-jsx-board-suspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-suspend-update! board)` | suspend automatic redraws while batching changes. |
| [`js-jsx-board-unsuspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-unsuspend-update! board)` | resume automatic redraws after batching changes. |

### 7.3 Board Properties

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-board-id`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(string)` | `(js-jsx-board-id board)` | read the board id. |
| [`js-jsx-board-container`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-container board)` | read the board container element. |
| [`js-jsx-board-renderer`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-renderer board)` | read the board renderer. |
| [`js-jsx-board-canvas-width`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-canvas-width board)` | read the board canvas width. |
| [`js-jsx-board-canvas-height`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-canvas-height board)` | read the board canvas height. |
| [`js-jsx-board-bounding-box`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-bounding-box board)` | read the board bounding box. |
| [`js-jsx-board-num-objects`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-num-objects board)` | read the total number of objects ever created. |
| [`js-jsx-board-objects-list`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-objects-list board)` | read the board objects in construction order. |

## Chapter 8 — GeometryElement Bridge

Reference root: [JXG.GeometryElement](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-element-call`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-element-call element "setAttribute" args)` | call a GeometryElement method and keep the raw JavaScript result. |
| [`js-jsx-element-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-element-call/nullish element "remove" args)` | call a GeometryElement method and treat nullish results as absence. |
| [`js-jsx-element-add-event`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(void)` | `(js-jsx-element-add-event element "up" handler)` | register a GeometryElement event handler via the JSXGraph `addEvent` alias. |

## Chapter 9 — Line Bridge

Reference root: [JXG.Line](https://jsxgraph.org/docs/symbols/JXG.Line.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-line-call`](https://jsxgraph.org/docs/symbols/JXG.Line.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-line-call line "getSlope" args)` | call a line method and keep the raw JavaScript result. |
| [`js-jsx-line-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Line.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-line-call/nullish line "setFixedLength" args)` | call a line mutator and treat nullish results as absence. |

## Chapter 10 — Circle Bridge

Reference root: [JXG.Circle](https://jsxgraph.org/docs/symbols/JXG.Circle.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-circle-call`](https://jsxgraph.org/docs/symbols/JXG.Circle.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-circle-call circle "getRadius" args)` | call a circle method and keep the raw JavaScript result. |
| [`js-jsx-circle-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Circle.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-circle-call/nullish circle "setRadius" args)` | call a circle mutator and treat nullish results as absence. |

## Chapter 11 — Curve Bridge

Reference root: [JXG.Curve](https://jsxgraph.org/docs/symbols/JXG.Curve.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-curve-call`](https://jsxgraph.org/docs/symbols/JXG.Curve.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-curve-call curve "updateCurve" args)` | call a curve method and keep the raw JavaScript result. |
| [`js-jsx-curve-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Curve.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-curve-call/nullish curve "updateTransform" args)` | call a curve mutator and treat nullish results as absence. |

## Chapter 12 — Mini Workflows

### Configure Point Snapping

```racket
(js-jsx-set-point-snap-to-grid! pt 1)
(js-jsx-set-point-snap-size-x! pt 0.25)
(js-jsx-set-point-snap-size-y! pt 0.25)
(js-jsx-set-point-snatch-distance! pt 6.0)
```

### Hit-Testing and Projection

```racket
(define hits? (js-jsx-point-has-point pt 120.0 80.0))
(define on-curve? (js-jsx-point-is-on pt el1 0.05))
(list hits? on-curve?)
```

### Style and Renderer Refresh

```racket
(js-jsx-set-point-face! pt "cross")
(js-jsx-set-point-size! pt 5.0)
(js-jsx-point-update! pt 1)
(js-jsx-point-update-renderer! pt)
```

### Minimal Geometry Constructors

```racket
(define p (js-jsx-board-create-point board #[-3 1] (js-object (vector (vector "name" "P")))))
(define q (js-jsx-board-create-point board #[2 2] (js-object (vector (vector "name" "Q")))))
(define l (js-jsx-board-create-line board #[p q] (js-object (vector))))
(define s (js-jsx-board-create-segment board #[p q] (js-object (vector))))
(define c (js-jsx-board-create-circle board #[p q] (js-object (vector))))
(define t (js-jsx-board-create-text board #[-5 5 "PQ"] (js-object (vector))))
```

## Chapter 13 — Coverage Checklist

- This document covers **76** functions from `ffi/jsxgraph.ffi`.
- Total documented functions: **76**
- `board api`: 13 functions
- `board properties`: 8 functions
- `geometryelement bridge`: 3 functions
- `line bridge`: 2 functions
- `circle bridge`: 2 functions
- `curve bridge`: 3 functions
- `predicates`: 1 function
- `point getters`: 19 functions
- `point setters`: 17 functions
- `point methods`: 8 functions
