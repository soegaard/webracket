# Canvas FFI Reference

This document summarizes the foreign-function bindings that expose the HTML Canvas APIs defined in [`ffi/dom.ffi`]. It focuses on both the `HTMLCanvasElement` helpers and the much larger `CanvasRenderingContext2D` surface that WebRacket makes available.

## Overview

Canvas support is split into two layers:

1. **Canvas elements** – bindings whose arguments are HTML canvas nodes retrieved from the DOM. These cover size management, context acquisition, serialization helpers, and streaming.
2. **Canvas 2D rendering contexts** – bindings that mirror the methods and properties on `CanvasRenderingContext2D`. They allow you to configure drawing state, build paths, render shapes, manage images, and manipulate the transformation matrix.

Every binding is a thin wrapper around the corresponding browser API. Parameters typed as `value` accept any WebRacket value and are typically used when the JavaScript API expects either another DOM object or `undefined`. Passing `(void)` from Racket produces JavaScript `undefined`, matching the guidance in `ffi/dom.ffi` comments for optional parameters.

### Minimal example

```racket
(define window   (js-window-window))
(define document (js-window-document))
(define canvas   (js-get-element-by-id document "app-canvas"))
(define ctx      (js-canvas-get-context canvas "2d" (void)))

(js-set-canvas-width! canvas 640)
(js-set-canvas-height! canvas 480)
(js-set-canvas2d-fill-style! ctx "#4cc9f0")
(js-canvas2d-fill-rect ctx 40 40 200 120)
```

The remainder of this guide documents every canvas-related binding and shows a short usage example.

## HTMLCanvasElement bindings

- **`(js-canvas-capture-stream canvas fps)`** – Start streaming the canvas contents at `fps` frames per second. Returns a `MediaStream`.
  ```racket
  (define stream (js-canvas-capture-stream canvas 30.0))
  ```

- **`(js-canvas-get-context canvas kind attrs)`** – Retrieve a drawing context such as "2d" or "webgl". Pass `(void)` for `attrs` to get the default context options.
  ```racket
  (define ctx (js-canvas-get-context canvas "2d" (void)))
  ```

- **`(js-canvas-height canvas)`** – Read the intrinsic pixel height of the canvas.
  ```racket
  (define h (js-canvas-height canvas))
  ```

- **`(js-set-canvas-height! canvas height)`** – Set the pixel height of the canvas element.
  ```racket
  (js-set-canvas-height! canvas 480)
  ```

- **`(js-canvas-to-blob canvas callback type quality)`** – Serialize the canvas into a `Blob`, invoking `callback` with the result. Supply `""` for the default MIME type and `1.0` for full quality.
  ```racket
  (define (save-blob blob)
    (void))
  (js-canvas-to-blob canvas save-blob "" 1.0)
  ```

- **`(js-canvas-to-data-url canvas type quality)`** – Produce a data URL representing the canvas image.
  ```racket
  (define png-url (js-canvas-to-data-url canvas "image/png" 1.0))
  ```

- **`(js-canvas-transfer-control-to-offscreen canvas)`** – Move drawing control to an `OffscreenCanvas` for worker-based rendering.
  ```racket
  (define offscreen (js-canvas-transfer-control-to-offscreen canvas))
  ```

- **`(js-canvas-width canvas)`** – Read the canvas width in pixels.
  ```racket
  (define w (js-canvas-width canvas))
  ```

- **`(js-set-canvas-width! canvas width)`** – Set the intrinsic pixel width of the canvas element.
  ```racket
  (js-set-canvas-width! canvas 640)
  ```

## CanvasRenderingContext2D bindings

### Context accessors and global state

- **`(js-canvas2d-canvas ctx)`** – Retrieve the canvas element backing the context.
  ```racket
  (define backing (js-canvas2d-canvas ctx))
  ```

- **`(js-canvas2d-direction ctx)`** / **`(js-set-canvas2d-direction! ctx value)`** – Get or set the text direction (`"ltr"`, `"rtl"`, etc.).
  ```racket
  (js-set-canvas2d-direction! ctx "ltr")
  ```

- **`(js-canvas2d-fill-style ctx)`** / **`(js-set-canvas2d-fill-style! ctx style)`** – Access the current fill style (`string`, gradient, or pattern).
  ```racket
  (js-set-canvas2d-fill-style! ctx "#264653")
  ```

- **`(js-canvas2d-filter ctx)`** / **`(js-set-canvas2d-filter! ctx css-filter)`** – Get or set the CSS filter applied during drawing.
  ```racket
  (js-set-canvas2d-filter! ctx "blur(2px)")
  ```

- **`(js-canvas2d-font ctx)`** / **`(js-set-canvas2d-font! ctx css-font)`** – Manage the font string used for text rendering.
  ```racket
  (js-set-canvas2d-font! ctx "16px 'Fira Sans'")
  ```

- **`(js-canvas2d-global-alpha ctx)`** / **`(js-set-canvas2d-global-alpha! ctx alpha)`** – Read or set the global transparency multiplier.
  ```racket
  (js-set-canvas2d-global-alpha! ctx 0.75)
  ```

- **`(js-canvas2d-global-composite-operation ctx)`** / **`(js-set-canvas2d-global-composite-operation! ctx mode)`** – Control how new pixels are composited with existing content.
  ```racket
  (js-set-canvas2d-global-composite-operation! ctx "multiply")
  ```

- **`(js-canvas2d-image-smoothing-enabled ctx)`** / **`(js-set-canvas2d-image-smoothing-enabled! ctx enabled)`** – Toggle image smoothing for scaled images.
  ```racket
  (js-set-canvas2d-image-smoothing-enabled! ctx 0)
  ```

- **`(js-canvas2d-image-smoothing-quality ctx)`** / **`(js-set-canvas2d-image-smoothing-quality! ctx quality)`** – Inspect or set smoothing quality (`"low"`, `"medium"`, `"high"`).
  ```racket
  (js-set-canvas2d-image-smoothing-quality! ctx "high")
  ```

- **`(js-canvas2d-line-cap ctx)`** / **`(js-set-canvas2d-line-cap! ctx value)`** – Configure line cap style (`"butt"`, `"round"`, `"square"`).
  ```racket
  (js-set-canvas2d-line-cap! ctx "round")
  ```

- **`(js-canvas2d-line-dash-offset ctx)`** / **`(js-set-canvas2d-line-dash-offset! ctx offset)`** – Manage the phase for dashed strokes.
  ```racket
  (js-set-canvas2d-line-dash-offset! ctx 5.0)
  ```

- **`(js-canvas2d-line-join ctx)`** / **`(js-set-canvas2d-line-join! ctx value)`** – Control how line segments join (`"miter"`, `"round"`, `"bevel"`).
  ```racket
  (js-set-canvas2d-line-join! ctx "bevel")
  ```

- **`(js-canvas2d-line-width ctx)`** / **`(js-set-canvas2d-line-width! ctx width)`** – Inspect or set line stroke width.
  ```racket
  (js-set-canvas2d-line-width! ctx 6.0)
  ```

- **`(js-canvas2d-miter-limit ctx)`** / **`(js-set-canvas2d-miter-limit! ctx limit)`** – Adjust the miter limit for sharp corners.
  ```racket
  (js-set-canvas2d-miter-limit! ctx 8.0)
  ```

- **`(js-canvas2d-shadow-blur ctx)`** / **`(js-set-canvas2d-shadow-blur! ctx blur)`** – Control the amount of blur for shadow rendering.
  ```racket
  (js-set-canvas2d-shadow-blur! ctx 12.0)
  ```

- **`(js-canvas2d-shadow-color ctx)`** / **`(js-set-canvas2d-shadow-color! ctx color)`** – Manage the shadow color string.
  ```racket
  (js-set-canvas2d-shadow-color! ctx "rgba(0,0,0,0.4)")
  ```

- **`(js-canvas2d-shadow-offset-x ctx)`** / **`(js-set-canvas2d-shadow-offset-x! ctx dx)`** and **`(js-canvas2d-shadow-offset-y ctx)`** / **`(js-set-canvas2d-shadow-offset-y! ctx dy)`** – Configure the horizontal and vertical shadow offsets.
  ```racket
  (js-set-canvas2d-shadow-offset-x! ctx 8.0)
  (js-set-canvas2d-shadow-offset-y! ctx 10.0)
  ```

- **`(js-canvas2d-stroke-style ctx)`** / **`(js-set-canvas2d-stroke-style! ctx style)`** – Access the stroke style string, gradient, or pattern.
  ```racket
  (js-set-canvas2d-stroke-style! ctx "#e9c46a")
  ```

- **`(js-canvas2d-text-align ctx)`** / **`(js-set-canvas2d-text-align! ctx alignment)`** – Read or set horizontal text alignment.
  ```racket
  (js-set-canvas2d-text-align! ctx "center")
  ```

- **`(js-canvas2d-text-baseline ctx)`** / **`(js-set-canvas2d-text-baseline! ctx baseline)`** – Configure the baseline used for text placement.
  ```racket
  (js-set-canvas2d-text-baseline! ctx "middle")
  ```

- **`(js-canvas2d-text-rendering ctx)`** / **`(js-set-canvas2d-text-rendering! ctx mode)`** – Get or set text rendering hints (`"auto"`, `"optimizeSpeed"`, etc.).
  ```racket
  (js-set-canvas2d-text-rendering! ctx "geometricPrecision")
  ```

- **`(js-canvas2d-font-kerning ctx)`** / **`(js-set-canvas2d-font-kerning! ctx mode)`** – Control font kerning (`"auto"`, `"normal"`, `"none"`).
  ```racket
  (js-set-canvas2d-font-kerning! ctx "normal")
  ```

- **`(js-canvas2d-font-stretch ctx)`** / **`(js-set-canvas2d-font-stretch! ctx stretch)`** – Manage font stretch keywords (`"normal"`, `"expanded"`, etc.).
  ```racket
  (js-set-canvas2d-font-stretch! ctx "expanded")
  ```

- **`(js-canvas2d-font-variant-caps ctx)`** / **`(js-set-canvas2d-font-variant-caps! ctx variant)`** – Configure cap variant settings.
  ```racket
  (js-set-canvas2d-font-variant-caps! ctx "small-caps")
  ```

- **`(js-canvas2d-font-variant-ligatures ctx)`** / **`(js-set-canvas2d-font-variant-ligatures! ctx variant)`** – Toggle ligature usage.
  ```racket
  (js-set-canvas2d-font-variant-ligatures! ctx "none")
  ```

- **`(js-canvas2d-font-variant-numeric ctx)`** / **`(js-set-canvas2d-font-variant-numeric! ctx variant)`** – Choose numeric variant (`"tabular-nums"`, etc.).
  ```racket
  (js-set-canvas2d-font-variant-numeric! ctx "tabular-nums")
  ```

- **`(js-canvas2d-letter-spacing ctx)`** / **`(js-set-canvas2d-letter-spacing! ctx spacing)`** – Get or set letter spacing CSS value.
  ```racket
  (js-set-canvas2d-letter-spacing! ctx "0.1em")
  ```

- **`(js-canvas2d-word-spacing ctx)`** / **`(js-set-canvas2d-word-spacing! ctx spacing)`** – Manage word spacing CSS value.
  ```racket
  (js-set-canvas2d-word-spacing! ctx "0.2em")
  ```

- **`(js-canvas2d-get-line-dash ctx)`** / **`(js-canvas2d-set-line-dash ctx segments)`** – Inspect or set the dash pattern using an array of segment lengths.
  ```racket
  (define current-dash (js-canvas2d-get-line-dash ctx))
  (define dash-pattern ... ) ; obtain a JavaScript array-like object
  (js-canvas2d-set-line-dash ctx dash-pattern)
  ```

- **`(js-canvas2d-get-transform ctx)`**, **`(js-canvas2d-set-transform! ctx a b c d e f)`**, **`(js-canvas2d-set-transform-matrix! ctx matrix)`**, **`(js-canvas2d-transform ctx a b c d e f)`**, **`(js-canvas2d-reset-transform ctx)`**, **`(js-canvas2d-scale ctx sx sy)`**, **`(js-canvas2d-rotate ctx radians)`**, **`(js-canvas2d-translate ctx dx dy)`**, and **`(js-canvas2d-reset ctx)`** – Manage the current transformation matrix.
  ```racket
  (js-canvas2d-set-transform! ctx 1 0 0 1 0 0)
  (define current-transform (js-canvas2d-get-transform ctx))
  (js-canvas2d-set-transform-matrix! ctx current-transform)
  (js-canvas2d-transform ctx 1 0 0 1 40 20)
  (js-canvas2d-save ctx)
  (js-canvas2d-translate ctx 100 50)
  (js-canvas2d-rotate ctx (/ pi 6))
  (js-canvas2d-scale ctx 1.5 1.5)
  (js-canvas2d-reset-transform ctx)
  (js-canvas2d-reset ctx)
  (js-canvas2d-restore ctx)
  ```

### Path creation and drawing state stack

- **`(js-canvas2d-save ctx)`** / **`(js-canvas2d-restore ctx)`** – Push and pop the drawing state stack.
  ```racket
  (js-canvas2d-save ctx)
  (js-set-canvas2d-fill-style! ctx "#2a9d8f")
  (js-canvas2d-restore ctx)
  ```

- **`(js-canvas2d-begin-path ctx)`**, **`(js-canvas2d-close-path ctx)`**, **`(js-canvas2d-move-to ctx x y)`**, **`(js-canvas2d-line-to ctx x y)`**, **`(js-canvas2d-arc ctx x y radius start end ccw)`**, **`(js-canvas2d-arc-to ctx x1 y1 x2 y2 radius)`**, **`(js-canvas2d-bezier-curve-to ctx cp1x cp1y cp2x cp2y x y)`**, **`(js-canvas2d-quadratic-curve-to ctx cpx cpy x y)`**, **`(js-canvas2d-ellipse ctx x y rx ry rotation start end ccw)`**, **`(js-canvas2d-rect ctx x y width height)`**, and **`(js-canvas2d-round-rect ctx x y width height radii)`** – Build complex paths using straight lines and curves. Pass `(void)` for optional radii when calling `round-rect`.
  ```racket
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-move-to ctx 50 50)
  (js-canvas2d-line-to ctx 150 50)
  (js-canvas2d-arc-to ctx 150 150 50 150 30.0)
  (js-canvas2d-arc ctx 50 50 40 0 (* 2 pi) 0)
  (js-canvas2d-bezier-curve-to ctx 80 20 120 180 200 120)
  (js-canvas2d-quadratic-curve-to ctx 160 40 220 80)
  (js-canvas2d-ellipse ctx 150 120 40 60 0 0 (* 2 pi) 0)
  (js-canvas2d-rect ctx 20 20 60 40)
  (js-canvas2d-round-rect ctx 10 10 80 40 (void))
  (js-canvas2d-close-path ctx)
  ```

- **`(js-canvas2d-clear-rect ctx x y width height)`** – Clear a rectangle of pixels.
  ```racket
  (js-canvas2d-clear-rect ctx 0 0 640 480)
  ```

- **`(js-canvas2d-fill ctx path fill-rule)`**, **`(js-canvas2d-stroke ctx path)`**, **`(js-canvas2d-fill-rect ctx x y width height)`**, **`(js-canvas2d-stroke-rect ctx x y width height)`**, **`(js-canvas2d-fill-text ctx text x y max-width)`**, and **`(js-canvas2d-stroke-text ctx text x y max-width)`** – Render fills and strokes for the current path or primitives. For optional arguments supply `(void)` as needed.
  ```racket
  (js-canvas2d-fill ctx (void) (void))
  (js-canvas2d-fill-rect ctx 10 10 60 40)
  (js-canvas2d-stroke-rect ctx 20 20 120 80)
  (js-canvas2d-fill-text ctx "Hello" 100 100 (void))
  ```

- **`(js-canvas2d-draw-image ctx image dx dy)`**, **`(js-canvas2d-draw-image-5 ctx image dx dy dwidth dheight)`**, and **`(js-canvas2d-draw-image-9 ctx image sx sy swidth sheight dx dy dwidth dheight)`** – Draw raster images, optionally scaling or cropping.
  ```racket
  (js-canvas2d-draw-image ctx image 0 0)
  (js-canvas2d-draw-image-5 ctx image 0 0 128 128)
  (js-canvas2d-draw-image-9 ctx image 32 32 64 64 0 0 128 128)
  ```

- **`(js-canvas2d-draw-focus-if-needed! ctx element)`** and **`(js-canvas2d-draw-focus-if-needed-path! ctx path element)`** – Render a focus ring when the referenced element is focused.
  ```racket
  (js-canvas2d-draw-focus-if-needed! ctx canvas)
  (define focus-path ... ) ; obtain a Path2D instance
  (js-canvas2d-draw-focus-if-needed-path! ctx focus-path canvas)
  ```

- **`(js-canvas2d-clip ctx path fill-rule)`** – Intersect subsequent drawing with the specified clipping region. Optional arguments accept `(void)`.
  ```racket
  (js-canvas2d-clip ctx (void) (void))
  ```

### Text measurement

- **`(js-canvas2d-measure-text ctx text)`** – Measure text metrics for the current font settings.
  ```racket
  (define metrics (js-canvas2d-measure-text ctx "Hello"))
  ```

### Pixel and image data

- **`(js-canvas2d-create-image-data ctx width height)`** and **`(js-canvas2d-create-image-data-from ctx source)`** – Allocate new `ImageData` buffers.
  ```racket
  (define img (js-canvas2d-create-image-data ctx 320 200))
  (define clone (js-canvas2d-create-image-data-from ctx img))
  ```

- **`(js-canvas2d-get-image-data ctx sx sy sw sh settings)`** – Read pixels from the canvas. Pass `(void)` for `settings` to use defaults.
  ```racket
  (define pixels (js-canvas2d-get-image-data ctx 0 0 100 100 (void)))
  ```

- **`(js-canvas2d-put-image-data ctx image dx dy dirty-x dirty-y dirty-width dirty-height)`** – Write pixels back. Supply `(void)` for optional dirty rectangle parameters you do not need.
  ```racket
  (js-canvas2d-put-image-data ctx img 0 0 (void) (void) (void) (void))
  ```

- **`(js-canvas2d-create-linear-gradient ctx x0 y0 x1 y1)`**, **`(js-canvas2d-create-radial-gradient ctx x0 y0 r0 x1 y1 r1)`**, **`(js-canvas2d-create-conic-gradient ctx angle x y)`**, and **`(js-canvas2d-create-pattern ctx image repetition)`** – Build gradient and pattern objects for fills or strokes.
  ```racket
  (define gradient (js-canvas2d-create-linear-gradient ctx 0 0 0 200))
  (define radial (js-canvas2d-create-radial-gradient ctx 75 75 10 75 75 70))
  (define conic (js-canvas2d-create-conic-gradient ctx 0 150 150))
  (define pattern (js-canvas2d-create-pattern ctx image "repeat"))
  ```

### Hit testing

- **`(js-canvas2d-is-point-in-path ctx path x y fill-rule)`** and **`(js-canvas2d-is-point-in-stroke ctx path x y)`** – Test whether a coordinate falls within the current path or stroke. Optional parameters accept `(void)`.
  ```racket
  (define inside? (js-canvas2d-is-point-in-path ctx (void) 75.0 75.0 (void)))
  (define on-stroke? (js-canvas2d-is-point-in-stroke ctx (void) 75.0 75.0))
  ```

### Path measurement and configuration helpers

- **`(js-canvas2d-get-line-dash ctx)`**, **`(js-canvas2d-set-line-dash ctx segments)`**, **`(js-canvas2d-line-dash-offset ctx)`**, and **`(js-set-canvas2d-line-dash-offset! ctx offset)`** – Work with dash patterns and offsets.
  ```racket
  (define current-dash (js-canvas2d-get-line-dash ctx))
  (define dash-pattern ... ) ; obtain a JavaScript array-like object
  (js-canvas2d-set-line-dash ctx dash-pattern)
  (js-set-canvas2d-line-dash-offset! ctx 5.0)
  ```

- **`(js-canvas2d-line-width ctx)`**, **`(js-set-canvas2d-line-width! ctx width)`**, **`(js-canvas2d-miter-limit ctx)`**, and **`(js-set-canvas2d-miter-limit! ctx limit)`** – Control stroke thickness and join limits.
  ```racket
  (define current-width (js-canvas2d-line-width ctx))
  (js-set-canvas2d-line-width! ctx (+ current-width 2.0))
  (js-set-canvas2d-miter-limit! ctx 8.0)
  ```

(See examples above for these properties.)

### Additional utilities

- **`(js-canvas2d-stroke ctx path)`** – Stroke the current or provided path, optionally passing `(void)` to use the current path.
  ```racket
  (js-canvas2d-stroke ctx (void))
  ```

- **`(js-canvas2d-stroke-rect ctx x y width height)`** – Stroke an axis-aligned rectangle directly.
  ```racket
  (js-canvas2d-stroke-rect ctx 200 40 160 90)
  ```

- **`(js-canvas2d-stroke-text ctx text x y max-width)`** – Stroke text outlines; pass `(void)` for optional maximum width.
  ```racket
  (js-canvas2d-stroke-text ctx "Outline" 120 160 (void))
  ```

- **`(js-canvas2d-stroke ctx path)`**, **`(js-canvas2d-fill ctx path fill-rule)`**, and **`(js-canvas2d-fill-text ctx text x y max-width)`** share the same optional argument semantics illustrated earlier.
