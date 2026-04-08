#lang racket/base

(require racket/contract/base)

(provide
  (struct-out html-canvas-element)
  (struct-out canvas-rendering-context-2d)
  (struct-out image-data)
  (struct-out canvas-gradient)
  (struct-out canvas-pattern)
  (struct-out text-metrics)
  (struct-out dom-matrix)
  (struct-out offscreen-canvas)
  canvas-capture-stream
  canvas-get-context
  canvas-width
  canvas-set-width!
  canvas-height
  canvas-set-height!
  canvas-to-data-url
  canvas-to-blob
  canvas-transfer-control-to-offscreen
  canvas-2d-canvas
  canvas-2d-direction
  canvas-2d-set-direction!
  canvas-2d-fill-style
  canvas-2d-set-fill-style!
  canvas-2d-stroke-style
  canvas-2d-set-stroke-style!
  canvas-2d-filter
  canvas-2d-set-filter!
  canvas-2d-font
  canvas-2d-set-font!
  canvas-2d-global-alpha
  canvas-2d-set-global-alpha!
  canvas-2d-global-composite-operation
  canvas-2d-set-global-composite-operation!
  canvas-2d-image-smoothing-enabled?
  canvas-2d-set-image-smoothing-enabled!
  canvas-2d-image-smoothing-quality
  canvas-2d-set-image-smoothing-quality!
  canvas-2d-line-cap
  canvas-2d-set-line-cap!
  canvas-2d-line-dash-offset
  canvas-2d-set-line-dash-offset!
  canvas-2d-line-join
  canvas-2d-set-line-join!
  canvas-2d-line-width
  canvas-2d-set-line-width!
  canvas-2d-miter-limit
  canvas-2d-set-miter-limit!
  canvas-2d-shadow-blur
  canvas-2d-set-shadow-blur!
  canvas-2d-shadow-color
  canvas-2d-set-shadow-color!
  canvas-2d-shadow-offset-x
  canvas-2d-set-shadow-offset-x!
  canvas-2d-shadow-offset-y
  canvas-2d-set-shadow-offset-y!
  canvas-2d-text-align
  canvas-2d-set-text-align!
  canvas-2d-text-baseline
  canvas-2d-set-text-baseline!
  canvas-2d-text-rendering
  canvas-2d-set-text-rendering!
  canvas-2d-font-kerning
  canvas-2d-set-font-kerning!
  canvas-2d-font-stretch
  canvas-2d-set-font-stretch!
  canvas-2d-font-variant-caps
  canvas-2d-set-font-variant-caps!
  canvas-2d-font-variant-ligatures
  canvas-2d-set-font-variant-ligatures!
  canvas-2d-font-variant-numeric
  canvas-2d-set-font-variant-numeric!
  canvas-2d-letter-spacing
  canvas-2d-set-letter-spacing!
  canvas-2d-word-spacing
  canvas-2d-set-word-spacing!
  canvas-2d-fill-rect
  canvas-2d-stroke-rect
  canvas-2d-begin-path
  canvas-2d-close-path
  canvas-2d-move-to
  canvas-2d-line-to
  canvas-2d-arc
  canvas-2d-arc-to
  canvas-2d-bezier-curve-to
  canvas-2d-clear-rect
  canvas-2d-clip
  canvas-2d-fill
  canvas-2d-stroke
  canvas-2d-save
  canvas-2d-restore
  canvas-2d-translate
  canvas-2d-scale
  canvas-2d-rotate
  canvas-2d-fill-text
  canvas-2d-stroke-text
  canvas-2d-measure-text
  canvas-2d-create-image-data
  canvas-2d-create-image-data-from
  canvas-2d-create-pattern
  canvas-2d-create-linear-gradient
  canvas-2d-create-radial-gradient
  canvas-2d-create-conic-gradient
  canvas-gradient-add-color-stop!
  canvas-pattern-set-transform!
  canvas-2d-draw-focus-if-needed!
  canvas-2d-draw-focus-if-needed-path!
  canvas-2d-draw-image
  canvas-2d-draw-image-5
  canvas-2d-draw-image-9
  canvas-2d-ellipse
  canvas-2d-get-image-data
  canvas-2d-get-line-dash
  canvas-2d-get-transform
  canvas-2d-is-point-in-path
  canvas-2d-is-point-in-stroke
  canvas-2d-put-image-data
  canvas-2d-quadratic-curve-to
  canvas-2d-rect
  canvas-2d-reset
  canvas-2d-reset-transform
  canvas-2d-round-rect
  canvas-2d-set-line-dash
  canvas-2d-set-transform!
  canvas-2d-set-transform-matrix!
  canvas-2d-transform
  canvas-image-data-width
  canvas-image-data-height
  canvas-image-data-data
  canvas-text-metrics-width
  canvas-dom-matrix-a
  canvas-dom-matrix-b
  canvas-dom-matrix-c
  canvas-dom-matrix-d
  canvas-dom-matrix-e
  canvas-dom-matrix-f
  (for-label (all-defined-out)))

(struct html-canvas-element (raw) #:transparent)
(struct canvas-rendering-context-2d (raw) #:transparent)
(struct image-data (raw) #:transparent)
(struct canvas-gradient (raw) #:transparent)
(struct canvas-pattern (raw) #:transparent)
(struct text-metrics (raw) #:transparent)
(struct dom-matrix (raw) #:transparent)
(struct offscreen-canvas (raw) #:transparent)
(define canvas-capture-stream any/c)
(define canvas-get-context any/c)
(define canvas-width any/c)
(define canvas-set-width! any/c)
(define canvas-height any/c)
(define canvas-set-height! any/c)
(define canvas-to-data-url any/c)
(define canvas-to-blob any/c)
(define canvas-transfer-control-to-offscreen any/c)
(define canvas-2d-canvas any/c)
(define canvas-2d-direction any/c)
(define canvas-2d-set-direction! any/c)
(define canvas-2d-fill-style any/c)
(define canvas-2d-set-fill-style! any/c)
(define canvas-2d-stroke-style any/c)
(define canvas-2d-set-stroke-style! any/c)
(define canvas-2d-filter any/c)
(define canvas-2d-set-filter! any/c)
(define canvas-2d-font any/c)
(define canvas-2d-set-font! any/c)
(define canvas-2d-global-alpha any/c)
(define canvas-2d-set-global-alpha! any/c)
(define canvas-2d-global-composite-operation any/c)
(define canvas-2d-set-global-composite-operation! any/c)
(define canvas-2d-image-smoothing-enabled? any/c)
(define canvas-2d-set-image-smoothing-enabled! any/c)
(define canvas-2d-image-smoothing-quality any/c)
(define canvas-2d-set-image-smoothing-quality! any/c)
(define canvas-2d-line-cap any/c)
(define canvas-2d-set-line-cap! any/c)
(define canvas-2d-line-dash-offset any/c)
(define canvas-2d-set-line-dash-offset! any/c)
(define canvas-2d-line-join any/c)
(define canvas-2d-set-line-join! any/c)
(define canvas-2d-line-width any/c)
(define canvas-2d-set-line-width! any/c)
(define canvas-2d-miter-limit any/c)
(define canvas-2d-set-miter-limit! any/c)
(define canvas-2d-shadow-blur any/c)
(define canvas-2d-set-shadow-blur! any/c)
(define canvas-2d-shadow-color any/c)
(define canvas-2d-set-shadow-color! any/c)
(define canvas-2d-shadow-offset-x any/c)
(define canvas-2d-set-shadow-offset-x! any/c)
(define canvas-2d-shadow-offset-y any/c)
(define canvas-2d-set-shadow-offset-y! any/c)
(define canvas-2d-text-align any/c)
(define canvas-2d-set-text-align! any/c)
(define canvas-2d-text-baseline any/c)
(define canvas-2d-set-text-baseline! any/c)
(define canvas-2d-text-rendering any/c)
(define canvas-2d-set-text-rendering! any/c)
(define canvas-2d-font-kerning any/c)
(define canvas-2d-set-font-kerning! any/c)
(define canvas-2d-font-stretch any/c)
(define canvas-2d-set-font-stretch! any/c)
(define canvas-2d-font-variant-caps any/c)
(define canvas-2d-set-font-variant-caps! any/c)
(define canvas-2d-font-variant-ligatures any/c)
(define canvas-2d-set-font-variant-ligatures! any/c)
(define canvas-2d-font-variant-numeric any/c)
(define canvas-2d-set-font-variant-numeric! any/c)
(define canvas-2d-letter-spacing any/c)
(define canvas-2d-set-letter-spacing! any/c)
(define canvas-2d-word-spacing any/c)
(define canvas-2d-set-word-spacing! any/c)
(define canvas-2d-fill-rect any/c)
(define canvas-2d-stroke-rect any/c)
(define canvas-2d-begin-path any/c)
(define canvas-2d-close-path any/c)
(define canvas-2d-move-to any/c)
(define canvas-2d-line-to any/c)
(define canvas-2d-arc any/c)
(define canvas-2d-arc-to any/c)
(define canvas-2d-bezier-curve-to any/c)
(define canvas-2d-clear-rect any/c)
(define canvas-2d-clip any/c)
(define canvas-2d-fill any/c)
(define canvas-2d-stroke any/c)
(define canvas-2d-save any/c)
(define canvas-2d-restore any/c)
(define canvas-2d-translate any/c)
(define canvas-2d-scale any/c)
(define canvas-2d-rotate any/c)
(define canvas-2d-fill-text any/c)
(define canvas-2d-stroke-text any/c)
(define canvas-2d-measure-text any/c)
(define canvas-2d-create-image-data any/c)
(define canvas-2d-create-image-data-from any/c)
(define canvas-2d-create-pattern any/c)
(define canvas-2d-create-linear-gradient any/c)
(define canvas-2d-create-radial-gradient any/c)
(define canvas-2d-create-conic-gradient any/c)
(define canvas-gradient-add-color-stop! any/c)
(define canvas-pattern-set-transform! any/c)
(define canvas-2d-draw-focus-if-needed! any/c)
(define canvas-2d-draw-focus-if-needed-path! any/c)
(define canvas-2d-draw-image any/c)
(define canvas-2d-draw-image-5 any/c)
(define canvas-2d-draw-image-9 any/c)
(define canvas-2d-ellipse any/c)
(define canvas-2d-get-image-data any/c)
(define canvas-2d-get-line-dash any/c)
(define canvas-2d-get-transform any/c)
(define canvas-2d-is-point-in-path any/c)
(define canvas-2d-is-point-in-stroke any/c)
(define canvas-2d-put-image-data any/c)
(define canvas-2d-quadratic-curve-to any/c)
(define canvas-2d-rect any/c)
(define canvas-2d-reset any/c)
(define canvas-2d-reset-transform any/c)
(define canvas-2d-round-rect any/c)
(define canvas-2d-set-line-dash any/c)
(define canvas-2d-set-transform! any/c)
(define canvas-2d-set-transform-matrix! any/c)
(define canvas-2d-transform any/c)
(define canvas-image-data-width any/c)
(define canvas-image-data-height any/c)
(define canvas-image-data-data any/c)
(define canvas-text-metrics-width any/c)
(define canvas-dom-matrix-a any/c)
(define canvas-dom-matrix-b any/c)
(define canvas-dom-matrix-c any/c)
(define canvas-dom-matrix-d any/c)
(define canvas-dom-matrix-e any/c)
(define canvas-dom-matrix-f any/c)
