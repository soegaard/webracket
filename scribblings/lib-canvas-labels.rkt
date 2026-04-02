#lang racket/base

(require racket/contract/base)

(provide
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
  canvas-2d-fill-rect
  canvas-2d-stroke-rect
  canvas-2d-begin-path
  canvas-2d-close-path
  canvas-2d-move-to
  canvas-2d-line-to
  canvas-2d-arc
  canvas-2d-clear-rect
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
  canvas-2d-create-linear-gradient
  canvas-2d-create-radial-gradient
  (for-label (all-defined-out)))

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
(define canvas-2d-fill-rect any/c)
(define canvas-2d-stroke-rect any/c)
(define canvas-2d-begin-path any/c)
(define canvas-2d-close-path any/c)
(define canvas-2d-move-to any/c)
(define canvas-2d-line-to any/c)
(define canvas-2d-arc any/c)
(define canvas-2d-clear-rect any/c)
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
(define canvas-2d-create-linear-gradient any/c)
(define canvas-2d-create-radial-gradient any/c)
