#lang webracket

;;;
;;; Canvas wrappers
;;;

;; canvas-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (canvas-i32->boolean v)
  (not (zero? v)))

;; canvas-capture-stream : external? -> external/raw
;;   Capture the canvas stream.
(define (canvas-capture-stream canvas [frame-rate (void)])
  (js-canvas-capture-stream canvas frame-rate))

;; canvas-get-context : external? string? [any/c] -> (or/c #f external?)
;;   Read a drawing context from a canvas.
(define (canvas-get-context canvas context-id [options (void)])
  (unless (string? context-id)
    (raise-argument-error 'canvas-get-context "string?" context-id))
  (js-canvas-get-context canvas context-id options))

;; canvas-width : external? -> exact-nonnegative-integer?
;;   Read the canvas width.
(define (canvas-width canvas)
  (js-canvas-width canvas))

;; canvas-set-width! : external? exact-nonnegative-integer? -> void?
;;   Set the canvas width.
(define (canvas-set-width! canvas width)
  (js-set-canvas-width! canvas width)
  (void))

;; canvas-height : external? -> exact-nonnegative-integer?
;;   Read the canvas height.
(define (canvas-height canvas)
  (js-canvas-height canvas))

;; canvas-set-height! : external? exact-nonnegative-integer? -> void?
;;   Set the canvas height.
(define (canvas-set-height! canvas height)
  (js-set-canvas-height! canvas height)
  (void))

;; canvas-to-data-url : external? string? [any/c] -> string?
;;   Encode a canvas as a data URL.
(define (canvas-to-data-url canvas [type "image/png"] [quality (void)])
  (unless (string? type)
    (raise-argument-error 'canvas-to-data-url "string?" type))
  (js-canvas-to-data-url canvas type quality))

;; canvas-to-blob : external? external? string? [any/c] -> void?
;;   Encode a canvas into a Blob callback.
(define (canvas-to-blob canvas callback [type "image/png"] [quality (void)])
  (unless (string? type)
    (raise-argument-error 'canvas-to-blob "string?" type))
  (js-canvas-to-blob canvas callback type quality)
  (void))

;; canvas-transfer-control-to-offscreen : external? -> external/raw
;;   Transfer control to an OffscreenCanvas.
(define (canvas-transfer-control-to-offscreen canvas)
  (js-canvas-transfer-control-to-offscreen canvas))

;; canvas-2d-canvas : external? -> external/raw
;;   Read the backing canvas from a 2D context.
(define (canvas-2d-canvas ctx)
  (js-canvas2d-canvas ctx))

;; canvas-2d-direction : external? -> string?
;;   Read the canvas direction.
(define (canvas-2d-direction ctx)
  (js-canvas2d-direction ctx))

;; canvas-2d-set-direction! : external? string? -> void?
;;   Set the canvas direction.
(define (canvas-2d-set-direction! ctx direction)
  (js-set-canvas2d-direction! ctx direction)
  (void))

;; canvas-2d-fill-style : external? -> string?
;;   Read the fill style.
(define (canvas-2d-fill-style ctx)
  (js-ref ctx "fillStyle"))

;; canvas-2d-set-fill-style! : external? any/c -> void?
;;   Set the fill style.
(define (canvas-2d-set-fill-style! ctx style)
  (js-set-canvas2d-fill-style! ctx style)
  (void))

;; canvas-2d-stroke-style : external? -> string?
;;   Read the stroke style.
(define (canvas-2d-stroke-style ctx)
  (js-ref ctx "strokeStyle"))

;; canvas-2d-set-stroke-style! : external? any/c -> void?
;;   Set the stroke style.
(define (canvas-2d-set-stroke-style! ctx style)
  (js-set-canvas2d-stroke-style! ctx style)
  (void))

;; canvas-2d-fill-rect : external? real? real? real? real? -> void?
;;   Fill a rectangle.
(define (canvas-2d-fill-rect ctx x y w h)
  (js-canvas2d-fill-rect ctx x y w h)
  (void))

;; canvas-2d-stroke-rect : external? real? real? real? real? -> void?
;;   Stroke a rectangle.
(define (canvas-2d-stroke-rect ctx x y w h)
  (js-canvas2d-stroke-rect ctx x y w h)
  (void))

;; canvas-2d-begin-path : external? -> void?
;;   Begin a new path.
(define (canvas-2d-begin-path ctx)
  (js-canvas2d-begin-path ctx)
  (void))

;; canvas-2d-close-path : external? -> void?
;;   Close the current path.
(define (canvas-2d-close-path ctx)
  (js-canvas2d-close-path ctx)
  (void))

;; canvas-2d-move-to : external? real? real? -> void?
;;   Move the current path cursor.
(define (canvas-2d-move-to ctx x y)
  (js-canvas2d-move-to ctx x y)
  (void))

;; canvas-2d-line-to : external? real? real? -> void?
;;   Add a line segment to the path.
(define (canvas-2d-line-to ctx x y)
  (js-canvas2d-line-to ctx x y)
  (void))

;; canvas-2d-arc : external? real? real? real? real? real? -> void?
;;   Add an arc to the path.
(define (canvas-2d-arc ctx x y radius start-angle end-angle)
  (js-canvas2d-arc ctx x y radius start-angle end-angle)
  (void))

;; canvas-2d-clear-rect : external? real? real? real? real? -> void?
;;   Clear a rectangle.
(define (canvas-2d-clear-rect ctx x y w h)
  (js-canvas2d-clear-rect ctx x y w h)
  (void))

;; canvas-2d-fill : external? [any/c] [any/c] -> void?
;;   Fill the current path.
(define (canvas-2d-fill ctx [path (void)] [fill-rule (void)])
  (js-canvas2d-fill ctx path fill-rule)
  (void))

;; canvas-2d-stroke : external? [any/c] -> void?
;;   Stroke the current path.
(define (canvas-2d-stroke ctx [path (void)])
  (js-canvas2d-stroke ctx path)
  (void))

;; canvas-2d-save : external? -> void?
;;   Save drawing state.
(define (canvas-2d-save ctx)
  (js-canvas2d-save ctx)
  (void))

;; canvas-2d-restore : external? -> void?
;;   Restore drawing state.
(define (canvas-2d-restore ctx)
  (js-canvas2d-restore ctx)
  (void))

;; canvas-2d-translate : external? real? real? -> void?
;;   Translate the context.
(define (canvas-2d-translate ctx x y)
  (js-canvas2d-translate ctx x y)
  (void))

;; canvas-2d-scale : external? real? real? -> void?
;;   Scale the context.
(define (canvas-2d-scale ctx x y)
  (js-canvas2d-scale ctx x y)
  (void))

;; canvas-2d-rotate : external? real? -> void?
;;   Rotate the context.
(define (canvas-2d-rotate ctx angle)
  (js-canvas2d-rotate ctx angle)
  (void))

;; canvas-2d-fill-text : external? string? real? real? [any/c] -> void?
;;   Draw filled text.
(define (canvas-2d-fill-text ctx text x y [max-width (void)])
  (js-canvas2d-fill-text ctx text x y max-width)
  (void))

;; canvas-2d-stroke-text : external? string? real? real? [any/c] -> void?
;;   Draw stroked text.
(define (canvas-2d-stroke-text ctx text x y [max-width (void)])
  (js-canvas2d-stroke-text ctx text x y max-width)
  (void))

;; canvas-2d-measure-text : external? string? -> external/raw
;;   Measure text width and metrics.
(define (canvas-2d-measure-text ctx text)
  (js-canvas2d-measure-text ctx text))

;; canvas-2d-create-linear-gradient : external? real? real? real? real? -> external/raw
;;   Create a linear gradient.
(define (canvas-2d-create-linear-gradient ctx x0 y0 x1 y1)
  (js-canvas2d-create-linear-gradient ctx x0 y0 x1 y1))

;; canvas-2d-create-radial-gradient : external? real? real? real? real? real? -> external/raw
;;   Create a radial gradient.
(define (canvas-2d-create-radial-gradient ctx x0 y0 r0 x1 y1 r1)
  (js-canvas2d-create-radial-gradient ctx x0 y0 r0 x1 y1 r1))
