;;;
;;; Helpers
;;;

(define (->f x)
  (if (exact? x) (exact->inexact x) x))

;;;
;;; TILES
;;; 

(define tile-size   32)
(define tile-margin 16)
(define tile-count   3)

;;;
;;; CANVAS
;;;

(define canvas-width (+ (* tile-count tile-size)
                        (* (+ tile-count 1) tile-margin)))
(define canvas-height (+ tile-size (* 2 tile-margin)))


;;;
;;; DOM
;;;

(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas canvas-width)
(js-set-canvas-height! canvas canvas-height)
(js-set-attribute! canvas "style"
                   (string-append
                    "background: #202124; image-rendering: pixelated; "
                    "display: block; margin: 32px auto; border: 2px solid #333; "
                    "box-shadow: 0 12px 36px rgba(0, 0, 0, 0.35);"))
(js-set-attribute! (js-document-body) "style"
                   "margin: 0; background: #111; font-family: 'Fira Code', monospace; color: #fafafa;")
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

;;;
;;; Graphics
;;;

(define (fill color x y w h)
  (js-set-canvas2d-fill-style! ctx color)
  (js-canvas2d-fill-rect ctx (->f x) (->f y) (->f w) (->f h)))

(define (draw-grass x y)
  (define (patch color dx dy w h)
    (fill color (+ x dx) (+ y dy) w h))
  (fill "#3fa436" x y tile-size tile-size)
  (patch "#2f7d1a" 0 24 tile-size 8)
  (patch "#2f7d1a" 20 18 12 6)
  (patch "#49b02d" 0 0 tile-size 5)
  (patch "#5ccd3d" 4 8 8 8)
  (patch "#5ccd3d" 16 12 10 6)
  (patch "#7be25f" 8 4 6 6)
  (patch "#4ca936" 6 20 10 6)
  (patch "#347b1f" 12 18 6 8)
  (patch "#2c641a" 2 12 6 6))

(define (draw-stone-wall x y)
  (define (patch color dx dy w h)
    (fill color (+ x dx) (+ y dy) w h))
  (fill "#8c8c94" x y tile-size tile-size)
  (patch "#4f4f57" 0 0 tile-size 4)
  (patch "#4f4f57" 0 (- tile-size 4) tile-size 4)
  (patch "#4f4f57" 0 0 4 tile-size)
  (patch "#4f4f57" (- tile-size 4) 0 4 tile-size)
  (patch "#bcbcc4" 4 4 24 8)
  (patch "#a4a4ac" 4 12 24 8)
  (patch "#94949c" 4 20 12 8)
  (patch "#787880" 16 20 12 8)
  (patch "#d6d6de" 4 4 12 4)
  (patch "#d6d6de" 16 12 8 4)
  (patch "#6c6c74" 10 14 6 4)
  (patch "#6c6c74" 20 6 6 4)
  (patch "#5a5a62" 18 22 8 4))

(define (draw-flag x y)
  (define (patch color dx dy w h)
    (fill color (+ x dx) (+ y dy) w h))
  (fill "#78b9ff" x y tile-size tile-size)
  (patch "#3fa436" 0 (- tile-size 8) tile-size 8)
  (patch "#4ec748" 0 (- tile-size 8) tile-size 3)
  (patch "#d8d8d8" 12 6 4 20)
  (patch "#f0f0f0" 12 6 2 10)
  (patch "#7a7a7a" 10 6 2 20)
  (patch "#f5d142" 16 8 12 10)
  (patch "#fff3a3" 16 8 12 4)
  (patch "#d49b26" 16 14 12 4)
  (patch "#cfd8ff" 6 6 6 4)
  (patch "#cfd8ff" 20 4 8 4)
  (patch "#cfd8ff" 18 10 6 4)
  (patch "#b8862f" 10 26 12 2)
  (patch "#a07125" 10 28 12 2))

;;;
;;; DRAW
;;;


(fill "#181818" 0 0 canvas-width canvas-height)

(define tile-y  tile-margin)
(define grass-x tile-margin)
(define stone-x (+ grass-x tile-size tile-margin))
(define flag-x  (+ stone-x tile-size tile-margin))

(draw-grass      grass-x tile-y)
(draw-stone-wall stone-x tile-y)
(draw-flag       flag-x  tile-y)
