;;;
;;; Helpers
;;;

(define (->f x)
  (if (exact? x) (exact->inexact x) x))

(define (->i x)
  (if (inexact? x) (inexact->exact x) x))

;;;
;;; TILES
;;; 

(define tile-size    128)
(define tile-margin   32)
(define tile-columns   4)

(define px (/ tile-size 16))

;;;
;;; PALETTE
;;;

(define col-night "#181823")
(define col-deep-shadow "#0c0d16")
(define col-outline "#111423")
(define col-fur-light "#f5e6c6")
(define col-fur-main "#d5b591")
(define col-fur-dark "#b98d62")
(define col-mask-dark "#3d3f56")
(define col-mask-light "#6a6d86")
(define col-eye "#0f1220")
(define col-eye-shine "#f8f1dc")
(define col-flag-main "#f7c744")
(define col-flag-light "#ffe27a")
(define col-flag-shadow "#b98524")
(define col-grass-dark "#2f4d1d")
(define col-grass-main "#3e6f2b")
(define col-grass-light "#62a434")
(define col-mud-main "#4a3523")
(define col-mud-light "#6b4a2d")
(define col-mud-dark "#2e2116")
(define col-water-dark "#1e364f")
(define col-water-main "#265779")
(define col-water-light "#4b8bc4")
(define col-bridge-dark "#5b3b20")
(define col-bridge-main "#80522c")
(define col-bridge-light "#a0652f")
(define col-stone-dark "#2e303f")
(define col-stone-main "#4f5265")
(define col-stone-light "#7a7d90")
(define col-stone-edge "#9da1ba")
(define col-log-main "#6a3a1d")
(define col-log-light "#8b4c26")
(define col-metal-dark "#302620")
(define col-metal-main "#4a3526")
(define col-metal-highlight "#6b4a32")
(define col-key-main "#f7c744")
(define col-key-light "#ffe27a")
(define col-key-dark "#b98524")
(define col-door-wood "#7d4b28")
(define col-door-shadow "#512f1a")
(define col-door-frame "#c68d44")
(define col-door-highlight "#f5e6c6")
(define col-bomb-body "#1e2233")
(define col-bomb-light "#333a52")
(define col-bomb-fuse "#f3d47c")
(define col-bomb-spark "#ff9c3a")

;;;
;;; TILES
;;;

(define tiles-per-row tile-columns)
(define tiles
  '(draw-hero-raccoon
    draw-yellow-flag
    draw-mud
    draw-grass
    draw-water
    draw-bridge
    draw-stone-wall
    draw-tree-log
    draw-golden-key
    draw-lock
    draw-closed-door
    draw-open-door
    draw-bomb))


(define tile-count (length tiles))
(define tile-rows  (ceiling (/ tile-count tiles-per-row)))


;;;
;;; CANVAS
;;;

(define canvas-width  (->i (+ (* tiles-per-row tile-size) (* (+ tiles-per-row 1) tile-margin))))
(define canvas-height (->i (+ (* tile-rows     tile-size) (* (+ tile-rows     1) tile-margin))))


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

(define (tile-patch x y color dx dy w h)
  (fill color (+ x (* dx px)) (+ y (* dy px)) (* w px) (* h px)))

(define (draw-hero-raccoon x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-night x y tile-size tile-size)
  (patch col-deep-shadow 0 12 16 4)
  (patch col-outline 4 1 3 2)
  (patch col-outline 9 1 3 2)
  (patch col-fur-light 5 2 1 1)
  (patch col-fur-light 10 2 1 1)
  (patch col-fur-main 3 3 10 6)
  (patch col-fur-light 4 4 8 2)
  (patch col-mask-dark 3 6 10 3)
  (patch col-mask-light 4 6 8 1)
  (patch col-eye 5 7 1 1)
  (patch col-eye 10 7 1 1)
  (patch col-eye-shine 5 7 1/2 1/2)
  (patch col-eye-shine 10 7 1/2 1/2)
  (patch col-fur-dark 3 9 2 2)
  (patch col-fur-dark 11 9 2 2)
  (patch col-fur-light 5 9 6 2)
  (patch col-eye 7 10 2 1)
  (patch col-fur-main 4 11 8 5)
  (patch col-fur-light 5 12 6 3)
  (patch col-fur-dark 3 12 1 3)
  (patch col-fur-dark 12 12 1 3)
  (patch col-fur-main 12 11 2 5)
  (patch col-fur-light 12 12 2 2)
  (patch col-mask-dark 12 14 2 1)
  (patch col-outline 5 15 3 1)
  (patch col-outline 8 15 3 1))

(define (draw-yellow-flag x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-water-dark x y tile-size tile-size)
  (patch col-night 0 12 16 4)
  (patch col-grass-dark 0 11 16 1)
  (patch col-grass-main 0 10 16 1)
  (patch "#8f949f" 7 2 2 12)
  (patch "#cfd6e4" 7 2 1 12)
  (patch col-flag-shadow 8 4 5 4)
  (patch col-flag-main 8 3 6 6)
  (patch col-flag-light 9 3 4 2)
  (patch col-flag-light 9 5 3 1)
  (patch col-flag-shadow 8 7 6 1))

(define (draw-mud x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-mud-dark x y tile-size tile-size)
  (patch col-mud-main 0 2 16 12)
  (patch col-mud-light 0 2 16 2)
  (patch col-mud-dark 1 8 3 2)
  (patch col-mud-dark 6 5 4 2)
  (patch col-mud-dark 11 9 3 2)
  (patch col-mud-light 3 12 4 1)
  (patch col-mud-light 9 10 3 1)
  (patch col-mud-light 12 6 2 1))

(define (draw-grass x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-grass-dark x y tile-size tile-size)
  (patch col-grass-main 0 2 16 12)
  (patch col-grass-light 0 2 16 2)
  (patch col-grass-light 2 5 2 4)
  (patch col-grass-light 5 4 2 5)
  (patch col-grass-light 8 5 2 4)
  (patch col-grass-light 11 4 2 5)
  (patch col-grass-light 13 5 1 4)
  (patch col-grass-dark 0 12 16 2)
  (patch col-night 0 12 16 4))

(define (draw-water x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-water-dark x y tile-size tile-size)
  (patch col-water-main 0 2 16 12)
  (patch col-water-light 0 3 16 1)
  (patch col-water-light 2 7 12 1)
  (patch col-water-light 1 11 14 1)
  (patch col-water-main 0 12 16 1)
  (patch col-night 0 12 16 4)
  (patch col-water-light 12 5 2 1)
  (patch col-water-light 3 9 3 1))

(define (draw-bridge x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-water-dark x y tile-size tile-size)
  (patch col-water-main 0 2 16 12)
  (patch col-water-light 0 3 16 1)
  (patch col-water-light 2 9 3 1)
  (patch col-water-light 12 7 2 1)
  (patch col-bridge-dark 0 7 16 2)
  (patch col-bridge-main 0 6 16 4)
  (patch col-bridge-light 0 6 16 1)
  (patch col-bridge-light 0 9 16 1)
  (patch col-outline 0 6 16 1/2)
  (patch col-outline 0 9 16 1/2)
  (patch col-bridge-dark 2 6 1 4)
  (patch col-bridge-dark 6 6 1 4)
  (patch col-bridge-dark 10 6 1 4)
  (patch col-bridge-dark 14 6 1 4)
  (patch col-night 0 12 16 4))


(define (draw-stone-wall x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-stone-dark x y tile-size tile-size)
  (patch col-stone-main 0 2 16 12)
  (patch col-stone-edge 0 2 16 1)
  (patch col-stone-light 0 7 16 1)
  (patch col-stone-light 0 11 16 1)
  (patch col-outline 0 6 16 1/2)
  (patch col-outline 0 10 16 1/2)
  (patch col-stone-light 1 3 5 3)
  (patch col-stone-main 6 3 5 3)
  (patch col-stone-light 11 3 4 3)
  (patch col-stone-main 1 8 4 3)
  (patch col-stone-light 5 8 5 3)
  (patch col-stone-main 10 8 5 3)
  (patch col-night 0 12 16 4))

(define (draw-tree-log x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-grass-dark x y tile-size tile-size)
  (patch col-grass-main 0 2 16 12)
  (patch col-grass-light 0 2 16 1)
  (patch col-log-main 1 6 14 5)
  (patch col-log-light 1 6 14 1)
  (patch col-log-light 1 9 14 1)
  (patch col-outline 1 6 14 1/2)
  (patch col-outline 1 21/2 14 1/2)
  (patch col-log-light 3 7 2 3)
  (patch col-log-light 7 7 2 3)
  (patch col-log-light 11 7 2 3)
  (patch col-night 0 12 16 4))

(define (draw-golden-key x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-night x y tile-size tile-size)
  (patch col-deep-shadow 0 12 16 4)
  (patch col-key-main 3 5 6 6)
  (patch col-key-dark 3 7 6 2)
  (patch col-key-light 4 6 4 2)
  (patch col-night 4 6 2 2)
  (patch col-night 5 7 1 1)
  (patch col-key-main 8 7 6 2)
  (patch col-key-main 11 6 2 4)
  (patch col-key-light 11 6 2 1)
  (patch col-key-light 8 7 4 1)
  (patch col-key-dark 11 8 2 1)
  (patch col-key-main 11 9 2 2)
  (patch col-key-dark 11 9 2 1/2)
  (patch col-key-main 9 8 2 1)
  (patch col-key-main 9 9 1 1)
  (patch col-key-main 7 9 2 1)
  (patch col-key-dark 7 9 1 1/2))

(define (draw-lock x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-night x y tile-size tile-size)
  (patch col-deep-shadow 0 12 16 4)
  (patch col-metal-dark 5 3 6 4)
  (patch col-metal-main 5 3 6 4)
  (patch col-night 5 3 1 1)
  (patch col-night 10 3 1 1)
  (patch col-metal-highlight 6 4 4 1)
  (patch col-metal-main 4 6 8 7)
  (patch col-metal-dark 4 6 8 1)
  (patch col-metal-highlight 5 7 6 1)
  (patch col-metal-dark 4 12 8 1)
  (patch col-metal-highlight 5 12 6 1/2)
  (patch col-key-light 7 8 2 3)
  (patch col-key-dark 7 9 2 1)
  (patch col-outline 6 6 1 7)
  (patch col-outline 10 6 1 7)
  (patch col-outline 6 9 4 1))

(define (draw-closed-door x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-stone-dark x y tile-size tile-size)
  (patch col-stone-main 0 2 16 12)
  (patch col-door-frame 2 3 12 11)
  (patch col-door-wood 4 4 8 9)
  (patch col-door-shadow 4 8 8 1)
  (patch col-door-shadow 4 11 8 1)
  (patch col-door-highlight 10 9 1 1)
  (patch col-outline 2 3 12 1)
  (patch col-outline 2 14 12 1)
  (patch col-outline 2 3 1 11)
  (patch col-outline 13 3 1 11)
  (patch col-night 0 12 16 4))

(define (draw-open-door x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-stone-dark x y tile-size tile-size)
  (patch col-stone-main 0 2 16 12)
  (patch col-door-frame 2 3 12 11)
  (patch col-night 4 4 8 9)
  (patch col-door-shadow 3 4 1 9)
  (patch col-door-shadow 4 4 1 9)
  (patch col-door-highlight 11 9 1 1)
  (patch col-outline 2 3 12 1)
  (patch col-outline 2 14 12 1)
  (patch col-outline 2 3 1 11)
  (patch col-outline 13 3 1 11)
  (patch col-night 0 12 16 4))

(define (draw-bomb x y)
  (define (patch color dx dy w h)
    (tile-patch x y color dx dy w h))
  (fill col-night x y tile-size tile-size)
  (patch col-deep-shadow 0 12 16 4)
  (patch col-bomb-body 4 5 8 7)
  (patch col-bomb-light 5 6 6 2)
  (patch col-bomb-light 6 8 4 2)
  (patch col-outline 4 5 8 1)
  (patch col-outline 4 11 8 1)
  (patch col-outline 4 5 1 7)
  (patch col-outline 11 5 1 7)
  (patch col-bomb-light 7 4 2 1)
  (patch col-bomb-body 7 3 2 1)
  (patch col-bomb-fuse 7 2 2 1)
  (patch col-bomb-spark 8 1 1 1)
  (patch col-bomb-spark 7 0 1 1/2)
  (patch col-bomb-spark 9 1 1 1/2))



;;;
;;; DRAW
;;;


(fill "#181818" 0 0 canvas-width canvas-height)

(define tile-drawers
  (list draw-hero-raccoon
        draw-yellow-flag
        draw-mud
        draw-grass
        draw-water
        draw-bridge
        draw-stone-wall
        draw-tree-log
        draw-golden-key
        draw-lock
        draw-closed-door
        draw-open-door
        draw-bomb))

#;((car tile-drawers) 0. 0.)


(let loop ([ts tile-drawers] [index 0])
  (when (pair? ts)
    (define row    (quotient  index tiles-per-row))
    (define col    (remainder index tiles-per-row))
    (define tile-x (+ tile-margin (* col (+ tile-size tile-margin))))
    (define tile-y (+ tile-margin (* row (+ tile-size tile-margin))))
    ((car ts) tile-x tile-y)
    (loop (cdr ts) (add1 index))))


#;(draw-hero-raccoon 0 0)
