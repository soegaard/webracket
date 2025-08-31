(define width 256)
(define height 256)

(define canvas (js-create-element "canvas"))
(js-set-canvas-width! canvas width)
(js-set-canvas-height! canvas height)
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(define (color-string r g b)
  (string-append "rgb(" (number->string r) ","
                 (number->string g) ","
                 (number->string b) ")"))

(let loop-y ([y 0])
  (when (< y height)
    (let loop-x ([x 0])
      (when (< x width)
        (define fx (exact->inexact x))
        (define fy (exact->inexact y))
        (define intensity (+ 2. (flsin (/ fx 16.)) (flsin (/ fy 8.))))
        (define c (inexact->exact (floor (* 64. intensity))))
        (define r (remainder c 256))
        (define g (remainder (+ c 85) 256))
        (define b (remainder (+ c 170) 256))
        (js-set-canvas2d-fill-style! ctx (color-string r g b))
        (js-canvas2d-fill-rect ctx fx fy 1. 1.)
        (loop-x (add1 x))))
    (loop-y (add1 y))))
