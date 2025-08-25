(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas 300)
(js-set-canvas-height! canvas 300)
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(js-set-canvas2d-stroke-style! ctx "green")
(js-set-canvas2d-line-width!   ctx 2.)

(define angle-step (/ pi 6.))
(define shrink     0.7)

(define (draw-branch ctx x y len angle depth)
  (define x2 (+ x (* len (flsin angle))))
  (define y2 (- y (* len (flcos angle))))
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-move-to ctx x y)
  (js-canvas2d-line-to ctx x2 y2)
  (js-canvas2d-stroke ctx)
  (when (> depth 0)
    (define new-l (* len shrink))
    (draw-branch ctx x2 y2 new-l (+ angle angle-step) (sub1 depth))
    (draw-branch ctx x2 y2 new-l (- angle angle-step) (sub1 depth))))

(draw-branch ctx 150. 280. 60. 0. 8)
