(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas 800)
(js-set-canvas-height! canvas 800)
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(js-set-canvas2d-fill-style! ctx "white")   ; background
(js-canvas2d-fill-rect ctx 0. 0. 800. 800.)
(js-set-canvas2d-fill-style! ctx "purple")  ; foreground

(define (draw-triangle x1 y1 x2 y2 x3 y3)
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-move-to ctx x1 y1)
  (js-canvas2d-line-to ctx x2 y2)
  (js-canvas2d-line-to ctx x3 y3)
  (js-canvas2d-close-path ctx)
  (js-canvas2d-fill ctx (void) "nonzero"))

(define (sierpinski x1 y1 x2 y2 x3 y3 depth)
  (if (zero? depth)
      (draw-triangle x1 y1 x2 y2 x3 y3)
      (let ([mx12 (/ (+ x1 x2) 2.)]
            [my12 (/ (+ y1 y2) 2.)]
            [mx23 (/ (+ x2 x3) 2.)]
            [my23 (/ (+ y2 y3) 2.)]
            [mx31 (/ (+ x3 x1) 2.)]
            [my31 (/ (+ y3 y1) 2.)])
        (sierpinski x1 y1 mx12 my12 mx31 my31 (- depth 1))
        (sierpinski mx12 my12 x2 y2 mx23 my23 (- depth 1))
        (sierpinski mx31 my31 mx23 my23 x3 y3 (- depth 1)))))

(sierpinski 400. 0. 0. 800. 800. 800. 5)
