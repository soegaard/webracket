(define width  (* 1 256))
(define height (* 1 256))

(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas width)
(js-set-canvas-height! canvas height)
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(define (clamp x)
  (remainder (exact-floor x) 256))

(define i 0)
(define (draw t)
  ; (js-log t) ; t is a floating point adds 500-1000 each time.
  (set! i (+ i 1))
  (define ft  (* 0.0004 (exact->inexact t)))
  (define img (js-canvas2d-create-image-data ctx
                                             (exact->inexact width)
                                             (exact->inexact height)))
  (js-set! (js-global-this) "imgData" img)
  (define data (js-eval "imgData.data"))
  (let loop-y ([y 0])
    (when (< y height)
      (let loop-x ([x 0])
        (when (< x width)
          (define fx (exact->inexact x))
          (define fy (exact->inexact y))
          (define intensity (+ 2.
                               (flsin (/ (+ fx (* 10. ft)) 16.))
                               (flcos (/ (+ fy (* 20. ft)) 8.))))
          (define c   (inexact->exact (floor (* 64. intensity))))
          (define r   (clamp    c ))
          (define g   (clamp (+ c (* 256. (flsin (- ft (* 0.01 fx)))))))
          (define b   (clamp (+ c (* 256. (flcos (- ft (* 2. 0.01 fy)))))))
          (define idx (* 4 (+ (* y width) x)))
          (js-set! data idx r)
          (js-set! data (add1 idx) g)
          (js-set! data (+ idx 2) b)
          (js-set! data (+ idx 3) 255)
          (loop-x (add1 x))))
      (loop-y (add1 y))))
  (js-canvas2d-put-image-data ctx img 0. 0.
                              (js-undefined) (js-undefined)
                              (js-undefined) (js-undefined))
  (js-window-request-animation-frame (procedure->external draw)))

(js-window-request-animation-frame (procedure->external draw))
