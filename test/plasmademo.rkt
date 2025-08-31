(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas 200)
(js-set-canvas-height! canvas 200)
(js-append-child! (js-document-body) canvas)
(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(define width  200)
(define height 200)
(define frame    0)

(define (sqr x) (* x x))

(define color-gradients
  (hash
   'rainbow
   (list #(0 35 255 0 255 0 0 255)
         #(36 71 0 0 255 0 255 255)
         #(72 107 0 255 255 0 255 0)
         #(108 143 0 255 0 255 255 0)
         #(144 179 255 255 0 255 127 0)
         #(180 215 255 127 0 255 0 0)
         #(216 255 255 0 0 255 0 255))
   'nebula
   (list #(0 31 0 0 0 0 0 127)
         #(32 95 0 0 127 127 0 255)
         #(96 159 127 0 255 255 0 0)
         #(160 191 255 0 0 255 255 255)
         #(192 255 255 255 255 0 0 0))
   'fire
   (list #(0 23 0 0 0 0 0 127)
         #(24 47 0 0 127 255 0 0)
         #(48 95 255 0 0 255 255 0)
         #(96 127 255 255 0 255 255 255)
         #(128 159 255 255 255 255 255 0)
         #(160 207 255 255 0 255 0 0)
         #(208 231 255 0 0 0 0 127)
         #(232 255 0 0 127 0 0 0))
   'bluegreen
   (list #(0 23 0 0 0 0 0 127)
         #(24 47 0 0 127 0 127 255)
         #(48 95 0 127 255 0 255 0)
         #(96 127 0 255 0 255 255 255)
         #(128 159 255 255 255 0 255 0)
         #(160 207 0 255 0 0 127 255)
         #(208 231 0 127 255 0 0 127)
         #(232 255 0 0 127 0 0 0))
   'rgb
   (list #(0 63 0 0 0 255 0 0)
         #(64 127 0 0 0 0 255 0)
         #(128 191 0 0 0 0 0 255)
         #(192 255 0 0 0 255 255 255))))

(define palR (make-vector 256 0))
(define palG (make-vector 256 0))
(define palB (make-vector 256 0))

(define (set-palette name)
  (when (hash-has-key? color-gradients name)
    (for ([grad (in-list (hash-ref color-gradients name))])
      (define start (vector-ref grad 0))
      (define end   (vector-ref grad 1))
      (define r1    (vector-ref grad 2))
      (define g1    (vector-ref grad 3))
      (define b1    (vector-ref grad 4))
      (define r2    (vector-ref grad 5))
      (define g2    (vector-ref grad 6))
      (define b2    (vector-ref grad 7))
      (define num (- end start))
      (for ([i (in-range 0 (+ num 1))])
        (define k (/ i num))
        (define j (+ start i))
        (vector-set! palR j (inexact->exact (round (+ r1 (* (- r2 r1) k)))))
        (vector-set! palG j (inexact->exact (round (+ g1 (* (- g2 g1) k)))))
        (vector-set! palB j (inexact->exact (round (+ b1 (* (- b2 b1) k)))))))))

(define plasma1 #f)
(define plasma2 #f)

(define (setup-plasma)
  (define bigw (* width 2))
  (define bigh (* height 2))
  (set! plasma1 (make-vector (* bigw bigh) 0))
  (set! plasma2 (make-vector (* bigw bigh) 0))
  (define dst 0)
  (define wf (exact->inexact width))
  (define hf (exact->inexact height))
  (for ([y (in-range bigh)])
    (define yf (exact->inexact y))
    (for ([x (in-range bigw)])
      (define xf (exact->inexact x))
      (vector-set! plasma1 dst
                   (inexact->exact
                    (round
                     (+ 64.
                        (* 63.
                           (flsin (/ (flsqrt (+ (sqr (- hf yf))
                                                 (sqr (- wf xf))))
                                     (/ wf 20.))))))))
      (vector-set! plasma2 dst
                   (inexact->exact
                    (round
                     (+ 64.
                        (* 63.
                           (fl*
                            (flsin (/ xf (+ 74. (* 15. (flcos (/ yf 140.))))) )
                            (flcos (/ yf (+ 61. (* 11. (flsin (/ xf 114.))))) )))))))
      (set! dst (add1 dst)))))

(set-palette 'rainbow)
(setup-plasma)

(define (draw)
  (define hw (quotient width 2))
  (define hh (quotient height 2))
  (define hw-1 (sub1 hw))
  (define hh-1 (sub1 hh))
  (define bigw (* width 2))
  (define x1 (inexact->exact (floor (+ hw (* hw-1 (flcos (/ frame 97.)))))))
  (define x2 (inexact->exact (floor (+ hw (* hw-1 (flsin (/ frame -114.)))))))
  (define x3 (inexact->exact (floor (+ hw (* hw-1 (flsin (/ frame -137.)))))))
  (define y1 (inexact->exact (floor (+ hh (* hh-1 (flsin (/ frame 123.)))))))
  (define y2 (inexact->exact (floor (+ hh (* hh-1 (flcos (/ frame -75.)))))))
  (define y3 (inexact->exact (floor (+ hh (* hh-1 (flcos (/ frame -108.)))))))
  (define src1 (+ x1 (* y1 bigw)))
  (define src2 (+ x2 (* y2 bigw)))
  (define src3 (+ x3 (* y3 bigw)))
  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (define i (bitwise-and #xFF (+ (vector-ref plasma1 src1)
                                     (vector-ref plasma2 src2)
                                     (vector-ref plasma2 src3))))
      (define r (vector-ref palR i))
      (define g (vector-ref palG i))
      (define b (vector-ref palB i))
      (js-set-canvas2d-fill-style!
       ctx
       (string-append "rgb("
                      (number->string r) ","
                      (number->string g) ","
                      (number->string b) ")"))
      (js-canvas2d-fill-rect ctx (exact->inexact x) (exact->inexact y) 1. 1.)
      (set! src1 (add1 src1))
      (set! src2 (add1 src2))
      (set! src3 (add1 src3)))
    (set! src1 (+ src1 width))
    (set! src2 (+ src2 width))
    (set! src3 (+ src3 width)))
  (set! frame (add1 frame)))

(js-window-set-interval (procedure->external draw) 50.)
