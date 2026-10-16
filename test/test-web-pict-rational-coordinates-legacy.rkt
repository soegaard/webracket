(include-lib web-pict)

;; check-equal : any/c any/c string? -> void?
;;   Report an unexpected canvas result through the include-lib entry point.
(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define canvas (js-create-element "canvas"))
(js-set-canvas-width! canvas 32)
(js-set-canvas-height! canvas 32)
(js-append-child! (js-document-body) canvas)
(define context (js-canvas-get-context canvas "2d" (js-undefined)))
(define drawing (canvas-context->dc context))

(define test-rect (filled-rectangle 4 6 #f))
(define test-superimposed (ctl-superimpose (blank 11 6) test-rect))
(define-values (super-x super-y) (lt-find test-superimposed test-rect))
(check-equal (list super-x super-y) '(7/2 0) "exact superimpose position")
(draw-pict test-superimposed drawing 0 0)

(define test-appended (vc-append 1/2 (blank 11 1) test-rect))
(define-values (append-x append-y) (lt-find test-appended test-rect))
(check-equal (list append-x append-y) '(7/2 3/2) "exact append position")
(draw-pict test-appended drawing 0 0)

(js-set-attribute! (js-document-body) "data-state" "loaded")
(js-log "web-pict-rational-coordinates-legacy-ok")
