#lang webracket/base

(require browser/canvas
         browser/dom
         web-pict)

;; check-equal : any/c any/c string? -> void?
;;   Report an unexpected canvas result.
(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define canvas (js-create-element "canvas"))
(canvas-set-width! canvas 32)
(canvas-set-height! canvas 32)
(js-append-child! (js-document-body) canvas)

(define ctx (canvas-get-context canvas "2d"))
(define dc (canvas-context->dc (canvas-2d-context-unwrap ctx)))

(define test-rect (filled-rectangle 4 6 #f))
(define test-superimposed (ctl-superimpose (blank 11 6) test-rect))
(define-values (super-x super-y) (lt-find test-superimposed test-rect))
(check-equal (list super-x super-y) '(7/2 0) "exact superimpose position")
(draw-pict test-superimposed dc 0 0)
(check-equal
 (bytes-ref (canvas-image-data-data (canvas-2d-get-image-data ctx 5 3 1 1)) 3)
 255
 "centered rectangle interior")
(check-equal
 (bytes-ref (canvas-image-data-data (canvas-2d-get-image-data ctx 1 1 1 1)) 3)
 0
 "outside centered rectangle")

(dc 'clear-rect 0 0 32 32)
(define test-appended (vc-append 1/2 (blank 11 1) test-rect))
(define-values (append-x append-y) (lt-find test-appended test-rect))
(check-equal (list append-x append-y) '(7/2 3/2) "exact append position")
(draw-pict test-appended dc 0 0)
(check-equal
 (bytes-ref (canvas-image-data-data (canvas-2d-get-image-data ctx 5 4 1 1)) 3)
 255
 "appended rectangle interior")
(check-equal
 (bytes-ref (canvas-image-data-data (canvas-2d-get-image-data ctx 1 1 1 1)) 3)
 0
 "outside appended rectangle")

(js-set-attribute! (js-document-body) "data-state" "loaded")
(js-log "web-pict-rational-coordinates-ok")
