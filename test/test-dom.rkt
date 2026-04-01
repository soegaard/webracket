;;;
;;; dom.ffi
;;;

;; Smoke tests for the raw split DOM FFI aggregate.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom.rkt

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(list
 (list "Raw DOM aggregate"
       (let ()
         (check-equal (js-typeof (js-window-window)) "object" "window available")
         (check-equal (js-typeof (js-document)) "object" "document available")
         (check-equal (js-typeof (js-event-new "submit" (js-eval "({})"))) "object" "event available")
         (check-equal (js-typeof (js-document-head)) "object" "document head available")
         (check-equal (js-typeof (js-canvas-width (js-eval "document.createElement('canvas')"))) "number"
                      "canvas available")
         (check-equal (js-typeof (js-media-current-time (js-eval "document.createElement('audio')"))) "number"
                      "media available")
         (check-equal (js-typeof (js-image-alt (js-eval "new Image()"))) "string"
                      "image available")
         #t)))
