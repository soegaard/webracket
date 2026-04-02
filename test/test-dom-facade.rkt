;;;
;;; dom.ffi
;;;

;; Minimal smoke test for the top-level DOM wrapper facade.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-facade.rkt

(include-lib dom)

(define (check-equal got want label)
  (unless (if (and (number? got) (number? want))
              (= got want)
              (equal? got want))
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(list
 (list "DOM facade"
       (let ()
         (check-true (procedure? Window) "Window accessor available")
         (check-true (procedure? window?) "window struct available")
         (check-true (procedure? window-name) "window wrapper available")
         (check-true (procedure? Iterator) "iterator wrapper available")
         (check-true (procedure? performance-now) "performance wrapper available")
         (check-true (procedure? document-body) "document wrapper available")
         (check-true (procedure? canvas-width) "canvas wrapper available")
         (check-true (procedure? image-alt) "image wrapper available")
         (check-true (procedure? media-current-time) "media wrapper available")
         (check-true (procedure? event-type) "event wrapper available")
         (check-true (procedure? dom-rect-left) "domrect wrapper available")
         #t)))
