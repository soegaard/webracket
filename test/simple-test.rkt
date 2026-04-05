;;;
;;; simple-pretty smoke test
;;;

;; Minimal smoke test for `(include-lib simple-pretty)`.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi -r simple-test.rkt

(include-lib simple-pretty)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(list
 (list "simple-pretty include-lib"
       (let ()
         (check-equal (simple-pretty-format '(1 2 3)) "(1 2 3)" "format smoke test")
         #t)))
