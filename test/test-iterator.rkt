;;; iterator

;; Smoke test for the Iterator wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi -r test-iterator.rkt

(include-lib iterator)

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(list
 (list "Iterator wrapper"
       (let ()
         (check-true (external? (iterator)) "iterator constructor available")
         (check-equal (js-typeof (iterator)) "function" "iterator typeof")
         (define source (js-eval "[1, 2, 3].values()"))
         (define iter (iterator-from source))
         (check-true (external? iter) "iterator-from returns external")
         (define first-step (js-send/value iter "next" (vector)))
         (check-equal (js-ref first-step "done") #f "iterator next done")
         (check-equal (js-ref first-step "value") 1 "iterator next value")
         #t)))
