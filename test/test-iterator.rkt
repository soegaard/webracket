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

(define (fresh-iter)
  (iterator-from (js-array/extern (vector 1 2 3 4))))

(list
 (list "Iterator constructor and prototype"
       (let ()
         (check-true (external? (Iterator)) "Iterator constructor available")
         (check-equal (js-typeof (Iterator)) "function" "Iterator typeof")
         (check-true (external? (iterator-prototype)) "iterator prototype available")
         (check-true (external? (iterator-prototype-constructor)) "iterator prototype constructor available")
         (check-equal (iterator-prototype-to-string-tag) "Iterator" "iterator toStringTag")
         #t))
 (list "Iterator static helpers"
       (let ()
         (define wrapped (iterator-from (js-array/extern (vector 9 8 7))))
         (check-true (iterator? wrapped) "iterator from wrapper")
         (check-true (external? (iterator-raw wrapped)) "iterator raw wrapper")
         (define concatenated
           (iterator-concat (js-array/extern (vector 1 2))
                            (js-array/extern (vector 3 4))))
         (check-true (iterator? concatenated) "iterator concat wrapper")
         (check-equal (iterator-to-array concatenated)
                      #(1 2 3 4)
                      "iterator concat")
         (check-true (iterator? (iterator-zip (js-array/extern (vector (js-array/extern (vector 1 2))
                                                                       (js-array/extern (vector 10 20))))))
                     "iterator zip available")
         (check-true (iterator? (iterator-zip-keyed
                                 (js-object (vector (vector "left" (js-array/extern (vector 1 2)))
                                                    (vector "right" (js-array/extern (vector 10 20)))))))
                     "iterator zipKeyed available")
         #t))
 (list "Iterator instance methods"
       (let ()
         (define iter (fresh-iter))
         (check-true (iterator? iter) "iterator wrapper available")
         (define first-step (iterator-next iter))
         (check-equal (js-ref first-step "done") #f "iterator next done")
         (check-equal (js-ref first-step "value") 1 "iterator next value")

         (check-equal (iterator-to-array (iterator-map (fresh-iter)
                                                       (lambda (n) (* n 2))))
                      #(2 4 6 8)
                      "iterator map")
         (check-equal (iterator-to-array (iterator-filter (fresh-iter)
                                                          (lambda (n) (even? n))))
                      #(2 4)
                      "iterator filter")
         (check-equal (iterator-to-array (iterator-flat-map (fresh-iter)
                                                            (lambda (n)
                                                              (js-array/extern (vector n n)))))
                      #(1 1 2 2 3 3 4 4)
                      "iterator flatMap")
         (check-equal (iterator-to-array (iterator-drop (fresh-iter) 2))
                      #(3 4)
                      "iterator drop")
         (check-equal (iterator-to-array (iterator-take (fresh-iter) 2))
                      #(1 2)
                      "iterator take")
         (check-true (iterator-every (fresh-iter)
                                     (lambda (n) (> n 0)))
                     "iterator every")
         (check-true (iterator-some (fresh-iter)
                                    (lambda (n) (= n 3)))
                     "iterator some")
         (check-equal (iterator-find (fresh-iter)
                                     (lambda (n) (= n 3)))
                      3
                      "iterator find")
         (check-equal (iterator-reduce (fresh-iter)
                                       (lambda (acc n) (+ acc n))
                                       0)
                      10
                      "iterator reduce")
         (define seen (box 0))
         (iterator-for-each (fresh-iter)
                            (lambda (n)
                              (set-box! seen (+ (unbox seen) n))))
         (check-equal (unbox seen) 10 "iterator forEach")
         (define returned (iterator-return (fresh-iter) "done"))
         (check-equal (js-ref returned "done") #t "iterator return done")
         (check-equal (js-ref returned "value") "done" "iterator return value")
         (check-true (iterator? (iterator-symbol-iterator (fresh-iter)))
                     "iterator Symbol.iterator")
         (check-true (external? (iterator-raw (iterator-symbol-iterator (fresh-iter))))
                     "iterator Symbol.iterator raw")
         (iterator-dispose! (fresh-iter))
         #t)))
