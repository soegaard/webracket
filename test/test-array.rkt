;;;
;;; array.ffi
;;;

;; Focused tests for the standalone `array` wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/array.ffi -r test-array.rkt

(include-lib array)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-array-vector got want label)
  (check-true (array? got) (string-append label " wrapper"))
  (check-equal (array->vector got) want label))

(list
 (list "Array wrappers"
       (let ()
         (define arr (array-of 1 2 3))
         (check-true (array? arr) "array wrapper")
         (define source #(7 8 9))
         (check-equal (array-length arr) 3 "array length")
         (check-equal (array-ref arr 1) 2 "array ref")
         (check-equal (array-at arr 2) 3 "array at")
         (check-equal (array->vector arr) #(1 2 3) "array to vector")
         (check-equal (array->list arr) '(1 2 3) "array to list")
         (check-array-vector (array-from source) #(7 8 9) "array from")
         (check-array-vector (vector->array #(4 5)) #(4 5) "vector to array")
         (check-array-vector (list->array '(6 7)) #(6 7) "list to array")
         (check-array-vector (sequence->array '(8 9)) #(8 9) "sequence to array list")
         (check-array-vector (sequence->array #(10 11)) #(10 11) "sequence to array vector")
         (check-equal (array-join arr "-") "1-2-3" "array join")
         (check-array-vector (array-slice arr 1 #f) #(2 3) "array slice")
         (check-array-vector (array-concat (array-of 1 2) (array-of 3 4))
                             #(1 2 3 4)
                             "array concat")
         (define pushed (array-of 10 20))
         (check-equal (array-push! pushed 30 40) 4 "array push length")
         (check-equal (array->vector pushed) #(10 20 30 40) "array push mutation")
         (check-equal (array-pop pushed) 40 "array pop")
         (check-equal (array->vector pushed) #(10 20 30) "array pop mutation")
         (check-equal (array-shift pushed) 10 "array shift")
         (check-equal (array->vector pushed) #(20 30) "array shift mutation")
         (check-equal (array-unshift! pushed 5 6) 4 "array unshift length")
         (check-equal (array->vector pushed) #(5 6 20 30) "array unshift mutation")
         (check-array-vector (array-copy-within! (array-of 1 2 3 4) 1 2)
                             #(1 3 4 4)
                             "array copy within")
         (check-array-vector (array-fill! (array-of 1 2 3 4) 9 1 3)
                             #(1 9 9 4)
                             "array fill")
         (check-array-vector (array-reverse! (array-of 1 2 3)) #(3 2 1) "array reverse")
         (check-array-vector (array-sort! (array-of 3 1 2)) #(1 2 3) "array sort")
         (check-array-vector (array-splice! (array-of 1 2 3 4) 1 2 9 8)
                             #(2 3)
                             "array splice removed")
         (check-array-vector (array-to-reversed (array-of 1 2 3))
                             #(3 2 1)
                             "array to reversed")
         (check-array-vector (array-to-sorted (array-of 3 1 2)) #(1 2 3) "array to sorted")
         (check-array-vector (array-to-spliced (array-of 1 2 3 4) 1 2 9 8)
                             #(1 9 8 4)
                             "array to spliced")
         (check-array-vector (array-with (array-of 1 2 3) 1 99) #(1 99 3) "array with")
         (check-equal (array->vector (array-raw arr)) #(1 2 3) "array raw accessor")
         (check-equal (array-to-string (array-of 1 2 3)) "1,2,3" "array to string")
         (check-equal (array-to-locale-string (array-of 1 2 3) #f) "1,2,3" "array locale string")
         #t)))
