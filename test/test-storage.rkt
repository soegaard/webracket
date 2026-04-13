;;;
;;; storage.ffi
;;;

;; Focused tests for the `storage` wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser -r test-storage.rkt

(include-lib storage)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(list
 (list "Storage wrappers"
       (let ()
         (define ls (local-storage))
         (define ss (session-storage))
         (storage-set-item! ls 'webracket-test '123)
         (storage-set-item! ss 'webracket-test '456)
         (check-true (storage? ls) "local storage predicate")
         (check-true (storage? ss) "session storage predicate")
         (check-true (>= (storage-length ls) 1) "local storage length")
         (check-true (>= (storage-length ss) 1) "session storage length")
         (check-equal (storage-get-item ls 'webracket-test)
                      "123"
                      "local storage get")
         (check-equal (storage-get-item ss 'webracket-test)
                      "456"
                      "session storage get")
         (storage-remove-item! ls 'webracket-test)
         (storage-remove-item! ss 'webracket-test)
         #t)))
