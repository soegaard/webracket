;;;
;;; indexed-db.ffi
;;;

;; Focused tests for the `indexed-db` wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser -r test-indexed-db.rkt

(include-lib indexed-db)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(list
 (list "IndexedDB wrappers"
       (let ()
         (define db (indexed-db))
         (check-true (indexed-db? db) "indexed-db predicate")
         (check-equal (js-typeof (indexed-db-raw db))
                      "object"
                      "indexed-db raw type")
         #t)))
