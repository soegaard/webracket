;;;
;;; query wrapper smoke test
;;;

;; Minimal smoke test for `(include-lib query)`.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser test-query.rkt

(include-lib query)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define body (document-body))
(define host (document-create-element 'div))
(define title (document-create-element 'h1))

(element-set-attribute! host 'id 'query-host)
(element-set-attribute! title 'id 'hw)
(element-append! title (document-create-text-node "Hello World!"))
(element-append! host title)
(element-append! body host)

(define sel ($ "#hw"))

(list
 (list "query include-lib"
       (let ()
         (check-equal (safe-list-ref '(a b c) 2) 'c "safe list ref")
         (check-equal (safe-vector-ref #(7 8 9) 1) 8 "safe vector ref")
         (check-true ($selection? sel) "selection wrapper")
         (check-equal ($length sel) 1 "selection length")
         (check-equal (.text sel) "Hello World!" "selection text")
         #t)))
