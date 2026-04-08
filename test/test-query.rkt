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
(define page
  (sxml->dom
   '(div (@ (id "query-root"))
         (p (@ (id "text-host")))
         (div (@ (id "query-host"))
              (h1 (@ (id "hw")))))))

(element-append! body page)

(define hw-elem (document-get-element-by-id "hw"))
(define sel ($ "#hw"))
(define text-sel ($ "#text-host"))

(element-set-text-content! ($first sel) "Hello World!")
(element-set-text-content! ($first text-sel) "Text node child")

(list
 (list "query include-lib"
       (let ()
         (check-equal (safe-list-ref '(a b c) 2) 'c "safe list ref")
         (check-equal (safe-vector-ref #(7 8 9) 1) 8 "safe vector ref")
         (check-true ($selection? sel) "selection wrapper")
         (check-equal ($length sel) 1 "selection length")
         (check-equal (element-id hw-elem) "hw" "selected id")
         (check-equal (element-tag-name hw-elem) "H1" "selected tag")
         (check-equal (element-text-content hw-elem) "Hello World!" "element-text-content binding")
         (check-equal (.text sel) "Hello World!" "selection text")
         (check-equal (.text text-sel) "Text node child" "text node append")
         (check-equal (chain ($ "#hw") .text) "Hello World!" "chain text")
         (check-equal (chain ($ "#hw")
                             .for-each
                             (lambda (node) (element-set-attribute! node 'data-seen 'yes))
                             .text)
                      "Hello World!"
                      "chain with args")
         #t)))
