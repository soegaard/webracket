;;;
;;; primitive arity errors
;;;

;; This file tests arity errors for normal primitives.

;;; Build
;;   racket -l errortrace -t ../webracket.rkt -- -r test-primitive-arity-error-handling.rkt
;;
;; On success you will see an s-expression where #t indicates success.
;; If you (or grep) spot an #f, then you can surround the particular test with:
;;     (let ([equal? list] [and list]) <test-here> )
;; to see the actual results.

(list
 (list "map arity error via apply"
       (with-handlers ([exn:fail:contract:arity?
                        (lambda (e)
                          (and (string-contains? (exn-message e) "arity mismatch")
                               (string-contains? (exn-message e) "map")))])
         (apply map '())
         #f))
 (list "map arity error via local binding"
       (with-handlers ([exn:fail:contract:arity?
                        (lambda (e)
                          (and (string-contains? (exn-message e) "arity mismatch")
                               (string-contains? (exn-message e) "map")))])
         (let ([f map]) (f))
         #f)))
