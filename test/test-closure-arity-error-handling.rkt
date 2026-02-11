;;;
;;; closure arity errors
;;;

;; This file tests arity errors for closures.

;;; Build
;;   racket -l errortrace -t ../webracket.rkt -- -r test-closure-arity-error-handling.rkt
;;
;; On success you will see an s-expression where #t indicates success.

(list
 (list "plain-lambda arity error"
       (with-handlers ([exn:fail:contract:arity?
                        (lambda (e)
                          (string-contains? (exn-message e) "arity mismatch"))])
         ((#%plain-lambda (x) x))
         #f)))
