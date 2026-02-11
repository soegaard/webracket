;;;
;;; standard.ffi
;;;

;; This file tests arity errors for foreign functions.

;;; Build
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi -r test-foreign-function-arity-error-handling.rkt
;;
;; On success you will see an s-expression where #t indicates success.
;; If you (or grep) spot an #f, then you can surround the particular test with:
;;     (let ([equal? list] [and list]) <test-here> )
;; to see the actual results.

(list
 (list "foreign arity error"
       (with-handlers ([exn:fail:contract:arity?
                        (lambda (e)
                          (and (string-contains? (exn-message e) "arity mismatch")
                               (string-contains? (exn-message e)
                                                 "js-parse-int")))])
         (js-parse-int)
         #f)))
