;;;
;;; standard.ffi
;;;

;; This file contains tests for `standard.ffi`.

;;;
;;; Build
;;;

;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../standard.ffi -r test-ffi.rkt

;; On success you will see an s-expression where #t indicates success.
;; If you (or grep) spot an #f, then you can surround the particular test with:
;;     (let ([equal? list] [and list]) <test-here> )
;; to see the actual results.

;; Also, to get a pretty printed version of the output, use the `pretty` tool
;;                                                                           
;;   ...as-before... | racket ../tools/pretty.rkt | less                     
;;                                                                           



(list
 (list "standard.ffi"
       (list "Number functions"
             (list
              (list "js-number-finite?"
                    (and (equal? (js-number-finite? 1.0)    #t)
                         (equal? (js-number-finite? 0.0)    #t)
                         (equal? (js-number-finite? +inf.0) #f)
                         (equal? (js-number-finite? -inf.0) #f)
                         (equal? (js-number-finite? +nan.0) #f)
                         (equal? (js-number-finite? (js-number-positive-infinity)) #f)
                         (equal? (js-number-finite? (js-number-negative-infinity)) #f)
                         (equal? (js-number-finite? (js-number-nan))               #f)))))))
