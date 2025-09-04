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
              (list "js-number-epsilon"
                    (equal? (js-number-epsilon) 2.220446049250313e-16))
              (list "js-number-max-safe-integer"
                    (equal? (js-number-max-safe-integer) 9007199254740991.0))
              (list "js-number-max-value"
                    (equal? (js-number-max-value) 1.7976931348623157e+308))
              (list "js-number-min-safe-integer"
                    (equal? (js-number-min-safe-integer) -9007199254740991.0))
              (list "js-number-min-value"
                    (equal? (js-number-min-value) 5e-324))
              (list "js-number-nan"
                    (equal? (js-number-nan? (js-number-nan)) #t))
              (list "js-number-negative-infinity"
                    (equal? (js-number-negative-infinity) -inf.0))
              (list "js-number-positive-infinity"
                    (equal? (js-number-positive-infinity) +inf.0))
              (list "js-number-finite?"
                    (and (equal? (js-number-finite? 1.0)    #t)
                         (equal? (js-number-finite? 0.0)    #t)
                         (equal? (js-number-finite? +inf.0) #f)
                         (equal? (js-number-finite? -inf.0) #f)
                         (equal? (js-number-finite? +nan.0) #f)
                         (equal? (js-number-finite? (js-number-positive-infinity)) #f)
                         (equal? (js-number-finite? (js-number-negative-infinity)) #f)
                         (equal? (js-number-finite? (js-number-nan))               #f)))
              (list "js-number-integer?"
                    (and (equal? (js-number-integer? 1.0) #t)
                         (equal? (js-number-integer? 1.1) #f)))
              (list "js-number-nan?"
                    (and (equal? (js-number-nan? +nan.0) #t)
                         (equal? (js-number-nan? 1.0) #f)))
              (list "js-number-safe-integer?"
                    (and (equal? (js-number-safe-integer? 42.0) #t)
                         (equal? (js-number-safe-integer? 1e16) #f)))
              (list "js-number-parse-float"
                    (and (equal? (js-number-parse-float "42.1abc") 42.1)
                         (equal? (js-number-parse-float "   -17.5") -17.5)))
              (list "js-number-parse-int"
                    (and (equal? (js-number-parse-int "42") 42.0)
                         (equal? (js-number-parse-int "-17") -17.0)))
              (list "js-number-to-exponential"
                    (equal? (js-number-to-exponential 12345.0 (void)) "1.2345e+4"))
              (list "js-number-to-fixed"
                    (equal? (js-number-to-fixed 123.456 (void)) "123"))
              (list "js-number-to-locale-string"
                    (equal? (js-number-to-locale-string 0.0 (void) (void)) "0"))
              (list "js-number-to-precision"
                    (equal? (js-number-to-precision 123.456 (void)) "123.456"))
              (list "js-number-to-string"
                    (and (equal? (js-number-to-string 123.456 (void)) "123.456")
                         (equal? (js-number-to-string 255.0 16) "ff")))
              (list "js-number-value-of"
                    (equal? (js-number-value-of 123.456) 123.456)))))
