;;; standard.ffi
;;;
;; This file contains tests for `standard.ffi`.

;;; Build
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
       (list "Value properties"
             (list
              (list "js-global-this"
                    (equal? (js-typeof (js-global-this)) "object"))
              (list "js-infinity"
                    (equal? (js-infinity) +inf.0))
              (list "js-nan"
                    (equal? (js-is-nan (js-nan)) 1))
              (list "js-undefined"
                    (equal? (js-typeof (js-undefined)) "undefined"))))
       (list "Function properties"
             (list
              (list "js-is-finite"
                    (and (equal? (js-is-finite 1.0) 1)
                         (equal? (js-is-finite +inf.0) 0)))
              (list "js-is-nan"
                    (and (equal? (js-is-nan +nan.0) 1)
                         (equal? (js-is-nan 1.0) 0)))
              (list "js-parse-float"
                    (and (equal? (js-parse-float "42.5abc") 42.5)
                         (equal? (js-parse-float "   -17") -17.0)))
              (list "js-parse-int"
                    (and (equal? (js-parse-int "42") 42.0)
                         (equal? (js-parse-int "-17") -17.0)))
              (list "js-decode-uri/encode-uri"
                    (let ([s "https://example.com/?a=1&b=2"])
                      (equal? (js-decode-uri (js-encode-uri s)) s)))
              (list "js-decode-uri-component/encode-uri-component"
                    (let ([s "a b+c"])
                      (equal? (js-decode-uri-component (js-encode-uri-component s)) s)))
              (list "js-escape/unescape"
                    (let ([s "a b+c"])
                      (equal? (js-unescape (js-escape s)) s)))
              (list "js-ref/js-set!/js-assign"
                    (let ([obj (js-object (list))])
                      (js-set! obj "a" 1)
                      (and (equal? (js-ref obj "a") 1)
                           (equal? (js-assign "b" 2) 2)
                           (equal? (js-ref (js-global-this) "b") 2))))))

       (list "Basic operations"
             (list
              (list "js-array/js-index"
                    (let ([arr (js-array (list 1 2 3))])
                      (equal? (js-index arr 1) 2)))
              (list "js-object"
                    (let ([obj (js-object (list (list "a" 1)))])
                      (equal? (js-ref obj "a") 1)))
              (list "js-new"
                    (equal? (js-typeof (js-new (js-date) (list))) "object"))
              (list "js-send"
                    (let ([arr (js-array (list 1 2 3))])
                      (equal? (js-send arr "join" (list "-")) "1-2-3")))
              (list "js-send/flonum"
                    (equal? (js-send/flonum (js-math) "abs" (list -1.0)) 1.0))
              (list "js-operator"
                    (equal? (js-operator "+" (list 1 2)) 3))
              (list "js-typeof"
                    (equal? (js-typeof (js-infinity)) "number"))
              (list "js-instanceof"
                    (let ([arr (js-array (list))])
                      (equal? (js-instanceof arr (js-ref (js-global-this) "Array")) 1)))
              (list "js-null"
                    (equal? (js-typeof (js-null)) "object"))
              (list "js-this"
                    (equal? (js-this) (js-global-this)))))

       (list "Fundamental objects"
             (list
              (list "js-object-constructor"
                    (equal? (js-typeof (js-object-constructor)) "function"))
              (list "js-function"
                    (equal? (js-typeof (js-function)) "function"))
              (list "js-boolean"
                    (equal? (js-typeof (js-boolean)) "function"))
              (list "js-symbol"
                    (equal? (js-typeof (js-symbol)) "function"))))

       (list "Error objects"
             (list
              (list "js-error"
                    (equal? (js-typeof (js-error)) "function"))
              (list "js-aggregate-error"
                    (equal? (js-typeof (js-aggregate-error)) "function"))
              (list "js-eval-error"
                    (equal? (js-typeof (js-eval-error)) "function"))
              (list "js-range-error"
                    (equal? (js-typeof (js-range-error)) "function"))
              (list "js-reference-error"
                    (equal? (js-typeof (js-reference-error)) "function"))
              (list "js-suppressed-error"
                    (equal? (js-typeof (js-suppressed-error)) "function"))
              (list "js-syntax-error"
                    (equal? (js-typeof (js-syntax-error)) "function"))
              (list "js-type-error"
                    (equal? (js-typeof (js-type-error)) "function"))
              (list "js-uri-error"
                    (equal? (js-typeof (js-uri-error)) "function"))
              (list "js-internal-error"
                    (equal? (js-typeof (js-internal-error)) "function"))))

       (list "Numbers and dates"
             (list
              (list "js-number"
                    (equal? (js-typeof (js-number)) "function"))
              (list "js-bigint"
                    (equal? (js-typeof (js-bigint)) "function"))
              (list "js-math"
                    (equal? (js-typeof (js-math)) "object"))
              (list "js-date"
                    (equal? (js-typeof (js-date)) "function"))
              (list "js-temporal"
                    (equal? (js-typeof (js-temporal)) "object"))))

       (list "Text processing"
             (list
              (list "js-string"
                    (equal? (js-typeof (js-string)) "function"))
              (list "js-reg-exp"
                    (equal? (js-typeof (js-reg-exp)) "function"))))

       (list "Indexed collections"
             (list
              (list "js-typed-array"
                    (equal? (js-typeof (js-typed-array)) "function"))
              (list "js-int8-array"
                    (equal? (js-typeof (js-int8-array)) "function"))
              (list "js-uint8-array"
                    (equal? (js-typeof (js-uint8-array)) "function"))
              (list "js-uint8-clamped-array"
                    (equal? (js-typeof (js-uint8-clamped-array)) "function"))
              (list "js-int16-array"
                    (equal? (js-typeof (js-int16-array)) "function"))
              (list "js-uint16-array"
                    (equal? (js-typeof (js-uint16-array)) "function"))
              (list "js-int32-array"
                    (equal? (js-typeof (js-int32-array)) "function"))
              (list "js-uint32-array"
                    (equal? (js-typeof (js-uint32-array)) "function"))
              (list "js-bigint64-array"
                    (equal? (js-typeof (js-bigint64-array)) "function"))
              (list "js-biguint64-array"
                    (equal? (js-typeof (js-biguint64-array)) "function"))
              (list "js-float16-array"
                    (equal? (js-typeof (js-float16-array)) "function"))
              (list "js-float32-array"
                    (equal? (js-typeof (js-float32-array)) "function"))
              (list "js-float64-array"
                    (equal? (js-typeof (js-float64-array)) "function"))))

       (list "Keyed collections"
             (list
              (list "js-map"
                    (equal? (js-typeof (js-map)) "function"))
              (list "js-set"
                    (equal? (js-typeof (js-set)) "function"))
              (list "js-weak-map"
                    (equal? (js-typeof (js-weak-map)) "function"))
              (list "js-weak-set"
                    (equal? (js-typeof (js-weak-set)) "function"))))

       (list "Structured data"
             (list
              (list "js-array-buffer"
                    (equal? (js-typeof (js-array-buffer)) "function"))
              (list "js-shared-array-buffer"
                    (equal? (js-typeof (js-shared-array-buffer)) "function"))
              (list "js-data-view"
                    (equal? (js-typeof (js-data-view)) "function"))
              (list "js-atomics"
                    (equal? (js-typeof (js-atomics)) "object"))
              (list "js-json"
                    (equal? (js-typeof (js-json)) "object"))))

       (list "Managing memory"
             (list
              (list "js-weak-ref"
                    (equal? (js-typeof (js-weak-ref)) "function"))
              (list "js-finalization-registry"
                    (equal? (js-typeof (js-finalization-registry)) "function"))))

       (list "Control abstraction objects"
             (list
              (list "js-iterator"
                    (equal? (js-typeof (js-iterator)) "function"))
              (list "js-async-iterator"
                    (equal? (js-typeof (js-async-iterator)) "function"))
              (list "js-promise"
                    (equal? (js-typeof (js-promise)) "function"))
              (list "js-generator-function"
                    (equal? (js-typeof (js-generator-function)) "function"))
              (list "js-async-generator-function"
                    (equal? (js-typeof (js-async-generator-function)) "function"))
              (list "js-generator"
                    (equal? (js-typeof (js-generator)) "function"))
              (list "js-async-generator"
                    (equal? (js-typeof (js-async-generator)) "function"))
              (list "js-async-function"
                    (equal? (js-typeof (js-async-function)) "function"))
              (list "js-disposable-stack"
                    (equal? (js-typeof (js-disposable-stack)) "function"))
              (list "js-async-disposable-stack"
                    (equal? (js-typeof (js-async-disposable-stack)) "function"))))

       (list "Reflection"
             (list
              (list "js-reflect"
                    (equal? (js-typeof (js-reflect)) "object"))
              (list "js-proxy"
                    (equal? (js-typeof (js-proxy)) "function"))))

       (list "Internationalization"
             (list
              (list "js-intl"
                    (equal? (js-typeof (js-intl)) "object"))
              (list "js-intl-collator"
                    (equal? (js-typeof (js-intl-collator)) "function"))
              (list "js-intl-date-time-format"
                    (equal? (js-typeof (js-intl-date-time-format)) "function"))
              (list "js-intl-display-names"
                    (equal? (js-typeof (js-intl-display-names)) "function"))
              (list "js-intl-duration-format"
                    (equal? (js-typeof (js-intl-duration-format)) "function"))
              (list "js-intl-list-format"
                    (equal? (js-typeof (js-intl-list-format)) "function"))
              (list "js-intl-locale"
                    (equal? (js-typeof (js-intl-locale)) "function"))
              (list "js-intl-number-format"
                    (equal? (js-typeof (js-intl-number-format)) "function"))
              (list "js-intl-plural-rules"
                    (equal? (js-typeof (js-intl-plural-rules)) "function"))
              (list "js-intl-relative-time-format"
                    (equal? (js-typeof (js-intl-relative-time-format)) "function"))
              (list "js-intl-segmenter"
                    (equal? (js-typeof (js-intl-segmenter)) "function"))))

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
