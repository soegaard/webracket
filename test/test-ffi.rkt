;;;
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

;; TODO

;; - js-typeof


(list
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
        (list "js-ref/js-set!/js-assign"
              (let ([obj (js-object (list))])
                (js-set! obj "a" 1)
                (and (equal? (js-ref obj "a") 1)
                     (equal? (js-assign! "b" 2) (void))
                     (equal? (js-ref (js-global-this) "b") 2))))))

 (list "Basic operations"
       (list
        (list "js-array"
              (list (equal? (js-array (vector 11 22 33)) #(11 22 33))))
        (list "js-array/extern"
              (list (equal? (external? (js-array/extern (vector 11 22 33))) #t)))
        (list "js-index" ; requires an external array
              (let ([arr (js-array/extern (vector 11 22 33))])
                (equal? (js-index arr 1) 22)))        
        (list "js-object"
              (let ([obj (js-object (vector (vector "a" 1)))])
                (equal? (js-ref obj "a") 1))
              (let ([obj (js-object (list (list "a" 1)))])
                (equal? (js-ref obj "a") 1)))
        #;(list "js-new"
              (equal? (js-typeof (js-new (js-date) (list))) "object"))
        #;(list "js-send"
              (let ([arr (js-array 1 2 3)])
                (list (js-send arr "join" (list "-")) "1-2-3")))
        #;(list "js-send/flonum"
              (equal? (js-send/flonum (js-math) "abs" (list -1.0)) 1.0))
        #;(list "js-operator"
              (equal? (js-operator "+" (list 1 2)) 3))
        #;(list "js-typeof"
                (list (js-typeof (js-array (vector 1 2 3))) "number"))
        ;; (list "js-instanceof"
        ;;       (let ([arr (js-array (list))])
        ;;         (equal? (js-instanceof arr (js-ref (js-global-this) "Array")) 1)))
        ;; (list "js-null"
        ;;       (equal? (js-typeof (js-null)) "object"))
        ;; (list "js-this"
        ;;       (equal? (js-this) (js-global-this)))
        ))

 #;(list "Fundamental objects"
       (list
        (list "js-Object"
              (equal? (js-typeof (js-Object)) "function"))
        (list "js-Function"
              (equal? (js-typeof (js-Function)) "function"))
        (list "js-Boolean"
              (equal? (js-typeof (js-Boolean)) "function"))
        (list "js-Symbol"
              (equal? (js-typeof (js-Symbol)) "function"))))

 #;(list "Error objects"
       (list
        (list "js-Error"
              (equal? (js-typeof (js-Error)) "function"))
        (list "js-AggregateError"
              (equal? (js-typeof (js-AggregateError)) "function"))
        (list "js-EvalError"
              (equal? (js-typeof (js-EvalError)) "function"))
        (list "js-RangeError"
              (equal? (js-typeof (js-RangeError)) "function"))
        (list "js-ReferenceError"
              (equal? (js-typeof (js-ReferenceError)) "function"))
        (list "js-SuppressedError"
              (equal? (js-typeof (js-SuppressedError)) "function"))
        (list "js-SyntaxError"
              (equal? (js-typeof (js-SyntaxError)) "function"))
        (list "js-TypeError"
              (equal? (js-typeof (js-TypeError)) "function"))
        (list "js-URIError"
              (equal? (js-typeof (js-URIError)) "function"))
        (list "js-InternalError"
              (equal? (js-typeof (js-InternalError)) "function"))))

 #;(list "Numbers and dates"
       (list
        (list "js-Number"
              (equal? (js-typeof (js-Number)) "function"))
        (list "js-BigInt"
              (equal? (js-typeof (js-BigInt)) "function"))
        (list "js-Math"
              (equal? (js-typeof (js-Math)) "object"))
        (list "js-Date"
              (equal? (js-typeof (js-Date)) "function"))
        (list "js-Temporal"
              (equal? (js-typeof (js-Temporal)) "object"))))

 #;(list "Text processing"
       (list
        (list "js-String"
              (equal? (js-typeof (js-String)) "function"))
        (list "js-RegExp"
              (equal? (js-typeof (js-RegExp)) "function"))))
 
 #;(list "Indexed collections"
       (list
        (list "js-TypedArray"
              (equal? (js-typeof (js-TypedArray)) "function"))
        (list "js-Int8Array"
              (equal? (js-typeof (js-Int8Array)) "function"))
        (list "js-Uint8Array"
              (equal? (js-typeof (js-Uint8Array)) "function"))
        (list "js-Uint8ClampedArray"
              (equal? (js-typeof (js-Uint8ClampedArray)) "function"))
        (list "js-Int16Array"
              (equal? (js-typeof (js-Int16Array)) "function"))
        (list "js-Uint16Array"
              (equal? (js-typeof (js-Uint16Array)) "function"))
        (list "js-Int32Array"
              (equal? (js-typeof (js-Int32Array)) "function"))
        (list "js-Uint32Array"
              (equal? (js-typeof (js-Uint32Array)) "function"))
        (list "js-BigInt64Array"
              (equal? (js-typeof (js-BigInt64Array)) "function"))
        (list "js-BigUint64Array"
              (equal? (js-typeof (js-BigUint64Array)) "function"))
        (list "js-Float16Array"
              (equal? (js-typeof (js-Float16Array)) "function"))
        (list "js-Float32Array"
              (equal? (js-typeof (js-Float32Array)) "function"))
        (list "js-Float64Array"
              (equal? (js-typeof (js-Float64Array)) "function"))))

 #;(list "Keyed collections"
       (list
        (list "js-Map"
              (equal? (js-typeof (js-Map)) "function"))
        (list "js-Set"
              (equal? (js-typeof (js-Set)) "function"))
        (list "js-WeakMap"
              (equal? (js-typeof (js-WeakMap)) "function"))
        (list "js-WeakSet"
              (equal? (js-typeof (js-WeakSet)) "function"))))

 #;(list "Structured data"
       (list
        (list "js-ArrayBuffer"
              (equal? (js-typeof (js-ArrayBuffer)) "function"))
        (list "js-SharedArrayBuffer"
              (equal? (js-typeof (js-SharedArrayBuffer)) "function"))
        (list "js-DataView"
              (equal? (js-typeof (js-DataView)) "function"))
        (list "js-Atomics"
              (equal? (js-typeof (js-Atomics)) "object"))
        (list "js-JSON"
              (equal? (js-typeof (js-JSON)) "object"))))
 
 #;(list "Managing memory"
       (list
        (list "js-WeakRef"
              (equal? (js-typeof (js-WeakRef)) "function"))
        (list "js-FinalizationRegistry"
              (equal? (js-typeof (js-FinalizationRegistry)) "function"))))

 #;(list "Control abstraction objects"
       (list
        (list "js-Iterator"
              (equal? (js-typeof (js-Iterator)) "function"))
        (list "js-AsyncIterator"
              (equal? (js-typeof (js-AsyncIterator)) "function"))
        (list "js-Promise"
              (equal? (js-typeof (js-Promise)) "function"))
        (list "js-GeneratorFunction"
              (equal? (js-typeof (js-GeneratorFunction)) "function"))
        (list "js-AsyncGeneratorFunction"
              (equal? (js-typeof (js-AsyncGeneratorFunction)) "function"))
        (list "js-Generator"
              (equal? (js-typeof (js-Generator)) "function"))
        (list "js-AsyncGenerator"
              (equal? (js-typeof (js-AsyncGenerator)) "function"))
        (list "js-AsyncFunction"
              (equal? (js-typeof (js-AsyncFunction)) "function"))
        (list "js-DisposableStack"
              (equal? (js-typeof (js-DisposableStack)) "function"))
        (list "js-AsyncDisposableStack"
              (equal? (js-typeof (js-AsyncDisposableStack)) "function"))))
 
 #;(list "Reflection"
       (list
        (list "js-Reflect"
              (equal? (js-typeof (js-Reflect)) "object"))
        (list "js-Proxy"
              (equal? (js-typeof (js-Proxy)) "function"))))

 #;(list "Internationalization"
       (list
        (list "js-Intl"
              (equal? (js-typeof (js-Intl)) "object"))
        (list "js-IntlCollator"
              (equal? (js-typeof (js-IntlCollator)) "function"))
        (list "js-IntlDateTimeFormat"
              (equal? (js-typeof (js-IntlDateTimeFormat)) "function"))
        (list "js-IntlDisplayNames"
              (equal? (js-typeof (js-IntlDisplayNames)) "function"))
        (list "js-IntlDurationFormat"
              (equal? (js-typeof (js-IntlDurationFormat)) "function"))
        (list "js-IntlListFormat"
              (equal? (js-typeof (js-IntlListFormat)) "function"))
        (list "js-IntlLocale"
              (equal? (js-typeof (js-IntlLocale)) "function"))
        (list "js-IntlNumberFormat"
              (equal? (js-typeof (js-IntlNumberFormat)) "function"))
        (list "js-IntlPluralRules"
              (equal? (js-typeof (js-IntlPluralRules)) "function"))
        (list "js-IntlRelativeTimeFormat"
              (equal? (js-typeof (js-IntlRelativeTimeFormat)) "function"))
        (list "js-IntlSegmenter"
              (equal? (js-typeof (js-IntlSegmenter)) "function"))))

 ;;;
 ;;; Number
 ;;;
 
 ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number
 #;(list "Number functions"
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
