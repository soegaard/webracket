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
              ; todo (?) - we don't support lists here
              #;(let ([obj (js-object (list (list "a" 1)))])
                (equal? (js-ref obj "a") 1)))
        (list "js-new"
              ; The current time:
              #;(external-string->string (js-new (js-Date) (vector))) 
              (string? (external-string->string (js-new (js-Date) (vector)))))
        (list "js-send"
              (let ([arr (js-array/extern (vector 1 2 3))])
                (equal? (js-send arr "join" (vector "-")) "1-2-3")))
        (list "js-send/extern"
              (let* ([arr (js-array/extern (vector 1 2 3))]
                     [res (js-send/extern arr "join" (vector "-"))])
                (and (external? res)
                     (equal? (external-string->string res) "1-2-3"))))
        (list "js-send/value primitive result"
              (let ([arr (js-array/extern (vector 1 2 3))])
                (equal? (js-send/value arr "join" (vector "-"))
                        "1-2-3")))
        (list "js-send/value array result"
              (let* ([arr (js-array/extern (vector 10 20 30))]
                     [res (js-send/value arr "slice" (vector 1))])
                (and (vector? res)
                     (equal? res #(20 30)))))
        (list "js-send/boolean"
              (let ([arr (js-array/extern (vector 1 2 3))])
                (and (equal? (js-send/boolean arr "includes" (vector 2)) #t)
                     (equal? (js-send/boolean arr "includes" (vector 9)) #f))))
        (list "js-send/boolean accepts Boolean object"
              (let ([obj (js-eval "({ f: function(){ return new Boolean(false); } })")])
                (equal? (js-send/boolean obj "f" (vector)) #f)))
        (list "js-send/boolean non-boolean result raises"
              (with-handlers ([exn? (λ (_) #t)])
                (js-send/boolean (js-Math) "abs" (vector -1.0))
                #f))
        (list "js-send/extern host exception is catchable"
              (with-handlers ([exn? (λ (_) #t)])
                (js-send/extern (js-array/extern (vector 1 2 3)) "notAMethod" (vector))
                #f))
        (list "js-send/truthy"
              (let ([obj (js-eval "({ z: function(){ return 0; }, n: function(){ return 42; }, e: function(){ return ''; } })")])
                (and (equal? (js-send/truthy obj "z" (vector)) #f)
                     (equal? (js-send/truthy obj "n" (vector)) #t)
                     (equal? (js-send/truthy obj "e" (vector)) #f))))
        (list "js-send/flonum"
              (equal? (js-send/flonum (js-Math) "abs" (vector -1.0)) 1.0))
        (list "js-operator"
              (equal? (external-number->flonum
                       (js-operator "+" (vector 1 2)))   3.))
        (list "js-typeof"
              (list (js-typeof (js-array/extern (vector 1 2 3))) "object"))
        (list "js-instanceof"
              (let ([arr (js-array/extern #())])
                (equal? (js-instanceof arr (js-ref (js-global-this) "Array")) #t)))
         (list "js-null"
               (equal? (js-typeof (js-null)) "object"))
         #;(list "js-this"  ; this only makes sense in a function body
                 (equal? (js-this) (js-global-this)))
         ))

 (list "Callback bridge"
       (list
        (list "procedure->external/call basic"
              (let* ([f   (procedure->external (λ (a b) (+ a b)))]
                     [res (js-send f "call" (vector (js-global-this) 10 20))])
                (equal? res 30)))
        (list "procedure->external/undefined arg maps to void"
              (let* ([f   (procedure->external (λ (_a b) (void? b)))]
                     [res (js-send f "call" (vector (js-global-this) 1 (js-undefined)))])
                (equal? res #t)))
        (list "procedure->external/js-send inside callback"
              (let* ([obj (js-eval "({ next: function(){ return 'x'; } })")]
                     [f   (procedure->external
                           (λ (stream _state)
                             (define v (js-send stream "next" (vector)))
                             (string-length v)))]
                     [res (js-send f "call" (vector (js-global-this) obj (js-undefined)))])
                (equal? res 1)))
        (list "procedure->external/js-send undefined return"
              (let* ([obj (js-eval "({ next: function(){ return undefined; } })")]
                     [f   (procedure->external
                           (λ (stream _state)
                             (define v (js-send stream "next" (vector)))
                             (void? v)))]
                     [res (js-send f "call" (vector (js-global-this) obj (js-undefined)))])
                (equal? res #t)))))

 (list "Fundamental objects"
       (list
        (list "js-Object"
              (equal? (js-typeof (js-Object)) "function"))
        (list "js-Function"
              (equal? (js-typeof (js-Function)) "function"))
        (list "js-Boolean"
              (equal? (js-typeof (js-Boolean)) "function"))
        (list "js-Symbol"
              (equal? (js-typeof (js-Symbol)) "function"))))

 (list "Error objects"
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
              (equal? (js-typeof (js-SuppressedError)) "function"))  ; todo
        (list "js-SyntaxError"
              (equal? (js-typeof (js-SyntaxError)) "function"))
        (list "js-TypeError"
              (equal? (js-typeof (js-TypeError)) "function"))
        (list "js-URIError"
              (equal? (js-typeof (js-URIError)) "function"))
        (list "js-InternalError"
              (equal? (js-typeof (js-InternalError)) "function")))) ; todo

 (list "Numbers and dates"
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
              (equal? (js-typeof (js-Temporal)) "object")))) ; todo

 (list "Text processing"
       (list
        (list "js-String"
              (equal? (js-typeof (js-String)) "function"))
        (list "js-RegExp"
              (equal? (js-typeof (js-RegExp)) "function"))))
 
 (list "Indexed collections"
       (list
        #;(list "js-TypedArray" ; is not available everywhere
              (member (js-typeof (js-TypedArray)) '("undefined" "function")))  ; todo - implement member
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
        #;(list "js-Float16Array" ; is not available everywhere, baseline 2025 (upgrade!)
                (member (js-typeof (js-Float16Array)) '("undefined" "function"))) ; todo - implement member
        (list "js-Float32Array"
              (equal? (js-typeof (js-Float32Array)) "function"))
        (list "js-Float64Array"
              (equal? (js-typeof (js-Float64Array)) "function"))))

 (list "Keyed collections"
       (list
        (list "js-Map"
              (equal? (js-typeof (js-Map)) "function"))
        (list "js-Set"
              (equal? (js-typeof (js-Set)) "function"))
        (list "js-WeakMap"
              (equal? (js-typeof (js-WeakMap)) "function"))
        (list "js-WeakSet"
              (equal? (js-typeof (js-WeakSet)) "function"))))

 (list "Structured data"
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
 
 (list "Managing memory"
       (list
        (list "js-WeakRef"
              (equal? (js-typeof (js-WeakRef)) "function"))
        (list "js-FinalizationRegistry"
              (equal? (js-typeof (js-FinalizationRegistry)) "function"))))

 (list "Control abstraction objects"
       (list
        (list "js-Iterator"
              (equal? (js-typeof (js-Iterator)) "function"))
        (list "js-AsyncIterator"  
              (list (js-typeof (js-AsyncIterator)) "function"))  ; undefined ?
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
 
 (list "Reflection"
       (list
        (list "js-Reflect"
              (equal? (js-typeof (js-Reflect)) "object"))
        (list "js-Proxy"
              (equal? (js-typeof (js-Proxy)) "function"))))

 (list "Internationalization"
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
