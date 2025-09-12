#lang racket/base
(require ; version-case
         racket/bool
         racket/fasl
         racket/fixnum
         racket/flonum
         racket/hash
         racket/keyword
         racket/list
         racket/math
         racket/mpair         
         racket/mutability
         racket/port
         racket/string
         racket/symbol
         racket/vector
         racket/unsafe/ops         
         ; racket/namespace

         math/flonum
         (only-in math/base
                  asinh acosh atanh                  
                  float-complex?)
         
         (prefix-in imm: "immediates.rkt")

         racket/match
         )

;; The primitives are 

(provide
 match

 
 ;; Test functions
 always-throw ; todo - remove

 ;; checkers
 check-list
 check-mlist
 check-range
 check-range-generic
 check-naturals
 
 
 ;; 4.1 Equality
 eq?
 eqv?
 equal?
 eq-hash-code
 eqv-hash-code
 equal-hash-code
 equal-always?
 ; equal?/recur
 ; equal-always?/recur
 ;; equal-hash-code/recur
 ;; equal-secondary-hash-code
 ;; equal-always-hash-code
 ;; equal-always-hash-code/recur
 ;; equal-always-secondary-hash-code


 ;; 4.2 Booleans
 not
 boolean?
 boolean=?
 false?
 xor
 immutable?
 mutable-string?
 immutable-string?
 mutable-bytes?
 immutable-bytes?
 mutable-vector?
 immutable-vector?
 mutable-box?
 immutable-box?
 mutable-hash?
 immutable-hash?

 ;; 4.3 Numbers

;; 4.3.1 Number Types
 number?
 real?
 integer?
 exact?
 exact-integer?
 exact-nonnegative-integer?
 exact-positive-integer?
 nan?
 infinite?
 positive-integer?
 negative-integer?
 nonpositive-integer?
 nonnegative-integer?
 natural?
 inexact-real?
 inexact?
 inexact->exact
 exact->inexact
 real->double-flonum
  fixnum?
 flonum?
 double-flonum?
 single-flonum?
 single-flonum-available?
 zero?
 positive?
 negative?
 even?
 odd?

 ;; 4.3.2 Generic Numerics
 ;; 4.3.2.1 Arithmetic
 +
 -
 *
 /
 quotient
 remainder
 modulo
 quotient/remainder
 add1
 sub1
 gcd
 lcm
 abs
 sgn
 max
 min
 exact-round exact-floor exact-ceiling exact-truncate
 round
 floor
 ceiling
 truncate
 ;; 4.3.2.2 Number Comparison
 =
 <
 >
 <=
 >=
 ;; 4.3.2.3 Powers and Roots
 sqr
 sqrt
 integer-sqrt
 integer-sqrt/remainder
 expt
 exp
 log
 ;; 4.3.2.4 Trigonometric Functions
 sin
 cos
 tan
 asin
 acos
 atan
 sinh
 cosh
 tanh
 asinh
 acosh
 atanh
 ;; 4.3.2.5 Complex Numbers
 ;; 4.3.2.6 Bitwise Operations
 bitwise-ior
  bitwise-and
  bitwise-xor
  bitwise-not
  bitwise-bit-set?
  bitwise-first-bit-set  ; added in version 8.16
  bitwise-bit-field
  arithmetic-shift
  integer-length
  random
 ;; 4.3.2.7 Random Numbers
 ;; 4.3.2.8 Other Randomness Utilities (racket/random)
 ;; 4.3.2.9 Number–String Conversions
 number->string
 string->number
 floating-point-bytes->real
 real->floating-point-bytes
 ;; 4.3.2.10 Extra Constants and Functions (racket/math)

 degrees->radians
 radians->degrees
 order-of-magnitude
 system-big-endian?

 ;; 4.3.3 Flonums (racket/flonum)
 ;; 4.3.3.1 Flonum Arithmetic
 fl+
 fl-
 fl*
 fl/
 fl=
 fl<
 fl>
 fl<=
 fl>=
 flabs
 fllog
 flexp
 flsqrt
 flsin
 flcos
 fltan
 flasin
 flacos
 flatan
 flround
 flfloor
 flceiling
 fltruncate
 flsingle
 flexpt
 flmin
 flmax
 ->fl
 fl->exact-integer
 ;; 4.3.3.2 Flonum Vectors

 ;; 4.3.4 Fixnums (racket/fixnum)
 ;; 4.3.4.1 Fixnum Arithmetic
 fxzero?
 fx+
 fx-
 fx*
 fxquotient
 unsafe-fxquotient
 fxremainder
 fxmodulo
 fxabs
 fxand
 fxior
 fxxor
 fxnot
 fxlshift
 fxrshift
 fxpopcount
 fxpopcount32
 fxpopcount16
 fx+/wraparound
 fx-/wraparound
 fx*/wraparound
 fxlshift/wraparound
 fxrshift/logical
 fx=
 fx<
 fx>
 fx<=
 fx>=
 fxmin
 fxmax
 fx->fl
 fl->fx
 ;; 4.3.4.2 Fixnum Vectors
 ;; 4.3.4.3 Fixnum Range
 most-positive-fixnum
 most-negative-fixnum

 
 ;; 4.4 Strings
 build-string
 make-string
 list->string
 substring

 string?
 string-ref
 string-set!
 string-length
 string-append
 string-append-immutable
 string-append*
 string-join
 string-copy
 string-copy!
 string-fill!
 string-upcase
 string-downcase
 string-titlecase
 string-foldcase
 string->immutable-string
 string=? string<? string<=? string>? string>=?
 string-ci=?  
 string-prefix?
 string-suffix?
 string-contains?
 string-replace
 string-find
 string-take string-take-right
 string-drop string-drop-right
 string-trim-left string-trim-right
 
 string->list
 string->bytes/utf-8
 string-utf-8-length
 string->immutable-string
 
 ;; 4.5 Byte Strings
 bytes?
 make-bytes
 bytes-ref
 bytes-set!
 bytes-length
 subbytes
 bytes-copy!
 bytes-copy
 bytes-fill!
bytes-append
bytes->immutable-bytes
 byte?
bytes->list
list->bytes
bytes=?
bytes->string/utf-8

 ;; 4.6 Characters
 char? char->integer integer->char char-utf-8-length
 char=? char<? char<=? char>? char>=?
 char-downcase char-foldcase char-titlecase char-upcase
 char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
 char-whitespace? char-general-category char-grapheme-break-property
 char-alphabetic? char-lower-case? char-upper-case? char-title-case?
 char-numeric? char-symbolic? char-punctuation? char-graphic?
 char-whitespace? char-blank? char-iso-control? char-extended-pictographic?

 ;; 4.7 Symbols
 symbol?
 symbol=?  ; from racket/bool
 symbol<?
 string->symbol
 symbol->string
 string->uninterned-symbol
 symbol-interned?
 ; symbol-unreadable?
 ; gensym

 ;; 4.7.1
 symbol->immutable-string
 

;; 4.9 Keywords

 keyword?
 keyword->string
 keyword<?
 ; 4.9.1 Additional Keyword Functions
 keyword->immutable-string
 
 
  ;; 4.10 Pairs and Lists
 pair?
 cons?
 null?
 empty?
 cons
 car
 cdr
 list?
 list
 list*
 make-list     ; racket/list
 build-list
 range range-proc
 inclusive-range inclusive-range-proc

 length
 list-ref
 list-tail
 list-update
 list-set
 first
 rest
 second
  third
  fourth
  fifth
 sixth
 seventh
 eighth
 ninth
 tenth
 eleventh
 twelfth
 thirteenth
 fourteenth
 fifteenth
 last
 last-pair
 append
 flatten
 reverse

 map
 andmap
 ormap
 append-map
 foldl foldr
 for-each
 count
 add-between
 cartesian-product
 permutations
 ; foldl
 ; foldr

 filter
 filter-map
 filter-not
 shuffle
 partition
 index-of
 index-where
 indexes-of
 indexes-where
 list-prefix?
 take-common-prefix
 drop-common-prefix
 split-common-prefix

 member memq memv memw
 ; memf
 remove  remf  remq  remv  remw
 remove* remf* remq* remv* remw*
 ; sort

 argmax argmin
 group-by
 sort
 
 ; findf
 ; assoc
 ; assw
 ; assv
 ; assq
 ; assf

 ; caar
 ; cadr
 ; cdar
 ; cddr

 ;; 4.11 Mutable Pairs and Lists

 mpair?
 mcons
 mcar
 mcdr
 set-mcar!
 set-mcdr!
 
 ;; 4.12 Vectors
 vector vector-immutable vector? make-vector vector-ref vector-set!
 vector-length vector-fill! vector-copy! vector-empty?
 vector-take vector-take-right
 vector-drop vector-drop-right
 vector-split-at vector-split-at-right 
 vector-copy
 vector->list vector->values vector->immutable-vector
 list->vector
 vector-map vector-map! vector-append
 vector-extend ; racket 8.12
 vector-count
 vector-argmax vector-argmin
 vector-filter vector-filter-not
 vector-member vector-memq vector-memv
 vector-sort! vector-sort
 build-vector
 vector-set/copy
 
 ;; 4.14 Boxes
 ; boxed unboxed set-boxed!  ; internal
 box? box box-immutable unbox set-box! 

 ;; 4.15 Hash Tables
 hash? make-empty-hasheq make-hasheq hash-ref hash-set! hash-remove! hash-clear!
 hash-has-key? eq-hash-code
 
 ;; 4.20 Procedures
 procedure? apply procedure-rename procedure->external
 procedure-arity procedure-arity-mask procedure-arity-includes?
 primitive? primitive-closure? primitive-result-arity

 ;; 4.21 Void
 void? make-void void

 ;; 5.1 Structures
 make-struct-type make-struct-field-accessor make-struct-field-mutator
 struct-constructor-procedure? struct-predicate-procedure?
 struct-accessor-procedure? struct-mutator-procedure?
 struct? struct-type? current-inspector
 
 ;; 10.1 Multiple Values
 values
 call-with-values

 ;; 10.2 Exceptions
 raise
 raise-argument-error
 ; raise-unbound-variable-reference
 
 ;; 13.5 Writing
 display
 displayln
 newline

 ;; 13.7 String Ports
 string-port? open-output-bytes get-output-bytes write-byte port-next-location
 
 ;; 14.1 Namespaces
 namespace? make-empty-namespace namespace-variable-value-simple
 namespace-set-variable-value! namespace-undefine-variable!

 ;; 17. Unsafe Operations
 unsafe-fx+ unsafe-fl/
 unsafe-flabs unsafe-flround unsafe-flfloor unsafe-flceiling unsafe-fltruncate
 unsafe-flsingle unsafe-flsin unsafe-flcos unsafe-fltan unsafe-flasin
 unsafe-flacos unsafe-flatan unsafe-fllog unsafe-flexp unsafe-flsqrt
 unsafe-flmin unsafe-flmax unsafe-flexpt
 unsafe-fx= unsafe-fx< unsafe-car unsafe-cdr
 unsafe-struct-ref unsafe-vector-length unsafe-vector-ref unsafe-vector*-length unsafe-vector*-set! unsafe-struct-set!
 

 ;; 14.1 Namespaces

 namespace?
 make-empty-namespace
 ; namespace-variable-value
 namespace-set-variable-value!
 namespace-undefine-variable!
 ; namespace-has-key?

 ;; FFI
 
 s-exp->fasl
 fasl->s-exp
 procedure->external
 external-number->flonum
 external-string->string
 js-log
 external?

)

(define (js_log v)
  (displayln v))

(define (procedure->external p)
  (unless (procedure? p)
    (raise-argument-error 'procedure->external "procedure?" p))
  p)

(define (external-number->flonum x)
  (cond [(number? x) (exact->inexact x)]
        [else (raise-argument-error 'external-number->flonum "number?" x)]))

(define (external-string->string x)
  (cond [(string? x) x]
        [else (raise-argument-error 'external-string->string "string?" x)]))


;; Changed in version 8.15.0.7: Added string-find.

(define (string-find s contained)
  (define m
    (regexp-match-positions (regexp (regexp-quote contained)) s))
  (and m (car (car m))))

(define (fxzero? x)
  (and (fixnum? x)
       (zero? x)))

(define (most-positive-fixnum)
  imm:most-positive-fixnum)

(define (most-negative-fixnum)
  imm:most-negative-fixnum)


(define (string-take s n)
  (define l (string-length s))
  (when (> n l)
    (error 'string-take "attempt to take substring longer than string"))
  (substring s 0 n))

(define (string-take-right s n)
  (define l (string-length s))
  (when (> n l)
    (error 'string-take-right
           "attempt to take substring longer than string"))
  (substring s (- l n) l))

(define (string-drop s n)
  (define l (string-length s))
  (when (> n l)
    (error 'string-drop "attempt to drop substring longer than string"))
  (substring s n l))

(define (string-drop-right s n)
  (define l (string-length s))
  (when (> n l)
    (error 'string-drop-right
           "attempt to drop substring longer than string"))
  (substring s 0 (- l n)))

(define (string-trim-left s sep)
  ; trims s to the left by removing sep
  (define l (string-length s))
  (define n (let loop ([i 0])
              (cond
                [(> i l)                     n]
                [(eqv? (string-ref s i) sep) (loop (+ i 1))]
                [else                        i])))  
  (substring s n l))

(define (string-trim-right s sep)
  ; trims s to the left by removing sep
  (define l (string-length s))
  (define n (let loop ([i 0])
              (cond
                [(> i l)                     n]
                [(eqv? (string-ref s i) sep) (loop (+ i 1))]
                [else                        i])))  
  (substring s (- l n) l))


(struct boxed (x) #:transparent #:mutable)
(define (unboxed b)      (boxed-x b))
(define (set-boxed! b x) (set-boxed-x! b x))

(define (make-empty-hasheq)
  (make-hasheq))

(define (make-void) (void))

(define (namespace-variable-value-simple ns sym)
  (namespace-variable-value sym #t #f ns))

;; The inverse hyperbolic functions are from
;;   math/private/base/base-functions.rkt

(define (asinh x)
  (cond [(flonum? x)  (flasinh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flasinh (fl x))]
        [(float-complex? x)  (log (+ x (sqrt (+ (* x x) 1.0))))]
        [else  (log (+ x (sqrt (+ (* x x) 1))))]))

(define (acosh x)
  (cond [(flonum? x)  (flacosh x)]
        [(eqv? x 1)  0]
        [(and (real? x) (x . >= . 1))  (flacosh (fl x))]
        [(float-complex? x)  (log (+ x (* (sqrt (+ x 1.0)) (sqrt (- x 1.0)))))]
        [else  (log (+ x (* (sqrt (+ x 1)) (sqrt (- x 1)))))]))

(define (atanh x)
  (cond [(flonum? x)  (flatanh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flatanh (fl x))]
        [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
        [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))


;; (require (for-syntax racket/base))  ; for version-case
;; (version-case
;;  [(version< (version) "8.16")
;;   (define (bitwise-first-bit-set n)
;;     (unless (exact-integer? n)
;;       (raise-argument-error 'bitwise-first-bit-set
;;                             "exact-integer?" n))
;;     (if (zero? n)
;;         -1
;;         (sub1 (integer-length (bitwise-and n (- n))))))])

(define (bitwise-first-bit-set n)
  (unless (exact-integer? n)
    (raise-argument-error 'bitwise-first-bit-set
                          "exact-integer?" n))
  (if (zero? n)
      -1
      (sub1 (integer-length (bitwise-and n (- n))))))

; This simplified version of random doesn't allow passing
; a random number generator.
(define random
  ;; Simple wrapper around Racket's `random` that exposes the same
  ;; optional argument behaviour to the rest of the system.  The RNG
  ;; state itself lives in `runtime-wasm.rkt` and is shared across
  ;; invocations of the Wasm primitive.
  (case-lambda
    [()        (racket:random)]
    [(k)       (racket:random k)]
    [(min max) (racket:random min max)]))

; The "real" apply is a macro (due to keyword arguments)
; This defines a plain apply as a procedure.
; (the machinery in `priminfo.rkt` expects a procedure)
(require (prefix-in racket: racket/base))
(define (apply proc . xss)
  (racket:apply racket:apply proc xss))

; Added in Racket 8.15.
(define (eleventh    xs) (list-ref xs 11))
(define (twelfth     xs) (list-ref xs 12))
(define (thirteenth  xs) (list-ref xs 13))
(define (fourteenth  xs) (list-ref xs 14))
(define (fifteenth   xs) (list-ref xs 15))


; FFI
(define (external? x) #f)

(define (js-log x)
  (displayln x))


(define (always-throw)
  (raise 42))


(define (inclusive-range-proc start end [step #f])
  ;; Step defaults to 1 or -1 depending on order of start and end.
  ;; Uses flonum defaults when either boundary is inexact.
  (define default-step
    (if (<= start end)
        (if (or (inexact? start) (inexact? end)) 1.0 1)
        (if (or (inexact? start) (inexact? end)) -1.0 -1)))
  (inclusive-range start end (if step step default-step)))

(define (range-proc start-or-end [end #f] [step #f])
  (cond [(eq? end #f) (range start-or-end)]
        [(eq? step #f) (range start-or-end end)]
        [else (range start-or-end end step)]))

(define (range start-or-end [end #f] [step #f]) 
  (cond [(eq? end #f) (range start-or-end)]
        [(eq? step #f) (range start-or-end end)]
        [else (range start-or-end end step)]))

;; Checkers

(define (check-list l)
  (unless (list? l)
    (raise-argument-error 'in-list "list?" l)))

(define (check-mlist l)
  (unless (or (mpair? l) (null? l))
    (raise-argument-error 'in-mlist "(or/c mpair? null?)" l)))

(define (check-range a b step)
  (check-range-generic 'in-range a b step))

(define (check-range-generic who a b step)
  (unless (real? a) (raise-argument-error who "real?" a))
  (unless (real? b) (raise-argument-error who "real?" b))
  (unless (real? step) (raise-argument-error who "real?" step)))

(define (check-naturals n)
  (unless (and (integer? n)
               (exact? n)
               (n . >= . 0))
    (raise-argument-error 'in-naturals
                          "exact-nonnegative-integer?"
                          n)))

(require (prefix-in racket: racket/list))
(define (add-between xs v)   ; simplified version without keyword arguments.
  (racket:add-between xs v))


; A simplified no-keywords version of string-join.
(define (string-join strs [maybe-sep " "])
  (define xs (add-between strs maybe-sep))
  (string-append* xs))

(require (prefix-in racket: racket/string))

; A simplified no-keywords version 
(define (string-replace str from to [all? #t])
  (racket:string-replace str from to all?))

; A simplified no-keywords version 
(define (sort xs less-than?)
  (racket:sort xs less-than?))


; Added in version Racket 8.12.
(define (vector-extend vec new-size [val 0])
  (define old-size (vector-length vec))
  (unless (and (exact-nonnegative-integer? new-size)
               (>= new-size old-size))
    (raise-argument-error 'vector-extend
                          "(and/c exact-nonnegative-integer? (>=/c (vector-length vec)))"
                          new-size))
  (define new-vec (make-vector new-size val))
  (for ([i (in-range old-size)])
    (vector-set! new-vec i (vector-ref vec i)))
  new-vec)

(require (prefix-in racket: racket/vector))
         
; A simplified no-keywords version 
(define (vector-sort! vec less-than? [start 0] [end (vector-length #f)])
  (racket:vector-sort! vec less-than? start end))

; A simplified no-keywords version 
(define (vector-sort vec less-than? [start 0] [end (vector-length #f)])
  (racket:vector-sort vec less-than? start end))

