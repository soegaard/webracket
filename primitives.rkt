#lang racket/base
 (require racket/bool
          racket/fasl
          racket/fixnum
          racket/flonum
          racket/hash
          racket/keyword
          racket/list
          ; racket/namespace
          racket/port
          racket/string
          racket/symbol
          racket/vector
          racket/unsafe/ops
          (prefix-in imm: "immediates.rkt"))

;; The primitives are 

(provide

 ;; 4.1 Equality
 eq?
 eqv?
 equal?
 eq-hash-code
 eqv-hash-code
 equal-hash-code
 ; equal-always?
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
 immutable?

 ;; 4.3 Numbers

 ;; 4.3.2 Generic Numerics
 ;; 4.3.2.1 Arithmetic
 ;; 4.3.2.2 Number Comparison
 ;; 4.3.2.3 Powers and Roots
 ;; 4.3.2.4 Trigonometric Functions
 ;; 4.3.2.5 Complex Numbers
 ;; 4.3.2.6 Bitwise Operations
 ;; 4.3.2.7 Random Numbers
 ;; 4.3.2.8 Other Randomness Utilities (racket/random)
 ;; 4.3.2.9 Number–String Conversions
 ;; 4.3.2.10 Extra Constants and Functions (racket/math)
 
 ;; 4.3.3 Flonums (racket/flonum)
 ;; 4.3.3.1 Flonum Arithmetic
 ;; 4.3.3.2 Flonum Vectors

 ;; 4.3.4 Fixnums (racket/fixnum)
 ;; 4.3.4.1 Fixnum Arithmetic
 ;; 4.3.4.2 Fixnum Vectors
 ;; 4.3.4.3 Fixnum Range
 
 number? integer? exact? exact-integer?
 exact-nonnegative-integer? exact-positive-integer?
 inexact->exact number->string
 round floor ceiling truncate
 abs sqrt
 sin cos tan asin acos atan 
 + - * / = < > <= >= zero? positive? negative? add1 sub1

 fixnum? fxzero? fx+ fx- fx* fx= fx> fx< fx<= fx>= fxquotient unsafe-fxquotient
 fx->fl fl->fx
 flonum? fl+ fl- fl* fl/ fl= fl< fl> fl<= fl>=
 flabs fllog flexp flsqrt
 flsin flcos fltan flasin flacos flatan
 flround flfloor flceiling fltruncate flsingle
 flexpt
 flmin flmax ->fl fl->exact-integer
 
 byte? 

 ;; Fixnum
 fx+
 fx-
 fx*
 fxquotient
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

 fx= fx< fx>
 fx<= fx>=
 fxmin fxmax ; variadic

 fx->fl
 fl->fx
 most-positive-fixnum
 most-negative-fixnum

   inexact->exact abs round floor ceiling truncate sqrt sin cos tan asin acos atan
 flabs flround flfloor flceiling fltruncate flsingle
 flsin flcos fltan flasin flacos flatan fllog flexp flsqrt flmin flmax flexpt

 
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
 string-copy
 string-copy!
 string-fill! 
 string->immutable-string
 string=? string<? string<=? string>? string>=?
 string-ci=?  
 string-prefix?
 string-suffix?
 string-contains?
 string-find
 string-take string-take-right
 string-drop string-drop-right
 string-trim-left string-trim-right
 
 string->list
 string->bytes/utf-8
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
 bytes->list
 list->bytes
 bytes=?
 bytes->string/utf-8

 ;; 4.6 Characters
 char? char->integer integer->char
 char=? char<? char<=? char>? char>=?
 char-downcase char-foldcase char-titlecase char-upcase
 char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
 char-whitespace?

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
 null?
 cons
 car
 cdr
 list?
 list
 list*
 make-list     ; racket/list
 ; build-list

 length
 list-ref
 list-tail
 append
 reverse

 map
 ; andmap
 ; ormap
 for-each
 ; foldl
 ; foldr

 filter
 ; remove
 ; remq
 ; remv
 ; remw
 ; remove*
 ; remq*
 ; remv*
 ; remw*
 ; sort

 ; member
 ; memw
 ; memv
 memq
 ; memf
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

 ;; 4.12 Vectors
 vector vector-immutable vector? make-vector vector-ref vector-set!
 vector-length vector-fill! vector-copy! vector-empty? vector-take vector-drop
 vector-drop-right vector-split-at vector->list vector-copy list->vector

 ;; 4.14 Boxes
 ; boxed unboxed set-boxed!  ; internal
 box unbox set-box!

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
 unsafe-struct-ref unsafe-vector*-length unsafe-vector*-set! unsafe-struct-set!
 

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
 js_log

)

(define (js_log v)
  (displayln v))

(define (procedure->external p)
  (unless (procedure? p)
    (raise-argument-error 'procedure->external "procedure?" p))
  p)


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



