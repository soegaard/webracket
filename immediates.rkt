#lang racket/base
(provide (all-defined-out))

;;;
;;; Representation of immediates
;;;

;; Racket being a dynamic language, we will use a uniform representation for all
;; values. On platforms like x86 and arm64 most often tagged pointers are used 
;; for a uniform representation. On Web Assembly we do not have pointers.
;; Instead we have references. Heap allocated objects (arrays, structs) are
;; referred to by reference. In order to use references to represent other
;; immediates such as fixnums, booleans, null, etc. we will use `i31` which
;; denotes unboxed scalars.
;;
;; The specifications says:
;;   > The abstract type `i31` denotes unboxed scalars, that is, integers
;;   > injected into references. Their observable value range is limited to 31 bits.
;;
;;   > An `i31` is not actually allocated in the store, but represented in a way
;;   > that allows them to be mixed with actual references into the store
;;   > without ambiguity. Engines need to perform some form of pointer
;;   > tagging to achieve this, which is why 1 bit is reserved.
;;
;; That is, an `i31` is in effect a "tagged pointer" with a 1-bit tag.
;; Like `struct` and `array` we have that `i31` also is a heaptype.
;; We can therefore use `(ref eq)` for our uniform representation.
;;
;; We will use the lower bits of an `i31` as tags, so we can have other
;; immediates besides fixnums. We have 31 bits to work with.

;; Data Representation
;;   Fixnums:   Lower 1 bits are 0. Upper bits are an 30-bit integer.
;;   Immediate: Lower 1 bits are 1. Upper 30 bits contain data and a subtag.

;; Immediate Values (non-fixnum)
;;   Characters: Lower 8 bits are  cc...cc 0000 1111. The 21 bits marked c are a unicode scalar value.
;;   Booleans:   Lower 7 bits are          b001 1111. Bit b is 1 for true and 0 for false.
;;   Void:       Lower 8 bits are          0010 1111. Upper bits are zero.
;;   Empty:      Lower 8 bits are          0011 1111. Upper bits are zero.
;;   Undefined:  Lower 8 bits are          0100 1111. Upper bits are zero.
;;   Eof:        Lower 8 bits are          0101 1111. Upper bits are zero.
;;   Missing [*] All bits are 1.   1 ...   1111 1111. 

;; Note: We do not have tags for pairs, vectors, etc. since they need to heap allocated.
;; The missing value is not a Racket value. It is an immediate used to indicate
;; "not found" in hash tables. It needs to distinct from all valid Racket values.
;; [The missing immediates was introduced to avoid the null type in WebAssembly.]

(define fixnum-tag           #b0)
(define immediate-tag     #b1111)
(define char-tag       #b00001111)
(define boolean-tag    #b0011111)

(define void-value      #b00101111)   ; 0010 1111
(define empty-value     #b00111111)   ; 0011 1111
(define undefined-value #b01001111)   ; 0100 1111
(define eof-value       #b01011111)   ; 0101 1111
; The `missing` and `tombstone` value are used internally.
; In particular in the implementation of hash tables.
(define missing-value   #b1111111111111111111111111111111)
(define tombstone-value #b0111111111111111111111111111111)

(define immediate-mask      #b1111)
(define fixnum-mask            #b1)
(define boolean-mask     #b1111111)
(define char-mask        #b11111111) ; mask for tag only

(define fixnum-shift    1)
(define boolean-shift   7)
(define char-shift      8)

(define most-positive-fixnum (- (expt 2 29) 1)) ; 30-bit fixnum
(define most-negative-fixnum (- (expt 2 29)))   ; 30-bit fixnum

(define (fixnum? x)
  (and (number? x)
       (integer? x)
       (<= most-negative-fixnum x most-positive-fixnum)))

(define (immediate? x)
  (or (fixnum? x) (char? x) (boolean? x) (null? x) (void? x)))

(define the-undefined-value (gensym 'undefined))
(define (undefined)    the-undefined-value)
(define (undefined? x) (eq? x the-undefined-value))

(define (immediate-rep x)
  (when (flonum? x)
    (displayln (list 'immediate-rep x)))
  (define (shift x m) (arithmetic-shift x m))
  (cond
    [(fixnum?    x)              (shift x                 fixnum-shift)]  ; the tag is 0
    [(boolean?   x) (bitwise-ior (shift (if x 1 0)        boolean-shift)   boolean-tag)]
    [(char?      x) (bitwise-ior (shift (char->integer x) char-shift)      char-tag)]
    [(null?      x) empty-value]
    [(void?      x) void-value]
    [(undefined? x) undefined-value]
    [else          (error 'immediate-rep "expected immediate value, got: ~a" x)]))
