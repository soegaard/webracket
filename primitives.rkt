#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/fasl
         racket/symbol
         racket/list
         racket/string
         (only-in racket/bool symbol=?)
         (only-in racket/keyword keyword->immutable-string))

;; The primitives are 

(provide
 ;; 4.1 Equality
 equal?
 ; equal-always?
 eqv?
 eq?
 ; equal?/recur
 ; equal-always?/recur

 eq-hash-code
 eqv-hash-code
 equal-hash-code

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
 number?


 ;; 4.4 Strings
 string?
 string-set!
 string-length
 string-ref
 string-append
 string-append-immutable
 string-copy
 string-fill!
 string-copy!
 string->immutable-string
 string-ci=?
 build-string 
 string-prefix?
 string-suffix?
 string-contains?
 string-find

 ;; 4.5 Byte Strings

 ;; 4.6 Characters
 char-downcase
 char-foldcase
 char-titlecase
 char-upcase

 ;; 4.7 Symbols
 symbol?
 symbol-interned?
 ; symbol-unreadable?
 symbol->string
 string->symbol
 string->uninterned-symbol
 ; gensym
 symbol<?

 symbol=?  ; from racket/bool

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

 ;; 4.20 Procedures
 procedure?
 
 ;; 10.1 Multiple Values
 values
 call-with-values

 ;; 10.2 Exceptions
 raise-argument-error
 
 ;; 13.5 Writing
 display
 displayln
 newline
 
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
 fxmin fxmax

 fx->fl
 fl->fx

 inexact->exact round sqrt flround flsin flcos fltan flsqrt

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
  
