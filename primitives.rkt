#lang racket/base
(require racket/fixnum
         racket/fasl
         racket/symbol
         (only-in racket/bool symbol=?))

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
 ; immutable?

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
 ; symbol->immutable-string  ; from racket/keyword
 
 
 ;; 4.10 Pairs and Lists
 pair?
 null?
 cons
 car
 cdr
 list?
 list
 list*
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

 ; filter
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

 ;; 14.1 Namespaces

 namespace?
 make-empty-namespace
 ; namespace-variable-value
 namespace-set-variable-value!
 namespace-undefine-variable!
 ; namespace-has-key?

;;

 s-exp->fasl
 fasl->s-exp
 js_log

)

(define (js_log v)
  (displayln v))

         
         
