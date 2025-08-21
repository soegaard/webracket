#lang racket/base
(require "fully.rkt"
         "primitives.rkt")

(provide (all-from-out "fully.rkt"
                       "primitives.rkt")
         ; core
         (rename-out [#%plain-module-begin #%module-begin]
                     [#%plain-app          #%app]
                     [lambda lambda] ; [#%plain-lambda       lambda]
                     [#%plain-lambda       λ]))


(require (for-syntax (only-in racket/base
                              syntax-case
                              syntax/loc
                              identifier?
                              #%app
                              syntax)))

(define-syntax (web-lambda stx)
  (syntax-case stx ()
    [(_lambda formals body0 body ...)
     (syntax/loc stx
       (#%plain-lambda formals body0 body ...))]))

(define-syntax (web-λ stx)
  (syntax-case stx ()
    [(_lambda formals body0 body ...)
     (syntax/loc stx
       (#%plain-lambda formals body0 body ...))]))

;; Constants


(provide null undefined empty true false pi eof)
(require (only-in racket/base       null eof)
         (only-in racket/undefined  undefined)
         (only-in racket/bool       true false)
         (only-in racket/list       empty)
         (only-in racket/math       pi))


;; Note: When bootstrapping we need implementations
;;       for these.

;; 3.3 Literals: quote and #%datum
(require (only-in racket/base #%datum))
(provide #%datum)

;; 3.4 Expression Wrapper: #%expression
(require (only-in racket/base #%expression))
(provide #%expression)

;; 3.5 Variable References and #%top
(require (only-in racket/base #%top))
(provide #%top)

;; 3.6 Locations: #%variable-reference
(require (only-in racket/base #%variable-reference))
(provide #%variable-reference)

;; 3.7 Procedure Applications and #%app
;; Only #%plain-app for now.
;; (require (only-in racket/base #%app))
;; (provide #%app)

;; 3.8 Procedure Expressions: lambda, case-lambda
;; (require (only-in racket/base case-lambda))
;; (provide case-lambda)

;; 3.9 Local binding: let, let*, letrec, ...
(require (only-in racket/base let let* letrec
                  let-values let*-values letrec-values))
(provide let let* letrec
         let-values let*-values letrec-values)

;; 3.10 Local Definitions: local
(require (only-in racket/local local))
(provide local)

;; 3.11 Constructing Graphs: shared
;; (require (only-in racket/shared shared))
;; (provide shared)


;; 3.12 Conditionals: cond, or, and
(require (only-in racket/base cond else => or and))
(provide cond else => or and)

;; 3.13. Dispatch: case
(require (only-in racket/base case))
(provide case)

;; 3.14. Definitions: define
; (require (only-in racket/base define))
(provide define define-values)
; define-values provided from `fully`

;; 3.15 Sequencing: begin, begin0
;;   Provided form `fully`.

;; 3.16 Guard Evaluation: when, unless
(require (only-in racket/base when unless))
(provide when unless)

;; 3.17 Assignment: set!, set!-values
(require (only-in racket/base set!-values))
(provide set!-values)
; Note: set!-values expands into let-values + set!


;; 3.18 Iterations and comprehensions
(require (only-in racket/base
                  for for/list for/vector
                  in-list in-vector
                  in-range in-naturals
                  ))
(provide for for/list for/vector
         in-list in-vector
         in-range in-naturals)

;; 3.19 Continuations marks
;; (require (only-in racket/base with-continuation-mark))
;; (provide with-continuation-mark)

;; 3.20 Quasiquoting
(require (only-in racket/base quasiquote unquote unquote-splicing))
(provide quasiquote unquote unquote-splicing)


;; 3.22 Interaction Wrapper
(require (only-in racket/base #%top-interaction))
(provide #%top-interaction)

;; 3.23 Blocks
(require (only-in racket/block block))
(provide block)

;; 4.7.1 Additional Symbol Functions
(require (only-in racket/symbol symbol->immutable-string))
(provide symbol->immutable-string)

;; 4.9.1 Additional Keyword Functions
(require (only-in racket/keyword keyword->immutable-string))
(provide keyword->immutable-string)

;; 4.10
(require (only-in racket/base
                  null))
(provide null)


;; 13.10

(require (prefix-in rkt: racket/fasl))

; We simplify s-exp->fasl here in order to avoid keyword functions

(define (s-exp->fasl v [out #f]) (rkt:s-exp->fasl v out))

(define (fasl->s-exp bs)
  (unless (bytes? bs)
    (raise-argument-error 'fasl->s-exp "bytes?" bs))
  (define in (open-input-bytes bs))
  (define v (rkt:fasl->s-exp in))
  (close-input-port in)
  v)

(provide s-exp->fasl fasl->s-exp)

;; 14. 10

(provide namespace-variable-value-simple)

(define (namespace-variable-value-simple ns sym)
  (namespace-variable-value-simple sym #t #f ns))




