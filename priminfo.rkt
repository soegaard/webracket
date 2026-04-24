#lang racket
;; This module reports information on primitives using reflection on
;; Racket builtins.
(require racket/runtime-path)

(provide primitive->description
         (struct-out primitive-description))

(struct primitive-description
  (name
   arity
   result-arity
   required-keywords
   accepted-keywords
   primitive?
   primitive-closure?
   realm)
  #:transparent)


(define ns (make-base-namespace))
(parameterize ((current-namespace ns))
  (namespace-require 'racket)
  (namespace-require 'racket/unsafe/ops)
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/fixnum)
  (namespace-require 'racket/fasl)
  (namespace-require 'racket/mpair)  
  (namespace-require 'racket/symbol)
  (namespace-require 'racket/string)
  (namespace-require 'racket/bytes)
  (namespace-require 'racket/keyword)            ; keyword->immutable-string
  (namespace-require 'rnrs/arithmetic/fixnums-6) ; fxzero?
  (namespace-require 'math/base)
  #;(namespace-require 'racket/kernel)
  #;(namespace-require 'racket/base)
  #;(namespace-require 'racket/fixnum)
  #;(namespace-require 'racket/bool)       ; symbol=?
  #;(namespace-require 'racket/vector)
  )


(define not-primitives-in-racket
  '(make-empty-hasheq
    string-trim-left
    string-trim-right
    string-drop         ; srfi 13
    string-drop-right   ; srfi 13
    string-take         ; srfi 13
    string-take-right   ; srfi 13
    ; string-replace    ; 
    make-void
    
    set-boxed!          ; assignment boxes
    initialize-boxed!   ; letrec initialization boxes
    boxed
    unboxed

    namespace-variable-value-simple
    
    s-exp->fasl
    fasl->s-exp

    alt-reverse         ; in expansion of for/list
    
    raise-unbound-variable-reference ; used for unbound variables outside modules
    js-log
    procedure->external
    external-number->flonum
    external-string->string
    inclusive-range

    ;; check-list
    ;; check-mlist
    ;; check-range
    ;; check-range-generic
    ;; check-naturals
    ))

;; Arity overrides for primitives where Racket's reflected arity
;; does not match WebRacket's documented behavior.
;; Example: vector-member gained an optional comparator in Racket 8.15.0.1,
;; but older Rackets report arity 2 while docs show (2 3).
(define arity-overrides
  (hash 'vector-member '(2 3)))


;; These WebRacket primitives are not primitives in full Racket,
;; so introspection can not be used to find the arities.
(define js-log-description
  (primitive-description 'js-log
                         1
                         1
                         '()
                         '()
                         #f
                         #f
                         #f))

(define make-void-description
  (primitive-description 'make-void
                         0
                         1
                         '()
                         '()
                         #f
                         #f
                         #f))

(define (arity-at-least-description name)
  (primitive-description name
                         1
                         1
                         '()
                         '()
                         #t
                         #f
                         'racket/primitive))

(define procedure->external-description
  (primitive-description 'procedure->external
                         1
                         1
                         '()
                         '()
                         #f
                         #f
                         #f))

(define external-number->flonum-description
  (primitive-description 'external-number->flonum
                         1
                         1
                         '()
                         '()
                         #f
                         #f
                         #f))

(define external-string->string-description
  (primitive-description 'external-string->string
                         1
                         1
                         '()
                         '()
                         #f
                         #f
                         #f))


(require "parameters.rkt"
         "define-foreign.rkt")

(define ffi-primitives
  ; cache 
  (let ([syms   '()]
        [old-fs #f])
    (λ ()
      (define fs (current-ffi-foreigns))    
      (cond
        [(eq? fs old-fs) syms]
        [else            (set! old-fs fs)
                         (set! syms (foreigns->primitive-names fs))
                         syms]))))


(require (prefix-in primitives: "primitives.rkt")) ; avoid clashes, instantiates
(define-runtime-path primitives.rkt "primitives.rkt")
(define primitives-ns (module->namespace primitives.rkt))

;; Struct constructors are bound to syntax, so we need to get the
;; primitive first, before using reflection go the the description.

(define exception-constructors
  '(exn
    exn:fail
    exn:fail:contract
    exn:fail:contract:arity
    exn:fail:contract:divide-by-zero
    exn:fail:contract:non-fixnum-result
    exn:fail:contract:variable
    exn:fail:read
    exn:fail:read:eof
    exn:fail:read:non-char
    exn:fail:syntax
    exn:fail:syntax:missing-module
    exn:fail:syntax:unbound
    ))

(define (exception-constructor? sym)  
  (and (member sym exception-constructors) #t))


(define (exception-constructor->primitive sym)
  (eval sym ns))

(define (primitive->description sym-or-primitive)
  (cond
    [(eq? sym-or-primitive 'js-log)                  js-log-description]
    [(eq? sym-or-primitive 'make-void)               make-void-description]
    [(eq? sym-or-primitive 'arity-at-least)          (arity-at-least-description 'arity-at-least)]
    [(eq? sym-or-primitive 'make-arity-at-least)     (arity-at-least-description 'make-arity-at-least)]
    [(eq? sym-or-primitive 'procedure->external)     procedure->external-description]
    [(eq? sym-or-primitive 'external-number->flonum) external-number->flonum-description]
    [(eq? sym-or-primitive 'external-string->string) external-string->string-description]
    [(exception-constructor? sym-or-primitive)
     (define pr (exception-constructor->primitive sym-or-primitive))
     (define x pr)
     (define-values (required accepted) (procedure-keywords x))
     (define desc
       (primitive-description (object-name x)
                              (procedure-arity x)
                              (if (primitive? x)
                                  (primitive-result-arity x)
                                  (procedure-result-arity x))
                              required
                              accepted
                              (primitive? x)
                              (primitive-closure? x)
                              (procedure-realm x)))
     (if (and (symbol? sym-or-primitive) (hash-has-key? arity-overrides sym-or-primitive))
         (struct-copy primitive-description desc
                      [arity (hash-ref arity-overrides sym-or-primitive)])
         desc)]
    [(and (symbol? sym-or-primitive)
          (or (member sym-or-primitive not-primitives-in-racket)
              (member sym-or-primitive (ffi-primitives))))
     #f]
    [else
     #;(define x (if (primitive? sym-or-primitive)
                   sym-or-primitive
                   (eval sym-or-primitive ns)))

     (define x (namespace-variable-value sym-or-primitive
                                         #t ; use-mapping?
                                         #f ; failure-thunk
                                         primitives-ns))
     
     (unless (procedure? x)
       (error 'primitive->description "got: ~a" sym-or-primitive))

     (define-values (required accepted) (procedure-keywords x))

     (define desc
       (primitive-description (object-name x)
                              (procedure-arity x)
                              (if (primitive? x)
                                  (primitive-result-arity x)
                                  (procedure-result-arity x))
                              required
                              accepted
                              (primitive? x)
                              (primitive-closure? x)
                              (procedure-realm x)))
     (if (and (symbol? sym-or-primitive) (hash-has-key? arity-overrides sym-or-primitive))
         (struct-copy primitive-description desc
                      [arity (hash-ref arity-overrides sym-or-primitive)])
         desc)]))
