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
  (namespace-require 'racket/symbol)
  (namespace-require 'racket/string)
  (namespace-require 'racket/keyword)            ; keyword->immutable-string
  (namespace-require 'rnrs/arithmetic/fixnums-6) ; fxzero?
  (namespace-require 'math/base)
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
    string-replace      ; srfi 13
    make-void
    
    set-boxed!          ; assignment boxes
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
    ))


(require "parameters.rkt"
         "define-foreign.rkt")

(define ffi-primitives
  ; cache informations
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


(define (primitive->description sym-or-primitive)
  (cond
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

     (primitive-description (object-name x)
                            (procedure-arity x)                         
                            (if (primitive? x)
                                (primitive-result-arity x)
                                (procedure-result-arity x))
                            required
                            accepted
                            (primitive? x)
                            (primitive-closure? x)
                            (procedure-realm x))]))
