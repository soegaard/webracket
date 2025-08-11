#lang racket
;; This module reports information on primitives using reflection on
;; Racket builtins.

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
  (namespace-require 'rnrs/arithmetic/fixnums-6) ; fxzero?
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
    make-void
    
    set-boxed!          ; assignment boxes
    boxed
    unboxed

    s-exp->fasl
    fasl->s-exp

    alt-reverse         ; in expansion of for/list

    raise-unbound-variable-reference ; used for unbound variables outside modules
    js-log
    js-document-body
    js-create-text-node
    js-append-child!
    ))

(define (primitive->description sym-or-primitive)
  (cond
    [(and (symbol? sym-or-primitive)
          (member sym-or-primitive not-primitives-in-racket))
     #f]
    [else
     (define x (if (primitive? sym-or-primitive)
                   sym-or-primitive
                   (eval sym-or-primitive ns)))
     
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
