#lang webracket

(define arity1 (arity-at-least 2))
(define arity2 (make-arity-at-least 2))

(unless (and (arity-at-least? arity1)
             (arity-at-least? arity2)
             (equal? (arity-at-least-value arity1) 2)
             (equal? (arity-at-least-value arity2) 2)
             (equal? arity1 arity2))
  (error 'kernel-arity-at-least "failed"))
