
; from "/racket/private/qq-and-or.rkt"

(define-values (qq-append)
  (lambda (a b)
    (if (list? a)
        (append a b)
        (raise-argument-error 'unquote-splicing "list?" a))))
