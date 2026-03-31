;; See racket/private/ellipses.rkt

(require (for-syntax racket/base))
(define-syntaxes (_)
  (lambda (stx)
    (raise-syntax-error #f "wildcard not allowed as an expression" stx)))
