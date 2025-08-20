#lang racket/base
(provide
 (rename-out [my-read read]
             [my-read-syntax read-syntax]
             [my-get-info get-info]))

(define (my-read-syntax [source-name (object-name (current-input-port))]
                        [in          (current-input-port)])
  (read-syntax source-name in))


; The procedure `read` returns the same as `read-syntax`, except as a datum.
(define (my-read [in (current-input-port)])
  (syntax->datum (my-read-syntax #f in)))


; The procedure `get-info` is in general used by external
; tools to retrieve information about a program.
; Here we use `get-info` to tell editors which lexer it should use to
; syntax color programs.
(define (my-get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)]
      [else default])))
