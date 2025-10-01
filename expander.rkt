#lang racket/base
(provide topexpand)

(require syntax/toplevel)

(define (make-webracket-namespace)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'webracket)
    (namespace-require 'webracket/core))
    #;(namespace-require '(for-syntax webracket/core))
  ns)

; Notes:
;  Disabling `eval-jit-enabled` affects the expansion.
;  Unless disabled, `reverse` becomes `alt-reverse`.
(define (topexpand top-level-form-stx)
  (parameterize ([current-namespace (make-webracket-namespace)]
                 [eval-jit-enabled  #f])
    ; (namespace-require 'racket/private/struct) 
    ; (namespace-require 'racket/match)
    ; (namespace-require 'racket/symbol)

    ; Since `current-namespace` is set to `(make-webracket-namespace)`
    ; the `webracket` module is available.
    (expand-top-level-with-compile-time-evals top-level-form-stx)
    #;(expand top-level-form-stx)
    ))


;; Notes

; We are using `expand-top-level-with-compile-time-evals` instead of
; plain `expand`.
;
; Compare the expansion of:
;     (let () (struct foo (x y)) (foo 11 22))
; to  (begin  (struct foo (x y)) (foo 11 22)).
;
; With `expand` the call `(foo 11 22)` expands to `(#%app (#%top . pos) '11 '22)`.
; However, the name `pos` it not bound to the structure constructor.
; Instead it is bound to syntax that will expand into the constructor (a little simplified).
;
; With `expand-top-level-with-compile-time-evals` the call expands into a
; a call to (renamed) a pos bound to the proper constructor.


