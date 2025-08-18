#lang racket/base
(provide topexpand)


(define (make-webracket-namespace)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'webracket)
    (namespace-require 'webracket/core))  
  ns)

; Notes:
;  Disabling `eval-jit-enabled` affects the expansion.
;  Unless disabled, `reverse` becomes `alt-reverse`.
(define (topexpand top-level-form-stx)
  (parameterize ([current-namespace (make-webracket-namespace)]
                 [eval-jit-enabled  #f])    
    ; (namespace-require 'racket/private/struct) 
    (namespace-require 'racket/match) 
    ; (namespace-require 'racket/symbol)
    (expand top-level-form-stx)))
