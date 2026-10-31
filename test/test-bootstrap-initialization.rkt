#lang webracket/base

(require (only-in racket/math nan?))

;; Exercise interning, constant materialization, provider exports, top-level
;; bindings and application locals after the generated bootstrap has run.
(define token 'bootstrap-token)
(define text "bootstrap-token")
(define constants (vector token text #"bootstrap-bytes" +nan.0))

;; bootstrap-ready? : -> boolean?
;;   Check that generated initialization completed before application work.
(define (bootstrap-ready?)
  (define local-text (string-append text ""))
  (and (eq? (vector-ref constants 0) (string->symbol local-text))
       (equal? (vector-ref constants 1) local-text)
       (equal? (vector-ref constants 2) #"bootstrap-bytes")
       (nan? (vector-ref constants 3))))

(unless (bootstrap-ready?)
  (error 'bootstrap-initialization "initialization order was not preserved"))

(displayln "bootstrap-initialization-ok")
