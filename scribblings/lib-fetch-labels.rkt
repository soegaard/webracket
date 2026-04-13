#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the public fetch wrapper library.

(provide
  fetch
  fetch-headers
  fetch-headers-raw
  fetch-headers?
  fetch-request
  fetch-request-raw
  fetch-request?
  fetch-response
  fetch-response-raw
  fetch-response?
  make-fetch-headers
  make-fetch-request
  fetch-request-method
  fetch-request-url
  fetch-request-body-used?
  fetch-request-headers
  fetch-request-clone
  fetch-response-ok?
  fetch-response-status
  fetch-response-status-text
  fetch-response-url
  fetch-response-type
  fetch-response-body-used?
  fetch-response-headers
  fetch-response-clone
  fetch-headers-get
  fetch-headers-has?
  fetch-headers-set!
  fetch-headers-append!
  fetch-headers-delete!
  fetch-headers-for-each
  (for-label (all-defined-out)))

(define fetch any/c)
(define fetch-headers any/c)
(define fetch-headers-raw any/c)
(define fetch-headers? any/c)
(define fetch-request any/c)
(define fetch-request-raw any/c)
(define fetch-request? any/c)
(define fetch-response any/c)
(define fetch-response-raw any/c)
(define fetch-response? any/c)
(define make-fetch-headers any/c)
(define make-fetch-request any/c)
(define fetch-request-method any/c)
(define fetch-request-url any/c)
(define fetch-request-body-used? any/c)
(define fetch-request-headers any/c)
(define fetch-request-clone any/c)
(define fetch-response-ok? any/c)
(define fetch-response-status any/c)
(define fetch-response-status-text any/c)
(define fetch-response-url any/c)
(define fetch-response-type any/c)
(define fetch-response-body-used? any/c)
(define fetch-response-headers any/c)
(define fetch-response-clone any/c)
(define fetch-headers-get any/c)
(define fetch-headers-has? any/c)
(define fetch-headers-set! any/c)
(define fetch-headers-append! any/c)
(define fetch-headers-delete! any/c)
(define fetch-headers-for-each any/c)
