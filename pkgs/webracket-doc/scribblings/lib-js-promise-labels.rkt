#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the public js/promise wrapper library.

(provide
  (struct-out js-promise)
  js-promise-wrap
  js-promise-unwrap
  js-promise-thenable?
  js-promise-map-fulfilled
  js-promise-resolved
  js-promise-rejected
  js-promise-then
  js-promise-catch
  js-promise-finally
  (for-label (all-defined-out)))

(struct js-promise (raw) #:transparent)

(define js-promise-wrap any/c)
(define js-promise-unwrap any/c)
(define js-promise-thenable? any/c)
(define js-promise-map-fulfilled any/c)
(define js-promise-resolved any/c)
(define js-promise-rejected any/c)
(define js-promise-then any/c)
(define js-promise-catch any/c)
(define js-promise-finally any/c)
