#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the public storage wrapper library.

(provide
  storage
  storage-raw
  storage?
  local-storage
  session-storage
  storage-length
  storage-key
  storage-get-item
  storage-set-item!
  storage-remove-item!
  storage-clear!
  (for-label (all-defined-out)))

(define storage any/c)
(define storage-raw any/c)
(define storage? any/c)
(define local-storage any/c)
(define session-storage any/c)
(define storage-length any/c)
(define storage-key any/c)
(define storage-get-item any/c)
(define storage-set-item! any/c)
(define storage-remove-item! any/c)
(define storage-clear! any/c)
