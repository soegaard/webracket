#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the public indexed-db wrapper library.

(provide
  indexed-db
  indexed-db-raw
  indexed-db?
  indexed-db-open
  indexed-db-delete-database!
  indexed-db-cmp
  indexed-db-databases
  (for-label (all-defined-out)))

(define indexed-db any/c)
(define indexed-db-raw any/c)
(define indexed-db? any/c)
(define indexed-db-open any/c)
(define indexed-db-delete-database! any/c)
(define indexed-db-cmp any/c)
(define indexed-db-databases any/c)
