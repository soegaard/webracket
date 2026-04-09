#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for query Scribble links.
;;
;; These bindings exist only for for-label use so examples and API
;; entries can link to the documented query identifiers.

(provide
  (struct-out $selection)
  $
  $select
  $length
  $ref
  $first
  $selection->vector
  $map
  $for-each
  .map
  .for-each
  .text
  chain
  (for-label (all-defined-out)))

(struct $selection (elements) #:transparent)

(define $ any/c)
(define $select any/c)
(define $length any/c)
(define $ref any/c)
(define $first any/c)
(define $selection->vector any/c)
(define $map any/c)
(define $for-each any/c)
(define .map any/c)
(define .for-each any/c)
(define .text any/c)
(define chain any/c)
