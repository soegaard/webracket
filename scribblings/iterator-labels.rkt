#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the Iterator Scribble pages.

(provide
  iterator
  iterator-from
  iterator-next
  js-Iterator
  (for-label (all-defined-out)))

(define iterator any/c)
(define iterator-from any/c)
(define iterator-next any/c)
(define js-Iterator any/c)
