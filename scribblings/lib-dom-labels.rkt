#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the dom facade Scribble page.

(provide
  dom
  (for-label (all-defined-out)))

(define dom any/c)
