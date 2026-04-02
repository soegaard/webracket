#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the define Scribble page.

(provide
  define/key
  call/key
  (for-label (all-defined-out)))

(define define/key any/c)
(define call/key any/c)
