#lang racket/base

(require racket/contract/base)

(provide
  sxml->html
  (for-label (all-defined-out)))

(define sxml->html any/c)
