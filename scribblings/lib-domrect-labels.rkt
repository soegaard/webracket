#lang racket/base

(require racket/contract/base)

(provide
  (struct-out dom-rect)
  (struct-out dom-rect-list)
  dom-rect-left
  dom-rect-top
  dom-rect-width
  dom-rect-height
  dom-rect-list-length
  dom-rect-list-item
  (for-label (all-defined-out)))

(struct dom-rect (raw) #:transparent)
(struct dom-rect-list (raw) #:transparent)

(define dom-rect-left any/c)
(define dom-rect-top any/c)
(define dom-rect-width any/c)
(define dom-rect-height any/c)
(define dom-rect-list-length any/c)
(define dom-rect-list-item any/c)
