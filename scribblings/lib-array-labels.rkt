#lang racket/base

(require racket/contract/base)

(provide
  (struct-out array)
  array-from
  array-from-async
  array-of
  array-length
  array-ref
  array-at
  array->vector
  array->list
  vector->array
  list->array
  sequence->array
  array-join
  array-slice
  array-concat
  array-copy-within!
  array-fill!
  array-push!
  array-pop
  array-reverse!
  array-shift
  array-sort!
  array-splice!
  array-to-locale-string
  array-to-string
  array-unshift!
  array-flat
  array-to-reversed
  array-to-sorted
  array-to-spliced
  array-with
  (for-label (all-defined-out)))

(struct array (raw) #:transparent)

(define array-from any/c)
(define array-from-async any/c)
(define array-of any/c)
(define array-length any/c)
(define array-ref any/c)
(define array-at any/c)
(define array->vector any/c)
(define array->list any/c)
(define vector->array any/c)
(define list->array any/c)
(define sequence->array any/c)
(define array-join any/c)
(define array-slice any/c)
(define array-concat any/c)
(define array-copy-within! any/c)
(define array-fill! any/c)
(define array-push! any/c)
(define array-pop any/c)
(define array-reverse! any/c)
(define array-shift any/c)
(define array-sort! any/c)
(define array-splice! any/c)
(define array-to-locale-string any/c)
(define array-to-string any/c)
(define array-unshift! any/c)
(define array-flat any/c)
(define array-to-reversed any/c)
(define array-to-sorted any/c)
(define array-to-spliced any/c)
(define array-with any/c)
