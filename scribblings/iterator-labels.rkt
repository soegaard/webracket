#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the Iterator Scribble page.

(provide
  iterator
  iterator-prototype
  iterator-prototype-constructor
  iterator-prototype-to-string-tag
  iterator-from
  iterator-concat
  iterator-zip
  iterator-zip-keyed
  iterator-next
  iterator-return
  iterator-dispose!
  iterator-symbol-iterator
  iterator-drop
  iterator-every
  iterator-filter
  iterator-find
  iterator-flat-map
  iterator-for-each
  iterator-map
  iterator-reduce
  iterator-some
  iterator-take
  iterator-to-array
  js-Iterator
  (for-label (all-defined-out)))

(define iterator any/c)
(define iterator-prototype any/c)
(define iterator-prototype-constructor any/c)
(define iterator-prototype-to-string-tag any/c)
(define iterator-from any/c)
(define iterator-concat any/c)
(define iterator-zip any/c)
(define iterator-zip-keyed any/c)
(define iterator-next any/c)
(define iterator-return any/c)
(define iterator-dispose! any/c)
(define iterator-symbol-iterator any/c)
(define iterator-drop any/c)
(define iterator-every any/c)
(define iterator-filter any/c)
(define iterator-find any/c)
(define iterator-flat-map any/c)
(define iterator-for-each any/c)
(define iterator-map any/c)
(define iterator-reduce any/c)
(define iterator-some any/c)
(define iterator-take any/c)
(define iterator-to-array any/c)
(define js-Iterator any/c)
