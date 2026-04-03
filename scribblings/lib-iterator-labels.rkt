#lang racket/base

(require racket/contract/base)

(provide
  (struct-out iterator)
  (struct-out iterator-zip-options)
  Iterator
  iterator-prototype
  iterator-prototype-constructor
  iterator-prototype-to-string-tag
  iterable?
  iterator-zip-options-mode
  iterator-zip-options-padding
  make-iterator-zip-options
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
  iterator->vector
  js-Iterator
  (for-label (all-defined-out)))

(struct iterator (raw) #:transparent #:constructor-name make-iterator)
(struct iterator-zip-options (mode padding)
  #:transparent
  #:constructor-name make-iterator-zip-options)

(define Iterator any/c)
(define iterator-prototype any/c)
(define iterator-prototype-constructor any/c)
(define iterator-prototype-to-string-tag any/c)
(define iterable? any/c)
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
(define iterator->vector vector?)
(define js-Iterator any/c)
