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
  $attr
  $has-attr?
  $class-list
  $has-class?
  $add-class!
  $remove-class!
  $find
  $children
  $parent
  $closest
  $next
  $prev
  $siblings
  $selection->vector
  $vector
  $list
  $map
  $for-each
  $filter
  $where
  $not
  $last
  $empty?
  .map
  .for-each
  .filter
  .where
  .not
  .first
  .attr
  .has-attr?
  .class-list
  .has-class?
  .add-class!
  .remove-class!
  .find
  .children
  .parent
  .closest
  .next
  .prev
  .siblings
  .vector
  .list
  .last
  .empty?
  .text
  $chain
  (for-label (all-defined-out)))

(struct $selection (elements) #:transparent)

(define $ any/c)
(define $select any/c)
(define $length any/c)
(define $ref any/c)
(define $first any/c)
(define $attr any/c)
(define $has-attr? any/c)
(define $class-list any/c)
(define $has-class? any/c)
(define $add-class! any/c)
(define $remove-class! any/c)
(define $find any/c)
(define $children any/c)
(define $parent any/c)
(define $closest any/c)
(define $next any/c)
(define $prev any/c)
(define $siblings any/c)
(define $selection->vector any/c)
(define $vector any/c)
(define $list any/c)
(define $map any/c)
(define $for-each any/c)
(define $filter any/c)
(define $where any/c)
(define $not any/c)
(define $last any/c)
(define $empty? any/c)
(define .map any/c)
(define .for-each any/c)
(define .filter any/c)
(define .where any/c)
(define .not any/c)
(define .first any/c)
(define .attr any/c)
(define .has-attr? any/c)
(define .class-list any/c)
(define .has-class? any/c)
(define .add-class! any/c)
(define .remove-class! any/c)
(define .find any/c)
(define .children any/c)
(define .parent any/c)
(define .closest any/c)
(define .next any/c)
(define .prev any/c)
(define .siblings any/c)
(define .vector any/c)
(define .list any/c)
(define .last any/c)
(define .empty? any/c)
(define .text any/c)
(define $chain any/c)
