#lang racket/base

(require racket/contract/base)

(provide
  (struct-out dom-token-list)
  (struct-out shadow-root)
  (struct-out node-list)
  (struct-out html-collection)
  (struct-out animation)
  (struct-out computed-style-map)
  element-has-attributes?
  element-query-selector-all
  element-set-outer-html!
  element-shadow-root
  dom-token-list-value
  dom-token-list-length
  dom-token-list-item
  dom-token-list-contains?
  dom-token-list-add!
  dom-token-list-remove!
  dom-token-list-toggle!
  dom-token-list-replace!
  node-list-length
  node-list-item
  html-collection-length
  html-collection-item
  html-collection-named-item
  shadow-root-host
  shadow-root-mode
  shadow-root-delegates-focus?)

(struct dom-token-list (raw) #:transparent)
(struct shadow-root (raw) #:transparent)
(struct node-list (raw) #:transparent)
(struct html-collection (raw) #:transparent)
(struct animation (raw) #:transparent)
(struct computed-style-map (raw) #:transparent)

(define element-has-attributes? any/c)
(define element-query-selector-all any/c)
(define element-set-outer-html! any/c)
(define element-shadow-root any/c)
(define dom-token-list-value any/c)
(define dom-token-list-length any/c)
(define dom-token-list-item any/c)
(define dom-token-list-contains? any/c)
(define dom-token-list-add! any/c)
(define dom-token-list-remove! any/c)
(define dom-token-list-toggle! any/c)
(define dom-token-list-replace! any/c)
(define node-list-length any/c)
(define node-list-item any/c)
(define html-collection-length any/c)
(define html-collection-item any/c)
(define html-collection-named-item any/c)
(define shadow-root-host any/c)
(define shadow-root-mode any/c)
(define shadow-root-delegates-focus? any/c)
