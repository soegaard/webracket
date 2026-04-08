#lang webracket

;;;
;;; Query wrappers
;;;

(include-lib document)
(include-lib element)
(require-lib query-chain)

;; safe-list-ref : (listof any/c) exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the list element at i when it is in range, otherwise #f.
(define (safe-list-ref xs i)
  (define n (length xs))
  (if (and (number? i) (exact? i) (integer? i)
           (<= 0 i) (< i n))
      (list-ref xs i)
      #f))

;; safe-vector-ref : vector? exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the vector element at i when it is in range, otherwise #f.
(define (safe-vector-ref xs i)
  (define n (vector-length xs))
  (if (and (number? i) (exact? i) (integer? i)
           (<= 0 i) (< i n))
      (vector-ref xs i)
      #f))

;; $selection : (struct (elements)) -> selection?
;;   Represent a collection of DOM elements.
(struct $selection (elements) #:transparent)

;; check-$selection : symbol? any/c -> void?
;;   Ensure x is a selection wrapper.
(define (check-$selection who x)
  (unless ($selection? x)
    (raise-argument-error who "$selection?" x)))

;; list->$selection : (listof any/c) -> $selection?
;;   Pack a list into a selection wrapper.
(define (list->$selection xs)
  ($selection (list->vector xs)))

;; vector->$selection : vector? -> $selection?
;;   Copy a vector into a selection wrapper.
(define (vector->$selection xs)
  ($selection (vector-copy xs)))

;; make-$selection : (or/c list? vector? dom-node-list?) -> $selection?
;;   Normalize supported selection sources into a wrapper.
(define (make-$selection xs)
  (cond
    [(dom-node-list? xs) ($selection xs)]
    [(vector? xs)    (vector->$selection xs)]
    [(list? xs)      (list->$selection xs)]
    [else
     (raise-argument-error
      'make-$selection
      "(or/c dom-node-list? vector? list?)"
      xs)]))

;; $length : $selection? -> exact-nonnegative-integer?
;;   Return the number of wrapped elements.
(define ($length sel)
  (check-$selection '$length sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    (vector-length xs)]
    [(dom-node-list? xs) (node-list-length xs)]
    [else
     (error '$length
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $ref : $selection? exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the selected element at i, or #f when i is out of range.
(define ($ref sel i)
  (check-$selection '$ref sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    (safe-vector-ref xs i)]
    [(dom-node-list? xs)
     (define node (node-list-item xs i))
     (and node (element-wrap (dom-node-raw node)))]
    [else
     (error '$ref
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $first : $selection? -> (or/c #f any/c)
;;   Return the first selected element, if any.
(define ($first sel)
  (check-$selection '$first sel)
  ($ref sel 0))

;; $selection->vector : $selection? -> vector?
;;   Convert a selection wrapper into a vector of raw elements.
(define ($selection->vector sel)
  (check-$selection '$selection->vector sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    xs]
    [(dom-node-list? xs)
     (define n (node-list-length xs))
     (for/vector #:length n ([i (in-range n)])
       ($ref sel i))]
    [else
     (error '$selection->vector
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $for-each : (any/c -> any/c) $selection? -> $selection?
;;   Apply f to each wrapped element and return the original selection.
(define ($for-each f sel)
  (check-$selection '$for-each sel)
  (for ([x (in-vector ($selection->vector sel))])
    (f x))
  sel)

;; $map : (any/c -> any/c) $selection? -> $selection?
;;   Map f over the wrapped elements and return a new selection.
(define ($map f sel)
  (check-$selection '$map sel)
  (define n ($length sel))
  (vector->$selection
   (for/vector #:length n
               ([x (in-vector ($selection->vector sel))])
     (f x))))

;; .map : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $map.
(define (.map sel f)
  ($map f sel))

;; .for-each : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $for-each.
(define (.for-each sel f)
  ($for-each f sel))

;; $select : (or/c string? symbol?) -> $selection?
;;   Query the document for matching elements.
(define ($select s)
  (unless (string? s)
    (raise-argument-error '$select "string?" s))
  (define nl (document-query-selector-all s))
  (make-$selection (or nl '())))

;; $ : (or/c string? symbol?) -> $selection?
;;   Public jQuery-style selector alias.
(define ($ s)
  ($select s))

;; .text : $selection? -> string?
;;   Read the text content of the first selected element.
(define (.text sel)
  (check-$selection 'text sel)
  (define x ($first sel))
  (or (and x (element-text-content x))
      ""))
