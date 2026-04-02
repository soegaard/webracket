#lang webracket

;;;
;;; Shared DOM structs.
;;;

;; element : external/raw -> element?
;;   Wrap a browser Element object.
(struct element (raw) #:transparent)

;; element-wrap : any/c -> any/c
;;   Wrap a raw browser Element object, leaving wrapped values alone.
(define (element-wrap value)
  (if (element? value)
      value
      (element value)))

;; element-unwrap : any/c -> any/c
;;   Unwrap an element struct to its raw browser object.
(define (element-unwrap value)
  (if (element? value)
      (element-raw value)
      (if (text? value)
          (text-raw value)
          value)))

;; text : external/raw -> text?
;;   Wrap a browser Text node.
(struct text (raw) #:transparent)

;; text-wrap : any/c -> any/c
;;   Wrap a raw browser Text node, leaving wrapped values alone.
(define (text-wrap value)
  (if (text? value)
      value
      (text value)))

;; text-unwrap : any/c -> any/c
;;   Unwrap a text struct to its raw browser object.
(define (text-unwrap value)
  (if (text? value)
      (text-raw value)
      value))

;; node : external/raw -> node?
;;   Wrap a browser Node object.
(struct node (raw) #:transparent)

;; node-wrap : any/c -> any/c
;;   Wrap a raw browser Node object, leaving wrapped values alone.
(define (node-wrap value)
  (if (or (not value) (node? value) (element? value) (text? value) (attr? value))
      value
      (node value)))

;; node-unwrap : any/c -> any/c
;;   Unwrap a node struct to its raw browser object.
(define (node-unwrap value)
  (cond
    [(node? value) (node-raw value)]
    [(element? value) (element-raw value)]
    [(text? value) (text-raw value)]
    [(attr? value) (attr-raw value)]
    [else value]))

;; attr : external/raw -> attr?
;;   Wrap a browser Attr node.
(struct attr (raw) #:transparent)

;; attr-wrap : any/c -> any/c
;;   Wrap a raw browser Attr node, leaving wrapped values alone.
(define (attr-wrap value)
  (if (or (not value) (attr? value))
      value
      (attr value)))

;; attr-unwrap : any/c -> any/c
;;   Unwrap an attr struct to its raw browser object.
(define (attr-unwrap value)
  (if (attr? value)
      (attr-raw value)
      value))

;; dom-token-list : external/raw -> dom-token-list?
;;   Wrap a browser DOMTokenList object.
(struct dom-token-list (raw) #:transparent)

;; dom-token-list-wrap : any/c -> any/c
;;   Wrap a raw browser DOMTokenList object, leaving wrapped values alone.
(define (dom-token-list-wrap value)
  (if (or (not value) (dom-token-list? value))
      value
      (dom-token-list value)))

;; dom-token-list-unwrap : any/c -> any/c
;;   Unwrap a DOMTokenList struct to its raw browser object.
(define (dom-token-list-unwrap value)
  (if (dom-token-list? value)
      (dom-token-list-raw value)
      value))

;; shadow-root : external/raw -> shadow-root?
;;   Wrap a browser ShadowRoot object.
(struct shadow-root (raw) #:transparent)

;; shadow-root-wrap : any/c -> any/c
;;   Wrap a raw browser ShadowRoot object, leaving wrapped values alone.
(define (shadow-root-wrap value)
  (if (or (not value) (shadow-root? value))
      value
      (shadow-root value)))

;; shadow-root-unwrap : any/c -> any/c
;;   Unwrap a shadow-root struct to its raw browser object.
(define (shadow-root-unwrap value)
  (if (shadow-root? value)
      (shadow-root-raw value)
      value))

;; array-like->vector : symbol? any/c (-> any/c any/c) -> vector?
;;   Convert a browser array-like value into a WebRacket vector.
(define (array-like->vector who value wrap-item)
  (define length (js-ref value "length"))
  (unless (exact-nonnegative-integer? length)
    (raise-argument-error who "array-like browser value" value))
  (let loop ([i 0] [acc '()])
    (if (= i length)
        (list->vector (reverse acc))
        (loop (add1 i)
              (cons (wrap-item (js-ref value (number->string i))) acc)))))

;; dom-rect : external/raw -> dom-rect?
;;   Wrap a browser DOMRect object.
(struct dom-rect (raw) #:transparent)

;; dom-rect-wrap : any/c -> any/c
;;   Wrap a raw browser DOMRect object, leaving wrapped values alone.
(define (dom-rect-wrap value)
  (if (dom-rect? value)
      value
      (dom-rect value)))

;; dom-rect-unwrap : any/c -> any/c
;;   Unwrap a DOMRect struct to its raw browser object.
(define (dom-rect-unwrap value)
  (if (dom-rect? value)
      (dom-rect-raw value)
      value))
