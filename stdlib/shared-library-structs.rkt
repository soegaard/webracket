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

;; node-list : external/raw -> node-list?
;;   Wrap a browser NodeList object.
(struct node-list (raw) #:transparent)

;; node-list-wrap : any/c -> any/c
;;   Wrap a raw browser NodeList object, leaving wrapped values alone.
(define (node-list-wrap value)
  (if (or (not value) (node-list? value))
      value
      (node-list value)))

;; node-list-unwrap : any/c -> any/c
;;   Unwrap a node-list struct to its raw browser object.
(define (node-list-unwrap value)
  (if (node-list? value)
      (node-list-raw value)
      value))

;; html-collection : external/raw -> html-collection?
;;   Wrap a browser HTMLCollection object.
(struct html-collection (raw) #:transparent)

;; html-collection-wrap : any/c -> any/c
;;   Wrap a raw browser HTMLCollection object, leaving wrapped values alone.
(define (html-collection-wrap value)
  (if (or (not value) (html-collection? value))
      value
      (html-collection value)))

;; html-collection-unwrap : any/c -> any/c
;;   Unwrap an html-collection struct to its raw browser object.
(define (html-collection-unwrap value)
  (if (html-collection? value)
      (html-collection-raw value)
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

;; animation : external/raw -> animation?
;;   Wrap a browser Animation object.
(struct animation (raw) #:transparent)

;; animation-wrap : any/c -> any/c
;;   Wrap a raw browser Animation object, leaving wrapped values alone.
(define (animation-wrap value)
  (if (or (not value) (animation? value))
      value
      (animation value)))

;; animation-unwrap : any/c -> any/c
;;   Unwrap an animation struct to its raw browser object.
(define (animation-unwrap value)
  (if (animation? value)
      (animation-raw value)
      value))

;; computed-style-map : external/raw -> computed-style-map?
;;   Wrap a browser ComputedStyleMap object.
(struct computed-style-map (raw) #:transparent)

;; computed-style-map-wrap : any/c -> any/c
;;   Wrap a raw browser ComputedStyleMap object, leaving wrapped values alone.
(define (computed-style-map-wrap value)
  (if (or (not value) (computed-style-map? value))
      value
      (computed-style-map value)))

;; computed-style-map-unwrap : any/c -> any/c
;;   Unwrap a computed-style-map struct to its raw browser object.
(define (computed-style-map-unwrap value)
  (if (computed-style-map? value)
      (computed-style-map-raw value)
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

;; dom-rect-list : external/raw -> dom-rect-list?
;;   Wrap a browser DOMRectList object.
(struct dom-rect-list (raw) #:transparent)

;; dom-rect-list-wrap : any/c -> any/c
;;   Wrap a raw browser DOMRectList object, leaving wrapped values alone.
(define (dom-rect-list-wrap value)
  (if (or (not value) (dom-rect-list? value))
      value
      (dom-rect-list value)))

;; dom-rect-list-unwrap : any/c -> any/c
;;   Unwrap a dom-rect-list struct to its raw browser object.
(define (dom-rect-list-unwrap value)
  (if (dom-rect-list? value)
      (dom-rect-list-raw value)
      value))

;; selection : external/raw -> selection?
;;   Wrap a browser Selection object.
(struct selection (raw) #:transparent)

;; selection-wrap : any/c -> any/c
;;   Wrap a raw browser Selection object, leaving wrapped values alone.
(define (selection-wrap value)
  (if (or (not value) (selection? value))
      value
      (selection value)))

;; selection-unwrap : any/c -> any/c
;;   Unwrap a selection struct to its raw browser object.
(define (selection-unwrap value)
  (if (selection? value)
      (selection-raw value)
      value))

;; selection-range-count : selection? -> exact-nonnegative-integer?
;;   Read the number of ranges in the current selection.
(define (selection-range-count selection)
  (js-ref (selection-unwrap selection) "rangeCount"))

;; selection-is-collapsed? : selection? -> boolean?
;;   Report whether the current selection is collapsed.
(define (selection-is-collapsed? selection)
  (not (zero? (js-ref (selection-unwrap selection) "isCollapsed"))))

;; selection-anchor-node : selection? -> (or/c #f node?)
;;   Read the anchor node for the current selection.
(define (selection-anchor-node selection)
  (node-wrap (js-ref/extern (selection-unwrap selection) "anchorNode")))

;; selection-focus-node : selection? -> (or/c #f node?)
;;   Read the focus node for the current selection.
(define (selection-focus-node selection)
  (node-wrap (js-ref/extern (selection-unwrap selection) "focusNode")))

;; selection-to-string : selection? -> string?
;;   Convert the current selection to text.
(define (selection-to-string selection)
  (js-send/value (selection-unwrap selection) "toString" (vector)))

;; selection-remove-all-ranges! : selection? -> void?
;;   Clear all ranges from the current selection.
(define (selection-remove-all-ranges! selection)
  (js-send (selection-unwrap selection) "removeAllRanges" (vector))
  (void))
