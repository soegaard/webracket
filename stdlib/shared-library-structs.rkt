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
