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
      value))

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
