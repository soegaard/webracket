#lang webracket

;;;
;;; DOMRect wrappers
;;;

;; dom-rect-left : dom-rect? -> f64
;;   Read the left coordinate of a DOMRect.
(define (dom-rect-left rect)
  (js-dom-rect-left (dom-rect-unwrap rect)))

;; dom-rect-top : dom-rect? -> f64
;;   Read the top coordinate of a DOMRect.
(define (dom-rect-top rect)
  (js-dom-rect-top (dom-rect-unwrap rect)))

;; dom-rect-width : dom-rect? -> f64
;;   Read the width of a DOMRect.
(define (dom-rect-width rect)
  (js-dom-rect-width (dom-rect-unwrap rect)))

;; dom-rect-height : dom-rect? -> f64
;;   Read the height of a DOMRect.
(define (dom-rect-height rect)
  (js-dom-rect-height (dom-rect-unwrap rect)))
