#lang webracket

;;;
;;; DOMRect wrappers
;;;

;; dom-rect-left : external? -> f64
;;   Read the left coordinate of a DOMRect.
(define (dom-rect-left rect)
  (js-dom-rect-left rect))

;; dom-rect-top : external? -> f64
;;   Read the top coordinate of a DOMRect.
(define (dom-rect-top rect)
  (js-dom-rect-top rect))

;; dom-rect-width : external? -> f64
;;   Read the width of a DOMRect.
(define (dom-rect-width rect)
  (js-dom-rect-width rect))

;; dom-rect-height : external? -> f64
;;   Read the height of a DOMRect.
(define (dom-rect-height rect)
  (js-dom-rect-height rect))
