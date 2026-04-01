#lang webracket

;;;
;;; Performance wrappers
;;;

;; performance-now : -> f64
;;   Read a high-resolution monotonic timestamp.
(define (performance-now)
  (js-performance-now))
