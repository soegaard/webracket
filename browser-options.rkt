#lang racket/base

;; browser-options.rkt : browser-mode state for expansion-time use
;;   Carry browser-mode information into expansion-time code.

(provide browser-mode?
         set-browser-mode!
         reset-browser-mode!)

;; browser-mode? : (parameter/c boolean?) -> boolean?
;;   Return the current browser-mode flag.
(define browser-mode? (make-parameter #f))

;; set-browser-mode! : boolean? -> void?
;;   Set the browser-mode flag.
(define (set-browser-mode! browser?)
  (browser-mode? browser?))

;; reset-browser-mode! : -> void?
;;   Clear the browser-mode flag.
(define (reset-browser-mode!)
  (browser-mode? #f))
