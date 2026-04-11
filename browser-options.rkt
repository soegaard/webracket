#lang racket/base

;; browser-options.rkt : browser-mode state for expansion-time use
;;   Use a process-wide environment variable so all phases and namespaces
;;   see the same browser-mode choice.

(provide browser-mode?
         set-browser-mode!
         reset-browser-mode!)

(define browser-mode-envvar "WEBRACKET_BROWSER")

;; browser-mode? : -> boolean?
;;   Return the current browser-mode flag.
(define (browser-mode?)
  (equal? (getenv browser-mode-envvar) "1"))

;; set-browser-mode! : boolean? -> void?
;;   Set the browser-mode flag.
(define (set-browser-mode! browser?)
  (putenv browser-mode-envvar (if browser? "1" "0")))

;; reset-browser-mode! : -> void?
;;   Clear the browser-mode flag.
(define (reset-browser-mode!)
  (putenv browser-mode-envvar "0"))
