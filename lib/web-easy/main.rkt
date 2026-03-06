#lang webracket

;;;
;;; web-easy Main
;;;

;; Entry point that assembles web-easy by including the component source files.

(include/reader "observable.rkt" read-syntax/skip-first-line)
(include/reader "operator.rkt"   read-syntax/skip-first-line)
(include/reader "view.rkt"       read-syntax/skip-first-line)
(include/reader "theme-token.rkt" read-syntax/skip-first-line)
(include/reader "backend-dom-like.rkt" read-syntax/skip-first-line)
(include/reader "renderer.rkt"   read-syntax/skip-first-line)
