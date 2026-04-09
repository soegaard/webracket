#lang webracket

;;;
;;; web-easy Tests
;;;

;; Full-Racket entrypoint for web-easy tests.

(include-lib web-easy)
(include/reader "test-web-easy-body.rkt" read-syntax/skip-first-line)
(include/reader "test-html-elements-body.rkt" read-syntax/skip-first-line)
