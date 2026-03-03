;;;
;;; web-easy Run Tests
;;;

;; No-#lang entrypoint for `webracket.rkt -r`.

(include/reader "../main.rkt" read-syntax/skip-first-line)
(include/reader "test-web-easy-body.rkt" read-syntax/skip-first-line)
