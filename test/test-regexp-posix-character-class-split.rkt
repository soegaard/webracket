#lang webracket/base

(define actual
  (regexp-split #px"[[:space:]]+" "first-id second-id"))

(js-log
 (if (equal? actual '("first-id" "second-id"))
     "regexp-posix-character-class-split-ok"
     "regexp-posix-character-class-split-failed"))
