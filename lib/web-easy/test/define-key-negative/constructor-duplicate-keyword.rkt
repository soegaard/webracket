#lang webracket
(include/reader "../main.rkt" read-syntax/skip-first-line)

(choice '("a" "b")
        "a"
        (lambda (_v) (void))
        #:id "choice-a"
        #:id "choice-b")
