#lang webracket
(include-lib web-easy)

(choice '("a" "b")
        "a"
        (lambda (_v) (void))
        #:id "choice-a"
        #:id "choice-b")
