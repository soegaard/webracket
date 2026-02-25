;; Isolated MiniScheme repro for character literal handling.
;; Run from this directory:
;;   racket ../../../../webracket.rkt -r test-char-basics-only.rkt

(include "minischeme.rkt")

(minischeme-reset-state!)
(minischeme-process-input "(list (char? #\\a) (char->integer #\\A) (integer->char 66))")
