#lang webracket
(include/reader "exceptions.rkt" read-syntax/skip-first-line)
(include/reader "ports.rkt"      read-syntax/skip-first-line)
(include/reader "writing.rkt"    read-syntax/skip-first-line)
(include/reader "reading.rkt"    read-syntax/skip-first-line)


;; The standard library consists of files above.
;; Each file begins with `#lang webracket`.
;; When editing a file one can therefore run and test
;; everything in the standard repl.
;;
;; When a program is compiled the contents of the standard
;; library is added to the compiled program like this:
;;
;; (begin
;;    (include "stdlib.rkt")
;;    ... the program ...))
;;
;; The expander `topexpand` will then inline the contents
;; of "stdlib.rkt` into the top-level.
;; But actually `include` doesn't work, since the files
;; all begin with `#lang webracket`. Therefore
;;     (include/reader <filename> read-syntax/skip-first-line)
;; is used to skip the language declaration.
