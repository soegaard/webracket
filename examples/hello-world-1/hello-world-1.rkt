;;; Node hello world example
;;;
;;; Quick start (build + run in one command):
;;;   racket ../../webracket.rkt -r hello-world-1.rkt
;;;
;;; See README.md for other build options.
;;;
;;; Note:
;;;   The switch -r causes webracket to compile and run the program in one go.
;;;   Node is used to run the program.
;;;
;;;   The standard library is enabled by default, so `displayln` works.
;;;   Use `--no-stdlib` only for programs that do not require stdlib.

(displayln "Hello from WebRacket (Node) via displayln.")
(displayln "This example runs in the terminal.")
(js-log "Hello from WebRacket (Node) via js-log.")
