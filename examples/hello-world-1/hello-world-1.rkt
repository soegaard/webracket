;;; Node hello world example
;;;
;;; Quick start (build + run in one command):
;;;   racket ../../webracket.rkt --stdlib -r hello-world-1.rkt
;;;
;;; See README.md for other build options.
;;;
;;; Note:
;;;   The switch -r causes webracket to compile and run the program in one go.
;;;   Node is used to run the program.
;;;
;;;   The switch `--stdlib` is needed for `displayln`.
;;;   If you only use `js-log`, you can drop `--stdlib`.

(displayln "Hello from WebRacket (Node) via displayln.")
(displayln "This example runs in the terminal.")
(js-log "Hello from WebRacket (Node) via js-log.")
