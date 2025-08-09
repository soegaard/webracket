#lang racket
;;;
;;; COMMAND LINE TOOL
;;;

;; This file handles the parsing of the command line.
;; Based on the flags we set various parameters and
;; pass these on to the driver in "driver.rkt" which
;; handles file operations (and invokes the compiler).

;; usage: racket webracket.rkt [ <option> ... ] [<files>] ...

(require "driver.rkt")


(define run-after       (make-parameter #f))
(define verbose-mode    (make-parameter #f))
(define wat-filename    (make-parameter #f))
(define wasm-filename   (make-parameter #f))
(define host-filename   (make-parameter #f))

(define browser         (make-parameter #f))
(define nodejs          (make-parameter #t))   ; default

(define link-flags      (make-parameter '()))  ; ignored

(define source-filename (make-parameter #f))   ; the file to compile

(define file-to-compile
  (command-line
   #:program "webracket"

   #:once-each ; independent flags
   [("-r" "--run")           "Run the program after compilation."
                             (run-after #t)]
   [("-v" "--verbose")       "Compile with verbose messages"
                             (verbose-mode #t)]
   [("--wat-file")  filename "Filename for the wat file"
                             (wat-filename filename)]
   [("--wasm-file") filename "Filename for the wasm file"
                             (wasm-filename filename)]
   [("--host-file") filename "Filename for the host file"
                             (host-filename filename)]

   #:once-any ; only one flag from this group
   [("-b" "--browser") "Generate code for browser."
                       (browser #t)]
   [("-n" "--node")    "Generate code for Node.js"
                       (nodejs #t)]
                          
   #:multi ; can be used multiple times
   [("-l" "--link-flags") lf ; flag takes one argument
                          "Add a flag <lf> for the linker"
                          (link-flags (cons lf (link-flags)))]
   
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   (source-filename filename)))


(drive-compilation #:filename      (source-filename)
                   #:wat-filename  (wat-filename)
                   #:wasm-filename (wasm-filename)
                   #:host-filename (host-filename)
                   #:verbose?      (verbose-mode)
                   #:browser?      (browser)
                   #:node?         (nodejs)
                   #:run-after?    (run-after))
