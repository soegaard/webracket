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
(define label-map-forms (make-parameter #t))

(define browser         (make-parameter #f))
(define nodejs          (make-parameter #t))   ; default

(define link-flags      (make-parameter '()))  ; ignored
(define ffi-files       (make-parameter '()))  ; list of filenames for .ffi files

(define source-filename (make-parameter #f))   ; the file to compile

(define stdlib?         (make-parameter #f))   ; include standard library?

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
   [("--label-map-forms") "Include (form ...) entries in .wasm.map.sexp"
                          (label-map-forms #t)]
   [("--no-label-map-forms") "Omit (form ...) entries in .wasm.map.sexp"
                             (label-map-forms #f)]
   [("--stdlib")             "Include the standard library"
                             (stdlib? #t)]
   

   #:once-any ; only one flag from this group
   [("-b" "--browser") "Generate code for browser."
                       (nodejs  #f)
                       (browser #t)]
   [("-n" "--node")    "Generate code for Node.js"
                       (browser #f)
                       (nodejs  #t)]
                          
   #:multi ; can be used multiple times
   [("-l" "--link-flags") lf ; flag takes one argument
                          "Add a flag <lf> for the linker"
                          (link-flags (cons lf (link-flags)))]

   #:multi ; can be used multiple times
   [("--ffi") ffi-file
              "Add .ffi file"
              (ffi-files (cons ffi-file (ffi-files)))]
   
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   (source-filename filename)))


(drive-compilation #:filename      (source-filename)
                   #:wat-filename  (wat-filename)
                   #:wasm-filename (wasm-filename)
                   #:host-filename (host-filename)
                   #:label-map-forms? (label-map-forms)
                   #:verbose?      (verbose-mode)
                   #:browser?      (browser)
                   #:node?         (nodejs)
                   #:run-after?    (run-after)
                   #:ffi-files     (ffi-files)
                   #:stdlib?       (stdlib?))
