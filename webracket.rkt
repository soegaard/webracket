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
(define dump-passes-dir (make-parameter #f))
(define dump-passes-limit (make-parameter #f))
(define timings?        (make-parameter #f))

(define browser         (make-parameter #f))
(define nodejs          (make-parameter #t))   ; default

(define link-flags      (make-parameter '()))  ; ignored
(define ffi-files       (make-parameter '()))  ; list of filenames for .ffi files

(define source-filename (make-parameter #f))   ; the file to compile

(define stdlib?         (make-parameter #t))   ; include standard library by default

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
   [("--dump-passes") dir "Write per-pass dumps to directory <dir>"
                      (dump-passes-dir dir)]
   [("--dump-passes-limit") n "Dump at most <n> passes (0 means no dumps)"
                            (define maybe-n (string->number n))
                            (unless (and maybe-n (exact-integer? maybe-n) (>= maybe-n 0))
                              (error 'webracket
                                     (format "--dump-passes-limit expects an exact nonnegative integer, got: ~a" n)))
                            (dump-passes-limit maybe-n)]
   [("--timings") "Print timing breakdown for compilation steps"
                  (timings? #t)]
   [("--stdlib")             "Include the standard library (default)"
                             (stdlib? #t)]
   [("--no-stdlib")          "Do not include the standard library"
                             (stdlib? #f)]
   

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

;; In the case that -r is used to run the program directly,
;; we propagate the exit code from `node`.

(define exit-code
  (drive-compilation #:filename      (source-filename)
                     #:wat-filename  (wat-filename)
                     #:wasm-filename (wasm-filename)
                     #:host-filename (host-filename)
                     #:label-map-forms? (label-map-forms)
                     #:dump-passes-dir (dump-passes-dir)
                     #:dump-passes-limit (dump-passes-limit)
                     #:timings?     (timings?)
                     #:verbose?      (verbose-mode)
                     #:browser?      (browser)
                     #:node?         (nodejs)
                     #:run-after?    (run-after)
                     #:ffi-files     (ffi-files)
                     #:stdlib?       (stdlib?)))

(unless (zero? exit-code)
  (exit exit-code))
