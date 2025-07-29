#lang racket

;;;
;;; The command line tool `webracket`
;;;

(define verbose-mode   (make-parameter #f))
(define profiling-on   (make-parameter #f))
(define optimize-level (make-parameter 0))
(define link-flags     (make-parameter null))
 
(define file-to-compile
  (command-line
   #:program "webracket"
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages"
                       (verbose-mode #t)]
   [("-p" "--profile") "Compile with profiling"
                       (profiling-on #t)]
   #:once-any
   [("-o" "--optimize-1") "Compile with optimization level 1"
                          (optimize-level 1)]
   ["--optimize-2"        (; show help on separate lines
                           "Compile with optimization level 2,"
                           "which includes all of level 1")
                          (optimize-level 2)]
   #:multi
   [("-l" "--link-flags") lf ; flag takes one argument
                          "Add a flag <lf> for the linker"
                          (link-flags (cons lf (link-flags)))]
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(displayln file-to-compile)
