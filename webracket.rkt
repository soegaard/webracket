#lang racket
;;;
;;; COMMAND LINE TOOL
;;;

;; This file handles the parsing of the command line.
;; Based on the flags we set various parameters and
;; pass these on to the driver in "driver.rkt" which
;; handles file operations (and invokes the compiler).

;; usage: racket webracket.rkt [ <option> ... ] [<files>] ...

(require racket/format
         racket/runtime-path
         racket/string)

(define process-start-ms (current-inexact-milliseconds))

(define-runtime-path driver-rkt "driver.rkt")

(define parse-start-ms (current-inexact-milliseconds))


(define run-after       (make-parameter #f))
(define verbose-mode    (make-parameter #f))
(define wat-filename    (make-parameter #f))
(define wasm-filename   (make-parameter #f))
(define host-filename   (make-parameter #f))
(define label-map-forms (make-parameter #t))
(define dump-passes-dir (make-parameter #f))
(define dump-passes-limit (make-parameter #f))
(define timings?        (make-parameter #f))
(define pretty-wat?     (make-parameter #f))
(define list-primitives? (make-parameter #f))

(define browser         (make-parameter #f))
(define nodejs          (make-parameter #t))   ; default

(define link-flags      (make-parameter '()))  ; ignored
(define ffi-files       (make-parameter '()))  ; list of filenames for .ffi files

(define source-filename (make-parameter #f))   ; the file to compile

(define stdlib?         (make-parameter #t))   ; include standard library by default

(define positional-filenames
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
   [("--pretty-wat") "Write .wat with pretty formatting"
                     (pretty-wat? #t)]
   [("--no-pretty-wat") "Write .wat without pretty formatting (default)"
                        (pretty-wat? #f)]
   [("--list-primitives") "Print the list of all primitives and exit"
                          (list-primitives? #t)]
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
   
   #:args filenames
   filenames))

(define parse-end-ms (current-inexact-milliseconds))

(define drive-compilation
  #f)

(define list-available-primitives
  #f)

(define compilation-timings-driver-prelude
  #f)

(define compilation-timings-compile-total
  #f)

(define driver-load-ms 0.0)

(define driver-symbol-lookups-ms 0.0)

(define (load-driver!)
  (unless drive-compilation
    (define driver-load-start-ms (current-inexact-milliseconds))
    (set! drive-compilation (dynamic-require driver-rkt 'drive-compilation))
    (set! driver-load-ms (- (current-inexact-milliseconds) driver-load-start-ms))
    (define symbol-lookups-start-ms (current-inexact-milliseconds))
    (set! list-available-primitives
          (dynamic-require driver-rkt 'list-available-primitives))
    (set! compilation-timings-driver-prelude
          (dynamic-require driver-rkt 'compilation-timings-driver-prelude))
    (set! compilation-timings-compile-total
          (dynamic-require driver-rkt 'compilation-timings-compile-total))
    (set! driver-symbol-lookups-ms
          (- (current-inexact-milliseconds) symbol-lookups-start-ms))))

(define (ms->s ms) (/ ms 1000.0))

(define (pct part total)
  (if (zero? total) 0.0 (* 100.0 (/ part total))))

(define (format-process-timing-table rows)
  (define label-width
    (for/fold ([w 0]) ([row rows])
      (max w (string-length (first row)))))
  (define (row->string label ms total-ms)
    (define secs (ms->s ms))
    (define pct-val (pct ms total-ms))
    (define secs-str (~r secs #:min-width 6 #:precision '(= 1)))
    (define pct-str (~r pct-val #:min-width 5 #:precision '(= 1)))
    (define label-str (~a label #:min-width label-width))
    (format "  ~a :  ~a  ~a%" label-str secs-str pct-str))
  (define total-ms
    (let ([total-row (for/first ([row rows] #:when (string=? (first row) "total")) row)])
      (if total-row
          (second total-row)
          (for/sum ([row rows]) (second row)))))
  (define header
    (let* ([title "Timing breakdown"]
           [secs-right (+ 2 label-width 4 6)]
           [pct-right (+ secs-right 2 5)]
           [pad1 (max 1 (- secs-right (string-length title) 3))]
           [title+secs (string-append title (make-string pad1 #\space) "(s)")]
           [pad2 (max 1 (- pct-right (string-length title+secs) 5))])
      (string-append title+secs (make-string pad2 #\space) "(pct)")))
  (string-join
   (cons header
         (for/list ([row rows])
           (row->string (first row) (second row) total-ms)))
   "\n"))

(cond
  [(list-primitives?)
   (load-driver!)
   (for ([name (in-list (list-available-primitives #:ffi-files (ffi-files)))])
     (displayln name))
   (exit 0)]
  [else
   (unless (= (length positional-filenames) 1)
     (error 'webracket
            (format "expected exactly one source filename, got ~a"
                    (length positional-filenames))))
   (source-filename (car positional-filenames))])

;; In the case that -r is used to run the program directly,
;; we propagate the exit code from `node`.

(load-driver!)

(define startup-end-ms (current-inexact-milliseconds))

(define-values (exit-code compile-timings)
  (drive-compilation #:filename      (source-filename)
                     #:wat-filename  (wat-filename)
                     #:wasm-filename (wasm-filename)
                     #:host-filename (host-filename)
                     #:label-map-forms? (label-map-forms)
                     #:dump-passes-dir (dump-passes-dir)
                     #:dump-passes-limit (dump-passes-limit)
                     #:timings?     (timings?)
                     #:pretty-wat?  (pretty-wat?)
                     #:verbose?      (verbose-mode)
                     #:browser?      (browser)
                     #:node?         (nodejs)
                     #:run-after?    (run-after)
                     #:ffi-files     (ffi-files)
                     #:stdlib?       (stdlib?)))

(when (and (timings?) compile-timings)
  (define process-end-ms (current-inexact-milliseconds))
  (define cli-init-ms (- parse-start-ms process-start-ms))
  (define parse-ms (- parse-end-ms parse-start-ms))
  (define driver-prelude-ms (compilation-timings-driver-prelude compile-timings))
  (define compile-pipeline-ms (compilation-timings-compile-total compile-timings))
  (define total-ms (- process-end-ms process-start-ms))
  (displayln "=== Process Timings ===")
  (displayln
   (format-process-timing-table
    (list (list "cli-init" cli-init-ms)
          (list "parse-args" parse-ms)
          (list "driver-load" driver-load-ms)
          (list "driver-symbol-lookups" driver-symbol-lookups-ms)
          (list "driver-prelude" driver-prelude-ms)
          (list "compile-pipeline" compile-pipeline-ms)
          (list "total" total-ms)))))

(unless (zero? exit-code)
  (exit exit-code))
