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
(define dest-dir        (make-parameter #f))
(define label-map-forms (make-parameter #t))
(define dump-passes-dir (make-parameter #f))
(define dump-passes-limit (make-parameter #f))
(define timings?        (make-parameter #f))
(define pretty-wat?     (make-parameter #f))
(define list-primitives? (make-parameter #f))
(define tree-shake?     (make-parameter #t))
(define tree-shake-report (make-parameter #f))
(define print-top-level-results? (make-parameter #f))
(define console-bridge? (make-parameter #f))

(define browser         (make-parameter #f))
(define nodejs          (make-parameter #t))   ; default

(define link-flags      (make-parameter '()))  ; ignored
(define ffi-files       (make-parameter '()))  ; list of filenames for .ffi files
(define vfs-preloads    (make-parameter '()))  ; list of host-to-VFS preload specs

(define source-filename (make-parameter #f))   ; the file to compile

(define stdlib?         (make-parameter #t))   ; include standard library by default

(define (normalize-vfs-preload-path-for-duplicates path)
  (regexp-replace #px"/+$" path ""))

(define (add-vfs-preload-entry! entry)
  (define path     (hash-ref entry 'path))
  (define norm-path (normalize-vfs-preload-path-for-duplicates path))
  (when (for/or ([existing (in-list (vfs-preloads))])
          (equal? norm-path
                  (normalize-vfs-preload-path-for-duplicates
                   (hash-ref existing 'path))))
    (error 'webracket (format "duplicate VFS preload target path: ~a" path)))
  (vfs-preloads (cons entry (vfs-preloads))))

(define (validate-vfs-preload-path! who path)
  (when (string=? path "")
    (error who "VFS preload target path is empty"))
  (unless (regexp-match? #px"^/" path)
    (error who (format "VFS preload target path must be absolute, got: ~a" path)))
  (when (regexp-match? #rx#"\0" path)
    (error who (format "VFS preload target path contains NUL: ~a" path))))

(define (validate-vfs-base64-source! who source)
  (unless (regexp-match? #px"^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$" source)
    (error who (format "VFS preload base64 source is invalid: ~a" source))))

(define (parse-vfs-preload who kind spec)
  (match (regexp-match #px"^([^=]+)=(.*)$" spec)
    [(list _ path source)
     (validate-vfs-preload-path! who path)
     (when (and (string=? source "")
                (not (memq kind '(text base64))))
       (error who (format "VFS preload source is empty: ~a" spec)))
     (when (eq? kind 'base64)
       (validate-vfs-base64-source! who source))
     (hasheq 'path path 'kind kind 'source source)]
    [_ (error who (format "expected VFS=SOURCE, got: ~a" spec))]))

(define (add-vfs-preload! kind spec)
  (add-vfs-preload-entry! (parse-vfs-preload 'webracket kind spec)))

(define (add-vfs-mkdir! path)
  (validate-vfs-preload-path! 'webracket path)
  (add-vfs-preload-entry! (hasheq 'path path 'kind 'directory 'source #t)))

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
   [("--dest") dirname "Write default outputs under <dir>"
               (dest-dir dirname)]
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
   [("--tree-shake") "Tree shake runtime primitives (default)"
                     (tree-shake? #t)]
   [("--no-tree-shake") "Do not tree shake runtime primitives"
                        (tree-shake? #f)]
   [("--tree-shake-report") filename
                            "Write the runtime primitive report to <filename>"
                            (tree-shake-report filename)]
   [("--print-top-level-results")
    "Print each top-level expression result with print; useful for script-style tests"
    (print-top-level-results? #t)]
   [("--console-bridge") "Install the browser console bridge as globalThis.WR"
                         (console-bridge? #t)]
   [("--no-console-bridge") "Do not install the browser console bridge (default)"
                            (console-bridge? #f)]
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
   [("--vfs-file") spec
                   "Preload Node host file into VFS as VFS=SOURCE"
                   (add-vfs-preload! 'file spec)]
   [("--vfs-url") spec
                  "Preload URL into VFS as VFS=SOURCE"
                  (add-vfs-preload! 'url spec)]
   [("--vfs-text") spec
                   "Preload inline text into VFS as VFS=TEXT"
                   (add-vfs-preload! 'text spec)]
   [("--vfs-base64") spec
                     "Preload inline base64 bytes into VFS as VFS=BASE64"
                     (add-vfs-preload! 'base64 spec)]
   [("--vfs-mkdir") path
                   "Preload empty VFS directory"
                   (add-vfs-mkdir! path)]
   [("--vfs-dir") spec
                  "Preload Node host directory into VFS as VFS=SOURCE"
                  (add-vfs-preload! 'directory spec)]
   
   #:args filenames
   filenames))

(cond
  [(list-primitives?)
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

(define-values (exit-code _compile-timings)
  (drive-compilation #:filename      (source-filename)
                     #:wat-filename  (wat-filename)
                     #:wasm-filename (wasm-filename)
                     #:host-filename (host-filename)
                     #:dest-dir      (dest-dir)
                     #:label-map-forms? (label-map-forms)
                     #:dump-passes-dir (dump-passes-dir)
                     #:dump-passes-limit (dump-passes-limit)
                     #:timings?     (timings?)
                     #:pretty-wat?  (pretty-wat?)
                     #:verbose?      (verbose-mode)
                     #:browser?      (browser)
                     #:node?         (nodejs)
                     #:tree-shake?   (tree-shake?)
                     #:tree-shake-report (tree-shake-report)
                     #:print-top-level-results? (print-top-level-results?)
                     #:console-bridge? (console-bridge?)
                     #:vfs-preloads (reverse (vfs-preloads))
                     #:run-after?    (run-after)
                     #:ffi-files     (ffi-files)
                     #:stdlib?       (stdlib?)))

(unless (zero? exit-code)
  (exit exit-code))
