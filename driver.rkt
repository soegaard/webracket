#lang racket/base
;;;        
;;; DRIVER 
;;;        

;; The functions in this module "drive" the compiler so to speak.
;; The settings given by the user via the command line are
;; passed as arguments to `drive-compilation`.
;; 
;; Here we handle the necessary file operations and call the
;; appropriate compilation function from "compiler.rkt".
;;
;; Any external bookkeeping with respect to files are done here.


;;;
;;; PROVIDES
;;;

(provide drive-compilation
         (all-defined-out))


;;;
;;; DEPENDENCIES
;;;

(require (only-in syntax/modread    with-module-reading-parameterization)
         (only-in racket/path       path-only path-get-extension)
         (only-in racket/file       make-directory* make-temporary-file)
         (only-in racket/port       open-output-nowhere with-output-to-string)
         (only-in racket/pretty     pretty-write)
         (only-in racket/format     ~a)
         (only-in racket/system     system*)
         #;(only-in "lang/reader.rkt" read-syntax)
         (only-in "assembler.rkt"   run wat->wasm runtime)
         (only-in racket/list       append*)
         (only-in "parameters.rkt"  current-ffi-foreigns
                                    current-ffi-imports-wat
                                    current-ffi-funcs-wat)
         (only-in "timings.rkt"     now-ms format-timing-table)
         "wat-identifiers.rkt"
         racket/runtime-path
         racket/syntax
         "compiler.rkt"
         "define-foreign.rkt")

;;;
;;; THE MAIN DRIVER
;;;

(define (drive-compilation
         #:filename          filename
         #:wat-filename      wat-filename
         #:wasm-filename     wasm-filename
         #:host-filename     host-filename      ; default: "runtime.js"
         #:label-map-forms?  label-map-forms?
         #:dump-passes-dir   dump-passes-dir
         #:dump-passes-limit dump-passes-limit  ; max number of passes to dump
         #:timings?          timings?
         #:verbose?          verbose?
         #:browser?          browser?
         #:node?             node?
         #:run-after?        run-after?
         #:ffi-files         ffi-files    ; list of file paths for .ffi files
         #:stdlib?           stdlib?)     ; include standard library 
  (define exit-code 0)
  
  ; 0. Handle ffi-files
  (define resolved-ffi-files
    (for/list ([ffi-filename ffi-files])
      (define resolved (resolve-ffi-filename ffi-filename))
      (unless resolved
        (error 'drive-compilation
               (~a "ffi file not found: " ffi-filename)))
      resolved))

  (define ffi-foreigns  '()) ; list of `foreign` structures
  (define ffi-imports   '()) ; list of wat
  (define ffi-funcs     '()) ; list of wat

  (for ([ffi-filename resolved-ffi-files])
    (define fs (ffi-file->foreigns ffi-filename))
    (define ims   (map foreign->import fs))
    (define prims (map foreign->primitive fs))
    (set! ffi-foreigns (cons fs    ffi-foreigns))
    (set! ffi-imports  (cons ims   ffi-imports))
    (set! ffi-funcs    (cons prims ffi-funcs)))
  (set! ffi-foreigns (append* (reverse ffi-foreigns)))
  (set! ffi-imports  (append* (reverse ffi-imports)))
  (set! ffi-funcs    (append* (reverse ffi-funcs)))

  (current-ffi-foreigns    ffi-foreigns)
  (current-ffi-imports-wat ffi-imports)
  (current-ffi-funcs-wat   ffi-funcs)
  
  ; 1. Check that `filename` exists and is a source file.
  (ensure-source-file! 'drive-compilation filename)

  ; 2. Read the top-level forms from `filename`.
  (define stx
    (with-handlers ([exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "read failed: " (exn-message e))))])
      (read-top-level-from-file filename)))

  ; 3. Prepend standard library (if enabled)
  ;     "stdlib-for-browser.rkt" includes "stdlib.rkt" and adds `sxml->dom`
  (define stx-with-stdlib
    (cond
      [(and stdlib? browser?)
       (when verbose?
         (displayln "Including `stdlib/stdlib-for-browser.rkt`" (current-error-port)))
       #`(begin
           (include/reader "stdlib/stdlib-for-browser.rkt" read-syntax/skip-first-line)
           #,stx)]
      [stdlib?
       #`(begin
           (include/reader "stdlib/stdlib.rkt" read-syntax/skip-first-line)
           #,stx)] ; stx is a begin form      
      [else stx]))

  (define t0 (now-ms))

  ; 4. Preflight checks

  ; Preflight: `-r` requires Node host mode.
  (when (and run-after? browser?)
    (error 'drive-compilation
           "cannot use `-r` in browser mode; use Node mode or omit `-r`."))

  ; Preflight: `-r` in Node mode requires a Node.js executable.
  (when (and node? run-after?)
    (check-node! 'drive-compilation))

  ; Precompute output paths for preflight checks.
  (define out-wat  (or wat-filename  (path-replace-extension filename ".wat")))
  (define out-wasm (or wasm-filename (path-replace-extension filename ".wasm")))
  (define out-map  (path-replace-extension out-wasm ".wasm.map.sexp"))
  (define out-host (or host-filename
                       (if node?
                           (path-replace-extension filename ".js")
                           (path-replace-extension filename ".html"))))

  ; Preflight: ensure output files do not collide.
  (ensure-distinct-output-files!
   'drive-compilation
   (list (cons "WAT output file"       out-wat)
         (cons "Wasm output file"      out-wasm)
         (cons "label-map output file" out-map)
         (cons "host output file"      out-host)))

  ; Preflight: ensure output locations are writable.
  (ensure-output-path-writable! 'drive-compilation out-wat  "WAT output file")
  (ensure-output-path-writable! 'drive-compilation out-wasm "Wasm output file")
  (ensure-output-path-writable! 'drive-compilation out-map  "label-map output file")
  (ensure-output-path-writable! 'drive-compilation out-host "host output file")
  
  ; Preflight: ensure wasm-tools exists and can run.
  (check-wasm-tools! 'drive-compilation)

  ; 5. Compile the syntax object.
  (label-map-include-form? label-map-forms?)
  (current-pass-dump-dir dump-passes-dir)
  (current-pass-dump-limit dump-passes-limit)
  (current-pass-timings? timings?)
  (define t-compile-start (now-ms))
  (define wat
    (with-handlers (#;[exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "compile failed: " (exn-message e))))])
      (comp stx-with-stdlib)))
  (define t-compile-end (now-ms))

  ; 6. Save the resulting WAT module.
  (define t-write-wat-start (now-ms))
  (write-wat-to-file out-wat wat)
  (ensure-generated-file-exists! 'drive-compilation out-wat "WAT output file")
  (define t-write-wat-end (now-ms))

  ; 7. Compile the wat-file to wasm using `wat->wasm` from `assembler.rkt`
  (define t-assemble-start (now-ms))
  (define wat->wasm-success?
    (with-handlers ([exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "wat->wasm failed: " (exn-message e))))])
      (wat->wasm wat #:wat out-wat #:wasm out-wasm)))
  (unless wat->wasm-success?
    (error 'drive-compilation
           (~a "wat->wasm failed to produce bytecode for " filename)))
  (ensure-generated-file-exists! 'drive-compilation out-wasm "Wasm bytecode file")
  (define t-assemble-end (now-ms))

  ; 7b. Write label map sidecar
  (define t-write-map-start (now-ms))
  (with-output-to-file out-map
    (λ () (pretty-write (label-map->sexp)))
    #:exists 'replace)
  (ensure-generated-file-exists! 'drive-compilation out-map "label-map output file")
  (define t-write-map-end (now-ms))

  ; 8. Write the host file (default: "runtime.js")
  (define t-write-host-start (now-ms))
  (with-output-to-file out-host
    (λ () (displayln
           (runtime #:out out-wasm #:host (if node? 'node 'browser))))
    #:exists 'replace)
  (ensure-generated-file-exists! 'drive-compilation out-host "host output file")
  (define t-write-host-end (now-ms))

  (when timings?
    (define compile-ms    (- t-compile-end t-compile-start))
    (define write-wat-ms  (- t-write-wat-end t-write-wat-start))
    (define assemble-ms   (- t-assemble-end t-assemble-start))
    (define write-map-ms  (- t-write-map-end t-write-map-start))
    (define write-host-ms (- t-write-host-end t-write-host-start))
    (define total-ms      (- t-write-host-end t0))
    (define overall-table
      (format-timing-table
       (list (list "compile" compile-ms)
             (list "write-wat" write-wat-ms)
             (list "assemble" assemble-ms)
             (list "write-map" write-map-ms)
             (list "write-host" write-host-ms)
             (list "total" total-ms))))
    (displayln "=== Overall Compile ===")
    (displayln overall-table)
    (define pass-table (current-pass-timing-table))
    (when pass-table
      (displayln "=== Pass Timings ===")
      (displayln pass-table))
    (define gen-table (current-gen-timing-table))
    (when gen-table
      (displayln "=== Generate-Code Phases ===")
      (displayln gen-table)))

  ; 9. Optionally run the program via Node.js.
  (when (and node? run-after?)
    (ensure-generated-file-exists! 'drive-compilation out-wasm "Wasm bytecode file")
    (define runtime-js out-host)
    (set! exit-code
          (run #f #:wat out-wat #:wasm out-wasm #:runtime.js runtime-js)))
  exit-code)

;;;
;;; READ TOP-LEVEL FORMS FROM FILE 
;;;

(define (read-top-level-from-file filename)
  (define (read-forms port)
    (let loop ([forms '()])
      (define stx (read-syntax filename port))      
      (if (eof-object? stx)
          (reverse forms)
          (loop (cons stx forms)))))
  (call-with-input-file filename
    (λ (port)
      (port-count-lines! port)
      (with-module-reading-parameterization
        (λ ()
          (define forms (read-forms port))
          (datum->syntax #f `(begin ,@forms)))))))


;;;
;;; WRITE WAT TO FILE
;;;

(define (write-wat-to-file out-filename wat)
  (with-output-to-file out-filename
    (λ ()
      (displayln ";; This file was generated by `webracket`.")
      (display (rewrite-wat-identifiers
                (with-output-to-string
                  (λ ()
                    (pretty-write wat))))))
    #:exists 'replace))

;;;
;;; PREFLIGHT CHECK TOOLS
;;;

(define (ensure-source-file! who filename)
  (cond
    [(directory-exists? filename)
     (error who (~a "expected a source file, got directory: " filename))]
    [(not (file-exists? filename))
     (error who (~a "file not found: " filename))]))

(define (ensure-generated-file-exists! who path kind)
  (unless (file-exists? path)
    (error who (~a kind " was not created: " path))))

(define (ensure-output-path-writable! who output-path kind)
  (when (directory-exists? output-path)
    (error who
           (~a "cannot write " kind ": " output-path
               "\n" "target path is an existing directory")))
  (define dir (or (path-only output-path) (current-directory)))
  (with-handlers ([exn:fail:filesystem?
                   (λ (e)
                     (error who
                            (~a "cannot prepare directory for " kind ": " dir
                                "\n" (exn-message e))))])
    (make-directory* dir))
  (define probe #f)
  (with-handlers ([exn:fail:filesystem?
                   (λ (e)
                     (when (and probe (file-exists? probe))
                       (with-handlers ([exn:fail:filesystem? void])
                         (delete-file probe)))
                     (error who
                            (~a "cannot write " kind ": " output-path
                                "\n" (exn-message e))))])
    (set! probe (make-temporary-file "webracket-write-check~a.tmp" #f dir))
    (delete-file probe)))

(define (check-node! who)
  (define node-path (find-executable-path "node"))
  (unless node-path
    (error who
           (string-append
            "required executable `node` was not found in PATH.\n"
            "Install Node.js or run without `-r`.")))
  (define node-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* node-path "--version")))
  (unless node-ok?
    (error who
           (string-append
            "found `node` in PATH but could not run `node --version`.\n"
            "Check the installation and executable permissions.")))
  (define wasm-exnref-flag-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* node-path "--experimental-wasm-exnref" "-e" "")))
  (unless wasm-exnref-flag-ok?
    (error who
           (string-append
            "found `node`, but it does not accept `--experimental-wasm-exnref`.\n"
            "Install a compatible Node.js version or run without `-r`."))))

(define (ensure-distinct-output-files! who entries)
  (define seen (make-hash))
  (for ([entry entries])
    (define kind          (car entry))
    (define path          (cdr entry))
    (define normalized    (path->complete-path path (current-directory)))
    (define previous-kind (hash-ref seen normalized #f))
    (when previous-kind
      (error who
             (~a "output file collision: " previous-kind " and "
                 kind " resolve to the same path: " path)))
    (hash-set! seen normalized kind)))

(define (check-wasm-tools! who)
  (define wasm-tools-path (find-executable-path "wasm-tools"))
  (unless wasm-tools-path
    (error who
           (string-append
            "required executable `wasm-tools` was not found in PATH.\n"
            "Install `wasm-tools` to compile `.wat` files to `.wasm`.")))
  (define parse-help-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* wasm-tools-path "parse" "--help")))
  (unless parse-help-ok?
    (error who
           (string-append
            "found `wasm-tools` in PATH but could not run `wasm-tools parse --help`.\n"
            "Check the installation and executable permissions."))))

;;;
;;; READ MODULE
;;;

(define (read-lang-module port)
  (port-count-lines! port)
  ; Reset all reader parameters to default values.
  (with-module-reading-parameterization 
    (lambda ()
      (read-syntax (object-name port) port))))

; read-lang-file : path-string -> syntax
;   Read the program in `path-string`.
;   Return a syntax object representing a module.
(define (read-lang-file path-string)
  (call-with-input-file path-string
    (λ (port)
      (parameterize ([current-directory (or (path-only path-string)
                                            (current-directory))])
        (read-lang-module port)))))

;;;
;;; EXPAND MODULE
;;;

(require "expander.rkt") ; provides `topexpand`

;;;
;;; EXPAND FILE
;;;

; Note: The following procedure is modelled after:
;    racket/collects/compiler/compile-file.rkt
; It uses `dynamic-wind` to handle errors (removing temporary files etc.).
; With an eye towards bootstrapping, we postpone using `dynamic-wind`.

(define (expand-file src-path
                     [dest-path
                      (let-values ([(base name dir?) (split-path src-path)])
                        (build-path base "expanded"
                                    (path-add-suffix name #".erkt")))])
  ; create .../expanded/ if needed
  (make-directory* (path-only dest-path))

  ; We write to a temporary file and only move it to `dest-path`
  ; only if we encounter no errors.
  (define temp-filename (make-temporary-file "tmp~a" #f (path-only dest-path)))

  (define dir
    (let-values ([(base name dir?) (split-path src-path)])
      (if (eq? base 'relative)
          (current-directory)
          (path->complete-path base (current-directory)))))

  (define out (open-output-file temp-filename #:exists 'truncate/replace))

  (parameterize ([current-load-relative-directory dir]
                 [current-write-relative-directory dir])
    (define program (read-lang-file src-path))
    #;(pretty-write program (current-error-port))
    (pretty-write (syntax->datum
                   (topexpand program))
                   out))
  
  (close-output-port out)

  (rename-file-or-directory temp-filename dest-path #t) ; exists-ok? = #t

  (with-handlers ([exn:fail:filesystem? void])
    (delete-file temp-filename)))

;;;
;;; RESOLUTION OF FFI-PATHS
;;;

(define-runtime-path here ".")

(define system-ffi-directory
  (let ([candidate (build-path here "ffi")])
    (and (directory-exists? candidate) candidate)))

(define (resolve-ffi-filename ffi-filename)
  (define filename
    (if (path-get-extension ffi-filename)
        ffi-filename
        (path-add-extension ffi-filename ".ffi")))
  
  (define candidates
    (append (list filename
                  (build-path "ffi" filename))
            (if system-ffi-directory
                (list (build-path system-ffi-directory filename))
                '())))
  (for/or ([candidate (in-list candidates)])
    (and candidate (file-exists? candidate) candidate)))



;;;     
;;; TEST
;;;     

#;(begin
    (read-lang-file "test/test.rkt")

    (require racket/pretty)

    (pretty-print
     (syntax->datum
      (read-lang-file "test/test.rkt")))

    (expand-file "test/test.rkt")


    (pretty-print
     (syntax->datum
      (expand-file "test/test.rkt"))))
