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
         (only-in racket/pretty     pretty-write)
         (only-in racket/format     ~a)
         #;(only-in "lang/reader.rkt" read-syntax)
         (only-in "assembler.rkt"   run wat->wasm runtime)
         (only-in racket/list       append*)
         (only-in "parameters.rkt"  current-ffi-foreigns
                                    current-ffi-imports-wat
                                    current-ffi-funcs-wat)
         racket/runtime-path
         racket/syntax
         "compiler.rkt"
         "define-foreign.rkt")

;;;
;;; 
;;;

(define (drive-compilation
         #:filename      filename
         #:wat-filename  wat-filename
         #:wasm-filename wasm-filename
         #:host-filename host-filename ; default: "runtime.js"
         #:verbose?      verbose?
         #:browser?      browser?
         #:node?         node?
         #:run-after?    run-after?
         #:ffi-files     ffi-files    ; list of file paths for .ffi files
         #:stdlib?       stdlib?)     ; include standard library 
  
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
  
  ; 1. Check that `filename` exists.
  (unless (file-exists? filename)
    (error 'drive-compilation (~a "file not found: " filename)))

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
       (displayln "Including `stdlib/stdlib-for-browser.rkt`" (current-error-port))
       #`(begin
           (include/reader "stdlib/stdlib-for-browser.rkt" read-syntax/skip-first-line)
           #,stx)]
      [stdlib?
       #`(begin
           (include/reader "stdlib/stdlib.rkt" read-syntax/skip-first-line)
           #,stx)] ; stx is a begin form      
      [else stx]))

  ; 4. Compile the syntax object.
  (define wat
    (with-handlers (#;[exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "compile failed: " (exn-message e))))])
      (comp stx-with-stdlib)))

  ; 5. Save the resulting WAT module.
  (define out-wat (or wat-filename (path-replace-extension filename ".wat")))
  (write-wat-to-file out-wat wat)

  ; 6. Compile the wat-file to wasm using `wat->wasm` from `assembler.rkt`
  (define out-wasm (or wasm-filename (path-replace-extension filename ".wasm")))
  (with-handlers ([exn:fail? (λ (e)
                               (error 'drive-compilation
                                      (~a "wat->wasm failed: " (exn-message e))))])
    (wat->wasm wat #:wat out-wat #:wasm out-wasm))

  ; 7. Write the host file (default: "runtime.js")
  (define out-host (or host-filename
                       (if node?
                           (path-replace-extension filename ".js")
                           (path-replace-extension filename ".html"))))
  (with-output-to-file out-host
    (λ () (displayln
           (runtime #:out out-wasm #:host (if node? 'node 'browser))))
    #:exists 'replace)

  ; 8. Optionally run the program via Node.js.
  (when (and node? run-after?)
    (define runtime-js out-host)
    (run #f #:wat out-wat #:wasm out-wasm #:runtime.js runtime-js)))

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
      (pretty-write wat))
    #:exists 'replace))

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
