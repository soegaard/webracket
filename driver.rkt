#lang racket/base
;;;        
;;; DRIVER 
;;;        

;; The functions in this module "drive" the compiler so to speak.
;; The settings given by the user via the command line is set
;; in parameters in "webracket.rkt".
;; 
;; Here we handle the neccessary file operations and call the
;; appropriate compilation function from "compiler.rkt".
;;
;; Any external book keeping in files are done here.


;;;
;;; PROVIDES
;;;

(provide (all-defined-out))


;;;
;;; DEPENDENCIES
;;;

(require (only-in syntax/modread    with-module-reading-parameterization)
         (only-in racket/path       path-only)
         (only-in racket/file       make-directory* make-temporary-file)
         (only-in "lang/reader.rkt" read-syntax))

;;;
;;; 
;;;

(define (drive-compilation
         #:filename   filename
         #:verbose?   verbose?
         #:browser?   browser?
         #:node?      node?
         #:run-after? run-after?)

  ; 1. Check that `filename` exists.
  ; 2. Use `read-top-level-from-from-file` to get a syntax object.
  ;    - signal error if anything goes wrong
  ; 3. Call `comp` from `compiler.rkt` to compile the forms.
  ;    - signal error if anything goes wrong
  ; 4. Call `write-wat-to-file` to save the compilation result.
  ; 5. If `node?` and `run-after?` is true, then call `run` from `assembler.rkt`.
  )

;;;
;;; READ TOP-LEVEL FORMS FROM FILE 
;;;

(define (read-top-level-from-from-file filename)
  ...)


;;;
;;; WRITE WAT TO FILE
;;;

(define (write-wat-to-file out-filname wat)
  ...)


;;;
;;; READ MODULE
;;;

(define (read-lang-module port)
  (port-count-lines! port)
  ; resets all reader parameters to default values
  (with-module-reading-parameterization 
    (lambda ()
      (read-syntax (object-name port) port))))

; read-lang-file : path-string -> syntax
;   Read the program in `path-string`.
;   Return the a syntax object representing a module.
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
    (pretty-write (syntax->datum
                   (topexpand
                    (read-lang-file src-path)))
                   out))
  
  (close-output-port out)

  (rename-file-or-directory temp-filename dest-path #t) ; exists-ok? = #t

  (with-handlers ([exn:fail:filesystem? void])
    (delete-file temp-filename)))




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
