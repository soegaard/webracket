#lang racket/base

;;;
;;; check-constructor-arity
;;;

;; Scan Racket source files and report legacy decorator call forms with wrong arity.
;; Intended to catch mistakes like:
;;   with-class / with-id / with-attrs swallowing extra forms due to paren drift.

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         racket/vector)

;; tracked-arity : (hash/c symbol? exact-positive-integer?)
;;   Constructor/decorator name to expected argument count.
(define tracked-arity
  (hash 'with-class 2
        'with-id    2
        'with-attrs 2))

;; cfg-show-preview? : boolean?
;;   Print a compact preview of malformed calls.
(define cfg-show-preview? #t)

;; result counters
(define arity-errors 0)
(define read-errors 0)
(define scanned-files 0)

;; parse-error? : exn? -> boolean?
;;   Check whether e is a reader error.
(define (parse-error? e)
  (exn:fail:read? e))

;; source-files-for : path-string? -> (listof path-string?)
;;   Expand file or directory input into .rkt files.
(define (source-files-for p)
  (define maybe-path
    (simplify-path (path->complete-path p)))
  (cond
    [(directory-exists? maybe-path)
     (for/list ([x (in-list (find-files
                             (lambda (v)
                               (and (file-exists? v)
                                    (regexp-match? #rx"\\.rkt$" (path->string v))))
                             maybe-path))])
       (path->string x))]
    [(file-exists? maybe-path)
     (list (path->string maybe-path))]
    [else
     (eprintf "check-constructor-arity: path not found: ~a\n" p)
     '()]))

;; form-preview : any/c -> string?
;;   Generate short one-line preview for diagnostics.
(define (form-preview v)
  (define s (format "~s" v))
  (if (> (string-length s) 140)
      (~a (substring s 0 137) "...")
      s))

;; wrapped-symbol : syntax? -> (or/c symbol? #f)
;;   Extract head symbol for list forms.
(define (wrapped-symbol stx)
  (define maybe-list (syntax->list stx))
  (and maybe-list
       (pair? maybe-list)
       (let ([head (car maybe-list)])
         (cond
           [(identifier? head) (syntax-e head)]
           [else #f]))))

;; scan-form! : syntax? string? -> void?
;;   Recursively scan one syntax form.
(define (scan-form! stx file)
  (define maybe-list (syntax->list stx))
  (when maybe-list
    (define head-sym (wrapped-symbol stx))
    (when (and head-sym (hash-has-key? tracked-arity head-sym))
      (define expected (hash-ref tracked-arity head-sym))
      (define given    (sub1 (length maybe-list)))
      (unless (= given expected)
        (set! arity-errors (add1 arity-errors))
        (define ln (or (syntax-line stx) 0))
        (define col (or (syntax-column stx) 0))
        (eprintf "~a:~a:~a: arity mismatch in ~a; expected ~a args, got ~a\n"
                 file ln col head-sym expected given)
        (when cfg-show-preview?
          (eprintf "  call: ~a\n" (form-preview (syntax->datum stx))))))
    (for ([sub (in-list maybe-list)])
      (when (syntax? sub)
        (scan-form! sub file))))
  (define maybe-vec (syntax-e stx))
  (when (vector? maybe-vec)
    (for ([sub (in-vector maybe-vec)])
      (when (syntax? sub)
        (scan-form! sub file)))))

;; scan-file! : string? -> void?
;;   Read all top-level forms from file and scan each.
(define (scan-file! file)
  (set! scanned-files (add1 scanned-files))
  (call-with-input-file file
    (lambda (in)
      ;; Many project files use #lang and are later included via include/reader.
      ;; For this static scanner, skip that first line so plain form reading works.
      (define lang-prefix (peek-string 6 0 in))
      (when (and (string? lang-prefix)
                 (string=? lang-prefix "#lang "))
        (void (read-line in 'any)))
      (let loop ()
        (define stx
          (with-handlers ([parse-error?
                           (lambda (e)
                             (set! read-errors (add1 read-errors))
                             (eprintf "~a: read error: ~a\n" file (exn-message e))
                             eof)])
            (read-syntax file in)))
        (unless (eof-object? stx)
          (scan-form! stx file)
          (loop))))))

;; usage : -> void?
;;   Print usage.
(define (usage)
  (displayln "Usage: racket tools/check-constructor-arity.rkt <file-or-dir> [more ...]")
  (displayln "Example:")
  (displayln "  racket tools/check-constructor-arity.rkt lib/web-easy/smoke"))

(define argv (vector->list (current-command-line-arguments)))
(when (null? argv)
  (usage)
  (exit 2))

(define files
  (remove-duplicates
   (append*
    (for/list ([a (in-list argv)])
      (source-files-for a)))))

(when (null? files)
  (eprintf "No files to scan.\n")
  (exit 2))

(for ([f (in-list files)])
  (scan-file! f))

(printf "Scanned files: ~a\n" scanned-files)
(printf "Read errors:   ~a\n" read-errors)
(printf "Arity errors:  ~a\n" arity-errors)

(cond
  [(or (> read-errors 0) (> arity-errors 0)) (exit 1)]
  [else (exit 0)])
