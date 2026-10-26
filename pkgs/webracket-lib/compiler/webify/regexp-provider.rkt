#lang racket/base

;;; Cached demod support for the staged original-Racket regexp provider.

(provide webify-regexp-source-root
         webify-regexp-main-source
         webify-regexp-export-symbols
         webify-regexp-provider-owned-symbols
         webify-regexp-bootstrap-helper-symbols
         webify-regexp-demand-symbols
         webify-regexp-default-cache-root
         webify-regexp-cache-paths
         webify-regexp-lowered-provider-cache-path
         webify-regexp-source-fingerprint
         webify-regexp-demod-command
         webify-regexp-demod!
         webify-regexp-linklet
         webify-regexp-private-cell-symbol
         webify-regexp-private-alias-forms
         webify-regexp-public-alias-forms)

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/system
         (only-in "provider-cache.rkt"
                  cached-fingerprint-valid?
                  call-with-provider-cache-lock
                  write-cache-fingerprint!))

(define-runtime-path repo-root "../../../..")
(define simplified-repo-root (simplify-path repo-root))

(define racket-root
  (simplify-path (build-path repo-root "../racket/racket")))

(define racket-bin
  (build-path racket-root "bin/racket"))

(define bootstrap-run-rkt
  (build-path racket-root "src/expander/bootstrap-run.rkt"))

(define webify-regexp-source-root
  (build-path repo-root "racket-src/regexp"))

(define webify-regexp-main-source
  (build-path webify-regexp-source-root "main.rkt"))

(define webify-regexp-export-symbols
  '(byte-pregexp
    byte-pregexp?
    byte-regexp
    byte-regexp?
    pregexp
    pregexp?
    regexp
    regexp-capture-group-count
    regexp-match
    regexp-match-peek
    regexp-match-peek-immediate
    regexp-match-peek-positions
    regexp-match-peek-positions-immediate
    regexp-match-peek-positions-immediate/end
    regexp-match-peek-positions/end
    regexp-match-positions
    regexp-match-positions/end
    regexp-match/end
    regexp-match?
    regexp-max-lookbehind
    regexp-place-init!
    regexp-replace
    regexp-replace*
    regexp?))

(define webify-regexp-provider-owned-symbols
  webify-regexp-export-symbols)

(define webify-regexp-bootstrap-helper-symbols
  '(webracket-literal-byte-pregexp
    webracket-literal-byte-regexp
    webracket-literal-pregexp
    webracket-literal-regexp))

;; The private string helpers use the staged regexp provider without being
;; provider-owned exports themselves.
(define webify-regexp-demand-symbols
  (append webify-regexp-export-symbols
          webify-regexp-bootstrap-helper-symbols
          '(regexp-split)))

(define webify-regexp-default-cache-root
  (build-path (find-system-path 'temp-dir) "webracket-regexp-provider-cache"))

(define regexp-cache-format-version 2)

;; webify-regexp-cache-paths : [path-string?] -> (values path? path? path?)
;;   Return the flattened linklet, dependency file, and bootstrap cache paths.
(define (webify-regexp-cache-paths [cache-root webify-regexp-default-cache-root])
  (define root (if (path? cache-root) cache-root (string->path cache-root)))
  (values (build-path root "regexp.rktl")
          (build-path root "regexp-dep.rktd")
          (build-path root "bootstrap-cache")))

;; webify-regexp-lowered-provider-cache-path : boolean? [path-string?] -> path?
;;   Return the cache file for lowered provider forms.
(define (webify-regexp-lowered-provider-cache-path public-aliases?
                                                   [cache-root webify-regexp-default-cache-root])
  (define root (if (path? cache-root) cache-root (string->path cache-root)))
  (build-path root
              (if public-aliases?
                  "regexp-provider-public.rktd"
                  "regexp-provider-private.rktd")))

;; webify-regexp-source-paths : -> (listof path?)
;;   Return source files that invalidate the regexp provider cache.
(define (webify-regexp-source-paths)
  (sort
   (map simplify-path
        (cons (build-path webify-regexp-source-root "../common/check.rkt")
              (find-files file-exists? webify-regexp-source-root)))
   string<?
   #:key path->string))

;; webify-regexp-source-fingerprint : -> any/c
;;   Fingerprint the staged regexp source tree using path, size, and mtime.
(define (webify-regexp-source-fingerprint)
  `(regexp-cache-format
    ,regexp-cache-format-version
    ,@(for/list ([path (in-list (webify-regexp-source-paths))])
        (list (path->string (find-relative-path simplified-repo-root path))
              (file-size path)
              (file-or-directory-modify-seconds path)))))

;; webify-regexp-demod-command : [path-string?] -> (listof string?)
;;   Build the Racket CS demodularizer command for the staged regexp source.
(define (webify-regexp-demod-command [cache-root webify-regexp-default-cache-root])
  (define-values (out-rktl dep-rktd bootstrap-cache)
    (webify-regexp-cache-paths cache-root))
  (list (path->string racket-bin)
        (path->string bootstrap-run-rkt)
        "-t" (path->string webify-regexp-main-source)
        "-c" (path->string bootstrap-cache)
        "-k" (path->string racket-root)
        "-s" "-x"
        "--depends" (path->string dep-rktd)
        "--no-global"
        "-o" (path->string out-rktl)))

;; webify-regexp-demod! : [#:cache-root path-string?] [#:force? boolean?]
;;                     -> (values exact-integer? string? path?)
;;   Generate or reuse the flattened regexp linklet for provider experiments.
(define (webify-regexp-demod! #:cache-root [cache-root webify-regexp-default-cache-root]
                              #:force? [force? #f]
                              #:verbose? [verbose? #f])
  (define-values (out-rktl _dep-rktd _bootstrap-cache)
    (webify-regexp-cache-paths cache-root))
  (define fingerprint-path
    (path-add-extension out-rktl #".fingerprint.rktd"))
  (define fingerprint (webify-regexp-source-fingerprint))
  (make-directory* cache-root)
  (cond
    [(and (not force?)
          (file-exists? out-rktl)
          (cached-fingerprint-valid? fingerprint-path fingerprint))
     (when verbose?
       (fprintf (current-error-port)
                "provider-cache: regexp demod hit: ~a\n"
                out-rktl))
     (values 0 "using cached regexp demod linklet\n" out-rktl)]
    [else
     (call-with-provider-cache-lock
      out-rktl
      (lambda ()
        (cond
          [(and (not force?)
                (file-exists? out-rktl)
                (cached-fingerprint-valid? fingerprint-path fingerprint))
           (when verbose?
             (fprintf (current-error-port)
                      "provider-cache: regexp demod hit: ~a\n"
                      out-rktl))
           (values 0 "using cached regexp demod linklet\n" out-rktl)]
          [else
           (when verbose?
             (fprintf (current-error-port)
                      "provider-cache: regexp demod miss; regenerating: ~a\n"
                      out-rktl))
           (define stdout (open-output-string))
           (define stderr (open-output-string))
           (define command (webify-regexp-demod-command cache-root))
           (define status
             (parameterize ([current-output-port stdout]
                            [current-error-port stderr])
               (apply system*/exit-code command)))
           (when (zero? status)
             (write-cache-fingerprint! fingerprint-path fingerprint))
           (values status
                   (string-append (get-output-string stdout)
                                  (get-output-string stderr))
                   out-rktl)])))]))

;; webify-regexp-linklet : [#:cache-root path-string?] [#:force? boolean?] -> any/c
;;   Return the cached flattened regexp linklet, generating it when needed.
(define (webify-regexp-linklet #:cache-root [cache-root webify-regexp-default-cache-root]
                               #:force? [force? #f]
                               #:verbose? [verbose? #f])
  (define-values (status output out-rktl)
    (webify-regexp-demod! #:cache-root cache-root
                          #:force? force?
                          #:verbose? verbose?))
  (unless (zero? status)
    (error 'webify-regexp-linklet
           "regexp demod failed with status ~a\n~a"
           status
           output))
  (call-with-input-file out-rktl read))

;; webify-regexp-private-cell-symbol : symbol? -> symbol?
;;   Private top-level cell used by provider-slice smokes.
(define (webify-regexp-private-cell-symbol export)
  (unless (memq export webify-regexp-export-symbols)
    (raise-argument-error 'webify-regexp-private-cell-symbol
                          "regexp export symbol"
                          export))
  (string->symbol (format "webify-regexp-export-cell$~a" export)))

;; webify-regexp-private-alias-forms : (hash/c symbol? symbol?) -> (listof datum?)
;;   Bind private top-level cells to generated regexp provider export cells.
(define (webify-regexp-private-alias-forms export-cells)
  (for/list ([export (in-list webify-regexp-export-symbols)])
    `(define ,(webify-regexp-private-cell-symbol export)
       ,(hash-ref export-cells export))))

;; webify-regexp-public-alias-forms : (hash/c symbol? symbol?) -> (listof datum?)
;;   Bind private cells, then publish public aliases through module-variable-ref.
(define (webify-regexp-public-alias-forms export-cells)
  (append
   (webify-regexp-private-alias-forms export-cells)
   (for/list ([export (in-list webify-regexp-export-symbols)])
     `(define ,export
        (module-variable-ref ,(webify-regexp-private-cell-symbol export))))))
