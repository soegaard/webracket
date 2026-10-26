#lang racket/base

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/system)

;; find-repo-root : path? -> path?
;;   Find the checkout root containing the WebRacket command entry point.
(define (find-repo-root start)
  (let loop ([dir (simplify-path start)])
    (cond
      [(file-exists? (build-path dir "webracket.rkt")) dir]
      [else
       (define parent (path-only dir))
       (unless parent
         (error 'test-console-bridge-standard-ffi-fresh-cache
                "could not find repository root containing webracket.rkt"))
       (loop (path->complete-path parent))])))

;; normalized-path=? : path-string? path-string? -> boolean?
;;   Compare paths after resolving complete simplified forms.
(define (normalized-path=? left right)
  (equal? (simplify-path (path->complete-path left))
          (simplify-path (path->complete-path right))))

;; environment-with-checkout-collections : path? path? path? -> environment-variables?
;;   Prefer fixture and checkout collections and use an isolated compiled-code cache.
(define (environment-with-checkout-collections repo-root source-root compiled-root)
  (define package-root (build-path repo-root "pkgs"))
  (define base-collections
    (filter (lambda (collection-path)
              (and (not (normalized-path=? source-root collection-path))
                   (not (normalized-path=? package-root collection-path))))
            (current-library-collection-paths)))
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   env
   #"PLTCOLLECTS"
   (string->bytes/utf-8
    (string-join (map path->string
                      (append (list source-root package-root) base-collections))
                 ":")))
  (environment-variables-set!
   env
   #"PLTCOMPILEDROOTS"
   (string->bytes/utf-8 (path->string compiled-root)))
  env)

;; wat-function-count : path? string? -> exact-nonnegative-integer?
;;   Count definitions of one named Wasm function in generated WAT.
(define (wat-function-count wat-path function-name)
  (length
   (regexp-match*
    (regexp (string-append "\\(func " (regexp-quote function-name)))
    (file->string wat-path))))

;; compile-fixture : path? path? path? boolean? -> path?
;;   Compile the standard-FFI fixture with a fresh cache and return its WAT path.
(define (compile-fixture repo-root racket-exe root console-bridge?)
  (define compiled-root (build-path root "compiled"))
  (define provider-cache-dir (build-path root "provider-cache"))
  (define runtime-provider-cache-dir (build-path root "runtime-provider-cache"))
  (define output-dir (build-path root "output"))
  (define wat-path (build-path root "program.wat"))
  (define source-path
    (build-path repo-root "test" "h3-standard-ffi-fresh-graph.rkt"))
  (define source-root
    (or (path-only source-path)
        (error 'compile-fixture "fixture source has no parent directory")))
  (make-directory* compiled-root)
  (define status
    (parameterize ([current-directory repo-root]
                   [current-environment-variables
                    (environment-with-checkout-collections
                     repo-root
                     source-root
                     compiled-root)])
      (apply system*/exit-code
             racket-exe
             (path->string (build-path repo-root "webracket.rkt"))
             (append (list "--browser"
                           "--ffi" "standard.ffi"
                           "--provider-cache-dir" (path->string provider-cache-dir)
                           "--runtime-provider-cache-dir"
                           (path->string runtime-provider-cache-dir)
                           "--dest" (path->string output-dir)
                           "--wat-file" (path->string wat-path))
                     (if console-bridge? (list "--console-bridge") '())
                     (list (path->string source-path))))))
  (check-equal? status 0)
  (check-true (file-exists? wat-path))
  (check-true
   (file-exists?
    (build-path output-dir "h3-standard-ffi-fresh-graph.wasm")))
  wat-path)

;; run-console-bridge-standard-ffi-fresh-cache-regression : path? path? -> void?
;;   Compile plain and console bridge standard-FFI graphs from separate caches.
(define (run-console-bridge-standard-ffi-fresh-cache-regression repo-root racket-exe)
  (define base
    (make-temporary-file "webracket-console-bridge-standard-ffi-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define plain-wat
        (compile-fixture repo-root racket-exe (build-path base "plain") #f))
      (define console-wat
        (compile-fixture repo-root racket-exe (build-path base "console") #t))
      (check-equal? (wat-function-count plain-wat "$wr-bridge-error-message") 0)
      (check-equal? (wat-function-count console-wat "$wr-bridge-error-message") 1))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

(define (run-regression)
  (define repo-root
    (find-repo-root (or (current-load-relative-directory)
                        (current-directory))))
  (run-console-bridge-standard-ffi-fresh-cache-regression
   repo-root
   (find-system-path 'exec-file)))

(module+ test
  (run-regression))

(module+ main
  (run-regression))
