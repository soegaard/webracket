#lang racket/base

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/system)

;; find-repo-root : path? -> path?
;;   Find the checkout root that contains the WebRacket launcher.
(define (find-repo-root start)
  (let loop ([dir (simplify-path start)])
    (cond
      [(file-exists? (build-path dir "webracket.rkt"))
       dir]
      [else
       (define parent (path-only dir))
       (unless parent
         (error 'test-launcher-collection-paths
                "could not find repository root containing webracket.rkt"))
       (loop (path->complete-path parent))])))

;; normalized-path=? : path-string? path-string? -> boolean?
;;   Compare two paths after resolving their complete simplified forms.
(define (normalized-path=? left right)
  (equal? (simplify-path (path->complete-path left))
          (simplify-path (path->complete-path right))))

;; environment-with-checkout-collections : path? path? path? -> environment-variables?
;;   Put the local source root and checkout package root first without duplicates.
(define (environment-with-checkout-collections repo-root source-root compiled-root)
  (define package-root (build-path repo-root "pkgs"))
  (define base-collections
    (filter (lambda (collection-path)
              (and (not (normalized-path=? source-root collection-path))
                   (not (normalized-path=? package-root collection-path))))
            (current-library-collection-paths)))
  (define collection-paths
    (append (list source-root package-root) base-collections))
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   env
   #"PLTCOLLECTS"
   (string->bytes/utf-8
    (string-join (map path->string collection-paths) ":")))
  (environment-variables-set!
   env
   #"PLTCOMPILEDROOTS"
   (string->bytes/utf-8 (path->string compiled-root)))
  env)

;; run-launcher-collection-paths-smoke : path? path? path? -> void?
;;   Compile a base module when PLTCOLLECTS already exposes the checkout packages.
(define (run-launcher-collection-paths-smoke repo-root racket-exe raco-exe)
  (define base
    (make-temporary-file "webracket-launcher-collection-paths-~a" 'directory))
  (define source-path (build-path base "minimal.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define output-dir (build-path base "output"))
  (define launcher-path (build-path repo-root "webracket.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n(define program-reached? #t)\n" out))
        #:exists 'truncate/replace)
      (define env
        (environment-with-checkout-collections repo-root base compiled-root))
      (define (run args)
        (parameterize ([current-directory repo-root]
                       [current-environment-variables env])
          (apply system*/exit-code args)))
      (check-equal? (run (list raco-exe "make" (path->string launcher-path))) 0)
      (check-equal?
       (run (list racket-exe
                  (path->string launcher-path)
                  "--browser"
                  "--ffi"
                  "standard.ffi"
                  "--dest"
                  (path->string output-dir)
                  (path->string source-path)))
       0)
      (check-true (file-exists? (build-path output-dir "minimal.html")))
      (check-true (file-exists? (build-path output-dir "minimal.wasm"))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

(module+ main
  (define repo-root
    (find-repo-root (or (current-load-relative-directory)
                        (current-directory))))
  (define racket-exe
    (find-system-path 'exec-file))
  (define raco-exe
    (build-path (or (path-only racket-exe)
                    (error 'test-launcher-collection-paths
                           "could not find the Racket executable directory"))
                "raco"))
  (unless (file-exists? raco-exe)
    (error 'test-launcher-collection-paths "could not find raco"))
  (run-launcher-collection-paths-smoke repo-root racket-exe raco-exe))
