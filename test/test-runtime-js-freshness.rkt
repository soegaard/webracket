#lang racket/base

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/system)

(define-runtime-path here ".")
(define repo-root
  (simplify-path (build-path here 'up)))
(define assembler-path
  (build-path repo-root "pkgs" "webracket-lib" "compiler" "assembler.rkt"))
(define runtime-imports-path
  (build-path repo-root "pkgs" "webracket-lib" "compiler" "runtime-js" "imports.js"))
(define webracket-path
  (build-path repo-root "webracket.rkt"))

;; racket-bin-dir : path?
;;   Locate the directory that anchors relative Racket system paths.
(define racket-bin-dir
  (path-only (find-system-path 'exec-file)))

;; racket-path : path?
;;   Use the Racket executable paired with the test runner.
(define racket-path
  (build-path racket-bin-dir "racket"))

;; raco-path : path?
;;   Use the `raco` executable paired with the test's Racket executable.
(define raco-path
  (build-path (path-only racket-path) "raco"))

;; collects-root : path?
;;   Use the collection root that matches the test's Racket executable.
(define collects-root
  (simplify-path
   (path->complete-path (find-system-path 'collects-dir)
                        racket-bin-dir)))

;; skip! : string? -> none
;;   Exit cleanly with a visible skip reason instead of failing misleadingly.
(define (skip! message)
  (displayln (string-append "SKIP: " message))
  (exit 0))

(unless (file-exists? racket-path)
  (skip! "could not find a Racket executable"))

(unless (file-exists? raco-path)
  (skip! "could not find a raco executable"))

(unless (directory-exists? collects-root)
  (skip! (format "collects directory not found: ~a" collects-root)))

;; build-env : path? -> environment-variables?
;;   Construct an isolated subprocess environment for compiler checks.
(define (build-env compiled-root)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   env
   #"PLTCOLLECTS"
   (string->bytes/utf-8
    (format "~a:~a"
            (path->string (build-path repo-root "pkgs"))
            (path->string collects-root))))
  (environment-variables-set!
   env
   #"PLTCOMPILEDROOTS"
   (string->bytes/utf-8 (path->string compiled-root)))
  env)

;; run/collect : path? (listof string?) path? environment-variables? exact-positive-integer? -> values
;;   Run one subprocess, capture its output, and enforce a timeout.
(define (run/collect executable arguments cwd env timeout-seconds)
  (parameterize ([current-directory cwd]
                 [current-environment-variables env])
    (define-values (process stdout stdin stderr)
      (apply subprocess #f #f #f (path->string executable) arguments))
    (close-output-port stdin)
    (define stdout-channel (make-channel))
    (define stderr-channel (make-channel))
    (thread
     (lambda ()
       (channel-put
        stdout-channel
        (with-handlers ([exn:fail? (lambda (_error) "")])
          (port->string stdout)))))
    (thread
     (lambda ()
       (channel-put
        stderr-channel
        (with-handlers ([exn:fail? (lambda (_error) "")])
          (port->string stderr)))))
    (unless (sync/timeout timeout-seconds process)
      (subprocess-kill process #t)
      (sync process)
      (error 'run/collect
             (string-append
              "subprocess timed out\n"
              (channel-get stdout-channel)
              (channel-get stderr-channel))))
    (subprocess-wait process)
    (values (subprocess-status process)
            (string-append (channel-get stdout-channel)
                           (channel-get stderr-channel)))))

;; run-raco-make! : environment-variables? -> void?
;;   Rebuild the assembler through the ordinary `raco make` route.
(define (run-raco-make! env)
  (define-values (status output)
    (run/collect raco-path
                 (list "make" (path->string assembler-path))
                 repo-root
                 env
                 240))
  (unless (zero? status)
    (error 'run-raco-make!
           (format "assembler rebuild failed\n--- output ---\n~a" output))))

;; compile-browser! : path? path? path? path? environment-variables? -> void?
;;   Compile a module through the regular browser `--dest` route.
(define (compile-browser! source-path dest-dir provider-cache-dir runtime-cache-dir env)
  (define-values (status output)
    (run/collect racket-path
                 (list (path->string webracket-path)
                       "--browser"
                       "--dest" (path->string dest-dir)
                       "--provider-cache-dir" (path->string provider-cache-dir)
                       "--runtime-provider-cache-dir" (path->string runtime-cache-dir)
                       (path->string source-path))
                 repo-root
                 env
                 360))
  (unless (zero? status)
    (error 'compile-browser!
           (format "browser compile failed\n--- output ---\n~a" output))))

;; write-bytes! : path? bytes? -> void?
;;   Replace path with bytes, including a restored generated-host source file.
(define (write-bytes! path content)
  (call-with-output-file path
    (lambda (out)
      (write-bytes content out))
    #:exists 'truncate/replace))

;; write-source! : path? string? -> void?
;;   Write a temporary WebRacket module.
(define (write-source! path source)
  (call-with-output-file path
    (lambda (out)
      (display source out))
    #:exists 'truncate/replace))

;; assembler-dependency-path : path? -> (or/c path? #f)
;;   Find the compilation-manager dependency file for the isolated assembler.
(define (assembler-dependency-path compiled-root)
  (for/first ([path (in-list (find-files file-exists? compiled-root))]
              #:when (equal? (path->string (file-name-from-path path))
                             "assembler_rkt.dep"))
    path))

;; host-path : path? -> path?
;;   Return the browser host path for the fixed temporary source name.
(define (host-path dest-dir)
  (build-path dest-dir "runtime-js-freshness.html"))

(define temp-root
  (make-temporary-file "webracket-runtime-js-freshness-~a" 'directory))
(define compiled-root (build-path temp-root "compiled"))
(define provider-cache-dir (build-path temp-root "provider-cache"))
(define runtime-cache-dir (build-path temp-root "runtime-provider-cache"))
(define dest-dir (build-path temp-root "dest"))
(define source-path (build-path temp-root "runtime-js-freshness.rkt"))
(define env (build-env compiled-root))
(define original-runtime-imports (file->bytes runtime-imports-path))
(define marker-a "runtime-js-freshness-marker-a")
(define marker-b "runtime-js-freshness-marker-b")

(make-directory* compiled-root)
(make-directory* provider-cache-dir)
(make-directory* runtime-cache-dir)
(make-directory* dest-dir)
(write-source! source-path "#lang webracket\n#t\n")

(dynamic-wind
  void
  (lambda ()
    ;; The first output bundle establishes the ordinary destination cache.
    (write-bytes!
     runtime-imports-path
     (bytes-append original-runtime-imports
                   (string->bytes/utf-8 (string-append "\n// " marker-a "\n"))))
    (run-raco-make! env)
    (define dep-path (assembler-dependency-path compiled-root))
    (check-true (path? dep-path) "assembler dependency file was not generated")
    (check-true (regexp-match? #rx"imports[.]js" (file->string dep-path))
                "assembler dependency file does not list imports.js")
    (compile-browser! source-path dest-dir provider-cache-dir runtime-cache-dir env)
    (check-true (file-exists? (host-path dest-dir)) "browser host was not generated")
    (check-true (regexp-match? (regexp marker-a) (file->string (host-path dest-dir)))
                "initial browser host did not contain its runtime JavaScript marker")

    ;; Keep the same destination and caches. A JavaScript-only change must
    ;; rebuild the assembler and must not materialize the stale output bundle.
    (write-bytes!
     runtime-imports-path
     (bytes-append original-runtime-imports
                   (string->bytes/utf-8 (string-append "\n// " marker-b "\n"))))
    (run-raco-make! env)
    (compile-browser! source-path dest-dir provider-cache-dir runtime-cache-dir env)
    (define regenerated-host (file->string (host-path dest-dir)))
    (check-true (regexp-match? (regexp marker-b) regenerated-host)
                "regenerated browser host did not contain the new runtime JavaScript marker")
    (check-false (regexp-match? (regexp marker-a) regenerated-host)
                 "compiled-output cache restored the old browser host"))
  (lambda ()
    (write-bytes! runtime-imports-path original-runtime-imports)
    (with-handlers ([exn:fail:filesystem? void])
      (delete-directory/files temp-root))))

(module+ main
  (displayln "ok"))
