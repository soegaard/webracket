#lang racket/base

(require racket/file
         racket/path
         racket/system)

;; find-repo-root : path? -> path?
;;   Find the checkout root that contains `webracket.rkt`.
(define (find-repo-root start)
  (let loop ([dir (simplify-path start)])
    (cond
      [(file-exists? (build-path dir "webracket.rkt"))
       dir]
      [else
       (define parent (path-only dir))
       (unless parent
         (error 'test-module-bridge-local-paths-and-prefix
                "could not find repository root containing webracket.rkt"))
       (loop (path->complete-path parent))])))

;; repo-racket-executable : path? -> path?
;;   Prefer the adjacent Racket checkout with the bridge's expected toolchain.
(define (repo-racket-executable repo-root)
  (define candidate
    (build-path repo-root ".." "racket" "racket" "bin" "racket"))
  (cond
    [(file-exists? candidate) candidate]
    [else
     (or (find-executable-path "racket")
         (error 'test-module-bridge-local-paths-and-prefix
                "could not find a racket executable"))]))

;; env-with-collections : path? path? -> environment-variables?
;;   Build a subprocess environment for the package-root collection layout.
(define (env-with-collections repo-root compiled-root)
  (define env (environment-variables-copy (current-environment-variables)))
  (define racket-collects
    (build-path repo-root ".." "racket" "racket" "collects"))
  (define collects
    (string-append
     (path->string (build-path repo-root "pkgs"))
     ":"
     (path->string racket-collects)))
  (environment-variables-set! env #"PLTCOLLECTS" (string->bytes/utf-8 collects))
  (environment-variables-set! env
                              #"PLTCOMPILEDROOTS"
                              (string->bytes/utf-8 (path->string compiled-root)))
  env)

;; write-file! : path? string? -> void?
;;   Write one temporary source file.
(define (write-file! path content)
  (make-directory* (or (path-only path) (current-directory)))
  (call-with-output-file path
    (lambda (out)
      (display content out))
    #:exists 'truncate/replace))

;; run-local-path-and-prefix-smoke : path? path? -> void?
;;   Reproduce local-path canonicalization and prefix provenance bridge cases.
(define (run-local-path-and-prefix-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-module-bridge-local-paths-~a" 'directory))
  (define src-dir (build-path base "src"))
  (define applet-dir (build-path src-dir "applets"))
  (define compiled-root (build-path "/tmp" "webracket-module-bridge-local-paths-compiled"))
  (define cache-dir (build-path base "provider-cache"))
  (define script-path (build-path base "bridge-local-paths-smoke.rkt"))
  (define geometry-path (build-path src-dir "construction-geometry.rkt"))
  (define bisector-path (build-path applet-dir "perpendicular-bisector.rkt"))
  (define angle-path (build-path applet-dir "angle-bisector.rkt"))
  (define flat-bisector-path (build-path src-dir "flat-perpendicular-bisector.rkt"))
  (define flat-angle-path (build-path src-dir "flat-angle-bisector.rkt"))
  (define nested-dir (build-path src-dir "nested"))
  (define nested-helper-path (build-path nested-dir "helper.rktl"))
  (define nested-wrapper-path (build-path nested-dir "wrapper.rktl"))
  (define nested-main-path (build-path src-dir "main-nested-rktl.rkt"))
  (define api-dir (build-path src-dir "api"))
  (define browser-dir (build-path src-dir "browser"))
  (define public-dir (build-path src-dir "public"))
  (define repro-dir (build-path src-dir "repro"))
  (define api-keybindings-path (build-path api-dir "keybindings.rkt"))
  (define browser-keybindings-path (build-path browser-dir "keybindings.rkt"))
  (define direct-api-path (build-path api-dir "direct.rkt"))
  (define public-main-path (build-path public-dir "main.rkt"))
  (define public-main-repro-path (build-path repro-dir "public-main.rkt"))
  (define file-main-path (build-path src-dir "main-file.rkt"))
  (define subdir-main-path (build-path src-dir "main-subdir-string.rkt"))
  (define prefix-main-path (build-path src-dir "main-prefix-duplicate.rkt"))
  (define flat-prefix-main-path (build-path src-dir "main-flat-prefix-duplicate.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* applet-dir)
      (make-directory* nested-dir)
      (make-directory* api-dir)
      (make-directory* browser-dir)
      (make-directory* public-dir)
      (make-directory* repro-dir)
      (make-directory* compiled-root)
      (make-directory* cache-dir)
      (write-file! geometry-path
                   "#lang webracket/base\n(provide construction-value)\n(define construction-value 10)\n")
      (write-file! bisector-path
                   "#lang webracket/base\n(provide applet-title applet-success?)\n(define applet-title 101)\n(define applet-success? #t)\n")
      (write-file! angle-path
                   "#lang webracket/base\n(provide applet-title applet-success?)\n(define applet-title 202)\n(define applet-success? #f)\n")
      (write-file! flat-bisector-path
                   "#lang webracket/base\n(provide applet-title applet-success?)\n(define applet-title 301)\n(define applet-success? #t)\n")
      (write-file! flat-angle-path
                   "#lang webracket/base\n(provide applet-title applet-success?)\n(define applet-title 302)\n(define applet-success? #f)\n")
      (write-file! nested-helper-path
                   "#lang webracket/base\n(provide nested-value)\n(define nested-value 17)\n")
      (write-file! nested-wrapper-path
                   "#lang webracket/base\n(require \"helper.rktl\")\n(provide wrapped-value)\n(define wrapped-value (+ nested-value 1))\n")
      (write-file! nested-main-path
                   "#lang webracket/base\n(require \"nested/wrapper.rktl\")\nwrapped-value\n")
      (write-file! api-keybindings-path
                   "#lang webracket/base\n(provide api-keybinding)\n(define api-keybinding 11)\n")
      (write-file! browser-keybindings-path
                   "#lang webracket/base\n(provide browser-keybinding)\n(define browser-keybinding 31)\n")
      (write-file! direct-api-path
                   "#lang webracket/base\n(require \"keybindings.rkt\"\n         \"../browser/keybindings.rkt\")\n(provide direct-keybinding-value)\n(define direct-keybinding-value (+ api-keybinding browser-keybinding))\n")
      (write-file! public-main-path
                   "#lang webracket/base\n(require \"../api/direct.rkt\")\n(provide direct-keybinding-value)\n")
      (write-file! public-main-repro-path
                   "#lang webracket/base\n(require \"../public/main.rkt\")\ndirect-keybinding-value\n")
      (write-file! file-main-path
                   "#lang webracket/base\n(require (file \"construction-geometry.rkt\"))\nconstruction-value\n")
      (write-file! subdir-main-path
                   "#lang webracket/base\n(require \"applets/perpendicular-bisector.rkt\")\napplet-title\n")
      (write-file! prefix-main-path
                   "#lang webracket/base\n(require (prefix-in bisector: \"applets/perpendicular-bisector.rkt\")\n         (prefix-in angle: \"applets/angle-bisector.rkt\"))\n(define left bisector:applet-success?)\n(define right angle:applet-success?)\n(if left angle:applet-title bisector:applet-title)\n")
      (write-file! flat-prefix-main-path
                   "#lang webracket/base\n(require (prefix-in bisector: \"flat-perpendicular-bisector.rkt\")\n         (prefix-in angle: \"flat-angle-bisector.rkt\"))\n(define left bisector:applet-success?)\n(define right angle:applet-success?)\n(if left angle:applet-title bisector:applet-title)\n")
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require racket/list\n" out)
          (display "         (file " out)
          (write (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "))\n" out)
          (display "(current-provider-cache-dir " out)
          (write (path->string cache-dir) out)
          (display ")\n" out)
          (display "(current-provider-cache-force? #f)\n" out)
          (display "(current-provider-cache-verbose? #t)\n" out)
          (display "(define failures '())\n" out)
          (display "(define (case-failure! label exn)\n" out)
          (display "  (define message (exn-message exn))\n" out)
          (display "  (printf \"case-failed ~a\\n~a\\n\" label message)\n" out)
          (display "  (set! failures (cons (list label message) failures)))\n" out)
          (display "(define (assert-local label source forbidden)\n" out)
          (display "  (printf \"case-start ~a\\n\" label)\n" out)
          (display "  (with-handlers ([exn:fail? (lambda (exn) (case-failure! label exn))])\n" out)
          (display "    (define-values (root reports local-source-paths)\n" out)
          (display "      (webracket/base-file->local-module-reports source))\n" out)
          (display "    (printf \"case-reports ~a root=~s providers=~s local-sources=~s\\n\"\n" out)
          (display "            label root (module-reports->provider-names reports) local-source-paths)\n" out)
          (display "    (define external (webracket/base-external-provider-names reports))\n" out)
          (display "    (define leaked (filter (lambda (name) (member name external)) forbidden))\n" out)
          (display "    (unless (null? leaked)\n" out)
          (display "      (raise-arguments-error label\n" out)
          (display "                             \"local module escaped into external providers\"\n" out)
          (display "                             \"leaked providers\" leaked\n" out)
          (display "                             \"external providers\" external))\n" out)
          (display "    (define graph\n" out)
          (display "      (webracket/base-reports->module-graph\n" out)
          (display "       root reports #:local-source-paths local-source-paths))\n" out)
          (display "    (printf \"case-ok ~a root=~s external=~s\\n\"\n" out)
          (display "            label\n" out)
          (display "            (webracket/base-local-module-graph-root graph)\n" out)
          (display "            external)))\n" out)
          (display "(assert-local 'file-local-require " out)
          (write (path->string file-main-path) out)
          (display " '(construction-geometry construction-geometry.rkt))\n" out)
          (display "(assert-local 'subdir-string-require " out)
          (write (path->string subdir-main-path) out)
          (display " '(applets/perpendicular-bisector applets/perpendicular-bisector.rkt))\n" out)
          (display "(assert-local 'prefix-duplicate-exports " out)
          (write (path->string prefix-main-path) out)
          (display " '(applets/perpendicular-bisector applets/perpendicular-bisector.rkt applets/angle-bisector applets/angle-bisector.rkt))\n" out)
          (display "(assert-local 'flat-prefix-duplicate-exports " out)
          (write (path->string flat-prefix-main-path) out)
          (display " '(flat-perpendicular-bisector flat-perpendicular-bisector.rkt flat-angle-bisector flat-angle-bisector.rkt))\n" out)
          (display "(assert-local 'nested-rktl-require " out)
          (write (path->string nested-main-path) out)
          (display " '(nested/wrapper.rktl helper.rktl))\n" out)
          (display "(assert-local 'public-main-keybinding-aliases " out)
          (write (path->string public-main-repro-path) out)
          (display " '(api/keybindings api/keybindings.rkt browser/keybindings browser/keybindings.rkt))\n" out)
          (display "(unless (null? failures)\n" out)
          (display "  (raise-arguments-error 'module-bridge-local-paths-and-prefix\n" out)
          (display "                         \"local module path/prefix reproductions failed\"\n" out)
          (display "                         \"failures\" (reverse failures)))\n" out)
          (display "(displayln 'module-bridge-local-paths-and-prefix-ok)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe "-y" (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-local-paths-and-prefix
               (format "module bridge local path/prefix smoke failed (~a): ~a"
                       status
                       output)))
      (unless (regexp-match? #rx"module-bridge-local-paths-and-prefix-ok" output)
        (error 'test-module-bridge-local-paths-and-prefix
               (format "expected success marker: ~a" output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

(module+ main
  (define repo-root
    (find-repo-root (or (current-load-relative-directory)
                        (current-directory))))
  (define racket-exe (repo-racket-executable repo-root))
  (run-local-path-and-prefix-smoke repo-root racket-exe))
