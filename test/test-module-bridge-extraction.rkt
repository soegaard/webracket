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
         (error 'test-module-bridge-extraction
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
         (error 'test-module-bridge-extraction
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

;; run-bridge-extraction-smoke : path? path? -> void?
;;   Assert that module-bridge extraction succeeds before Wasm compilation.
(define (run-bridge-extraction-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-module-bridge-extraction-~a" 'directory))
  (define script-path (build-path base "bridge-extract-smoke.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path
    (build-path repo-root "pkgs" "web-easy" "examples" "hello-world" "hello-world.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(unless (pair? (webracket/base-local-module-graph-reports graph))\n" out)
          (display "  (error 'bridge-extract-smoke \"expected local reports\"))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "module bridge extraction failed (~a): ~a" status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-imported-binding-extraction-smoke : path? path? -> void?
;;   Assert that referenced imports from extracted providers resolve during
;;   module-bridge extraction.
(define (run-imported-binding-extraction-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-module-bridge-import-binding-~a" 'directory))
  (define script-path (build-path base "bridge-extract-import-binding.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path (build-path base "repro-web-easy-text-binding.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require web-easy)\n" out)
          (display "text\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(unless (pair? (webracket/base-local-module-graph-reports graph))\n" out)
          (display "  (error 'bridge-extract-import-binding \"expected local reports\"))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "imported binding extraction failed (~a): ~a" status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-browser-dom-no-io-format-support-smoke : path? path? -> void?
;;   Assert that a module-language `browser/dom` root does not seed staged
;;   `io/format` support providers unless the bridge explicitly asked for them.
(define (run-browser-dom-no-io-format-support-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-browser-dom-no-io-format-~a" 'directory))
  (define script-path (build-path base "browser-dom-no-io-format.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path (build-path base "repro-browser-dom-42.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require browser/dom)\n" out)
          (display "42\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require racket/list\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\")\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "webify" "module-registry.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(define registry-names\n" out)
          (display "  (sort (webify-module-registry-names\n" out)
          (display "         (webracket/base-local-module-graph-registry graph))\n" out)
          (display "        symbol<?))\n" out)
          (display "(define forbidden\n" out)
          (display "  '(map-shim\n" out)
          (display "    racket-src/io/format/main-web\n" out)
          (display "    racket-src/io/format/main-web.rkt\n" out)
          (display "    racket-src/io/format/printf-adapter.rkt\n" out)
          (display "    racket-src/io/format/printf-web.rkt\n" out)
          (display "    racket-src/racket/format-web\n" out)
          (display "    racket/format\n" out)
          (display "    webracket/contract\n" out)
          (display "    racket/contract/base\n" out)
          (display "    racket/contract/combinator\n" out)
          (display "    racket/contract/region\n" out)
          (display "    racket/in-out\n" out)
          (display "    racket/module-boundary-ctc\n" out)
          (display "    racket/contract/private/base\n" out)
          (display "    racket/contract/private/blame\n" out)
          (display "    racket/contract/private/guts\n" out)
          (display "    racket/contract/private/object\n" out)
          (display "    racket/contract/private/object-c-wrapper))\n" out)
          (display "(for ([name (in-list forbidden)])\n" out)
          (display "  (when (member name registry-names)\n" out)
          (display "    (error 'browser-dom-no-io-format\n" out)
          (display "           \"unexpected staged io/format support provider\"\n" out)
          (display "           name)))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "browser/dom extraction seeded staged io/format support (~a): ~a"
                       status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-web-pict-no-erased-contract-support-smoke : path? path? -> void?
;;   Assert that a tiny `web-pict` root does not seed erased-contract support
;;   providers or broad string/math/match helpers when no composed module actually
;;   depends on them.
(define (run-web-pict-no-erased-contract-support-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-web-pict-no-contract-support-~a" 'directory))
  (define script-path (build-path base "web-pict-no-contract-support.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path (build-path base "repro-web-pict-require-42.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require web-pict)\n" out)
          (display "42\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require racket/list\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\")\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "webify" "module-registry.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(define registry-names\n" out)
          (display "  (sort (webify-module-registry-names\n" out)
          (display "         (webracket/base-local-module-graph-registry graph))\n" out)
          (display "        symbol<?))\n" out)
          (display "(define forbidden\n" out)
          (display "  '(webracket/contract\n" out)
          (display "    racket/contract/base\n" out)
          (display "    racket/contract/combinator\n" out)
          (display "    racket/contract/region\n" out)
          (display "    racket/in-out\n" out)
          (display "    racket/module-boundary-ctc\n" out)
          (display "    racket/contract/private/base\n" out)
          (display "    racket/contract/private/blame\n" out)
          (display "    racket/contract/private/guts\n" out)
          (display "    racket/contract/private/object\n" out)
          (display "    racket/contract/private/object-c-wrapper\n" out)
          (display "    racket/string\n" out)
          (display "    racket/math\n" out)
          (display "    racket/match/runtime\n" out)
          (display "    racket/private/string))\n" out)
          (display "(for ([name (in-list forbidden)])\n" out)
          (display "  (when (member name registry-names)\n" out)
          (display "    (error 'web-pict-no-contract-support\n" out)
          (display "           \"unexpected erased-contract support provider\"\n" out)
          (display "           name)))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "web-pict extraction seeded erased-contract support (~a): ~a"
                       status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-browser-canvas-unsafe-undefined-smoke : path? path? -> void?
;;   Assert that browser/canvas roots can resolve the lazy unsafe-undefined
;;   support provider during module-bridge extraction.
(define (run-browser-canvas-unsafe-undefined-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-browser-canvas-unsafe-undefined-~a" 'directory))
  (define script-path (build-path base "browser-canvas-unsafe-undefined.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path (build-path base "repro-browser-canvas-42.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require browser/canvas)\n" out)
          (display "42\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(unless (pair? (webracket/base-local-module-graph-reports graph))\n" out)
          (display "  (error 'browser-canvas-unsafe-undefined \"expected local reports\"))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "browser/canvas extraction could not resolve unsafe-undefined (~a): ~a"
                       status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-staged-read-list-extraction-smoke : path? path? -> void?
;;   Assert that user-facing staged `read` ownership does not leak into support
;;   provider extraction and create a registry cycle through list/for helpers.
(define (run-staged-read-list-extraction-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-module-bridge-staged-read-list-~a" 'directory))
  (define script-path (build-path base "bridge-extract-staged-read-list.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define source-path (build-path base "repro-staged-read-list.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require racket/list)\n" out)
          (display "(read (open-input-string \"42\"))\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require racket/set\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\")\n" out)
          (display "         (only-in (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "private" "parameters.rkt")) out)
          (display "\") current-provider-owned-primitive-aliases))\n" out)
          (display "(parameterize ([current-provider-owned-primitive-aliases\n" out)
          (display "                (list->seteq\n" out)
          (display "                 (provider-owned-primitive-alias-symbols\n" out)
          (display "                  #:expander-read-provider? #t\n" out)
          (display "                  #:io-format-provider? #f))])\n" out)
          (display "  (define graph\n" out)
          (display "    (webracket/base-file->local-module-graph\n" out)
          (display "     \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "  (unless (pair? (webracket/base-local-module-graph-reports graph))\n" out)
          (display "    (error 'bridge-extract-staged-read-list \"expected local reports\"))\n" out)
          (display "  (write (webracket/base-local-module-graph-root graph))\n" out)
          (display "  (newline))\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "staged read + list extraction failed (~a): ~a" status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-local-sibling-module-extraction-smoke : path? path? -> void?
;;   Assert that a root module can require a local sibling module while also
;;   pulling installed providers such as `web-easy`.
(define (run-local-sibling-module-extraction-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-local-sibling-bridge-~a" 'directory))
  (define script-path (build-path base "local-sibling-bridge.rkt"))
  (define compiled-root (build-path base "compiled"))
  (define helper-path (build-path base "helper.rkt"))
  (define source-path (build-path base "root.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file helper-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(provide answer)\n" out)
          (display "(define answer 42)\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file source-path
        (lambda (out)
          (display "#lang webracket/base\n" out)
          (display "(require web-easy\n" out)
          (display "         \"helper.rkt\")\n" out)
          (display "answer\n" out))
        #:exists 'truncate/replace)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\"))\n" out)
          (display "(define graph\n" out)
          (display "  (webracket/base-file->local-module-graph\n" out)
          (display "   \"" out)
          (display (path->string source-path) out)
          (display "\"))\n" out)
          (display "(unless (pair? (webracket/base-local-module-graph-reports graph))\n" out)
          (display "  (error 'local-sibling-bridge \"expected local reports\"))\n" out)
          (display "(write (webracket/base-local-module-graph-root graph))\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "local sibling extraction failed (~a): ~a" status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

;; run-racket-math-nan-provider-extraction-smoke : path? path? -> void?
;;   Assert that a provider compile stub retains an already-mapped re-export.
(define (run-racket-math-nan-provider-extraction-smoke repo-root racket-exe)
  (define base (make-temporary-file "webracket-racket-math-nan-provider-~a" 'directory))
  (define script-path (build-path base "bridge-extract-racket-math-nan.rkt"))
  (define compiled-root (build-path base "compiled"))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* compiled-root)
      (call-with-output-file script-path
        (lambda (out)
          (display "#lang racket/base\n" out)
          (display "(require (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "driver.rkt")) out)
          (display "\")\n" out)
          (display "         (file \"" out)
          (display (path->string (build-path repo-root "pkgs" "webracket-lib" "compiler" "webify" "module-extractor.rkt")) out)
          (display "\"))\n" out)
          (display "(define namespace (make-webify-contract-erasing-namespace))\n" out)
          (display "(define resolver (make-webify-contract-erasing-module-name-resolver))\n" out)
          (display "(unless (memq 'nan?\n" out)
          (display "              (provider-public-export-symbols 'racket/math namespace resolver))\n" out)
          (display "  (error 'racket-math-nan-provider \"missing provider export: nan?\"))\n" out)
          (display "(write 'nan?)\n" out)
          (display "(newline)\n" out))
        #:exists 'truncate/replace)
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-directory repo-root]
                       [current-output-port stdout]
                       [current-error-port stderr]
                       [current-environment-variables (env-with-collections repo-root compiled-root)])
          (system*/exit-code racket-exe (path->string script-path))))
      (define output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (zero? status)
        (error 'test-module-bridge-extraction
               (format "racket/math nan? provider extraction failed (~a): ~a"
                       status output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files base)))))

(module+ main
  (define repo-root
    (find-repo-root (or (current-load-relative-directory)
                        (current-directory))))
  (define racket-exe (repo-racket-executable repo-root))
  (run-bridge-extraction-smoke repo-root racket-exe)
  (run-imported-binding-extraction-smoke repo-root racket-exe)
  (run-racket-math-nan-provider-extraction-smoke repo-root racket-exe)
  (run-local-sibling-module-extraction-smoke repo-root racket-exe)
  (run-browser-dom-no-io-format-support-smoke repo-root racket-exe)
  (run-browser-canvas-unsafe-undefined-smoke repo-root racket-exe)
  (run-web-pict-no-erased-contract-support-smoke repo-root racket-exe)
  (run-staged-read-list-extraction-smoke repo-root racket-exe))
