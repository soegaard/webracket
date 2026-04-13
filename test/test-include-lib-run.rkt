#lang racket/base

(require racket/path
         racket/file
         racket/string
         racket/system)

;; test-include-lib-run : -> void
;;   Ensure `(require-lib define)` works in `webracket.rkt -r` flow
;;   for the main/include chain repro.
(define (test-include-lib-run)
  (define (find-repo-root start)
    (let loop ([dir (simplify-path start)])
      (cond
        [(file-exists? (build-path dir "webracket.rkt"))
         dir]
        [else
         (define parent (path-only dir))
         (unless parent
           (error 'test-include-lib-run
                  "could not find repository root containing webracket.rkt"))
         (loop (path->complete-path parent))])))
  (define repo-root
    (find-repo-root (or (current-load-relative-directory)
                        (current-directory))))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define (run-r path-string [cwd repo-root])
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define status
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory cwd])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string webracket-rkt)
         "-r"
         path-string)))
    (values status
            (string-append (get-output-string stdout)
                           (get-output-string stderr))))

  (define (run-browser path-string [cwd repo-root])
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define status
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory cwd])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string webracket-rkt)
         "--browser"
         path-string)))
    (values status
            (string-append (get-output-string stdout)
                           (get-output-string stderr))))

  (define repro-main
    (build-path repo-root
                "lib" "web-easy" "tmp" "repro-include-lib-main-only-view.rkt"))
  (define repro-base (path-replace-extension repro-main #""))
  (define stale-outputs
    (list (path-add-extension repro-base ".wat")
          (path-add-extension repro-base ".wasm")
          (path-add-extension repro-base ".js")
          (path-add-extension repro-base ".html")
          (path-add-extension repro-base ".wasm.map.sexp")))

  (for ([p (in-list stale-outputs)])
    (with-handlers ([exn:fail:filesystem? void])
      (when (file-exists? p) (delete-file p))))

  (define-values (status out-str) (run-r (path->string repro-main)))
  (unless (zero? status)
    (error 'test-include-lib-run
           (format "expected exit code 0, got ~a; stderr: ~a"
                   status
                   out-str)))
  (unless (string-contains? out-str "1")
    (error 'test-include-lib-run
           (format "expected output to contain 1, got: ~a" out-str)))

  ;; Regression: `(include-lib web-easy)` should resolve the collection
  ;; entrypoint in `webracket.rkt -r`.
  (let ()
    (define web-easy-main (make-temporary-file "webracket-include-lib-web-easy-~a.rkt"))
    (dynamic-wind
      void
      (lambda ()
        (call-with-output-file web-easy-main
          (lambda (out)
            (displayln "(include-lib web-easy)" out)
            (displayln "(displayln \"hello, web-easy\")" out))
          #:exists 'truncate/replace)
        (let-values ([(web-easy-status web-easy-output)
                      (run-r (path->string web-easy-main))])
          (unless (zero? web-easy-status)
            (error 'test-include-lib-run
                   (format "web-easy include-lib repro failed (~a): ~a"
                           web-easy-status web-easy-output)))
          (unless (string-contains? web-easy-output "hello, web-easy")
            (error 'test-include-lib-run
                   (format "web-easy include-lib repro missing sentinel: ~a"
                           web-easy-output)))))
      (lambda ()
        (with-handlers ([exn:fail:filesystem? void])
          (delete-file web-easy-main)))))

  ;; Regression: browser mode should select `web-easy/main-browser.rkt`.
  ;; We reference a browser-only helper so the wrong entrypoint would fail.
  (let ()
    (define web-easy-browser
      (make-temporary-file "webracket-include-lib-web-easy-browser-~a.rkt"))
    (define web-easy-browser-base (path-replace-extension web-easy-browser #""))
    (define browser-outputs
      (list (path-add-extension web-easy-browser-base ".wat")
            (path-add-extension web-easy-browser-base ".wasm")
            (path-add-extension web-easy-browser-base ".js")
            (path-add-extension web-easy-browser-base ".html")
            (path-add-extension web-easy-browser-base ".wasm.map.sexp")))
    (dynamic-wind
      void
      (lambda ()
        (for ([p (in-list browser-outputs)])
          (with-handlers ([exn:fail:filesystem? void])
            (when (file-exists? p) (delete-file p))))
        (call-with-output-file web-easy-browser
          (lambda (out)
            (displayln "(include-lib web-easy)" out)
            (displayln "(displayln (message-event-data (js-eval \"({data: 'hello'})\")))" out))
          #:exists 'truncate/replace)
        (let-values ([(browser-status browser-output)
                      (run-browser (path->string web-easy-browser))])
          (unless (zero? browser-status)
            (error 'test-include-lib-run
                   (format "web-easy browser include-lib repro failed (~a): ~a"
                           browser-status browser-output)))
          (unless (file-exists? (path-add-extension web-easy-browser-base ".html"))
            (error 'test-include-lib-run
                   (format "web-easy browser include-lib repro missing html output: ~a"
                           web-easy-browser)))))
      (lambda ()
        (for ([p (in-list browser-outputs)])
          (with-handlers ([exn:fail:filesystem? void])
            (when (file-exists? p) (delete-file p))))
        (with-handlers ([exn:fail:filesystem? void])
          (delete-file web-easy-browser)))))

  ;; Regression: include-lib should resolve library source from the collection,
  ;; even when compiling a source file outside the repository.
  (let ()
    (define outside-src (build-path repo-root "test" "test-sxml-lib.rkt"))
    (define outside-copy (make-temporary-file "webracket-include-lib-outside-~a.rkt"))
    (dynamic-wind
      void
      (lambda ()
        (copy-file outside-src outside-copy #t)
        (define outside-cwd (find-system-path 'temp-dir))
        (let-values ([(outside-status outside-output)
                      (run-r (path->string outside-copy) outside-cwd)])
          (unless (zero? outside-status)
            (error 'test-include-lib-run
                   (format "outside include-lib repro failed (~a): ~a"
                           outside-status outside-output)))
          (unless (string-contains? outside-output "12345")
            (error 'test-include-lib-run
                   (format "outside include-lib repro missing sentinel: ~a"
                           outside-output)))))
      (lambda ()
        (with-handlers ([exn:fail:filesystem? void])
          (delete-file outside-copy)))))

  ;; Regression: `(include-lib audio)` should work in `webracket.rkt -r`
  ;; without requiring an explicit `--ffi audio` flag.
  (let ()
    (define audio-main (make-temporary-file "webracket-include-lib-audio-~a.rkt"))
    (dynamic-wind
      void
      (lambda ()
        (call-with-output-file audio-main
          (lambda (out)
            (displayln "(include-lib audio)" out)
            (displayln "(begin (audio-context? #f) 12345)" out))
          #:exists 'truncate/replace)
        (let-values ([(audio-status audio-output)
                      (run-r (path->string audio-main))])
          (unless (zero? audio-status)
            (error 'test-include-lib-run
                   (format "audio include-lib repro failed (~a): ~a"
                           audio-status audio-output)))
          (unless (string-contains? audio-output "12345")
            (error 'test-include-lib-run
                   (format "audio include-lib repro missing sentinel: ~a"
                           audio-output)))))
      (lambda ()
        (with-handlers ([exn:fail:filesystem? void])
          (delete-file audio-main)))))

  )

(module+ main
  (test-include-lib-run)
  (displayln "ok"))
