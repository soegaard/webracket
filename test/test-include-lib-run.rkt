#lang racket/base

(require racket/path
         racket/file
         racket/string
         racket/system)

;; test-include-lib-run : -> void
;;   Ensure `(require-lib define)` works in `webracket.rkt -r` flow
;;   for the main/include chain repro.
(define (test-include-lib-run)
  (define repo-root (simplify-path (build-path (current-directory) "..")))
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

  ;; Regression: direct keyword call shape should not fail with
  ;; "#%datum: keyword misused as an expression".
  (let ()
    (define tmp-dir (make-temporary-file "webracket-include-lib-keyword-~a" 'directory))
    (define kw-main (build-path tmp-dir "kw-main.rkt"))
    (dynamic-wind
      void
    (lambda ()
      (call-with-output-file kw-main
        (lambda (out)
          (displayln "(require-lib define)" out)
          (displayln "(define/key (f x #:id [id #f]) x)" out)
          (displayln "(f 1 #:id \"x\")" out))
        #:exists 'truncate/replace)
      (let-values ([(kw-status kw-output) (run-r (path->string kw-main))])
        (unless (zero? kw-status)
          (error 'test-include-lib-run
                 (format "keyword-call repro failed (~a): ~a" kw-status kw-output)))
        (when (string-contains? kw-output "#%datum: keyword misused as an expression")
          (error 'test-include-lib-run
                 (format "keyword call still misparsed: ~a" kw-output)))))
      (lambda ()
        (with-handlers ([exn:fail:filesystem? void])
          (delete-directory/files tmp-dir)))))

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
