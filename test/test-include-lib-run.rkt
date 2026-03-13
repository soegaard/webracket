#lang racket/base

(require racket/path
         racket/file
         racket/string
         racket/system)

;; test-include-lib-run : -> void
;;   Ensure `(include-lib define)` works in `webracket.rkt -r` flow
;;   for the main/include chain repro.
(define (test-include-lib-run)
  (define repo-root (simplify-path (build-path (current-directory) "..")))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define (run-r path-string)
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define status
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory repo-root])
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
  (define tmp-dir (make-temporary-file "webracket-include-lib-keyword-~a" 'directory))
  (define kw-main (build-path tmp-dir "kw-main.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file kw-main
        (lambda (out)
          (displayln "(include-lib define)" out)
          (displayln "(define/key (f x #:id [id #f]) x)" out)
          (displayln "(f 1 #:id \"x\")" out))
        #:exists 'truncate/replace)
      (define-values (kw-status kw-output) (run-r (path->string kw-main)))
      (unless (zero? kw-status)
        (error 'test-include-lib-run
               (format "keyword-call repro failed (~a): ~a" kw-status kw-output)))
      (when (string-contains? kw-output "#%datum: keyword misused as an expression")
        (error 'test-include-lib-run
               (format "keyword call still misparsed: ~a" kw-output))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files tmp-dir)))))

(module+ main
  (test-include-lib-run)
  (displayln "ok"))
