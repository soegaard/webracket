#lang racket/base

(require racket/file
         racket/path
         racket/string
         racket/system)

;; test-dest-output : -> void
;;   Ensure `--dest` routes default outputs into the chosen directory.
(define (test-dest-output)
  (define repo-root
    (simplify-path (build-path (current-directory) "..")))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))

  (define (run-webracket args cwd)
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define status
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory cwd])
        (apply system*/exit-code
               (find-executable-path "racket")
               (path->string webracket-rkt)
               args)))
    (values status
            (string-append (get-output-string stdout)
                           (get-output-string stderr))))

  (define (write-program! path text)
    (call-with-output-file path
      (lambda (out) (display text out))
      #:exists 'truncate/replace))

  (define (assert-file-exists who path)
    (unless (file-exists? path)
      (error who (format "expected file to exist: ~a" path))))

  (define (assert-file-missing who path)
    (when (file-exists? path)
      (error who (format "expected file to be absent: ~a" path))))

  (define (default-output-paths base dest-dir browser?)
    (define host-ext (if browser? ".html" ".js"))
    (list (build-path dest-dir (path-add-extension base ".wat"))
          (build-path dest-dir (path-add-extension base ".wasm"))
          (build-path dest-dir (path-add-extension base ".wasm.map.sexp"))
          (build-path dest-dir (path-add-extension base host-ext))))

  (define source-dir (make-temporary-file "webracket-dest-src-~a" 'directory))
  (define dest-node-dir (make-temporary-file "webracket-dest-node-~a" 'directory))
  (define dest-explicit-dir (make-temporary-file "webracket-dest-explicit-out-~a" 'directory))
  (define dest-browser-dir (make-temporary-file "webracket-dest-browser-~a" 'directory))
  (define source-path (build-path source-dir "program.rkt"))
  (define browser-source-path (build-path source-dir "browser-program.rkt"))
  (define explicit-host-path (build-path dest-explicit-dir "custom-runtime.js"))

  (dynamic-wind
    void
    (lambda ()
      (write-program! source-path "(displayln 12345)\n")
      (let-values ([(node-status node-output)
                    (run-webracket (list "--dest" (path->string dest-node-dir)
                                         (path->string source-path))
                                   source-dir)])
        (unless (zero? node-status)
          (error 'test-dest-output
                 (format "node dest compile failed (~a): ~a"
                         node-status node-output)))

        (let* ([node-base (path-replace-extension (file-name-from-path source-path) #"")]
               [node-outputs (default-output-paths node-base dest-node-dir #f)])
          (for ([path (in-list node-outputs)])
            (assert-file-exists 'test-dest-output path))

          (for ([path (in-list (list (build-path source-dir "program.wat")
                                     (build-path source-dir "program.wasm")
                                     (build-path source-dir "program.wasm.map.sexp")
                                     (build-path source-dir "program.js")))])
            (assert-file-missing 'test-dest-output path))

          (let-values ([(explicit-status explicit-output)
                        (run-webracket (list "--dest" (path->string dest-explicit-dir)
                                             "--host-file" (path->string explicit-host-path)
                                             (path->string source-path))
                                       source-dir)])
            (unless (zero? explicit-status)
              (error 'test-dest-output
                     (format "explicit host compile failed (~a): ~a"
                             explicit-status explicit-output)))
            (assert-file-exists 'test-dest-output explicit-host-path)
            (assert-file-missing 'test-dest-output (build-path dest-explicit-dir "program.js"))))

        (write-program! browser-source-path "(displayln 67890)\n")
        (let-values ([(browser-status browser-output)
                      (run-webracket (list "--browser"
                                           "--dest" (path->string dest-browser-dir)
                                           (path->string browser-source-path))
                                     source-dir)])
          (unless (zero? browser-status)
            (error 'test-dest-output
                   (format "browser dest compile failed (~a): ~a"
                           browser-status browser-output)))
          (let* ([browser-base (path-replace-extension (file-name-from-path browser-source-path) #"")]
                 [browser-outputs (default-output-paths browser-base dest-browser-dir #t)])
            (for ([path (in-list browser-outputs)])
              (assert-file-exists 'test-dest-output path))
            (for ([path (in-list (list (build-path source-dir "browser-program.wat")
                                       (build-path source-dir "browser-program.wasm")
                                       (build-path source-dir "browser-program.wasm.map.sexp")
                                       (build-path source-dir "browser-program.html")))])
              (assert-file-missing 'test-dest-output path))
            (assert-file-missing 'test-dest-output (build-path dest-browser-dir "browser-program.js"))))))
    (lambda ()
      (for ([path (in-list (list explicit-host-path
                                 source-path
                                 browser-source-path))])
        (with-handlers ([exn:fail:filesystem? void])
          (when (file-exists? path)
            (delete-file path))))
      (for ([dir (in-list (list dest-explicit-dir
                                dest-node-dir
                                dest-browser-dir
                                source-dir))])
        (with-handlers ([exn:fail:filesystem? void])
          (delete-directory/files dir))))))

(module+ main
  (test-dest-output)
  (displayln "ok"))
