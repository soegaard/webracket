#lang racket/base

(require racket/file
         racket/path
         racket/string
         racket/system)

;; test-sxml-lib-run : -> void
;;   Validate loading `libs/sxml.rkt` via include/reader + skip-first-line.
(define (test-sxml-lib-run)
  (define cwd (current-directory))
  (define repo-root
    (if (file-exists? (build-path cwd "webracket.rkt"))
        (simplify-path cwd)
        (simplify-path (build-path cwd ".."))))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define main-file (build-path repo-root "test" "test-sxml-lib.rkt"))

  (define (run-r path-string)
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define exit-code
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory repo-root])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string webracket-rkt)
         "-r"
         path-string)))
    (values exit-code
            (string-append (get-output-string stdout)
                           (get-output-string stderr))))

  (define-values (status out-str) (run-r (path->string main-file)))
  (unless (zero? status)
    (error 'test-sxml-lib-run
           (format "expected exit code 0, got ~a; output: ~a"
                   status
                   out-str)))
  (unless (string-contains? out-str "12345")
    (error 'test-sxml-lib-run
           (format "expected sentinel output, got: ~a" out-str))))

(module+ main
  (test-sxml-lib-run)
  (displayln "ok"))
