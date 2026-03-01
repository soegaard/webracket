#lang racket/base

(require racket/file
         racket/path
         racket/string
         racket/system)

;; test-run-exit-code : -> void
;;   Ensure `webracket.rkt -r` propagates runtime failure as non-zero exit.
(define (test-run-exit-code)
  (define repo-root (simplify-path (build-path (current-directory) "..")))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define failing-program
    (make-temporary-file "webracket-run-exit-~a.rkt"))

  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file failing-program
        (lambda (out)
          (displayln "(error 'repro \"intentional failure\")" out))
        #:exists 'truncate/replace)

      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-output-port stdout]
                       [current-error-port stderr])
          (system*/exit-code
           (find-executable-path "racket")
           (path->string webracket-rkt)
           "-r"
           (path->string failing-program))))

      (unless (= status 1)
        (error 'test-run-exit-code
               (format "expected exit code 1, got ~a" status)))

      (define all-output
        (string-append (get-output-string stdout)
                       (get-output-string stderr)))
      (unless (positive? (string-length all-output))
        (error 'test-run-exit-code
               "expected failure output, got empty output"))
      (unless (or (string-contains? all-output "intentional failure")
                  (string-contains? all-output "Uncaught WebAssembly exception"))
        (error 'test-run-exit-code
               "expected failure signal in output")))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file failing-program)))))

(module+ main
  (test-run-exit-code)
  (displayln "ok"))
