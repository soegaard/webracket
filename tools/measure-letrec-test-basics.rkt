#lang racket/base

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

;; strategy-result : symbol? boolean? real? exact-nonnegative-integer? boolean? string? string? path-string? -> strategy-result?
;;   Summary of compile/run results for one letrec strategy.
(struct strategy-result (name compile-ok? compile-ms wat-size run-ok? saw-#f? stdout stderr dest-dir)
  #:transparent)

;; repo-dir : path?
;;   Absolute path to the repository root.
(define-runtime-path repo-dir "..")
(define repo-root
  (simplify-path repo-dir))

;; make-suite-path : string? -> path?
;;   Construct the absolute path to a test suite in `test/`.
(define (make-suite-path name)
  (build-path repo-root "test" name))

;; ensure-executable : string? -> path?
;;   Resolve an executable from PATH or fail with a helpful error.
(define (ensure-executable name)
  (or (find-executable-path name)
      (error 'measure-letrec-test-basics
             "required executable not found in PATH: ~a"
             name)))

;; run-command/capture : path-string? path-string? string? ... -> (values boolean? string? string? real?)
;;   Run a command under `cwd`, capturing stdout/stderr and elapsed wall time in ms.
(define (run-command/capture cwd exe . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define t0  (current-inexact-milliseconds))
  (define ok?
    (parameterize ([current-directory cwd]
                   [current-output-port out]
                   [current-error-port err])
      (apply system* exe args)))
  (define t1 (current-inexact-milliseconds))
  (values ok?
          (get-output-string out)
          (get-output-string err)
          (- t1 t0)))

;; compile-strategy : path? symbol? path? path? path? -> strategy-result?
;;   Compile and run one suite for one letrec strategy in `dest-dir`.
(define (compile-strategy source-file strategy racket-exe node-exe dest-dir)
  (when (directory-exists? dest-dir)
    (delete-directory/files dest-dir))
  (make-directory* dest-dir)
  (define source-base
    (file-name-from-path source-file))
  (define strategy-flag
    (case strategy
      [(basic)   "--letrec-basic"]
      [(waddell) "--letrec-waddell"]
      [else
       (error 'compile-strategy
              "unknown strategy: ~a"
              strategy)]))
  (define-values (compile-ok? compile-stdout compile-stderr compile-ms)
    (run-command/capture repo-root
                         racket-exe
                         "webracket.rkt"
                         "--no-stdlib"
                         strategy-flag
                         "--dest"
                         (path->string dest-dir)
                         (path->string source-file)))
  (define wat-path
    (build-path dest-dir
                (path-replace-extension source-base ".wat")))
  (define js-path
    (build-path dest-dir
                (path-replace-extension source-base ".js")))
  (define wat-size
    (if (file-exists? wat-path)
        (file-size wat-path)
        0))
  (cond
    [compile-ok?
     (define-values (run-ok? run-stdout run-stderr _run-ms)
       (run-command/capture dest-dir
                            node-exe
                            (path->string js-path)))
     (strategy-result strategy
                      #t
                      compile-ms
                      wat-size
                      run-ok?
                      (regexp-match? #rx"#f" run-stdout)
                      run-stdout
                      (string-append compile-stderr run-stderr)
                      dest-dir)]
    [else
     (strategy-result strategy
                      #f
                      compile-ms
                      wat-size
                      #f
                      #t
                      compile-stdout
                      compile-stderr
                      dest-dir)]))

;; run-strategy-in-thread : path? symbol? path? path? path? box? -> thread?
;;   Start one strategy measurement in a thread and store the result in `result-box`.
(define (run-strategy-in-thread source-file strategy racket-exe node-exe dest-dir result-box)
  (thread
   (lambda ()
     (set-box! result-box
               (compile-strategy source-file strategy racket-exe node-exe dest-dir)))))

;; result-status : strategy-result? -> string?
;;   Short textual status for a strategy result.
(define (result-status r)
  (cond
    [(not (strategy-result-compile-ok? r)) "compile failed"]
    [(not (strategy-result-run-ok? r))     "run failed"]
    [(strategy-result-saw-#f? r)           "output contains #f"]
    [else                                  "ok"]))

;; format-duration-ms : real? -> string?
;;   Format milliseconds as minutes plus seconds.
(define (format-duration-ms ms)
  (define sign (if (negative? ms) "-" ""))
  (define abs-ms (abs ms))
  (define total-seconds (/ abs-ms 1000.0))
  (define minutes
    (inexact->exact
     (floor (/ total-seconds 60.0))))
  (define seconds (- total-seconds (* minutes 60.0)))
  (if (zero? minutes)
      (format "~a~as" sign (~r seconds #:precision '(= 1)))
      (format "~a~am ~as"
              sign
              minutes
              (~r seconds #:precision '(= 1)))))

;; format-bytes->mb : exact-nonnegative-integer? -> string?
;;   Format a byte count in MB (10^6 bytes).
(define (format-bytes->mb n)
  (format "~a MB"
          (~r (/ n 1000000.0) #:precision '(= 3))))

;; report-result : strategy-result? -> void?
;;   Print a compact summary for one strategy.
(define (report-result r)
  (printf "~a\n" (string-titlecase (symbol->string (strategy-result-name r))))
  (printf "  status:       ~a\n" (result-status r))
  (printf "  compile time: ~a\n" (format-duration-ms (strategy-result-compile-ms r)))
  (printf "  wat size:     ~a\n" (format-bytes->mb (strategy-result-wat-size r)))
  (printf "  dest:         ~a\n" (path->string (strategy-result-dest-dir r)))
  (unless (string=? (string-trim (strategy-result-stderr r)) "")
    (printf "  stderr:\n")
    (for ([line (in-list (string-split (string-trim (strategy-result-stderr r)) "\n"))])
      (printf "    ~a\n" line))))

;; measure-suite : path? string? -> boolean?
;;   Measure one suite under both strategies; return #t when both succeed.
(define (measure-suite source-file suite-name)
  (define racket-exe (ensure-executable "racket"))
  (define node-exe   (ensure-executable "node"))
  (define suite-stem
    (path->string (path-replace-extension (file-name-from-path source-file) "")))
  (define basic-dir
    (build-path repo-root "tmp" (format "measure-~a-basic" suite-stem)))
  (define waddell-dir
    (build-path repo-root "tmp" (format "measure-~a-waddell" suite-stem)))
  (define basic-box   (box #f))
  (define waddell-box (box #f))
  (define basic-thread
    (run-strategy-in-thread source-file 'basic racket-exe node-exe basic-dir basic-box))
  (define waddell-thread
    (run-strategy-in-thread source-file 'waddell racket-exe node-exe waddell-dir waddell-box))
  (thread-wait basic-thread)
  (thread-wait waddell-thread)
  (define basic   (unbox basic-box))
  (define waddell (unbox waddell-box))
  (printf "Measured ~a\n\n" (path->string source-file))
  (report-result basic)
  (newline)
  (report-result waddell)
  (newline)
  (printf "Delta\n")
  (printf "  compile time: ~a\n"
          (format-duration-ms
           (- (strategy-result-compile-ms waddell)
              (strategy-result-compile-ms basic))))
  (printf "  wat size:     ~a MB\n"
          (~r (/ (- (strategy-result-wat-size waddell)
                    (strategy-result-wat-size basic))
                 1000000.0)
              #:precision '(= 6)))
  (newline)
  (define ok?
    (and (strategy-result-compile-ok? basic)
         (strategy-result-compile-ok? waddell)
         (strategy-result-run-ok? basic)
         (strategy-result-run-ok? waddell)
         (not (strategy-result-saw-#f? basic))
         (not (strategy-result-saw-#f? waddell))))
  (unless ok?
    (printf "Stopped after ~a failed.\n" suite-name))
  ok?)

;; main : -> void?
;;   Run the focused letrec gate first, then measure `test-basics.rkt` if it passes.
(define (main)
  (define letrec-ok?
    (measure-suite (make-suite-path "test-letrec.rkt")
                   "test-letrec.rkt"))
  (unless letrec-ok?
    (exit 1))
  (measure-suite (make-suite-path "test-basics.rkt")
                 "test-basics.rkt"))

(main)
