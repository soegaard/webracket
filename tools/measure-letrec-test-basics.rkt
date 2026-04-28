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
  (define-values (compile-ok? compile-stdout compile-stderr compile-ms)
    (run-command/capture repo-root
                         racket-exe
                         "webracket.rkt"
                         "--no-stdlib"
                         (strategy-flag strategy)
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

;; report-delta : strategy-result? strategy-result? -> void?
;;   Print compile-time and size deltas between two strategy results.
(define (report-delta base-r other-r)
  (printf "  compile time: ~a\n"
          (format-duration-ms
           (- (strategy-result-compile-ms other-r)
              (strategy-result-compile-ms base-r))))
  (printf "  wat size:     ~a MB\n"
          (~r (/ (- (strategy-result-wat-size other-r)
                    (strategy-result-wat-size base-r))
                 1000000.0)
              #:precision '(= 6))))

;; strategy-order : (listof symbol?)
;;   Letrec strategies to measure for each suite.
(define strategy-order '(basic waddell scc))

;; strategy-flag : symbol? -> string?
;;   Command-line flag for one letrec strategy.
(define (strategy-flag strategy)
  (case strategy
    [(basic)   "--letrec-basic"]
    [(waddell) "--letrec-waddell"]
    [(scc)     "--letrec-scc"]
    [else
     (error 'strategy-flag
            "unknown strategy: ~a"
            strategy)]))

;; strategy-dest-dir : string? symbol? -> path?
;;   Destination directory for one suite/strategy measurement.
(define (strategy-dest-dir suite-stem strategy)
  (build-path repo-root "tmp" (format "measure-~a-~a" suite-stem strategy)))

;; measure-suite : path? string? -> boolean?
;;   Measure one suite under all configured strategies; return #t when all succeed.
(define (measure-suite source-file suite-name)
  (define racket-exe (ensure-executable "racket"))
  (define node-exe   (ensure-executable "node"))
  (define suite-stem
    (path->string (path-replace-extension (file-name-from-path source-file) "")))
  (define result-boxes
    (for/list ([strategy (in-list strategy-order)])
      (cons strategy (box #f))))
  (define threads
    (for/list ([strategy (in-list strategy-order)]
               [entry (in-list result-boxes)])
      (run-strategy-in-thread source-file
                              strategy
                              racket-exe
                              node-exe
                              (strategy-dest-dir suite-stem strategy)
                              (cdr entry))))
  (for ([th (in-list threads)])
    (thread-wait th))
  (define results
    (for/list ([entry (in-list result-boxes)])
      (unbox (cdr entry))))
  (printf "Measured ~a\n\n" (path->string source-file))
  (for ([r (in-list results)]
        [i (in-naturals)])
    (when (positive? i)
      (newline))
    (report-result r))
  (newline)
  (define baseline
    (for/first ([r (in-list results)]
                #:when (eq? 'basic (strategy-result-name r)))
      r))
  (when baseline
    (for ([r (in-list results)]
          #:unless (eq? 'basic (strategy-result-name r)))
      (printf "Delta vs Basic (~a)\n"
              (string-titlecase (symbol->string (strategy-result-name r))))
      (report-delta baseline r)
      (newline)))
  (define waddell
    (for/first ([r (in-list results)]
                #:when (eq? 'waddell (strategy-result-name r)))
      r))
  (define scc
    (for/first ([r (in-list results)]
                #:when (eq? 'scc (strategy-result-name r)))
      r))
  (when (and waddell scc)
    (printf "Delta vs Waddell (Scc)\n")
    (report-delta waddell scc)
    (newline))
  (define ok?
    (for/and ([r (in-list results)])
      (and (strategy-result-compile-ok? r)
           (strategy-result-run-ok? r)
           (not (strategy-result-saw-#f? r)))))
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
