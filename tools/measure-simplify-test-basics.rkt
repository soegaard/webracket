#lang racket/base

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

;; mode-result : symbol? boolean? real? exact-nonnegative-integer? boolean? boolean? string? string? path-string? -> mode-result?
;;   Summary of compile/run results for one simplify mode.
(struct mode-result (name compile-ok? compile-ms wat-size run-ok? saw-#f? stdout stderr dest-dir)
  #:transparent)

;; suite-config : string? path? path? (listof string?) boolean? -> suite-config?
;;   Benchmark configuration for one source program.
(struct suite-config (name source-file cwd compile-args run-under-node?)
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

;; make-example-path : string? string? -> path?
;;   Construct the absolute path to an example source file.
(define (make-example-path dir name)
  (build-path repo-root "examples" dir name))

;; ensure-executable : string? -> path?
;;   Resolve an executable from PATH or fail with a helpful error.
(define (ensure-executable name)
  (or (find-executable-path name)
      (error 'measure-simplify-test-basics
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

;; mode-flag : symbol? -> (listof string?)
;;   Command-line flags for one simplify mode.
(define (mode-flag mode)
  (case mode
    [(baseline) '()]
    [(simplify) '("--enable-simplify")]
    [else
     (error 'mode-flag
            "unknown simplify mode: ~a"
            mode)]))

;; compile-mode : suite-config? symbol? path? path? path? -> mode-result?
;;   Compile and optionally run one benchmark under one simplify mode in `dest-dir`.
(define (compile-mode suite mode racket-exe node-exe dest-dir)
  (when (directory-exists? dest-dir)
    (delete-directory/files dest-dir))
  (make-directory* dest-dir)
  (define source-file
    (suite-config-source-file suite))
  (define source-base
    (file-name-from-path source-file))
  (define compile-args
    (append (list (path->string (build-path repo-root "webracket.rkt")))
            (suite-config-compile-args suite)
            (mode-flag mode)
            (list "--dest"
                  (path->string dest-dir)
                  (path->string source-file))))
  (define-values (compile-ok? compile-stdout compile-stderr compile-ms)
    (apply run-command/capture (suite-config-cwd suite) racket-exe compile-args))
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
       (if (suite-config-run-under-node? suite)
           (run-command/capture dest-dir
                                node-exe
                                (path->string js-path))
           (values #t "" "" 0.0)))
     (mode-result mode
                  #t
                  compile-ms
                  wat-size
                  run-ok?
                  (regexp-match? #rx"#f" run-stdout)
                  run-stdout
                  (string-append compile-stderr run-stderr)
                  dest-dir)]
    [else
     (mode-result mode
                  #f
                  compile-ms
                  wat-size
                  #f
                  #t
                  compile-stdout
                  compile-stderr
                  dest-dir)]))

;; run-mode-in-thread : suite-config? symbol? path? path? path? box? -> thread?
;;   Start one simplify mode measurement in a thread and store the result in `result-box`.
(define (run-mode-in-thread suite mode racket-exe node-exe dest-dir result-box)
  (thread
   (lambda ()
     (set-box! result-box
               (compile-mode suite mode racket-exe node-exe dest-dir)))))

;; result-status : mode-result? -> string?
;;   Short textual status for a mode result.
(define (result-status r)
  (cond
    [(not (mode-result-compile-ok? r)) "compile failed"]
    [(not (mode-result-run-ok? r))     "run failed"]
    [(mode-result-saw-#f? r)           "output contains #f"]
    [else                              "ok"]))

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

;; report-result : mode-result? -> void?
;;   Print a compact summary for one simplify mode.
(define (report-result r)
  (printf "~a\n" (string-titlecase (symbol->string (mode-result-name r))))
  (printf "  status:       ~a\n" (result-status r))
  (printf "  compile time: ~a\n" (format-duration-ms (mode-result-compile-ms r)))
  (printf "  wat size:     ~a\n" (format-bytes->mb (mode-result-wat-size r)))
  (printf "  dest:         ~a\n" (path->string (mode-result-dest-dir r)))
  (unless (string=? (string-trim (mode-result-stderr r)) "")
    (printf "  stderr:\n")
    (for ([line (in-list (string-split (string-trim (mode-result-stderr r)) "\n"))])
      (printf "    ~a\n" line))))

;; report-delta : mode-result? mode-result? -> void?
;;   Print compile-time and size deltas between two mode results.
(define (report-delta base-r other-r)
  (printf "  compile time: ~a\n"
          (format-duration-ms
           (- (mode-result-compile-ms other-r)
              (mode-result-compile-ms base-r))))
  (printf "  wat size:     ~a MB\n"
          (~r (/ (- (mode-result-wat-size other-r)
                    (mode-result-wat-size base-r))
                 1000000.0)
              #:precision '(= 6))))

;; format-percent-delta : real? real? -> string?
;;   Format the percentage change from `base` to `other`.
(define (format-percent-delta other base)
  (cond
    [(zero? base) "n/a"]
    [else
     (format "~a%"
             (~r (* 100.0 (/ (- other base) base))
                 #:precision '(= 1)))]))

;; report-delta-percentages : mode-result? mode-result? -> void?
;;   Print percentage deltas between two mode results.
(define (report-delta-percentages base-r other-r)
  (printf "  compile time: ~a\n"
          (format-percent-delta (mode-result-compile-ms other-r)
                                (mode-result-compile-ms base-r)))
  (printf "  wat size:     ~a\n"
          (format-percent-delta (mode-result-wat-size other-r)
                                (mode-result-wat-size base-r))))

;; mode-order : (listof symbol?)
;;   Simplify modes to measure for each suite.
(define mode-order '(baseline simplify))

;; mode-dest-dir : string? symbol? -> path?
;;   Destination directory for one suite/mode measurement.
(define (mode-dest-dir suite-stem mode)
  (build-path repo-root "tmp" (format "measure-~a-~a" suite-stem mode)))

;; measure-suite : suite-config? -> boolean?
;;   Measure one suite under all configured simplify modes; return #t when all succeed.
(define (measure-suite suite)
  (define racket-exe (ensure-executable "racket"))
  (define node-exe   (ensure-executable "node"))
  (define source-file
    (suite-config-source-file suite))
  (define suite-name
    (suite-config-name suite))
  (define suite-stem
    (path->string (path-replace-extension (file-name-from-path source-file) "")))
  (define result-boxes
    (for/list ([mode (in-list mode-order)])
      (cons mode (box #f))))
  (define threads
    (for/list ([mode (in-list mode-order)]
               [entry (in-list result-boxes)])
      (run-mode-in-thread suite
                          mode
                          racket-exe
                          node-exe
                          (mode-dest-dir suite-stem mode)
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
                #:when (eq? 'baseline (mode-result-name r)))
      r))
  (define simplify
    (for/first ([r (in-list results)]
                #:when (eq? 'simplify (mode-result-name r)))
      r))
  (when (and baseline simplify)
    (printf "Delta vs Baseline (Simplify)\n")
    (report-delta baseline simplify)
    (printf "  percentages:\n")
    (report-delta-percentages baseline simplify)
    (newline))
  (define ok?
    (for/and ([r (in-list results)])
      (and (mode-result-compile-ok? r)
           (mode-result-run-ok? r)
           (not (mode-result-saw-#f? r)))))
  (unless ok?
    (printf "Stopped after ~a failed.\n" suite-name))
  ok?)

;; main : -> void?
;;   Run the focused suite first, then measure `test-basics.rkt` if it passes.
(define (main)
  (define test-letrec-suite
    (suite-config "test-letrec.rkt"
                  (make-suite-path "test-letrec.rkt")
                  repo-root
                  '("--no-stdlib")
                  #t))
  (define test-basics-suite
    (suite-config "test-basics.rkt"
                  (make-suite-path "test-basics.rkt")
                  repo-root
                  '("--no-stdlib")
                  #t))
  (define pict-suite
    (suite-config "examples/pict/pict.rkt"
                  (make-example-path "pict" "pict.rkt")
                  (build-path repo-root "examples" "pict")
                  '("--browser" "--ffi" "dom" "--ffi" "standard")
                  #f))
  (define simplify-ok?
    (measure-suite test-letrec-suite))
  (unless simplify-ok?
    (exit 1))
  (define basics-ok?
    (measure-suite test-basics-suite))
  (unless basics-ok?
    (exit 1))
  (measure-suite pict-suite))

(main)
