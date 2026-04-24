#lang racket

;; Experimental runner for trying RacketScript's integration tests with
;; WebRacket. It creates a reference copy runnable by Racket and a WebRacket
;; copy with the language line stripped, then compares stdout.

(require file/glob
         racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string)

(define root-dir
  (make-parameter
   (let ([default "/tmp/racketscript-src/tests"])
     (and (directory-exists? default) default))))
(define work-dir       (make-parameter "tmp/racketscript-tests"))
(define limit          (make-parameter #f))
(define verbose?       (make-parameter #f))
(define keep-going?    (make-parameter #t))
(define job-count      (make-parameter 8))
(define use-stdlib?    (make-parameter #t))
(define flush-output-port? (make-parameter #t))
(define print-top-level-results? (make-parameter #t))
(define instrument-list-markers? (make-parameter #f))
(define list-stop-before-marker (make-parameter #f))
(define list-only-marker (make-parameter #f))
(define webracket-file (make-parameter "webracket.rkt"))

(struct run-result (status stdout stderr) #:transparent)
(struct test-result (source reference rewritten status detail racket-result webracket-result)
  #:transparent)

(define (path-basename-string p)
  (path->string (file-name-from-path p)))

(define (skip-test-file? p)
  (define basename (path-basename-string p))
  (or (string-prefix? basename "__")
      ;; WebRacket does not support regular expressions yet.
      (string=? basename "rx.rkt")))

(define (normalize-output s)
  (regexp-replace* #rx"\r\n?" s "\n"))

(define (normalize-webracket-stdout s)
  ;; The WebRacket runner prints the final result value; RacketScript's fixture
  ;; compares only program stdout.
  (regexp-replace #rx"\n?#<void>\n?$" (normalize-output s) ""))

(define (run/capture exe args #:cwd [cwd (current-directory)])
  (define-values (proc stdout _stdin stderr)
    (parameterize ([current-directory cwd])
      (apply subprocess #f #f #f exe args)))
  (subprocess-wait proc)
  (run-result (subprocess-status proc)
              (normalize-output (port->string stdout))
              (normalize-output (port->string stderr))))

(define (racket-run source)
  (run/capture (or (find-executable-path "racket")
                   (error 'run-racketscript-tests "could not find racket"))
               (list (path->string source))))

(define (webracket-run source)
  (define args
    (append (list (webracket-file)
                  (if (use-stdlib?) "--stdlib" "--no-stdlib"))
            (if (print-top-level-results?)
                (list "--print-top-level-results")
                '())
            (list "-r"
                  (path->string source))))
  (define result
    (run/capture (or (find-executable-path "racket")
                     (error 'run-racketscript-tests "could not find racket"))
                 args))
  (run-result (run-result-status result)
              (normalize-webracket-stdout (run-result-stdout result))
              (run-result-stderr result)))

(define (relative-test-path source)
  (define root
    (or (root-dir)
        (error 'run-racketscript-tests
               "no RacketScript test root configured; use --root")))
  (find-relative-path (simple-form-path root) (simple-form-path source)))

(define (write-rewritten dest content rewrite-lang-line)
  (make-directory* (or (path-only dest) (work-dir)))
  (call-with-output-file dest
    #:exists 'replace
    (lambda (out) (display (rewrite-lang-line content) out)))
  dest)

(define (reference-rewrite content)
  (cond
    [(regexp-match? #rx"^#lang +racketscript/base[^\n]*(\n|$)" content)
     (regexp-replace #rx"^#lang[^\n]*(\n|$)" content "#lang racket/base\n")]
    [(regexp-match? #rx"^#lang +racketscript[^\n]*(\n|$)" content)
     (regexp-replace #rx"^#lang[^\n]*(\n|$)" content "#lang racket\n")]
    [else content]))

(define test-utils-require-rx
  #rx"\"(?:\\.\\./)+test-utils\\.rkt\"[ \t]*")

(define empty-require-rx
  #rx"\\(require[ \t\r\n]*\\)[ \t]*\n?")

(define test-utils-shim
  (string-append
   ";; Inlined from RacketScript's test-utils.rkt for WebRacket.\n"
   "(define-syntax run-if-version\n"
   "  (syntax-rules ()\n"
   "    [(_ v test ...) (begin test ...)]))\n"
   "(define exn:application:mismatch? exn:fail:contract?)\n"
   "(define exn:application:type? exn:fail:contract?)\n"
   "(define exn:application:arity? exn:fail:contract:arity?)\n"
   "(define (test expect fun . args)\n"
   "  (define res (if (procedure? fun) (apply fun args) (car args)))\n"
   "  (displayln (equal? expect res)))\n"
   "(define-syntax err/rt-test\n"
   "  (syntax-rules ()\n"
   "    [(_ e)\n"
   "     (with-handlers ([(lambda (x) #t)\n"
   "                      (lambda (ex) (displayln (exn-message ex)))])\n"
   "       e)]\n"
   "    [(_ e e?)\n"
   "     (with-handlers ([e? (lambda (ex) (displayln (exn-message ex)))])\n"
   "       e)]\n"
   "    [(_ e e? msg-rx)\n"
   "     (with-handlers ([e? (lambda (ex) (displayln (exn-message ex)))])\n"
   "       e)]))\n"))

(define list-debug-markers
  '(("A" . #f)
    ("B" . ";; ---------- last, last-pair ----------")
    ("C" . ";; ---------- sort ----------")
    ("D" . ";; ---------- make-list ----------")
    ("E" . ";; ---------- take/drop/splt-at[-right] ----------")
    ("F" . ";; ---------- append* ----------")
    ("G" . ";; ---------- flatten ----------")
    ("H" . ";; ---------- add-between ----------")
    ("I" . ";; ---------- check-duplicates ----------")
    ("J" . ";; ---------- filter and filter-not ----------")
    ("K" . ";; ---------- partition ----------")
    ("L" . ";; ---------- filter-map ----------")
    ("M" . ";; ---------- count ----------")
    ("N" . ";; ---------- append-map ----------")
    ("O" . ";; ---------- shuffle ----------")
    ("P" . ";; ---------- argmin & argmax ----------")
    ("Q" . ";; ---------- range ----------")
    ("R" . ";; ---------- group-by ----------")
    ("S" . ";; ---------- cartesian-product ----------")
    ("T" . ";; ---------- list-update ----------")
    ("U" . ";; ---------- list-set ----------")
    ("V" . ";; ---------- list prefix functions ----------")
    ("W" . ";; ---------- remf / remf* ----------")
    ("X" . ";; ---------- index(es)-of / index(es)-where ----------")))

(define (list-marker-prefix label)
  (string-append
   (if (equal? (list-stop-before-marker) label)
       (format "(error \"STOP before marker ~a\")\n" label)
       "")
   (format "(js-log ~s)\n" label)))

(define (insert-list-debug-markers content)
  (for/fold ([marked (string-append (list-marker-prefix "A") content)])
            ([marker (in-list (cdr list-debug-markers))])
    (define label  (car marker))
    (define header (cdr marker))
    (string-replace marked header (string-append (list-marker-prefix label) header))))

(define (maybe-instrument-list rel content)
  (if (and (instrument-list-markers?)
           (equal? (path->string rel) "racket-core/list.rkt"))
      (insert-list-debug-markers content)
      content))

(define (webracket-rewrite rel content)
  (define uses-test-utils?
    (regexp-match? test-utils-require-rx content))
  (define without-test-utils
    (regexp-replace* empty-require-rx
                     (regexp-replace* test-utils-require-rx content "")
                     ""))
  (define without-lang
    (cond
      [(regexp-match? #rx"^#lang[^\n]*(\n|$)" without-test-utils)
       (regexp-replace #rx"^#lang[^\n]*(\n|$)" without-test-utils "")]
      [else without-test-utils]))
  (define with-markers
    (maybe-instrument-list rel without-lang))
  (define with-inlined-test-utils
    (if uses-test-utils?
        (string-append test-utils-shim "\n" with-markers)
        with-markers))
  (if (flush-output-port?)
      (string-append
       with-inlined-test-utils
       "\n"
       ;; WebRacket writes display/write output to the current output string
       ;; port. Flush it through js-log so the host runner can compare stdout.
       "(let ([rs-output (get-output-string (current-output-port))])\n"
       "  (unless (string=? rs-output \"\")\n"
       "    (js-log rs-output)))\n")
      with-inlined-test-utils))

(define list-sort-tests-rx
  #px";; ---------- sort ----------[\\s\\S]*?(?=;; ---------- make-list ----------)")

(define list-sequence-tests-rx
  #px";; ---------- take/drop/splt-at\\[-right\\] ----------[\\s\\S]*?(?=;; ---------- append\\* ----------)")

(define list-test-prologue-rx
  #px"^#lang[^\n]*\n(?:\\(require[^\n]*\\)\n)?\n?")

(define (list-marker-labels)
  (map car list-debug-markers))

(define (valid-list-marker? label)
  (member label (list-marker-labels)))

(define (string-match-start content needle)
  (define m (regexp-match-positions (regexp (regexp-quote needle)) content))
  (and m (caar m)))

(define (list-test-prologue content)
  (define m (regexp-match-positions list-test-prologue-rx content))
  (if m
      (substring content 0 (cdar m))
      ""))

(define (slice-list-test-to-marker content label)
  (define entries list-debug-markers)
  (define index
    (for/first ([entry (in-list entries)]
                [i     (in-naturals)]
                #:when (equal? (car entry) label))
      i))
  (unless index
    (raise-user-error 'run-racketscript-tests
                      "unknown list marker ~a; expected one of ~a"
                      label
                      (string-join (list-marker-labels) ", ")))
  (define prologue (list-test-prologue content))
  (define start
    (cond
      [(zero? index) (string-length prologue)]
      [else
       (define header (cdr (list-ref entries index)))
       (or (string-match-start content header)
           (raise-user-error 'run-racketscript-tests
                             "could not find list marker ~a header" label))]))
  (define end
    (or
     (for/first ([entry (in-list (list-tail entries (add1 index)))]
                 #:do [(define header (cdr entry))]
                 #:when header
                 #:do [(define pos (string-match-start content header))]
                 #:when pos)
       pos)
     (string-length content)))
  (string-append prologue (substring content start end)))

(define (maybe-slice-list-test rel content)
  (if (and (list-only-marker)
           (equal? (path->string rel) "racket-core/list.rkt"))
      (slice-list-test-to-marker content (list-only-marker))
      content))

(define (skip-known-unsupported-tests rel content)
  (cond
    [(equal? (path->string rel) "racket-core/list.rkt")
     ;; WebRacket's `sort` keyword forms and sequence-based list operations
     ;; are not supported yet, so keep the runner focused on supported bugs.
     (maybe-slice-list-test
      rel
      (regexp-replace
       list-sequence-tests-rx
       (regexp-replace list-sort-tests-rx
                       content
                       ";; ---------- sort ----------\n;; Skipped by WebRacket RacketScript runner.\n")
       ";; ---------- take/drop/splt-at[-right] ----------\n;; Skipped by WebRacket RacketScript runner.\n"))]
    [else content]))

(define (copy-reference-support-files!)
  (define root (root-dir))
  (when root
    (define test-utils (build-path root "test-utils.rkt"))
    (when (file-exists? test-utils)
      (define dest (build-path (work-dir) "reference" "test-utils.rkt"))
      (make-directory* (or (path-only dest) (work-dir)))
      (copy-file test-utils dest #t))))

(define (materialize-source source)
  (copy-reference-support-files!)
  (define rel (relative-test-path source))
  (define reference-dest (build-path (work-dir) "reference" rel))
  (define webracket-dest (build-path (work-dir) "webracket" rel))
  (define content (skip-known-unsupported-tests rel (file->string source)))
  (values (write-rewritten reference-dest content reference-rewrite)
          (write-rewritten webracket-dest content
                           (lambda (content) (webracket-rewrite rel content)))))

(define (path->test-files p)
  (cond
    [(file-exists? p)      (list p)]
    [(directory-exists? p) (sort (glob (build-path p "*.rkt")) path<?)]
    [else                 (sort (glob p) path<?)]))

(define (collect-tests specs)
  (define files
    (remove-duplicates
     (append*
      (for/list ([spec (in-list specs)])
        (path->test-files (string->path spec))))
     equal?))
  (define runnable (filter-not skip-test-file? files))
  (match (limit)
    [(? exact-nonnegative-integer? n) (take runnable (min n (length runnable)))]
    [_ runnable]))

(define (run-one source)
  (define-values (reference rewritten) (materialize-source source))
  (define racket-result (racket-run reference))
  (define webracket-result (webracket-run rewritten))
  (define status
    (cond
      [(not (zero? (run-result-status racket-result)))    'racket-crash]
      [(not (zero? (run-result-status webracket-result))) 'webracket-crash]
      [(equal? (run-result-stdout racket-result)
               (run-result-stdout webracket-result))      'pass]
      [else                                               'fail]))
  (define detail
    (case status
      [(pass) "stdout matches"]
      [(fail) "stdout differs"]
      [(racket-crash) "original test crashed under Racket"]
      [(webracket-crash) "rewritten test crashed under WebRacket"]))
  (test-result source reference rewritten status detail racket-result webracket-result))

(define (print-diff-ish expected actual)
  (displayln "  expected stdout:")
  (displayln (if (string=? expected "") "    <empty>" (indent expected)))
  (displayln "  actual stdout:")
  (displayln (if (string=? actual "") "    <empty>" (indent actual))))

(define (indent s)
  (string-append "    " (regexp-replace* #rx"\n" (string-trim s #:right? #f) "\n    ")))

(define (print-result r)
  (match-define (test-result source reference rewritten status detail racket-result webracket-result) r)
  (printf "~a ~a\n" (string-upcase (symbol->string status)) source)
  (when (or (verbose?) (not (eq? status 'pass)))
    (printf "  reference: ~a\n" reference)
    (printf "  rewritten: ~a\n" rewritten)
    (printf "  detail: ~a\n" detail)
    (unless (eq? status 'pass)
      (print-diff-ish (run-result-stdout racket-result)
                      (run-result-stdout webracket-result)))
    (when (or (verbose?) (eq? status 'webracket-crash))
      (unless (string=? (run-result-stderr webracket-result) "")
        (displayln "  webracket stderr:")
        (displayln (indent (run-result-stderr webracket-result)))))))

(define (summarize results)
  (define counts (make-hash))
  (for ([r (in-list results)])
    (hash-update! counts (test-result-status r) add1 0))
  (displayln "")
  (displayln "Summary")
  (for ([status '(pass fail racket-crash webracket-crash)])
    (printf "  ~a: ~a\n" status (hash-ref counts status 0)))
  (define bad-count
    (for/sum ([status '(fail racket-crash webracket-crash)])
      (hash-ref counts status 0)))
  (zero? bad-count))

(define (run-tests/sequential test-files)
  (define results '())
  (for ([source (in-list test-files)])
    (define result (run-one source))
    (set! results (cons result results))
    (print-result result)
    (when (and (not (keep-going?)) (not (eq? (test-result-status result) 'pass)))
      (exit (if (summarize (reverse results)) 0 1))))
  (reverse results))

(define (run-tests/parallel test-files)
  (define total (length test-files))
  (define workers (min (job-count) total))
  (define jobs    (make-channel))
  (define results (make-channel))
  (define done    (gensym 'done))
  (define result-vector (make-vector total #f))

  (for ([worker (in-range workers)])
    (thread
     (lambda ()
       (let loop ()
         (match (channel-get jobs)
           [(== done) (void)]
           [(cons index source)
            (channel-put results (cons index (run-one source)))
            (loop)])))))

  (thread
   (lambda ()
     (for ([source (in-list test-files)]
           [index  (in-naturals)])
       (channel-put jobs (cons index source)))
     (for ([worker (in-range workers)])
       (channel-put jobs done))))

  (for ([index (in-range total)])
    (match-define (cons result-index result) (channel-get results))
    (vector-set! result-vector result-index result))

  (define ordered-results (vector->list result-vector))
  (for ([result (in-list ordered-results)])
    (print-result result))
  ordered-results)

(define specs
  (command-line
   #:program "run-racketscript-tests"
   #:once-each
   [("--root") dir
    "RacketScript test root, default /tmp/racketscript-src/tests"
    (root-dir dir)]
   [("--work-dir") dir
    "Directory for rewritten WebRacket inputs"
    (work-dir dir)]
   [("--limit") n
    "Run at most n tests"
    (define maybe-n (string->number n))
    (unless (and (exact-nonnegative-integer? maybe-n))
      (raise-user-error 'run-racketscript-tests "--limit expects a nonnegative integer"))
    (limit maybe-n)]
   [("--jobs" "-j") n
    "Run n tests in parallel; default 8"
    (define maybe-n (string->number n))
    (unless (and (exact-positive-integer? maybe-n))
      (raise-user-error 'run-racketscript-tests "--jobs expects a positive integer"))
    (job-count maybe-n)]
   [("--stdlib")
    "Compile rewritten tests with WebRacket stdlib"
    (use-stdlib? #t)]
   [("--no-stdlib")
    "Compile rewritten tests without WebRacket stdlib"
    (use-stdlib? #f)]
   [("--no-output-port-flush")
    "Do not append a current-output-port flush to WebRacket copies"
    (flush-output-port? #f)]
   [("--no-print-top-level-results")
    "Do not ask WebRacket to print top-level expression results"
    (print-top-level-results? #f)]
   [("--instrument-list-markers")
    "Insert js-log A/B/C... markers in rewritten racket-core/list.rkt"
    (instrument-list-markers? #t)]
   [("--list-stop-before-marker") marker
    "In rewritten racket-core/list.rkt, raise an error before marker A-X"
    (instrument-list-markers? #t)
    (list-stop-before-marker marker)]
   [("--list-only-marker") marker
    "Only run one A-X section from rewritten racket-core/list.rkt"
    (unless (valid-list-marker? marker)
      (raise-user-error 'run-racketscript-tests
                        "--list-only-marker expects one of ~a"
                        (string-join (list-marker-labels) ", ")))
    (list-only-marker marker)]
   [("--webracket") file
    "Path to webracket.rkt"
    (webracket-file file)]
   [("--verbose" "-v")
    "Show stdout/stderr details for passing tests too"
    (verbose? #t)]
   [("--stop-on-fail")
    "Stop after the first non-passing test"
    (keep-going? #f)]
   #:args specs
   specs))

(define default-specs
  (cond
    [(root-dir) (list (build-path (root-dir) "basic"))]
    [else '()]))

(define test-files
  (collect-tests
   (if (null? specs)
       (map path->string default-specs)
       specs)))

(when (null? test-files)
  (raise-user-error 'run-racketscript-tests
                    "no test files found; pass a test file/directory or --root"))

(make-directory* (work-dir))

(define results
  (if (or (= (job-count) 1) (not (keep-going?)))
      (run-tests/sequential test-files)
      (run-tests/parallel test-files)))

(exit (if (summarize results) 0 1))
