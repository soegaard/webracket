#lang racket/base

(require (file "../compiler.rkt")
         (file "../parameters.rkt")
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string)

;; named-entry : (listof string?) any/c -> named-entry?
;;   One leaf test together with its section path.
(struct named-entry (path expr) #:transparent)

;; repo-dir : path?
;;   Absolute path to the repository root.
(define-runtime-path repo-dir "..")
(define repo-root
  (simplify-path repo-dir))

;; result-tree->entries : any/c -> (listof named-entry?)
;;   Flatten a result-tree test structure into ordered leaf entries.
(define (result-tree->entries tree)
  (define (walk node path)
    (match node
      [`(list ,(? string? name) ,expr)
       (if (and (pair? expr)
                (eq? 'list (car expr)))
           (walk expr (append path (list name)))
           (list (named-entry (append path (list name)) expr)))]
      [`(list ,children ...)
       (append-map (λ (child) (walk child path))
                   children)]
      [_ '()]))
  (walk tree '()))

;; suite-path->entries : path-string? -> (listof named-entry?)
;;   Read one suite file and return its leaf entries.
(define (suite-path->entries suite-path)
  (define tree
    (call-with-input-file suite-path read))
  (result-tree->entries tree))

;; suite-result-has-#f? : any/c -> boolean?
;;   Detect whether a suite leaf result contains a `#f` anywhere.
(define (suite-result-has-#f? v)
  (cond
    [(eq? v #f) #t]
    [(pair? v)
     (or (suite-result-has-#f? (car v))
         (suite-result-has-#f? (cdr v)))]
    [else #f]))

;; suite-result-ok? : any/c -> boolean?
;;   Accept any result that does not contain `#f`.
(define (suite-result-ok? v)
  (not (suite-result-has-#f? v)))

;; check-entry : named-entry? boolean? -> any/c
;;   Evaluate one leaf entry with or without simplify mode.
(define (check-entry entry simplify?)
  (parameterize ([current-enable-simplify? simplify?])
    (run-expr (datum->syntax #f (named-entry-expr entry)))))

;; main : -> void?
;;   Find the first leaf in a suite whose isolated result changes under simplify mode.
(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (unless (or (= (length argv) 1)
              (= (length argv) 2))
    (error 'find-simplify-suite-failure
           "expected SUITE-PATH [START-INDEX]"))
  (define suite-path
    (path->complete-path (car argv) repo-root))
  (define start-index
    (if (= (length argv) 2)
        (let ([n (string->number (cadr argv))])
          (unless (and n (exact-integer? n) (positive? n))
            (error 'find-simplify-suite-failure
                   "START-INDEX must be a positive exact integer"))
          n)
        1))
  (define entries
    (suite-path->entries suite-path))
  (printf "Found ~a leaf tests in ~a\n"
          (length entries)
          (path->string suite-path))
  (for ([entry (in-list (drop entries (sub1 start-index)))]
        [i (in-naturals start-index)])
    (printf "Checking leaf ~a/~a\n" i (length entries))
    (flush-output)
    (define baseline (check-entry entry #f))
    (define simplify (check-entry entry #t))
    (unless (equal? baseline simplify)
      (printf "First failing leaf: ~a\n"
              (string-join (named-entry-path entry) " > "))
      (printf "Baseline: ~s\n" baseline)
      (printf "Simplify: ~s\n" simplify)
      (exit 1)))
  (printf "All isolated leaves matched with and without simplify mode.\n"))

(main)
