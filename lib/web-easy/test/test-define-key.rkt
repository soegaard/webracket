#lang webracket

;;;
;;; web-easy define/key tests
;;;

;; Focused tests for `define/key`.

(include/reader "../define.rkt" read-syntax/skip-first-line)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

;; Baseline example.
(define/key (foo x #:bar [bar 42])
  bar)
(check-equal (foo 3) 42 "foo default keyword")
(check-equal (foo 3 #:bar 11) 11 "foo provided keyword")

;; Multiple optional keywords.
(define/key (sum+ x #:a [a 10] #:b [b 20] #:c [c 30])
  (+ x a b c))
(check-equal (sum+ 1) 61 "all defaults")
(check-equal (sum+ 1 #:c 3 #:a 2) 26 "reverse-order provided keywords")
(check-equal (sum+ 1 #:b 7) 48 "single provided keyword")

;; First-class use in value position.
(define f foo)
(check-equal (f 3) 42 "first-class procedure value")

;; Single evaluation checks.
(define eval-count 0)
(define (tick v)
  (set! eval-count (+ eval-count 1))
  v)

(define/key (single-eval x #:k [k 5])
  (+ x k))

(set! eval-count 0)
(check-equal (single-eval (tick 10) #:k (tick 7)) 17 "single evaluation with provided keyword")
(check-equal eval-count 2 "provided positional+keyword each evaluated once")

(set! eval-count 0)
(check-equal (single-eval (tick 10)) 15 "single evaluation with default keyword")
(check-equal eval-count 1 "only positional evaluated when keyword omitted")

(displayln "PASS define/key")
