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

;; Required keyword argument (no default).
(define/key (needs-k x #:k k)
  (+ x k))
(check-equal (needs-k 4 #:k 9) 13 "required keyword argument")

;; Positional optional arguments.
(define/key (opt-pos x [y 10] [z (+ y 1)])
  (+ x y z))
(check-equal (opt-pos 1) 22 "optional positional defaults")
(check-equal (opt-pos 1 2) 6 "optional positional one provided")
(check-equal (opt-pos 1 2 3) 6 "optional positional all provided")

;; Rest arguments.
(define/key (resty x . r)
  (list x r))
(check-equal (resty 1) '(1 ()) "rest no extras")
(check-equal (resty 1 2 3) '(1 (2 3)) "rest collects extras")

;; Mixed positional optional + rest + keyword forms.
(define/key (mixed a [b 10] #:k [k 1] #:req req . r)
  (list a b r k req))
(check-equal (mixed 5 #:req 9) '(5 10 () 1 9) "mixed defaults")
(check-equal (mixed 5 6 7 8 #:k 2 #:req 9) '(5 6 (7 8) 2 9) "mixed full call")

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
