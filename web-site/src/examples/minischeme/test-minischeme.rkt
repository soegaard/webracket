#lang racket

(require rackunit
         rackunit/text-ui
         racket/string)

;; Reuse the page-shared MiniScheme interpreter implementation.
(include "minischeme.rkt")

(define (run src)
  (minischeme-process-input src))

(define (reset!)
  (minischeme-reset-state!))

(define (check-error-match rx src)
  (check-true (regexp-match? rx (run src))))

(define (check-eval-error-match rx src)
  (define out (run src))
  (check-true (regexp-match? #rx"^=> eval error: " out))
  (check-true (regexp-match? rx out)))

(define minischeme-tests
  (test-suite
   "MiniScheme CEK Interpreter"

   (test-case "literal number"
     (reset!)
     (check-equal? (run "42") "=> 42"))

   (test-case "primitive application"
     (reset!)
     (check-equal? (run "(+ 42 1)") "=> 43"))

   (test-case "lambda application"
     (reset!)
     (check-equal? (run "((lambda (x) (+ x 1)) 41)") "=> 42"))

   (test-case "variadic lambda (rest-only)"
     (reset!)
     (check-equal? (run "((lambda xs xs) 1 2 3)") "=> (1 2 3)"))

   (test-case "variadic lambda (dotted formals)"
     (reset!)
     (check-equal? (run "((lambda (x . rest) rest) 1 2 3)") "=> (2 3)"))

   (test-case "variadic lambda arity lower bound"
     (reset!)
     (check-error-match
      #rx"arity mismatch: expected at least 2 arguments, got 1"
      "((lambda (x y . rest) x) 1)"))

   (test-case "fixed lambda arity mismatch"
     (reset!)
     (check-error-match
      #rx"arity mismatch: expected 2 arguments, got 1"
      "((lambda (x y) x) 1)"))

   (test-case "let"
     (reset!)
     (check-equal? (run "(let ((x 2) (y 3)) (+ x y))") "=> 5"))

   (test-case "if without else produces void"
     (reset!)
     (check-equal? (run "(if #f 1)") "=> #<void>"))

   (test-case "define + use in same input"
     (reset!)
     (check-equal? (run "(define x 10)\n(+ x 5)") "=> 15"))

   (test-case "definition persists across runs"
     (reset!)
     (void (run "(define x 10)"))
     (check-equal? (run "x") "=> 10"))

   (test-case "set! mutates binding"
     (reset!)
     (check-equal? (run "(define x 1)\n(set! x 9)\nx") "=> 9"))

   (test-case "lexical scoping and shadowing"
     (reset!)
     (check-equal?
      (run "(define x 100)\n((lambda (x) (+ x 1)) 41)\nx")
      "=> 100"))

   (test-case "closure capture"
     (reset!)
     (check-equal?
      (run "(define make-adder (lambda (x) (lambda (y) (+ x y))))\n(define add5 (make-adder 5))\n(add5 3)")
      "=> 8"))

   (test-case "set! across nested scope (captured variable)"
     (reset!)
     (check-equal?
      (run "(define make-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n))))\n(define c (make-counter))\n(c)\n(c)\n(c)")
      "=> 3"))

   (test-case "set! in inner scope does not mutate outer binding"
     (reset!)
     (check-equal?
      (run "(define x 1)\n((lambda (x) (set! x 9) x) 2)\nx")
      "=> 1"))

   (test-case "recursive procedure"
     (reset!)
     (check-equal?
      (run "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(fact 6)")
      "=> 720"))

   (test-case "mutual recursive self-reference"
     (reset!)
     (check-equal?
      (run "(define (evenp n) (if (= n 0) #t (oddp (- n 1))))\n(define (oddp n) (if (= n 0) #f (evenp (- n 1))))\n(evenp 10)")
      "=> #t"))

   (test-case "recursive self-reference variable reports uninitialized binding"
     (reset!)
     (check-error-match #rx"accessing uninitialized binding" "(define x x)"))

   (test-case "define then use regression"
     (reset!)
     (check-equal?
      (run "(define x 10)\n(define y x)\n(+ y 1)")
      "=> 11"))

   (test-case "quote"
     (reset!)
     (check-equal? (run "'(1 2 3)") "=> (1 2 3)"))

   (test-case "quasiquote list"
     (reset!)
     (check-equal? (run "`(1 2 3)") "=> (1 2 3)"))

   (test-case "quasiquote with unquote"
     (reset!)
     (check-equal? (run "`(1 ,(+ 1 2) 4)") "=> (1 3 4)"))

   (test-case "quasiquote with unquote-splicing in list"
     (reset!)
     (check-equal? (run "`(1 ,@(list 2 3) 4)") "=> (1 2 3 4)"))

   (test-case "quasiquote vector"
     (reset!)
     (check-equal? (run "`#(1 ,(+ 1 1) 3)") "=> #(1 2 3)"))

   (test-case "quasiquote with unquote-splicing in vector"
     (reset!)
     (check-equal? (run "`#(1 ,@(list 2 3) 4)") "=> #(1 2 3 4)"))

   (test-case "nested quasiquote"
     (reset!)
     (check-equal?
      (run "(define c 10)\n(define d 20)\n`(a `(b ,c) ,d)")
      "=> (a (quasiquote (b (unquote c))) 20)"))

   (test-case "unquote outside quasiquote"
     (reset!)
     (check-error-match #rx"unquote outside quasiquote" ",x"))

   (test-case "unquote-splicing outside list/vector context"
     (reset!)
     (check-error-match
      #rx"unquote-splicing outside list/vector context"
      "`,@'(1 2)"))

   (test-case "and short-circuits and returns #f"
     (reset!)
     (check-equal?
      (run "(define x 0)\n(and #f (set! x 1))\nx")
      "=> 0"))

   (test-case "or short-circuits and returns first truthy value"
     (reset!)
     (check-equal?
      (run "(define x 0)\n(or 7 (set! x 1))")
      "=> 7"))

   (test-case "or does not evaluate later branches"
     (reset!)
     (check-equal?
      (run "(define x 0)\n(or #t (set! x 1))\nx")
      "=> 0"))

   (test-case "when/unless"
     (reset!)
     (check-equal?
      (run "(define x 0)\n(when #t (set! x 3))\n(unless #t (set! x 9))\nx")
      "=> 3"))

   (test-case "let* sequential binding"
     (reset!)
     (check-equal?
      (run "(let* ((x 2) (y (+ x 3))) (* y 2))")
      "=> 10"))

   (test-case "let parallel binding (inits in outer env)"
     (reset!)
     (check-equal?
      (run "(define x 10)\n(let ((x 1) (y x)) y)")
      "=> 10"))

   (test-case "cond basic and else"
     (reset!)
     (check-equal?
      (run "(cond ((> 1 2) 'nope) ((< 1 2) 'ok) (else 'bad))")
      "=> ok"))

   (test-case "case basic and else"
     (reset!)
     (check-equal?
      (run "(case 3 ((1 2) 'small) ((3 4) 'mid) (else 'other))")
      "=> mid"))

   (test-case "letrec factorial"
     (reset!)
     (check-equal?
      (run "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 6))")
      "=> 720"))

   (test-case "letrec mutual recursion"
     (reset!)
     (check-equal?
      (run "(letrec ((evenp (lambda (n) (if (= n 0) #t (oddp (- n 1)))))\n                (oddp  (lambda (n) (if (= n 0) #f (evenp (- n 1))))))\n        (evenp 8))")
      "=> #t"))

   (test-case "letrec uninitialized reference error"
     (reset!)
     (check-error-match
      #rx"accessing uninitialized binding"
      "(letrec ((x x)) x)"))

   (test-case "append"
     (reset!)
     (check-equal? (run "(append '(1 2) '(3 4) '())") "=> (1 2 3 4)"))

   (test-case "reverse"
     (reset!)
     (check-equal? (run "(reverse '(1 2 3 4))") "=> (4 3 2 1)"))

   (test-case "reverse empty"
     (reset!)
     (check-equal? (run "(reverse '())") "=> ()"))

   (test-case "length"
     (reset!)
     (check-equal? (run "(length '(a b c d))") "=> 4"))

   (test-case "list-ref"
     (reset!)
     (check-equal? (run "(list-ref '(10 20 30) 1)") "=> 20"))

   (test-case "list-tail"
     (reset!)
     (check-equal? (run "(list-tail '(10 20 30) 1)") "=> (20 30)"))

   (test-case "list-tail at end"
     (reset!)
     (check-equal? (run "(list-tail '(10 20 30) 3)") "=> ()"))

   (test-case "memq"
     (reset!)
     (check-equal? (run "(memq 'b '(a b c))") "=> (b c)"))

   (test-case "memv"
     (reset!)
     (check-equal? (run "(memv 2 '(1 2 3))") "=> (2 3)"))

   (test-case "member"
     (reset!)
     (check-equal? (run "(member '(2) '((1) (2) (3)))") "=> ((2) (3))"))

   (test-case "member no match"
     (reset!)
     (check-equal? (run "(member 'x '(a b c))") "=> #f"))

   (test-case "assq"
     (reset!)
     (check-equal? (run "(assq 'b '((a . 1) (b . 2) (c . 3)))") "=> (b . 2)"))

   (test-case "assv"
     (reset!)
     (check-equal? (run "(assv 2 '((1 . one) (2 . two)))") "=> (2 . two)"))

   (test-case "assoc"
     (reset!)
     (check-equal? (run "(assoc '(2) '(((1) . one) ((2) . two)))") "=> ((2) . two)"))

   (test-case "assq no match"
     (reset!)
     (check-equal? (run "(assq 'z '((a . 1) (b . 2)))") "=> #f"))

   (test-case "cadr"
     (reset!)
     (check-equal? (run "(cadr '(a b c))") "=> b"))

   (test-case "caddr"
     (reset!)
     (check-equal? (run "(caddr '(a b c d))") "=> c"))

   (test-case "cadddr"
     (reset!)
     (check-equal? (run "(cadddr '(a b c d))") "=> d"))

   (test-case "caadr"
     (reset!)
     (check-equal? (run "(caadr '((a) (b c) (d)))") "=> b"))

   (test-case "number helpers"
     (reset!)
     (check-equal?
      (run "(list (zero? 0) (add1 4) (sub1 4) (abs -7) (positive? 3) (negative? -2) (even? 10) (odd? 11))")
      "=> (#t 5 3 7 #t #t #t #t)"))

   (test-case "apply with primitive"
     (reset!)
     (check-equal? (run "(apply + 1 2 '(3 4))") "=> 10"))

   (test-case "apply with closure"
     (reset!)
     (check-equal? (run "(apply (lambda (x y z) (+ x (* y z))) '(2 3 4))") "=> 14"))

   (test-case "map over list"
     (reset!)
     (check-equal? (run "(map (lambda (x) (+ x 10)) '(1 2 3))") "=> (11 12 13)"))

   (test-case "filter over list"
     (reset!)
     (check-equal? (run "(filter (lambda (x) (odd? x)) '(1 2 3 4 5 6))") "=> (1 3 5)"))

   (test-case "for-each returns void and runs effects"
     (reset!)
     (check-equal?
      (run "(define x 0)\n(for-each (lambda (n) (set! x (+ x n))) '(1 2 3 4))\nx")
      "=> 10"))

   (test-case "call-with-values: multiple to list"
     (reset!)
     (check-equal?
      (run "(call-with-values (lambda () (values 1 2 3)) list)")
      "=> (1 2 3)"))

   (test-case "call-with-values: single value producer"
     (reset!)
     (check-equal?
      (run "(call-with-values (lambda () 41) (lambda (x) (+ x 1)))")
      "=> 42"))

   (test-case "call-with-values: zero values"
     (reset!)
     (check-equal?
      (run "(call-with-values (lambda () (values)) (lambda xs (length xs)))")
      "=> 0"))

   (test-case "values in single-value context errors"
     (reset!)
     (check-eval-error-match #rx"expected 1 value, got 2" "(+ (values 1 2) 3)"))

   (test-case "top-level multiple values print all values"
     (reset!)
     (check-equal? (run "(values 1 2)") "=> 1\n=> 2"))

   (test-case "call-with-values consumer arity mismatch"
     (reset!)
     (check-eval-error-match
      #rx"arity mismatch: expected 1 arguments, got 2"
      "(call-with-values (lambda () (values 1 2)) (lambda (x) x))"))

   (test-case "call/cc basic escape"
     (reset!)
     (check-equal?
      (run "(+ 1 (call/cc (lambda (k) (k 41))))")
      "=> 42"))

   (test-case "call-with-current-continuation alias"
     (reset!)
     (check-equal?
      (run "(call-with-current-continuation (lambda (k) (k 7)))")
      "=> 7"))

   (test-case "call/cc no escape returns body value"
     (reset!)
     (check-equal?
      (run "(call/cc (lambda (k) 9))")
      "=> 9"))

   (test-case "call/cc escapes out of list context"
     (reset!)
     (check-equal?
      (run "(call/cc (lambda (k) (list 1 (k 2) 3)))")
      "=> 2"))

   (test-case "call/cc continuation stored and reused"
     (reset!)
     (check-equal?
      (run "(define saved #f)\n(+ 1 (call/cc (lambda (k) (set! saved k) 10)))")
      "=> 11")
     (check-equal?
      (run "(saved 50)")
      "=> 51"))

   (test-case "call/cc arity mismatch"
     (reset!)
     (check-eval-error-match #rx"call/cc expects 1 argument" "(call/cc)"))

   (test-case "continuation application arity mismatch"
     (reset!)
     (check-eval-error-match
      #rx"continuation expects 1 argument"
      "(call/cc (lambda (k) (k 1 2)))"))

   (test-case "dynamic-wind returns thunk value"
     (reset!)
     (check-equal?
      (run "(dynamic-wind (lambda () 1) (lambda () 42) (lambda () 3))")
      "=> 42"))

   (test-case "dynamic-wind normal before/thunk/after ordering"
     (reset!)
     (check-equal?
      (run "(define log '())\n(dynamic-wind (lambda () (set! log (cons 'before log))) (lambda () (set! log (cons 'thunk log)) 'ok) (lambda () (set! log (cons 'after log))))\nlog")
      "=> (after thunk before)"))

   (test-case "dynamic-wind runs after on call/cc escape"
     (reset!)
     (check-equal?
      (run "(define log '())\n(+ 1 (dynamic-wind (lambda () (set! log (cons 'before log))) (lambda () (call/cc (lambda (k) (set! log (cons 'thunk log)) (k 41)))) (lambda () (set! log (cons 'after log)))))\nlog")
      "=> (after thunk before)"))

   (test-case "dynamic-wind arity mismatch"
     (reset!)
     (check-eval-error-match #rx"dynamic-wind expects 3 arguments" "(dynamic-wind (lambda () 1) (lambda () 2))"))

   (test-case "dynamic-wind type checks"
     (reset!)
     (check-eval-error-match #rx"dynamic-wind: before must be a procedure" "(dynamic-wind 1 (lambda () 2) (lambda () 3))"))

   (test-case "unwind-protect with call/cc escape updates captured state"
     (reset!)
     (check-equal?
      (run "((call/cc\n   (let ([x 'a])\n     (lambda (k)\n       (unwind-protect\n         (k (lambda () x))\n         (set! x 'b))))))")
      "=> b"))

   ;; This test follows the historical Scheme letrec probe (Al Petrofsky).
   ;; Note: full Racket evaluates the same program to 1.
   (test-case "call/cc + letrec probe (Scheme semantics)"
     (reset!)
     (check-equal?
      (run "(let ((cont #f))\n   (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))\n            (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))\n     (if cont\n         (let ((c cont))\n           (set! cont #f)\n           (set! x 1)\n           (set! y 1)\n           (c 0))\n         (+ x y))))")
      "=> 0"))

   (test-case "dynamic-wind basic path"
     (reset!)
     (check-equal?
      (run "(let* ((path '())\n           (add (lambda (s) (set! path (cons s path)))))\n      (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))\n      (reverse path))")
      "=> (a b c)"))

   (test-case "dynamic-wind continuation re-entry path"
     (reset!)
     (check-equal?
      (run "(let ((path '())\n          (c #f))\n      (let ((add (lambda (s)\n                   (set! path (cons s path)))))\n        (dynamic-wind\n            (lambda () (add 'connect))\n            (lambda ()\n              (add (call-with-current-continuation\n                    (lambda (c0)\n                      (set! c c0)\n                      'talk1))))\n            (lambda () (add 'disconnect)))\n        (if (< (length path) 4)\n            (c 'talk2)\n            (reverse path))))")
      "=> (connect talk1 disconnect connect talk2 disconnect)"))

   (test-case "multiline program with blank lines"
     (reset!)
     (check-equal?
      (run "(define x 10)\n\n(define y 5)\n(+ x y)")
      "=> 15"))

   (test-case "line comments are ignored by reader"
     (reset!)
     (check-equal?
      (run "; initialize x\n(define x 7) ; trailing comment\n; compute answer\n(+ x 5)")
      "=> 12"))

   (test-case "block comments are ignored by reader"
     (reset!)
     (check-equal?
      (run "#| block\ncomment spanning\nlines |#\n(define x 9)\n(+ x 1)")
      "=> 10"))

   (test-case "datum comments are ignored by reader"
     (reset!)
     (check-equal?
      (run "#;(define x 100)\n(define x 4)\n#;(+ x 99)\n(+ x 6)")
      "=> 10"))

   (test-case "quote sugar through process-input"
     (reset!)
     (check-equal?
      (run "(define q '(a b c))\nq")
      "=> (a b c)"))

   (test-case "quasiquote/unquote sugar through process-input"
     (reset!)
     (check-equal?
      (run "(define x 3)\n`(1 ,x 5)")
      "=> (1 3 5)"))

   (test-case "dotted pair through process-input"
     (reset!)
     (check-equal? (run "'(a . b)") "=> (a . b)"))

   (test-case "dotted list through process-input"
     (reset!)
     (check-equal? (run "'(1 2 . 3)") "=> (1 2 . 3)"))

   (test-case "dotted pair car/cdr behavior"
     (reset!)
     (check-equal?
      (run "(define p '(a . b))\n(list (car p) (cdr p))")
      "=> (a b)"))

   (test-case "invalid dotted pair read error"
     (reset!)
     (check-true
      (string-prefix? (run "'(a . b c)") "=> read error:")))

   (test-case "mixed integration: multiline + comments + quote + dotted"
     (reset!)
     (check-equal?
      (run "; keep this ignored\n(define p '(x . y))\n#| comment |#\n(define q '(1 2 3))\n(list (car p) (cdr p) q)")
      "=> (x y (1 2 3))"))

   (test-case "read error"
     (reset!)
     (check-true (string-prefix? (run "(") "=> read error:")))

   (test-case "unbound identifier error"
     (reset!)
     (check-eval-error-match #rx"unbound identifier x" "x"))

   (test-case "malformed if error includes pattern"
     (reset!)
     (check-eval-error-match #rx"malformed if" "(if 1)"))

   (test-case "malformed lambda error includes pattern"
     (reset!)
     (check-eval-error-match #rx"malformed lambda" "(lambda)"))

   (test-case "malformed set! error includes pattern"
     (reset!)
     (check-eval-error-match #rx"malformed set!" "(set! x)"))

   (test-case "malformed define error includes pattern"
     (reset!)
     (check-eval-error-match #rx"malformed define" "(define x 1 2)"))

   (test-case "malformed let error includes pattern"
     (reset!)
     (check-eval-error-match #rx"malformed let" "(let)"))

   (test-case "eval error prefix contract: non-procedure application"
     (reset!)
     (check-eval-error-match #rx"application of non-procedure" "(0 1 2)"))

   (test-case "malformed form matrix: quote"
     (reset!)
     (check-eval-error-match #rx"malformed quote" "(quote 1 2)"))

   (test-case "malformed form matrix: quasiquote"
     (reset!)
     (check-eval-error-match #rx"quasiquote: malformed form" "(quasiquote 1 2)"))

   (test-case "malformed form matrix: set!"
     (reset!)
     (check-eval-error-match #rx"malformed set!" "(set! 1 2)"))

   (test-case "malformed form matrix: define"
     (reset!)
     (check-eval-error-match #rx"malformed define" "(define x)"))

   (test-case "malformed form matrix: let"
     (reset!)
     (check-eval-error-match #rx"let malformed|malformed let|malformed binding" "(let (x 1) x)"))

   (test-case "malformed form matrix: let*"
     (reset!)
     (check-eval-error-match #rx"let\\* malformed|malformed let\\*|malformed binding" "(let* (x 1) x)"))

   (test-case "malformed form matrix: letrec"
     (reset!)
     (check-eval-error-match #rx"letrec malformed|malformed letrec|malformed binding" "(letrec (x 1) x)"))

   (test-case "malformed form matrix: cond clause"
     (reset!)
     (check-eval-error-match #rx"malformed cond clause" "(cond 1)"))

   (test-case "malformed form matrix: case clause"
     (reset!)
     (check-eval-error-match #rx"malformed case clause" "(case 1 2)"))

   (test-case "malformed form matrix: when"
     (reset!)
     (check-eval-error-match #rx"malformed when" "(when)"))

   (test-case "malformed form matrix: unless"
     (reset!)
     (check-eval-error-match #rx"malformed unless" "(unless)"))))

(module+ main
  (define failures (run-tests minischeme-tests))
  (exit failures))
