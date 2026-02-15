#lang racket

(require rackunit
         rackunit/text-ui
         racket/string)

;; Reuse the page-shared MiniScheme interpreter implementation.
(include "../../web-site/src/examples/minischeme/minischeme.rkt")

(define (run src)
  (minischeme-process-input src))

(define (reset!)
  (minischeme-reset-state!))

(define (check-error-match rx src)
  (check-true (regexp-match? rx (run src))))

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

   (test-case "read error"
     (reset!)
     (check-true (string-prefix? (run "(") "=> read error:")))

   (test-case "unbound identifier error"
     (reset!)
     (check-error-match #rx"unbound identifier x" "x"))

   (test-case "malformed if error includes pattern"
     (reset!)
     (check-error-match #rx"malformed if" "(if 1)"))

   (test-case "malformed lambda error includes pattern"
     (reset!)
     (check-error-match #rx"malformed lambda" "(lambda)"))

   (test-case "malformed set! error includes pattern"
     (reset!)
     (check-error-match #rx"malformed set!" "(set! x)"))

   (test-case "malformed define error includes pattern"
     (reset!)
     (check-error-match #rx"malformed define" "(define x 1 2)"))

   (test-case "malformed let error includes pattern"
     (reset!)
     (check-error-match #rx"malformed let" "(let)"))))

(module+ main
  (define failures (run-tests minischeme-tests))
  (exit failures))
