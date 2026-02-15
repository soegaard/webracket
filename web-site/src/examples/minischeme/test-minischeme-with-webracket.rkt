;;;
;;; MiniScheme tests executed through WebRacket (`-r`).
;;;
;;; Run from this directory:
;;;   racket ../../../../webracket.rkt --stdlib -r test-minischeme-with-webracket.rkt
;;;
;;; This file avoids rackunit on purpose, so it can run as a plain
;;; WebRacket program and print a compact test report.

(include "minischeme.rkt")

(define (run src)
  (minischeme-process-input src))

(define (reset!)
  (minischeme-reset-state!))

(define (expect-equal name actual expected)
  (list name (equal? actual expected) actual expected))

(define (expect-prefix name actual prefix)
  (list name
        (and (string? actual)
             (<= (string-length prefix) (string-length actual))
             (string=? (substring actual 0 (string-length prefix)) prefix))
        actual
        prefix))

(define (expect-contains name actual needle)
  (list name
        (and (string? actual)
             (not (false? (string-contains? actual needle))))
        actual
        needle))

(define (starts-with? s prefix)
  (and (string? s)
       (<= (string-length prefix) (string-length s))
       (string=? (substring s 0 (string-length prefix)) prefix)))

(define (test-equal name src expected)
  (reset!)
  (expect-equal name (run src) expected))

(define (test-prefix name src prefix)
  (reset!)
  (expect-prefix name (run src) prefix))

(define (test-contains name src needle)
  (reset!)
  (expect-contains name (run src) needle))

(define (test-eval-contains name src needle)
  (reset!)
  (define out (run src))
  (list name
        (and (starts-with? out "=> eval error:")
             (not (false? (string-contains? out needle))))
        out
        needle))

(define tests
  (list
   (test-equal "literal number" "42" "=> 42")
   (test-equal "primitive application" "(+ 42 1)" "=> 43")
   (test-equal "lambda application" "((lambda (x) (+ x 1)) 41)" "=> 42")
   (test-equal "variadic lambda (rest-only)" "((lambda xs xs) 1 2 3)" "=> (1 2 3)")
   (test-equal "variadic lambda (dotted formals)" "((lambda (x . rest) rest) 1 2 3)" "=> (2 3)")
   (test-eval-contains "variadic lambda arity lower bound"
                       "((lambda (x y . rest) x) 1)"
                       "arity mismatch: expected at least 2 arguments, got 1")
   (test-eval-contains "fixed lambda arity mismatch"
                       "((lambda (x y) x) 1)"
                       "arity mismatch: expected 2 arguments, got 1")
   (test-equal "let" "(let ((x 2) (y 3)) (+ x y))" "=> 5")
   (test-equal "if without else produces void" "(if #f 1)" "=> #<void>")
   (test-equal "define + use in same input" "(define x 10)\n(+ x 5)" "=> 15")
   (let ()
     (reset!)
     (run "(define x 10)")
     (expect-equal "definition persists across runs" (run "x") "=> 10"))
   (test-equal "set! mutates binding" "(define x 1)\n(set! x 9)\nx" "=> 9")
   (test-equal "lexical scoping and shadowing"
               "(define x 100)\n((lambda (x) (+ x 1)) 41)\nx"
               "=> 100")
   (test-equal "closure capture"
               "(define make-adder (lambda (x) (lambda (y) (+ x y))))\n(define add5 (make-adder 5))\n(add5 3)"
               "=> 8")
   (test-equal "set! across nested scope (captured variable)"
               "(define make-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n))))\n(define c (make-counter))\n(c)\n(c)\n(c)"
               "=> 3")
   (test-equal "set! in inner scope does not mutate outer binding"
               "(define x 1)\n((lambda (x) (set! x 9) x) 2)\nx"
               "=> 1")
   (test-equal "recursive procedure"
               "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(fact 6)"
               "=> 720")
   (test-equal "mutual recursive self-reference"
               "(define (evenp n) (if (= n 0) #t (oddp (- n 1))))\n(define (oddp n) (if (= n 0) #f (evenp (- n 1))))\n(evenp 10)"
               "=> #t")
   (test-eval-contains "recursive self-reference variable reports uninitialized binding"
                       "(define x x)"
                       "accessing uninitialized binding")
   (test-equal "define then use regression"
               "(define x 10)\n(define y x)\n(+ y 1)"
               "=> 11")
   (test-equal "quote" "'(1 2 3)" "=> (1 2 3)")
   (test-equal "quasiquote list" "`(1 2 3)" "=> (1 2 3)")
   (test-equal "quasiquote with unquote" "`(1 ,(+ 1 2) 4)" "=> (1 3 4)")
   (test-equal "quasiquote with unquote-splicing in list" "`(1 ,@(list 2 3) 4)" "=> (1 2 3 4)")
   (test-equal "quasiquote vector" "`#(1 ,(+ 1 1) 3)" "=> #(1 2 3)")
   (test-equal "quasiquote with unquote-splicing in vector" "`#(1 ,@(list 2 3) 4)" "=> #(1 2 3 4)")
   (test-equal "nested quasiquote"
               "(define c 10)\n(define d 20)\n`(a `(b ,c) ,d)"
               "=> (a (quasiquote (b (unquote c))) 20)")
   (test-eval-contains "unquote outside quasiquote" ",x" "unquote outside quasiquote")
   (test-eval-contains "unquote-splicing outside list/vector context" "`,@'(1 2)" "unquote-splicing outside list/vector context")
   (test-equal "and short-circuits and returns #f"
               "(define x 0)\n(and #f (set! x 1))\nx"
               "=> 0")
   (test-equal "or short-circuits and returns first truthy value"
               "(define x 0)\n(or 7 (set! x 1))"
               "=> 7")
   (test-equal "or does not evaluate later branches"
               "(define x 0)\n(or #t (set! x 1))\nx"
               "=> 0")
   (test-equal "when/unless"
               "(define x 0)\n(when #t (set! x 3))\n(unless #t (set! x 9))\nx"
               "=> 3")
   (test-equal "let* sequential binding" "(let* ((x 2) (y (+ x 3))) (* y 2))" "=> 10")
   (test-equal "let parallel binding (inits in outer env)"
               "(define x 10)\n(let ((x 1) (y x)) y)"
               "=> 10")
   (test-equal "named let factorial"
               "(let fact-loop ((n 6) (acc 1)) (if (= n 0) acc (fact-loop (- n 1) (* acc n))))"
               "=> 720")
   (test-equal "named let captures outer binding in init expressions"
               "(define x 10)\n(let loop ((x 1) (y x)) (if (= x 1) y 0))"
               "=> 10")
   (test-equal "do basic accumulation"
               "(do ((i 0 (+ i 1)) (s 0 (+ s i))) ((> i 5) s))"
               "=> 15")
   (test-equal "do with omitted step"
               "(do ((i 0 (+ i 1)) (x 10)) ((= i 3) x))"
               "=> 10")
   (test-equal "do with commands"
               "(do ((i 0 (+ i 1)) (xs '() (cons i xs))) ((= i 4) (reverse xs)))"
               "=> (0 1 2 3)")
   (test-equal "promise? and force basic"
               "(list (promise? (delay (+ 1 2))) (force (delay (+ 1 2))))"
               "=> (#t 3)")
   (test-equal "force on non-promise is identity"
               "(force 42)"
               "=> 42")
   (test-equal "promise memoizes successful result"
               "(define n 0)\n(define p (delay (begin (set! n (+ n 1)) n)))\n(list (promise-forced? p) (force p) (promise-forced? p) (force p) n)"
               "=> (#f 1 #t 1 1)")
   (test-equal "promise-running? is true during thunk evaluation"
               "(define p #f)\n(set! p (delay (promise-running? p)))\n(force p)"
               "=> #t")
   (test-eval-contains "force reentrant promise reports error"
                       "(define p #f)\n(set! p (delay (force p)))\n(force p)"
                       "reentrant force on running promise")
   (let ()
     (reset!)
     (run "(define n 0)\n(define p (delay (begin (set! n (+ n 1)) (car 1))))")
     (define e1 (run "(force p)"))
     (define n1 (run "n"))
     (define e2 (run "(force p)"))
     (define n2 (run "n"))
     (expect-equal "promise failure is memoized"
                   (list (not (false? (string-contains? e1 "car expects a non-empty pair")))
                         n1
                         (not (false? (string-contains? e2 "car expects a non-empty pair")))
                         n2)
                   (list #t "=> 1" #t "=> 1")))
   (test-equal "cond basic and else"
               "(cond ((> 1 2) 'nope) ((< 1 2) 'ok) (else 'bad))"
               "=> ok")
   (test-equal "cond => recipient gets test value"
               "(cond ((+ 1 2) => (lambda (x) (+ x 10))) (else 0))"
               "=> 13")
   (test-equal "cond => falls through on #f"
               "(cond ((member 'z '(a b)) => car) (else 'no))"
               "=> no")
   (test-equal "case basic and else"
               "(case 3 ((1 2) 'small) ((3 4) 'mid) (else 'other))"
               "=> mid")
   (test-equal "letrec factorial"
               "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 6))"
               "=> 720")
   (test-equal "letrec mutual recursion"
               "(letrec ((evenp (lambda (n) (if (= n 0) #t (oddp (- n 1)))))\n                (oddp  (lambda (n) (if (= n 0) #f (evenp (- n 1))))))\n        (evenp 8))"
               "=> #t")
   (test-eval-contains "letrec uninitialized reference error" "(letrec ((x x)) x)" "accessing uninitialized binding")
   (test-equal "append" "(append '(1 2) '(3 4) '())" "=> (1 2 3 4)")
   (test-equal "list*" "(list* 1 2 '(3 4))" "=> (1 2 3 4)")
   (test-equal "reverse" "(reverse '(1 2 3 4))" "=> (4 3 2 1)")
   (test-equal "reverse empty" "(reverse '())" "=> ()")
   (test-equal "vector constructor and predicate"
               "(vector? (vector 1 2 3))"
               "=> #t")
   (test-equal "make-vector and vector-length"
               "(vector-length (make-vector 4 'x))"
               "=> 4")
   (test-equal "vector-ref"
               "(vector-ref (vector 10 20 30) 1)"
               "=> 20")
   (test-equal "vector-set!"
               "(define v (vector 1 2 3))\n(vector-set! v 1 99)\n(vector-ref v 1)"
               "=> 99")
   (test-equal "vector->list"
               "(vector->list (vector 1 2 3))"
               "=> (1 2 3)")
   (test-equal "list->vector"
               "(list->vector '(a b c))"
               "=> #(a b c)")
   (test-equal "vector-fill!"
               "(define v (vector 1 2 3))\n(vector-fill! v 7)\nv"
               "=> #(7 7 7)")
   (test-equal "char basics"
               "(list (char? #\\a) (char->integer #\\A) (integer->char 66))"
               "=> (#t 65 #\\B)")
   (test-equal "char comparisons"
               "(list (char<? #\\a #\\b) (char-ci=? #\\A #\\a) (char-ci<? #\\a #\\B))"
               "=> (#t #t #t)")
   (test-equal "string constructor/predicate/length"
               "(list (string? (string #\\a #\\b)) (string-length \"abc\") (make-string 3 #\\x))"
               "=> (#t 3 \"xxx\")")
   (test-equal "string-ref substring string-append"
               "(list (string-ref \"abc\" 1) (substring \"abcdef\" 2 5) (string-append \"ab\" \"cd\"))"
               "=> (#\\b \"cde\" \"abcd\")")
   (test-equal "string mutation"
               "(define s (string-copy \"abc\"))\n(string-set! s 1 #\\x)\n(string-fill! s #\\q)\ns"
               "=> \"qqq\"")
   (test-equal "string/list conversion"
               "(list (string->list \"ab\") (list->string '(#\\c #\\d)))"
               "=> ((#\\a #\\b) \"cd\")")
   (test-equal "string comparisons"
               "(list (string=? \"a\" \"a\") (string<? \"a\" \"b\") (string-ci=? \"Ab\" \"aB\"))"
               "=> (#t #t #t)")
   (test-equal "length" "(length '(a b c d))" "=> 4")
   (test-equal "list-ref" "(list-ref '(10 20 30) 1)" "=> 20")
   (test-equal "list-tail"
               "(list-tail '(10 20 30) 1)"
               "=> (20 30)")
   (test-equal "list-tail at end"
               "(list-tail '(10 20 30) 3)"
               "=> ()")
   (test-equal "memq"
               "(memq 'b '(a b c))"
               "=> (b c)")
   (test-equal "memv"
               "(memv 2 '(1 2 3))"
               "=> (2 3)")
   (test-equal "member"
               "(member '(2) '((1) (2) (3)))"
               "=> ((2) (3))")
   (test-equal "member no match"
               "(member 'x '(a b c))"
               "=> #f")
   (test-equal "assq"
               "(assq 'b '((a . 1) (b . 2) (c . 3)))"
               "=> (b . 2)")
   (test-equal "assv"
               "(assv 2 '((1 . one) (2 . two)))"
               "=> (2 . two)")
   (test-equal "assoc"
               "(assoc '(2) '(((1) . one) ((2) . two)))"
               "=> ((2) . two)")
   (test-equal "assq no match"
               "(assq 'z '((a . 1) (b . 2)))"
               "=> #f")
   (test-equal "cadr"
               "(cadr '(a b c))"
               "=> b")
   (test-equal "caddr"
               "(caddr '(a b c d))"
               "=> c")
   (test-equal "cadddr"
               "(cadddr '(a b c d))"
               "=> d")
   (test-equal "caadr"
               "(caadr '((a) (b c) (d)))"
               "=> b")
   (test-equal "eq?/eqv?"
               "(let ((x '(a))) (list (eq? 'a 'a) (eq? x x) (eqv? 2 2) (eqv? 2 2.0)))"
               "=> (#t #t #t #f)")
   (test-equal "symbol<->string"
               "(list (symbol->string 'apple) (string->symbol \"banana\") (symbol? (string->symbol \"x\")))"
               "=> (\"apple\" banana #t)")
   (test-equal "char classification and case conversion"
               "(list (char-alphabetic? #\\A) (char-numeric? #\\7) (char-whitespace? #\\space) (char-lower-case? #\\a) (char-upper-case? #\\A) (char-upcase #\\a) (char-downcase #\\A))"
               "=> (#t #t #t #t #t #\\A #\\a)")
   (test-equal "number helpers"
               "(list (zero? 0) (add1 4) (sub1 4) (abs -7) (positive? 3) (negative? -2) (even? 10) (odd? 11))"
               "=> (#t 5 3 7 #t #t #t #t)")
   (test-equal "numeric type predicates"
               "(list (real? 1) (rational? 0.5) (integer? 2) (exact? 2) (inexact? 2.0))"
               "=> (#t #t #t #t #t)")
   (test-equal "quotient/remainder/modulo"
               "(list (quotient 10 3) (remainder 10 3) (modulo 10 3))"
               "=> (3 1 1)")
   (test-equal "gcd/lcm"
               "(list (gcd 12 18) (lcm 12 18))"
               "=> (6 36)")
   (test-equal "numerator/denominator"
               "(list (numerator 6) (denominator 6))"
               "=> (6 1)")
   (test-equal "floor/ceiling/truncate/round"
               "(list (= (floor 2.7) 2.0) (= (ceiling 2.1) 3.0) (= (truncate -2.7) -2.0) (= (round 3.5) 4.0))"
               "=> (#t #t #t #t)")
   (test-equal "rationalize"
               "(rational? (rationalize 0.3 0.1))"
               "=> #t")
   (test-equal "max/min"
               "(list (max 3 7 5) (min 3 7 5))"
               "=> (7 3)")
   (test-equal "exp/log/sin/cos/tan/asin/acos/atan"
               "(list (number? (exp 1)) (number? (log 10)) (number? (sin 1)) (number? (cos 1)) (number? (tan 1)) (number? (asin 0.5)) (number? (acos 0.5)) (number? (atan 0 1)))"
               "=> (#t #t #t #t #t #t #t #t)")
   (test-equal "sqrt/expt"
               "(list (sqrt 9) (expt 2 5))"
               "=> (3 32)")
   (test-equal "exact<->inexact"
               "(list (exact->inexact 3) (inexact->exact 3.0))"
               "=> (3.0 3)")
   (test-equal "number->string/string->number"
               "(list (number->string 255) (string->number \"255\"))"
               "=> (\"255\" 255)")
   (test-equal "apply with primitive" "(apply + 1 2 '(3 4))" "=> 10")
   (test-equal "apply with closure" "(apply (lambda (x y z) (+ x (* y z))) '(2 3 4))" "=> 14")
   (test-equal "map over list" "(map (lambda (x) (+ x 10)) '(1 2 3))" "=> (11 12 13)")
   (test-equal "filter over list" "(filter (lambda (x) (odd? x)) '(1 2 3 4 5 6))" "=> (1 3 5)")
   (test-equal "for-each returns void and runs effects"
               "(define x 0)\n(for-each (lambda (n) (set! x (+ x n))) '(1 2 3 4))\nx"
               "=> 10")
   (test-equal "call-with-values: multiple to list"
               "(call-with-values (lambda () (values 1 2 3)) list)"
               "=> (1 2 3)")
   (test-equal "call-with-values: single value producer"
               "(call-with-values (lambda () 41) (lambda (x) (+ x 1)))"
               "=> 42")
   (test-equal "call-with-values: zero values"
               "(call-with-values (lambda () (values)) (lambda xs (length xs)))"
               "=> 0")
   (test-eval-contains "values in single-value context errors"
                       "(+ (values 1 2) 3)"
                       "expected 1 value, got 2")
   (test-equal "top-level multiple values print all values"
               "(values 1 2)"
               "=> 1\n=> 2")
   (test-eval-contains "call-with-values consumer arity mismatch"
                       "(call-with-values (lambda () (values 1 2)) (lambda (x) x))"
                       "arity mismatch: expected 1 arguments, got 2")
   (test-equal "call/cc basic escape"
               "(+ 1 (call/cc (lambda (k) (k 41))))"
               "=> 42")
   (test-equal "call-with-current-continuation alias"
               "(call-with-current-continuation (lambda (k) (k 7)))"
               "=> 7")
   (test-equal "call/cc no escape returns body value"
               "(call/cc (lambda (k) 9))"
               "=> 9")
   (test-equal "call/cc escapes out of list context"
               "(call/cc (lambda (k) (list 1 (k 2) 3)))"
               "=> 2")
   (test-equal "call/cc continuation stored and reused"
               "(define saved #f)\n(+ 1 (call/cc (lambda (k) (set! saved k) 10)))\n(saved 50)"
               "=> 51")
   (test-eval-contains "call/cc arity mismatch"
                       "(call/cc)"
                       "call/cc expects 1 argument")
   (test-eval-contains "continuation application arity mismatch"
                       "(call/cc (lambda (k) (k 1 2)))"
                       "continuation expects 1 argument")
   (test-equal "dynamic-wind returns thunk value"
               "(dynamic-wind (lambda () 1) (lambda () 42) (lambda () 3))"
               "=> 42")
   (test-equal "dynamic-wind normal before/thunk/after ordering"
               "(define log '())\n(dynamic-wind (lambda () (set! log (cons 'before log))) (lambda () (set! log (cons 'thunk log)) 'ok) (lambda () (set! log (cons 'after log))))\nlog"
               "=> (after thunk before)")
   (test-equal "dynamic-wind runs after on call/cc escape"
               "(define log '())\n(+ 1 (dynamic-wind (lambda () (set! log (cons 'before log))) (lambda () (call/cc (lambda (k) (set! log (cons 'thunk log)) (k 41)))) (lambda () (set! log (cons 'after log)))))\nlog"
               "=> (after thunk before)")
   (test-eval-contains "dynamic-wind arity mismatch"
                       "(dynamic-wind (lambda () 1) (lambda () 2))"
                       "dynamic-wind expects 3 arguments")
   (test-eval-contains "dynamic-wind type checks"
                       "(dynamic-wind 1 (lambda () 2) (lambda () 3))"
                       "dynamic-wind: before must be a procedure")
   (test-equal "unwind-protect with call/cc escape updates captured state"
               "((call/cc\n   (let ([x 'a])\n     (lambda (k)\n       (unwind-protect\n         (k (lambda () x))\n         (set! x 'b))))))"
               "=> b")
   ;; This test follows the historical Scheme letrec probe (Al Petrofsky).
   ;; Note: full Racket evaluates the same program to 1.
   (test-equal "call/cc + letrec probe (Scheme semantics)"
               "(let ((cont #f))\n   (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))\n            (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))\n     (if cont\n         (let ((c cont))\n           (set! cont #f)\n           (set! x 1)\n           (set! y 1)\n           (c 0))\n         (+ x y))))"
               "=> 0")
   (test-equal "dynamic-wind basic path"
               "(let* ((path '())\n           (add (lambda (s) (set! path (cons s path)))))\n      (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))\n      (reverse path))"
               "=> (a b c)")
   (test-equal "dynamic-wind continuation re-entry path"
               "(let ((path '())\n          (c #f))\n      (let ((add (lambda (s)\n                   (set! path (cons s path)))))\n        (dynamic-wind\n            (lambda () (add 'connect))\n            (lambda ()\n              (add (call-with-current-continuation\n                    (lambda (c0)\n                      (set! c c0)\n                      'talk1))))\n            (lambda () (add 'disconnect)))\n        (if (< (length path) 4)\n            (c 'talk2)\n            (reverse path))))"
               "=> (connect talk1 disconnect connect talk2 disconnect)")
   (test-equal "multiline program with blank lines"
               "(define x 10)\n\n(define y 5)\n(+ x y)"
               "=> 15")
   (test-equal "line comments are ignored by reader"
               "; initialize x\n(define x 7) ; trailing comment\n; compute answer\n(+ x 5)"
               "=> 12")
   (test-equal "block comments are ignored by reader"
               "#| block\ncomment spanning\nlines |#\n(define x 9)\n(+ x 1)"
               "=> 10")
   (test-equal "datum comments are ignored by reader"
               "#;(define x 100)\n(define x 4)\n#;(+ x 99)\n(+ x 6)"
               "=> 10")
   (test-equal "quote sugar through process-input"
               "(define q '(a b c))\nq"
               "=> (a b c)")
   (test-equal "quasiquote/unquote sugar through process-input"
               "(define x 3)\n`(1 ,x 5)"
               "=> (1 3 5)")
   (test-equal "dotted pair through process-input" "'(a . b)" "=> (a . b)")
   (test-equal "dotted list through process-input" "'(1 2 . 3)" "=> (1 2 . 3)")
   (test-equal "dotted pair car/cdr behavior"
               "(define p '(a . b))\n(list (car p) (cdr p))"
               "=> (a b)")
   (test-prefix "invalid dotted pair read error" "'(a . b c)" "=> read error:")
   (test-equal "mixed integration: multiline + comments + quote + dotted"
               "; keep this ignored\n(define p '(x . y))\n#| comment |#\n(define q '(1 2 3))\n(list (car p) (cdr p) q)"
               "=> (x y (1 2 3))")
   (test-prefix "read error" "(" "=> read error:")
   (test-eval-contains "unbound identifier error" "x" "unbound identifier x")
   (test-eval-contains "malformed if error includes pattern" "(if 1)" "malformed if")
   (test-eval-contains "malformed lambda error includes pattern" "(lambda)" "malformed lambda")
   (test-eval-contains "malformed set! error includes pattern" "(set! x)" "malformed set!")
   (test-eval-contains "malformed define error includes pattern" "(define x 1 2)" "malformed define")
   (test-eval-contains "malformed let error includes pattern" "(let)" "malformed let")
   (test-eval-contains "malformed do error includes pattern" "(do (x 0) ((= 1 1) 0))" "do binding malformed")
   (test-eval-contains "malformed delay error includes pattern" "(delay 1 2)" "malformed delay")
   (test-eval-contains "eval error prefix contract: non-procedure application" "(0 1 2)" "application of non-procedure")
   (test-eval-contains "malformed form matrix: quote" "(quote 1 2)" "malformed quote")
   (test-eval-contains "malformed form matrix: quasiquote" "(quasiquote 1 2)" "quasiquote: malformed form")
   (test-eval-contains "malformed form matrix: set!" "(set! 1 2)" "malformed set!")
   (test-eval-contains "malformed form matrix: define" "(define x)" "malformed define")
   (test-eval-contains "malformed form matrix: let" "(let (x 1) x)" "malformed binding")
   (test-eval-contains "malformed form matrix: let*" "(let* (x 1) x)" "malformed binding")
   (test-eval-contains "malformed form matrix: letrec" "(letrec (x 1) x)" "malformed binding")
   (test-eval-contains "malformed form matrix: do" "(do ((x 1 2 3)) ((= x 0) x))" "do binding malformed")
   (test-eval-contains "malformed form matrix: delay" "(delay)" "malformed delay")
   (test-eval-contains "vector primitive contract: vector-ref type" "(vector-ref 1 0)" "vector-ref expects a vector")
   (test-eval-contains "string primitive contract: string-ref type" "(string-ref 1 0)" "string-ref expects a string")
   (test-eval-contains "malformed form matrix: cond clause" "(cond 1)" "malformed cond clause")
   (test-eval-contains "malformed form matrix: cond => clause" "(cond ((+ 1 2) =>) (else 0))" "malformed cond => clause")
   (test-eval-contains "malformed form matrix: case clause" "(case 1 2)" "malformed case clause")
   (test-eval-contains "malformed form matrix: when" "(when)" "malformed when")
   (test-eval-contains "malformed form matrix: unless" "(unless)" "malformed unless")))

(define failures
  (filter (lambda (r) (not (cadr r))) tests))

(list
 (cons 'total (length tests))
 (cons 'passed (- (length tests) (length failures)))
 (cons 'failed (length failures))
 (cons 'failures failures))
