;;;
;;; read tests
;;;

;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- -r test-read.rkt
;;
;; Note: unlike test-basics.rkt, this file needs stdlib
;; (enabled by default), since `read` is provided by stdlib/reading.rkt.

(define (read-all-from-string s)
  (define in (open-input-string s))
  (let loop ([acc '()])
    (define v (read in))
    (if (eof-object? v)
        (reverse acc)
        (loop (cons v acc)))))

(list
 (list "13. Input and Output"
       (list "13.2 S-expression Reading"
             (list "read"
                   (list
                    ;; booleans
                    (equal? (read (open-input-string "#t")) #t)
                    (equal? (read (open-input-string "#f")) #f)
                    (equal? (read (open-input-string "#true")) #t)
                    (equal? (read (open-input-string "#false")) #f)

                    ;; numbers
                    (equal? (read (open-input-string "42")) 42)
                    (equal? (read (open-input-string "-17")) -17)
                    (equal? (read (open-input-string "3.5")) 3.5)
                    (equal? (read (open-input-string "#x10")) 16)
                    (equal? (read (open-input-string "#o10")) 8)
                    (equal? (read (open-input-string "#b10")) 2)

                    ;; symbols
                    (equal? (read (open-input-string "hello")) 'hello)
                    (equal? (read (open-input-string "#%app")) '#%app)
                    (equal? (read (open-input-string "(div .myClass)"))
                            '(div .myClass))

                    ;; keywords
                    (equal? (read (open-input-string "#:apple")) '#:apple)

                    ;; strings
                    (equal? (read (open-input-string "\"hi\"")) "hi")
                    (equal? (read (open-input-string "\"a\\n\\t\\\\\\\"b\"")) "a\n\t\\\"b")

                    ;; byte strings
                    (equal? (read (open-input-string "#\"ABC\"")) #"ABC")

                    ;; characters
                    (equal? (read (open-input-string "#\\a")) #\a)
                    (equal? (read (open-input-string "#\\A")) #\A)
                    (equal? (read (open-input-string "#\\x")) #\x)
                    (equal? (read (open-input-string "#\\X")) #\X)
                    (equal? (read (open-input-string "#\\space")) #\space)
                    (equal? (read (open-input-string "#\\newline")) #\newline)
                    (equal? (read (open-input-string "#\\tab")) #\tab)
                    (equal? (read (open-input-string "#\\return")) #\return)
                    (equal? (read (open-input-string "#\\x41")) #\A)
                    ;; delimiter characters as char literals
                    (equal? (read (open-input-string "#\\.")) #\.)
                    (equal? (read (open-input-string "#\\)")) #\))
                    (equal? (read (open-input-string "#\\;")) #\;)

                    ;; lists
                    (equal? (read (open-input-string "()")) '())
                    (equal? (read (open-input-string "[]")) '())
                    (equal? (read (open-input-string "{}")) '())
                    (equal? (read (open-input-string "(+ 1 2)")) '(+ 1 2))
                    (equal? (read (open-input-string "(define (f x) (+ x 1))"))
                            '(define (f x) (+ x 1)))
                    (equal? (read (open-input-string "((a b) (c d))"))
                            '((a b) (c d)))
                    (equal? (read (open-input-string "(a\nb\nc)"))
                            '(a b c))
                    (equal? (read (open-input-string "(a\rb\rc)"))
                            '(a b c))
                    (equal? (read (open-input-string "(a\r\nb\r\nc)"))
                            '(a b c))
                    (equal? (read (open-input-string "(a\n\rb\tc)"))
                            '(a b c))
                    (equal? (read (open-input-string "(a . b)")) '(a . b))

                    ;; vectors
                    (equal? (read (open-input-string "#(1 2 3)")) #(1 2 3))
                    (equal? (read (open-input-string "#[1 2 3]")) #(1 2 3))
                    (equal? (read (open-input-string "#{1 2 3}")) #(1 2 3))

                    ;; quote-like literals
                    (equal? (read (open-input-string "'x")) '(quote x))
                    (equal? (read (open-input-string "'(1 2 3)"))
                            '(quote (1 2 3)))

                    ;; comments
                    (equal? (read-all-from-string "1 ; line comment\n2") '(1 2))
                    (equal? (read-all-from-string "1 #| block #| nested |# comment |# 2")
                            '(1 2))
                    (equal? (read-all-from-string "1 #;(+ 40 2) 2") '(1 2))

                    ;; eof behavior and sequencing
                    (eof-object? (read (open-input-string "")))
                    (let ([p (open-input-string "1")])
                      (and (equal? (read p) 1)
                           (eof-object? (read p))))

                    (equal? (read-all-from-string "1 2 3")
                            '(1 2 3))
                    (equal? (read-all-from-string "(define x 1)\n(+ x 2)\n")
                            '((define x 1) (+ x 2)))
                    (equal? (read-all-from-string "(+ 1 2)\n")
                            '((+ 1 2))))))))
