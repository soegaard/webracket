#lang webracket
 
;;;
;;; 13.5 Writing
;;;

;; This file contains an implementation of the non-primitives in
;; section "13.5 Writing" of "The Reference".

;; Since parameters are not supported by webracket yet, we define
;; each parameter as function locally.

(define print-pair-curly-braces
  (let ([value #f]) ; default is #f
    (case-lambda
      [()    value]
      [(on?) (set! value on?)
             value])))

(define print-mpair-curly-braces       
  (let ([value #f]) ; default is #f
    (case-lambda
      [()    value]
      [(on?) (set! value on?)
             value])))

(define print-unreadable
  (let ([value #t]) ; default is #t
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-graph
  (let ([value #f]) ; default is #f
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-struct
  (let ([value #t]) ; default is #t
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-box
  (let ([value #t]) ; default is #t
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-vector-length
  (let ([value #f]) ; default is #f
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-hash-table
  (let ([value #t]) ; default is #t
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-boolean-long-form
  (let ([value #f]) ; default is #f
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-reader-abbreviations
  (let ([value #f]) ; default is #f
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-as-expression
  (let ([value #t]) ; default is #t
    (case-lambda
      [() value]
      [(on?)
       (set! value on?)
       value])))

(define print-syntax-width
  (let ([value 40]) ; default is 40 characters
    (case-lambda
      [() value]
      [(width)
       (set! value width)
       value])))

(define print-value-columns
  (let ([value 79]) ; default is 79 columns
    (case-lambda
      [() value]
      [(columns)
       (set! value columns)
       value])))

(define current-write-relative-directory
  (let ([value #f]) ; default is #f
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

;;;
;;; Current Output Port
;;;


(define current-output-port
  (let ([value (open-output-string)])
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

(define (reset-current-output-port!)
  (current-output-port (open-output-string)))

(define current-error-port
  (let ([value (open-output-string)])
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

(define (reset-current-error-port!)
  (current-output-port (open-error-string)))

;;;
;;; WRITE
;;;


(define (write datum [out (current-output-port)])
  (define (emit str)
    (write-string str out)
    (void))

  (define (emit-char ch)
    (write-char ch out)
    (void))

  (define (emit-hex-prefix prefix n digits)
    (define hex (string-upcase (number->string n 16)))
    (define len (string-length hex))
    (define padded
      (if (< len digits)
          (string-append (make-string (- digits len) #\0) hex)
          hex))
    (emit prefix)
    (emit padded))

  (define (emit-unicode-escape n)
    (emit "\\u{")
    (emit (string-upcase (number->string n 16)))
    (emit "}"))

  (define (write-string-char ch)
    (cond
      [(char=? ch #\")         (emit "\\\"")]
      [(char=? ch #\\)         (emit "\\\\")]
      [(char=? ch #\newline)   (emit "\\n")]
      [(char=? ch #\return)    (emit "\\r")]
      [(char=? ch #\tab)       (emit "\\t")]
      [(char=? ch #\backspace) (emit "\\b")]
      [(char=? ch #\vtab)      (emit "\\v")]
      [(char=? ch #\page)      (emit "\\f")]
      ;[(char=? ch #\alarm)     (emit "\\a")]
      [else
       (define code (char->integer ch))
       (if (or (< code 32) (= code 127))
           (emit-unicode-escape code)
           (emit-char ch))]))

  (define (write-string-literal str)
    (emit "\"")
    (let ([len (string-length str)])
      (let loop ([i 0])
        (if (< i len)
            (begin
              (write-string-char (string-ref str i))
              (loop (+ i 1)))
            (void))))
    (emit "\"")
    (void))

  (define (write-bytes-literal bs)
    (emit "#\"")
    (let ([len (bytes-length bs)])
      (let loop ([i 0])
        (if (< i len)
            (let ()
              (define b (bytes-ref bs i))
              (cond
                [(or (= b (char->integer #\")) (= b (char->integer #\\)))
                 (emit "\\")
                 (emit-char (integer->char b))]
                [(or (< b 32) (= b 127))
                 (emit "\\x")
                 (emit-hex-prefix "" b 2)]
                [else (emit-char (integer->char b))])
              (loop (+ i 1)))
            (void))))
    (emit "\"")
    (void))

  (define (write-char-literal ch)
    (emit "#\\")
    (cond
      [(char=? ch #\space)      (emit "space")]
      [(char=? ch #\newline)    (emit "newline")]
      [(char=? ch #\return)     (emit "return")]
      [(char=? ch #\tab)        (emit "tab")]
      [(char=? ch #\backspace)  (emit "backspace")]
      [(char=? ch #\vtab)       (emit "vtab")]
      [(char=? ch #\page)       (emit "page")]
      [(char=? ch #\nul)        (emit "nul")]
      ; [(char=? ch #\alarm) (emit "alarm")]
        [else
         (define code (char->integer ch))
         (if (or (< code 32) (= code 127))
             (begin (emit "\\u{")
                    (emit (string-upcase (number->string code 16)))
                    (emit "}"))
             (emit-char ch))])
    (void))

  (define (write-symbol sym)
    (emit (symbol->string sym)))

  (define (write-keyword kw)
    (emit "#:")
    (emit (keyword->string kw)))

  (define (write-proper-list lst open close)
    (emit open)
    (let loop ([rest lst] [first? #t])
      (cond
        [(pair? rest)
         (if first? (void) (emit " "))
         (write-value (car rest))
         (loop (cdr rest) #f)]
        [(null? rest) (emit close)]
        [else
         (emit " . ")
         (write-value rest)
         (emit close)]))
    (void))

  (define (write-mpair lst)
    (define curly? (print-mpair-curly-braces))
    (emit "#m")
    (emit (if curly? "{" "("))
    (let loop ([rest lst] [first? #t])
      (cond
        [(mpair? rest)
         (if first? (void) (emit " "))
         (write-value (mcar rest))
         (loop (mcdr rest) #f)]
        [(null? rest)
         (emit (if curly? "}" ")"))]
        [else
         (emit " . ")
         (write-value rest)
         (emit (if curly? "}" ")"))]))
    (void))

  (define (write-vector vec)
    (emit "#(")
    (let ([len (vector-length vec)])
      (let loop ([i 0])
        (if (< i len)
            (begin
              (if (> i 0) (emit " ") (void))
              (write-value (vector-ref vec i))
              (loop (+ i 1)))
            (void))))
    (emit ")")
    (void))

  (define (write-value v)
    (cond
      [(boolean? v)    (emit (if v "#t" "#f"))]
      [(void? v)       (emit "#<void>")]
      [(eof-object? v) (emit "#<eof>")]
      [(null? v)       (emit "()")]
      [(pair? v)       (let ([curly? (print-pair-curly-braces)])
                         (write-proper-list v
                                            (if curly? "{" "(")
                                            (if curly? "}" ")")))]
      [(mpair? v)     (write-mpair v)]
      [(symbol? v)    (write-symbol v)]
      [(keyword? v)   (write-keyword v)]
      [(char? v)      (write-char-literal v)]
      [(string? v)    (write-string-literal v)]
      [(bytes? v)     (write-bytes-literal v)]
      [(number? v)    (emit (number->string v))]
      [(vector? v)    (write-vector v)]
      [(struct? v)    (let ([vec (struct->vector v)])
                        (if (print-struct)
                            (write-vector vec)
                            (begin
                              (emit "#<")
                              (write-symbol (vector-ref vec 0))
                              (emit ">"))))]
      [(box? v)       (emit "#&")
                      (write-value (unbox v))]
      [(procedure? v) (emit "#<procedure>")]
      [else           (emit "#<unknown>")])
    (void))

  (write-value datum)
  (void))

;;;
;;; DISPLAY
;;;

(define (display datum [out (current-output-port)])
  (define (emit str)
    (write-string str out)
    (void))

  (define (emit-char ch)
    (write-char ch out)
    (void))

  (define (emit-bytes bs)
    (write-bytes bs out)
    (void))

  (define (display-proper-list lst open close)
    (emit open)
    (let loop ([rest lst] [first? #t])
      (cond
        [(pair? rest)
         (if first? (void) (emit " "))
         (display-value (car rest))
         (loop (cdr rest) #f)]
        [(null? rest) (emit close)]
        [else
         (emit " . ")
         (display-value rest)
         (emit close)]))
    (void))

  (define (display-mpair lst)
    (define curly? (print-mpair-curly-braces))
    (emit "#m")
    (emit (if curly? "{" "("))
    (let loop ([rest lst] [first? #t])
      (cond
        [(mpair? rest)
         (if first? (void) (emit " "))
         (display-value (mcar rest))
         (loop (mcdr rest) #f)]
        [(null? rest)
         (emit (if curly? "}" ")"))]
        [else
         (emit " . ")
         (display-value rest)
         (emit (if curly? "}" ")"))]))
    (void))

  (define (display-vector vec)
    (emit "#(")
    (let ([len (vector-length vec)])
      (let loop ([i 0])
        (if (< i len)
            (begin
              (if (> i 0) (emit " ") (void))
              (display-value (vector-ref vec i))
              (loop (+ i 1)))
            (void))))
    (emit ")")
    (void))

  (define (display-value v)
    (cond
      [(boolean? v)    (emit (if v "#t" "#f"))]
      [(void? v)       (emit "#<void>")]
      [(eof-object? v) (emit "#<eof>")]
      [(null? v)       (emit "()")]
      [(pair? v)       (let ([curly? (print-pair-curly-braces)])
                         (display-proper-list v
                                              (if curly? "{" "(")
                                              (if curly? "}" ")")))]
      [(mpair? v)     (display-mpair v)]
      [(symbol? v)    (emit (symbol->string v))]
      [(keyword? v)   (begin (emit "#:")
                              (emit (keyword->string v)))]
      [(char? v)      (emit-char v)]
      [(string? v)    (emit v)]
      [(bytes? v)     (emit-bytes v)]
      [(number? v)    (emit (number->string v))]
      [(vector? v)    (display-vector v)]
      [(struct? v)    (let ([vec (struct->vector v)])
                        (if (print-struct)
                            (display-vector vec)
                            (begin
                              (emit "#<")
                              (emit (symbol->string (vector-ref vec 0)))
                              (emit ">"))))]
      [(box? v)       (emit "#&")
                      (display-value (unbox v))]
      [(procedure? v) (emit "#<procedure>")]
      [else           (emit "#<unknown>")])
    (void))

  (display-value datum)
  (void))


;;;
;;; PRINT
;;;


(define (print datum [out (current-output-port)] [quote-depth 0])
  (define (print-self-evaluating? v)
    (or (number? v)
        (boolean? v)
        (char? v)
        (string? v)
        (bytes? v)
        (keyword? v)))

  (define (print-quotable? v)
    (cond
      [(null? v)    #t]
      [(pair? v)    (let loop ([rest v])
                      (cond
                        [(pair? rest)
                         (and (print-quotable? (car rest))
                              (loop (cdr rest)))]
                        [(null? rest) #t]
                        [else (print-quotable? rest)]))]
      [(vector? v)  (let ([len (vector-length v)])
                      (let loop ([i 0])
                        (cond
                          [(= i len) #t]
                          [else
                           (and (print-quotable? (vector-ref v i))
                                (loop (+ i 1)))])))]
      [(symbol? v)  #t]
      [(keyword? v) #t]
      [(print-self-evaluating? v) #t]
      [else #f]))

  (define (print-needs-quote? v depth)
    (and (= depth 0)
         (print-quotable? v)
         (not (print-self-evaluating? v))))

  (if (not (print-as-expression))
      (begin (write datum out) (void))
      (let ()
        (define (emit str)
          (write-string str out)
          (void))

        (define (emit-char ch)
          (write-char ch out)
          (void))

        (define (emit-hex-prefix prefix n digits)
          (define hex (string-upcase (number->string n 16)))
          (define len (string-length hex))
          (define padded
            (if (< len digits)
                (string-append (make-string (- digits len) #\0) hex)
                hex))
          (emit prefix)
          (emit padded))

        (define (emit-unicode-escape n)
          (emit "\\u{")
          (emit (string-upcase (number->string n 16)))
          (emit "}"))

        (define (write-string-char ch)
          (cond
            [(char=? ch #\")         (emit "\\\"")]
            [(char=? ch #\\)         (emit "\\\\")]
            [(char=? ch #\newline)   (emit "\\n")]
            [(char=? ch #\return)    (emit "\\r")]
            [(char=? ch #\tab)       (emit "\\t")]
            [(char=? ch #\backspace) (emit "\\b")]
            [(char=? ch #\vtab)      (emit "\\v")]
            [(char=? ch #\page)      (emit "\\f")]
            [else
             (define code (char->integer ch))
             (if (or (< code 32) (= code 127))
                 (emit-unicode-escape code)
                 (emit-char ch))]))

        (define (write-string-literal str)
          (emit "\"")
          (let ([len (string-length str)])
            (let loop ([i 0])
              (if (< i len)
                  (begin
                    (write-string-char (string-ref str i))
                    (loop (+ i 1)))
                  (void))))
          (emit "\"")
          (void))

        (define (write-bytes-literal bs)
          (emit "#\"")
          (let ([len (bytes-length bs)])
            (let loop ([i 0])
              (if (< i len)
                  (let ()
                    (define b (bytes-ref bs i))
                    (cond
                      [(or (= b (char->integer #\")) (= b (char->integer #\\)))
                       (emit "\\")
                       (emit-char (integer->char b))]
                      [(or (< b 32) (= b 127))
                       (emit "\\x")
                       (emit-hex-prefix "" b 2)]
                      [else (emit-char (integer->char b))])
                    (loop (+ i 1)))
                  (void))))
          (emit "\"")
          (void))

        (define (write-char-literal ch)
          (emit "#\\")
          (cond
            [(char=? ch #\space)      (emit "space")]
            [(char=? ch #\newline)    (emit "newline")]
            [(char=? ch #\return)     (emit "return")]
            [(char=? ch #\tab)        (emit "tab")]
            [(char=? ch #\backspace)  (emit "backspace")]
            [(char=? ch #\vtab)       (emit "vtab")]
            [(char=? ch #\page)       (emit "page")]
            [(char=? ch #\nul)        (emit "nul")]
            [else
             (define code (char->integer ch))
             (if (or (< code 32) (= code 127))
                 (begin (emit "\\u{")
                        (emit (string-upcase (number->string code 16)))
                        (emit "}"))
                 (emit-char ch))])
          (void))

        (define (write-symbol sym)
          (emit (symbol->string sym)))

        (define (write-keyword kw)
          (emit "#:")
          (emit (keyword->string kw)))

        (define (print-proper-list lst depth)
          (define curly? (print-pair-curly-braces))
          (define quote? (print-needs-quote? lst depth))
          (when quote? (emit "'"))
          (emit (if curly? "{" "("))
          (let loop ([rest lst] [first? #t])
            (cond
              [(pair? rest)
               (if first? (void) (emit " "))
               (print-value (car rest) (if quote? 1 depth))
               (loop (cdr rest) #f)]
              [(null? rest)
               (emit (if curly? "}" ")"))]
              [else
               (emit " . ")
               (print-value rest (if quote? 1 depth))
               (emit (if curly? "}" ")"))]))
          (void))

        (define (print-mpair lst depth)
          (define curly? (print-mpair-curly-braces))
          (emit "#m")
          (emit (if curly? "{" "("))
          (let loop ([rest lst] [first? #t])
            (cond
              [(mpair? rest)
               (if first? (void) (emit " "))
               (print-value (mcar rest) depth)
               (loop (mcdr rest) #f)]
              [(null? rest)
               (emit (if curly? "}" ")"))]
              [else
               (emit " . ")
               (print-value rest depth)
               (emit (if curly? "}" ")"))]))
          (void))

        (define (print-vector vec depth)
          (define quote? (print-needs-quote? vec depth))
          (when quote? (emit "'"))
          (emit "#(")
          (let ([len (vector-length vec)])
            (let loop ([i 0])
              (if (< i len)
                  (begin
                    (if (> i 0) (emit " ") (void))
                    (print-value (vector-ref vec i) (if quote? 1 depth))
                    (loop (+ i 1)))
                  (void))))
          (emit ")")
          (void))

        (define (print-boolean v)
          (emit (if v "#t" "#f")))

        (define (print-value v depth)
          (cond
            [(boolean? v)    (print-boolean v)]
            [(void? v)       (emit "#<void>")]
            [(eof-object? v) (emit "#<eof>")]
            [(null? v)
             (when (print-needs-quote? v depth) (emit "'"))
             (emit "()")]
            [(pair? v)       (print-proper-list v depth)]
            [(mpair? v)      (print-mpair v depth)]
            [(symbol? v)
             (when (print-needs-quote? v depth) (emit "'"))
             (write-symbol v)]
            [(keyword? v)    (write-keyword v)]
            [(char? v)       (write-char-literal v)]
            [(string? v)     (write-string-literal v)]
            [(bytes? v)      (write-bytes-literal v)]
            [(number? v)     (emit (number->string v))]
            [(vector? v)     (print-vector v depth)]
            [(struct? v)    (let ([vec (struct->vector v)])
                              (if (print-struct)
                                  (print-vector vec depth)
                                  (begin
                                    (emit "#<")
                                    (write-symbol (vector-ref vec 0))
                                    (emit ">"))))]
            [(box? v)       (emit "#&")
                             (print-value (unbox v) depth)]
            [(procedure? v) (emit "#<procedure>")]
            [else           (emit "#<unknown>")])
          (void))

        (print-value datum quote-depth)
        (void))))


;;;
;;; DERIVED
;;;

(define (writeln datum [out (current-output-port)])
  (write datum out)
  (newline out))

(define (displayln datum [out (current-output-port)])
  (display datum out)
  (newline out))

(define (println datum [out (current-output-port)] [quote-depth 0])
  (print datum out quote-depth)
  (newline out))



;;;
;;; TEST
;;;


(define (test-write x)
  (reset-current-output-port!)
  (write x)
  (get-output-string (current-output-port)))

(define (write-test-case label datum expected)
  (let ([actual (test-write datum)])
    (list label (string=? actual expected) actual expected)))

(define (write-test-case/with parameter new-value label datum expected)
  (let ([original (parameter)])
    (parameter new-value)
    (define result (write-test-case label datum expected))
    (parameter original)
    result))

(define write-tests
  (list
   (list "booleans"
         (write-test-case "#t" #t "#t")
         (write-test-case "#f" #f "#f"))

   (list "special values"
         (write-test-case "void" (void) "#<void>")
         #;(write-test-case "eof" (eof-object) "#<eof>")
         (write-test-case "null" '() "()"))

   (list "numbers"
         (write-test-case "zero" 0 "0")
         (write-test-case "negative fixnum" -42 "-42")
         (write-test-case "flonum" 3.25 "3.25"))

   (list "characters"
         (write-test-case "letter" #\a "#\\a")
         (write-test-case "newline" #\newline "#\\newline")
         (write-test-case "nul" #\nul "#\\nul"))

   (list "strings"
         (write-test-case "empty" "" (string #\" #\"))
         (write-test-case "newline escape" "a\nb"
                          (string #\" #\a #\\ #\n #\b #\"))
         (write-test-case "backslash escape" "a\\b"
                          (string #\" #\a #\\ #\\ #\b #\"))
         (write-test-case "double quote escape" "a\"b"
                          (string #\" #\a #\\ #\" #\b #\")))

   (list "bytes"
         (write-test-case "plain" #"A" (string #\# #\" #\A #\"))
         (write-test-case "hex escape" #"\n"
                          (string #\# #\" #\\ #\x #\0 #\A #\"))
         (write-test-case "quoted characters" #"\"\\"
                          (string #\# #\" #\\ #\" #\\ #\\ #\")))

   (list "symbols and keywords"
         (write-test-case "symbol" 'sample "sample")
         (write-test-case "keyword" '#:sample "#:sample"))

   (list "pairs and lists"
         (write-test-case "list" '(1 2 3) "(1 2 3)")
         (write-test-case "nested list" '(1 (2) 3) "(1 (2) 3)")
         (write-test-case "improper list" (cons 1 2) "(1 . 2)"))

   (list "pair parameters"
         (write-test-case/with print-pair-curly-braces #t
                               "curly braces" '(1 2) "{1 2}")
         (write-test-case "pair parameter reset" '(1 2) "(1 2)"))

   (list "vectors and boxes"
         (write-test-case "vector" '#(1 2 (3)) "#(1 2 (3))")
         (write-test-case "box" (box 'a) "#&a"))

   (list "procedures"
         (write-test-case "lambda" (lambda (x) x) "#<procedure>"))

   (list "write/struct-transparent"
         (let ()
           (struct write-point (x y) #:transparent)
           (let* ([port (open-output-string)]
                  [p    (write-point 1 2)])
             (write p port)
             (equal? (get-output-string port) "#(struct:write-point 1 2)"))))

   (list "write/struct-print-disabled"
         (let ()
           (struct hidden-point (x) #:transparent)
           (let* ([port (open-output-string)]
                  [p    (hidden-point 42)]
                  [old  (print-struct)])
             (print-struct #f)
             (write p port)
             (print-struct old)
             (equal? (get-output-string port) "#<struct:hidden-point>"))))
   ))


#;(define (test-writing)

  (define (test-display x)
    (reset-current-output-port!)
    (display x)
    (get-output-string (current-output-port)))

  (define (display-test-case label datum expected)
    (let ([actual (test-display datum)])
      (list label (string=? actual expected) actual expected)))

  (define (display-test-case/with parameter new-value label datum expected)
    (let ([original (parameter)])
      (parameter new-value)
      (define result (display-test-case label datum expected))
      (parameter original)
      result))

  (define display-tests
    (list
     (list "booleans"
           (display-test-case "#t" #t "#t")
           (display-test-case "#f" #f "#f"))

     (list "special values"
           (display-test-case "void" (void) "#<void>")
           (display-test-case "null" '() "()"))

     (list "numbers"
           (display-test-case "zero" 0 "0")
           (display-test-case "negative" -7 "-7")
           (display-test-case "flonum" 1.25 "1.25"))

     (list "characters"
           (display-test-case "letter" #\a (string #\a))
           (display-test-case "newline" #\newline (string #\newline)))

     (list "strings"
           (display-test-case "empty" "" "")
           (display-test-case "plain" "hello" "hello")
           (display-test-case "with quotes" "a\"b" "a\"b"))

     (list "bytes"
           (display-test-case "plain" #"A" "A")
           (display-test-case "multiple" #"ABC" "ABC"))

     (list "symbols and keywords"
           (display-test-case "symbol" 'sample "sample")
           (display-test-case "keyword" '#:sample "#:sample"))

     (list "pairs and lists"
           (display-test-case "list" '(1 2 3) "(1 2 3)")
           (display-test-case "nested" '(1 "a" 3) "(1 a 3)")
           (display-test-case "improper" (cons 1 2) "(1 . 2)"))

     (list "pair parameters"
           (display-test-case/with print-pair-curly-braces #t
                                   "curly braces" '(1 2) "{1 2}")
           (display-test-case "pair parameter reset" '(1 2) "(1 2)"))

     (list "vectors and boxes"
           (display-test-case "vector" '#(1 "a" 3) "#(1 a 3)")
           (display-test-case "box" (box 'a) "#&a"))

     (list "procedures"
           (display-test-case "lambda" (lambda (x) x) "#<procedure>"))

     (list "display/mpair"
           (display-test-case "mutable pair" (mcons 1 (mcons 2 null)) "#m(1 2)"))

     (list "display/struct"
           (let ()
             (struct display-point (x y) #:transparent)
             (let* ([port (open-output-string)]
                    [p    (display-point 1 2)])
               (display p port)
               (equal? (get-output-string port) "#(struct:display-point 1 2)"))))

     (list "display/struct-print-disabled"
           (let ()
             (struct hidden-point (x) #:transparent)
             (let* ([port (open-output-string)]
                    [p    (hidden-point 42)]
                    [old  (print-struct)])
               (print-struct #f)
               (display p port)
               (print-struct old)
               (equal? (get-output-string port) "#<struct:hidden-point>"))))
     ))

  (define (test-print x [quote-depth 0])
    (reset-current-output-port!)
    (let ([out (current-output-port)])
      (print x out quote-depth)
      (get-output-string out)))

  (define (print-test-case label datum expected [quote-depth 0])
    (let ([actual (test-print datum quote-depth)])
      (list label (string=? actual expected) actual expected)))

  (define (print-test-case/with parameter new-value label datum expected [quote-depth 0])
    (let ([original (parameter)])
      (parameter new-value)
      (define result (print-test-case label datum expected quote-depth))
      (parameter original)
      result))

  (define print-tests
    (list
     (list "self-evaluating"
           (print-test-case "boolean" #t "#t")
           (print-test-case "number" 42 "42")
           (print-test-case "string" "hi" (string #\" #\h #\i #\"))
           (print-test-case "bytes" #"hi" (string #\# #\" #\h #\i #\"))
           (print-test-case "char" #\a "#\\a"))

     (list "quotable values"
           (print-test-case "null" '() "'()")
           (print-test-case "symbol" 'sample "'sample")
           (print-test-case "list" '(1 2) "'(1 2)")
           (print-test-case "vector" '#(1 2) "'#(1 2)"))

     (list "quote depth"
           (print-test-case "symbol depth 1" 'sample "sample" 1)
           (print-test-case "list depth 1" '(1 2) "(1 2)" 1))

     (list "print parameters"
           (print-test-case/with print-pair-curly-braces #t
                                 "curly braces" '(1 2) "'{1 2}")
           (print-test-case "pair parameter reset" '(1 2) "'(1 2)")
           (print-test-case/with print-as-expression #f
                                 "print-as-expression #f" '(1 2) "(1 2)"))

     (list "struct printing"
           (let ()
             (struct print-point (x y) #:transparent)
             (let* ([port (open-output-string)]
                    [p    (print-point 1 2)])
               (print p port)
               (equal? (get-output-string port) "'#(struct:print-point 1 2)"))))

     (list "struct printing disabled"
           (let ()
             (struct hidden-point (x) #:transparent)
             (let* ([port (open-output-string)]
                    [p    (hidden-point 42)]
                    [old  (print-struct)])
               (print-struct #f)
               (print p port)
               (print-struct old)
               (equal? (get-output-string port) "#<struct:hidden-point>"))))))

  (list write-tests display-tests print-tests))

#;(test-writing)

;;;
;;; INITIALIZE
;;;

(reset-current-output-port!)
