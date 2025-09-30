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
;;; WRITE
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
      [(char=? ch #\space) (emit "space")]
      [(char=? ch #\newline) (emit "newline")]
      [(char=? ch #\return) (emit "return")]
      [(char=? ch #\tab) (emit "tab")]
      [(char=? ch #\backspace) (emit "backspace")]
      [(char=? ch #\vtab) (emit "vtab")]
      [(char=? ch #\page) (emit "page")]
      [(char=? ch #\nul) (emit "nul")]
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
      [(box? v)       (emit "#&")
                      (write-value (unbox v))]
      [(procedure? v) (emit "#<procedure>")]
      [else           (emit "#<unknown>")])
    (void))

  (write-value datum)
  (void))


;;;
;;; TEST
;;;


(define (test-write x)
  (reset-current-output-port!)
  (write x)
  (get-output-string (current-output-port)))

(test-write #"foo")
