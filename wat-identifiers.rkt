#lang racket/base

(provide rewrite-wat-identifiers)

(require (only-in racket/format ~a))

;; rewrite-wat-identifiers : string? -> string?
;;   Rewrite invalid unquoted WAT identifiers ($...) into quoted form ($"..."),
;;   while preserving strings and comments.
(define (rewrite-wat-identifiers s)
  (define n (string-length s))
  (define out (open-output-string))

  (define (emit str) (display str out))
  (define (emit-char ch) (write-char ch out))
  (define (ch i) (string-ref s i))
  (define (in-range? i) (< i n))

  (define (delimiter? c)
    (or (char-whitespace? c)
        (char=? c #\()
        (char=? c #\))
        (char=? c #\")
        (char=? c #\;)))

  (define (wat-unquoted-idchar? c)
    (define i (char->integer c))
    (and (<= #x21 i #x7e) ; printable ASCII without space
         (not (memq c '(#\" #\, #\; #\( #\) #\[ #\] #\{ #\})))))

  (define (valid-unquoted-id-token? token)
    (and (> (string-length token) 1)
         (char=? (string-ref token 0) #\$)
         (for/and ([c (in-string (substring token 1))])
           (wat-unquoted-idchar? c))))

  (define (escape-quoted-id-name name)
    (define b (open-output-string))
    (for ([c (in-string name)])
      (cond
        [(char=? c #\\) (display "\\\\" b)]
        [(char=? c #\") (display "\\\"" b)]
        [else
         (define i (char->integer c))
         (if (or (< i 32) (= i 127))
             (display (~a "\\u{" (number->string i 16) "}") b)
             (write-char c b))]))
    (get-output-string b))

  (let loop ([i 0] [state 'code] [comment-depth 0] [escaped? #f])
    (cond
      [(not (in-range? i))
       (get-output-string out)]

      [(eq? state 'string)
       (define c (ch i))
       (emit-char c)
       (cond
         [escaped? (loop (add1 i) 'string comment-depth #f)]
         [(char=? c #\\) (loop (add1 i) 'string comment-depth #t)]
         [(char=? c #\") (loop (add1 i) 'code comment-depth #f)]
         [else (loop (add1 i) 'string comment-depth #f)])]

      [(eq? state 'line-comment)
       (define c (ch i))
       (emit-char c)
       (if (char=? c #\newline)
           (loop (add1 i) 'code comment-depth #f)
           (loop (add1 i) 'line-comment comment-depth #f))]

      [(eq? state 'block-comment)
       (cond
         [(and (< (add1 i) n)
               (char=? (ch i) #\()
               (char=? (ch (add1 i)) #\;))
          (emit "(;")
          (loop (+ i 2) 'block-comment (add1 comment-depth) #f)]
         [(and (< (add1 i) n)
               (char=? (ch i) #\;)
               (char=? (ch (add1 i)) #\)))
          (emit ";)")
          (define next-depth (sub1 comment-depth))
          (loop (+ i 2)
                (if (zero? next-depth) 'code 'block-comment)
                next-depth
                #f)]
         [else
          (emit-char (ch i))
          (loop (add1 i) 'block-comment comment-depth #f)])]

      [else ; code
       (cond
         [(and (< (add1 i) n)
               (char=? (ch i) #\;)
               (char=? (ch (add1 i)) #\;))
          (emit ";;")
          (loop (+ i 2) 'line-comment comment-depth #f)]

         [(and (< (add1 i) n)
               (char=? (ch i) #\()
               (char=? (ch (add1 i)) #\;))
          (emit "(;")
          (loop (+ i 2) 'block-comment 1 #f)]

         [(char=? (ch i) #\")
          (emit-char #\")
          (loop (add1 i) 'string comment-depth #f)]

         [(char=? (ch i) #\$)
          (cond
            [(and (< (add1 i) n) (char=? (ch (add1 i)) #\"))
             (emit "$\"")
             (loop (+ i 2) 'string comment-depth #f)]
            [else
             (define j
               (let find-end ([k i])
                 (if (or (not (in-range? k))
                         (delimiter? (ch k)))
                     k
                     (find-end (add1 k)))))
             (define token (substring s i j))
             (if (valid-unquoted-id-token? token)
                 (emit token)
                 (let ([name (substring token 1)])
                   (emit (~a "$\"" (escape-quoted-id-name name) "\""))))
             (loop j 'code comment-depth #f)])]

         [else
          (emit-char (ch i))
          (loop (add1 i) 'code comment-depth #f)])])))

