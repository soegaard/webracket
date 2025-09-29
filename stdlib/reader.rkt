#lang racket/base
;; WebRacket Reader — Lexer (booleans + radix/exactness) — parens fixed v2
;; - #true / #false (case-insensitive, delimiter-checked)
;; - Radix/exactness prefixes (#x #o #b #d #e #i), including chains like #e#x10
;; - Strings and byte strings, characters, quote-like, delimiters, barewords
;; - Skips whitespace, BOM, ';' comments, and nested #| ... |# comments

(require racket/port
         racket/format
         racket/string)

;; ----------------------------- Data ---------------------------------------
(struct token (type val lexeme loc) #:transparent)
(struct lx    (in source buf)       #:transparent #:mutable)

;; --------------------------- Construction ---------------------------------
(define (make-lexer in source)
  (port-count-lines! in)
  (lx in source '()))

;; ----------------------------- Utilities ----------------------------------
(define (make-srcloc-from-port in source start-line start-col start-pos)
  (define-values (end-line end-col end-pos) (port-next-location in))
  (make-srcloc source start-line start-col start-pos (- end-pos start-pos)))

(define (raise-read-error who msg loc)
  (raise
   (exn:fail:read
    (format "~a: ~a" who msg)
    (current-continuation-marks)
    loc)))

(define (hex-digit->val ch)
  (cond [(and (char>=? ch #\0) (char<=? ch #\9)) (- (char->integer ch) (char->integer #\0))]
        [(and (char>=? ch #\a) (char<=? ch #\f)) (+ 10 (- (char->integer ch) (char->integer #\a)))]
        [(and (char>=? ch #\A) (char<=? ch #\F)) (+ 10 (- (char->integer ch) (char->integer #\A)))]
        [else #f]))

;; Character classes ---------------------------------------------------------
(define (delimiter-char? ch)
  (or (eof-object? ch)
      (char-whitespace? ch)
      (member ch '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\.))))

;; ------------------------------ Skipper -----------------------------------
(define (skip-space+comments L)
  (define in (lx-in L))
  (let loop ()
    (define ch (peek-char in))
    (cond
      [(eof-object? ch) (void)]
      [(or (char-whitespace? ch) (char=? ch #\uFEFF))
       (read-char in) (loop)]
      [(char=? ch #\;)
       (read-char in)
       (let line-skip ()
         (define c (read-char in))
         (cond [(eof-object? c) (void)]
               [(char=? c #\newline) (void)]
               [else (line-skip)]))
       (loop)]
      [(char=? ch #\#)
       (define s (peek-string 2 0 in))
       (cond
         [(and (string? s) (string=? s "#|"))
          (read-char in) (read-char in)
          (skip-nested-block-comment in (lx-source L))
          (loop)]
         [(and (string? s) (string=? s "#;")) (void)]
         [else (void)])]
      [else (void)])))

(define (skip-nested-block-comment in source)
  (define-values (sl sc sp) (port-next-location in))
  (define (err msg)
    (raise-read-error 'skip-nested-block-comment msg
                      (make-srcloc source sl sc sp 2)))
  (let loop ([depth 1])
    (define c (read-char in))
    (cond
      [(eof-object? c) (err "unterminated block comment #| ... |#")] 
      [(char=? c #\#)
       (define c2 (peek-char in))
       (cond
         [(eof-object? c2) (err "unterminated block comment #| ... |#")] 
         [(char=? c2 #\|) (read-char in) (loop (add1 depth))]
         [else (loop depth)])]
      [(char=? c #\|)
       (define c2 (peek-char in))
       (cond
         [(eof-object? c2) (err "unterminated block comment #| ... |#")] 
         [(char=? c2 #\#)
          (read-char in)
          (if (= depth 1) (void) (loop (sub1 depth)))]
         [else (loop depth)])]
      [else (loop depth)])))

;; --------------------------- Trivial scanners ------------------------------
(define (scan-delim-or-dot L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (define ch (read-char in))
  (define type
    (case ch
      [(#\() 'lparen]
      [(#\)) 'rparen]
      [(#\[) 'lbracket]
      [(#\]) 'rbracket]
      [(#\{) 'lbrace]
      [(#\}) 'rbrace]
      [(#\.) 'dot]
      [else (error 'scan-delim-or-dot (format "unexpected char ~a" ch))]))
  (define loc (make-srcloc-from-port in (lx-source L) sl sc sp))
  (token type #f (string ch) loc))

(define (scan-quote-like L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (define ch (read-char in))
  (case ch
    [(#\') (token 'quote #f "'" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    [(#\`) (token 'quasiquote #f "`" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    [(#\,)
     (define nxt (peek-char in))
     (if (and (char? nxt) (char=? nxt #\@))
         (begin
           (read-char in)
           (token 'unquote-splicing #f ",@"
                  (make-srcloc-from-port in (lx-source L) sl sc sp)))
         (token 'unquote #f ","
                (make-srcloc-from-port in (lx-source L) sl sc sp)))]
    [else (error 'scan-quote-like (format "unexpected char ~a" ch))]))

;; --------------------------- Strings & Bytes -------------------------------
(define (scan-string* L #:bytes? [bytes? #f])
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (when bytes? (read-char in))     ; consumed '#'
  (define ch0 (read-char in))      ; opening '"'
  (unless (char=? ch0 #\") (error 'scan-string* "internal: expected opening \""))
  (define out-str (open-output-string))
  (define out-bytes (if bytes? (open-output-bytes) #f))
  (define (return x) x)
  (let loop ()
    (define ch (read-char in))
    (cond
      [(eof-object? ch)
       (raise-read-error 'scan-string*
                         (if bytes? "unterminated byte string" "unterminated string")
                         (make-srcloc (lx-source L) sl sc sp 1))]
      [(char=? ch #\")
       (define lexeme
         (if bytes?
             (string-append "#\"" (get-output-string out-str) "\"")
             (string-append "\"" (get-output-string out-str) "\"")))
       (define val (if bytes?
                       (get-output-bytes out-bytes)
                       (get-output-string out-str)))
       (return (token (if bytes? 'bytes 'string) val lexeme
                      (make-srcloc-from-port in (lx-source L) sl sc sp)))]
      [(char=? ch #\\)
       (define esc (read-char in))
       (cond
         [(eof-object? esc)
          (raise-read-error 'scan-string* "dangling escape in string"
                            (make-srcloc (lx-source L) sl sc sp 1))]
         [(member esc '(#\n #\r #\t #\" #\\))
          (define real
            (case esc
              [(#\n) #\newline]
              [(#\r) #\return]
              [(#\t) #\tab]
              [(#\") #\"]
              [(#\\) #\\]))
          (write-char real out-str)
          (when bytes?
            (define b (char->integer real))
            (when (>= b 256)
              (raise-read-error 'scan-string* "non-byte in byte string escape"
                                (make-srcloc (lx-source L) sl sc sp 1)))
            (write-byte b out-bytes))
          (loop)]
         [(char=? esc #\x)
          ;; \xHEX...; terminated by ';'
          (define (read-hex-acc acc)
            (define c (read-char in))
            (cond
              [(eof-object? c)
               (raise-read-error 'scan-string* "unterminated \\x...; escape"
                                 (make-srcloc (lx-source L) sl sc sp 1))]
              [(char=? c #\;)
               acc]
              [else
               (define v (hex-digit->val c))
               (unless v (raise-read-error 'scan-string* "invalid hex digit in \\x"
                                           (make-srcloc (lx-source L) sl sc sp 1)))
               (read-hex-acc (+ (* acc 16) v))]))
          (define code (read-hex-acc 0))
          (when (and bytes? (>= code 256))
            (raise-read-error 'scan-string* "byte value > 255 in byte string"
                              (make-srcloc (lx-source L) sl sc sp 1)))
          (define ch2 (integer->char code))
          (write-char ch2 out-str)
          (when bytes? (write-byte code out-bytes))
          (loop)]
         [else
          (write-char esc out-str)
          (when bytes?
            (define b (char->integer esc))
            (when (>= b 256)
              (raise-read-error 'scan-string* "non-byte in byte string"
                                (make-srcloc (lx-source L) sl sc sp 1)))
            (write-byte b out-bytes))
          (loop)])]
      [else
       (write-char ch out-str)
       (when bytes?
         (define b (char->integer ch))
         (when (>= b 256)
           (raise-read-error 'scan-string* "non-byte character in byte string"
                             (make-srcloc (lx-source L) sl sc sp 1)))
         (write-byte b out-bytes))
       (loop)])))

(define (scan-string L)
  (scan-string* L))

(define (scan-bytes L)
  (scan-string* L #:bytes? #t))

;; --------------------------- Booleans & Chars ------------------------------
(define (scan-boolean L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (read-char in) ; '#'
  (define torf (read-char in))
  (define val (case torf [(#\t) #t] [(#\f) #f] [else (error 'scan-boolean "internal")]))
  (token 'boolean val (string-append "#" (string torf))
         (make-srcloc-from-port in (lx-source L) sl sc sp)))

(define (scan-char L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (define s2 (peek-string 2 0 in))
  (unless (and (string? s2) (string=? s2 "#\\"))
    (error 'scan-char "internal: expected #\\"))
  (read-char in) (read-char in)
  (define ch (peek-char in))
  (cond
    [(eof-object? ch)
     (raise-read-error 'scan-char "unexpected EOF after #\\"
                       (make-srcloc (lx-source L) sl sc sp 1))]
    [else
     (define (read-name)
       (define out (open-output-string))
       (let loop ()
         (define c (peek-char in))
         (cond
           [(eof-object? c) (get-output-string out)]
           [(delimiter-char? c) (get-output-string out)]
           [else (write-char (read-char in) out) (loop)])))
     (cond
       [(char-ci=? ch #\x)
        (read-char in)
        (define out 0)
        (let hex-loop ()
          (define c (peek-char in))
          (cond
            [(eof-object? c) (void)]
            [(delimiter-char? c) (void)]
            [else
             (define v (hex-digit->val c))
             (unless v (raise-read-error 'scan-char "invalid hex in #\\x..."
                                         (make-srcloc (lx-source L) sl sc sp 1)))
             (read-char in)
             (set! out (+ (* out 16) v))
             (hex-loop)]))
        (define the (integer->char out))
        (token 'char the (format "#\\x~x" (char->integer the))
               (make-srcloc-from-port in (lx-source L) sl sc sp))]
       [else
        (define name (string-downcase (read-name)))
        (define the
          (cond [(string=? name "space") #\space]
                [(string=? name "newline") #\newline]
                [(string=? name "tab") #\tab]
                [(string=? name "return") #\return]
                [(= (string-length name) 1) (string-ref name 0)]
                [else
                 (raise-read-error 'scan-char (format "unknown character name ~a" name)
                                   (make-srcloc (lx-source L) sl sc sp 1))]))
        (token 'char the (string-append "#\\" name)
               (make-srcloc-from-port in (lx-source L) sl sc sp))])]))

;; --------------------------- Sharp-dispatch -------------------------------
(define (scan-sharp-dispatch L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (define s2 (peek-string 2 0 in))
  (define s5 (peek-string 5 0 in))
  (define s6 (peek-string 6 0 in))
  (define (delim-after? n)
    (define s (peek-string (+ n 1) 0 in))
    (and (string? s)
         (or (< (string-length s) (+ n 1))
             (delimiter-char? (string-ref s n)))))
  (cond
    [(and (string? s2) (string=? s2 "#;"))
     (read-char in) (read-char in)
     (token 'datum-comment #f "#;" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    [(and (string? s2) (string=? s2 "#("))
     (read-char in) (read-char in)
     (token 'vector-start #f "#(" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    [(and (string? s2) (string=? s2 "#\\")) (scan-char L)]
    [(and (string? s2) (string=? s2 "#\"")) (scan-bytes L)]
    ;; boolean aliases #true / #false (case-insensitive)
    [(and (string? s5) (string-ci=? s5 "#true") (delim-after? 5))
     (for ([i (in-range 5)]) (read-char in))
     (token 'boolean #t "#true" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    [(and (string? s6) (string-ci=? s6 "#false") (delim-after? 6))
     (for ([i (in-range 6)]) (read-char in))
     (token 'boolean #f "#false" (make-srcloc-from-port in (lx-source L) sl sc sp))]
    ;; short booleans #t/#f
    [(and (string? s2) (or (string-ci=? s2 "#t") (string-ci=? s2 "#f")))
     (scan-boolean L)]
    ;; radix/exactness prefixes → treat as part of a bareword/number
    [(and (string? s2)
          (member (char-downcase (string-ref s2 1)) '(#\x #\o #\b #\d #\e #\i)))
     (scan-bareword L)]
    [else
     (raise-read-error 'lexer
                       (format "unsupported sharp-dispatch starting with ~a" s2)
                       (make-srcloc (lx-source L) sl sc sp 1))]))

;; --------------------------- Barewords -------------------------------------
(define (scan-bareword L)
  (define in (lx-in L))
  (define-values (sl sc sp) (port-next-location in))
  (define lexeme (accum-bareword in))
  (define type+val (classify-bareword lexeme))
  (token (car type+val) (cdr type+val) lexeme
         (make-srcloc-from-port in (lx-source L) sl sc sp)))

(define (accum-bareword in)
  (define out (open-output-string))
  (let loop ([in-bar? #f])
    (define ch (peek-char in))
    (cond
      [(eof-object? ch)
       (get-output-string out)]
      [(not in-bar?)
       (cond
         [(char=? ch #\|)
          (read-char in)
          (loop #t)]
         [(char=? ch #\\)
          (read-char in)
          (define next (peek-char in))
          (when (eof-object? next)
            (raise-read-error 'scan-bareword "dangling backslash in bareword"
                              (make-srcloc (object-name in) #f #f #f 0)))
          (write-char (read-char in) out)
          (loop #f)]
         ;; Treat '.' as part of a number when appropriate
         [(char=? ch #\.)
          (define s2 (peek-string 2 0 in))
          (define sofar (get-output-string out))
          (define prev-digit? (and (positive? (string-length sofar))
                                   (char-numeric? (string-ref sofar (sub1 (string-length sofar))))))
          (cond
            ;; leading . followed by digit → number like .5
            [(and (string? s2)
                  (= (string-length s2) 2)
                  (char=? (string-ref s2 0) #\.)
                  (char-numeric? (string-ref s2 1)))
             (read-char in) (write-char #\. out) (loop #f)]
            ;; trailing dot after digits → number like 3.
            [prev-digit?
             (read-char in) (write-char #\. out) (loop #f)]
            ;; else delimiter: end bareword
            [else (get-output-string out)])]
         [(delimiter-char? ch)
          (get-output-string out)]
         [else
          (write-char (read-char in) out)
          (loop #f)])]
      [in-bar?
       (cond
         [(eof-object? ch)
          (raise-read-error 'scan-bareword "unterminated |...| in bareword"
                            (make-srcloc (object-name in) #f #f #f 0))]
         [(char=? ch #\|)
          (read-char in)
          (loop #f)]
         [else
          (write-char (read-char in) out)
          (loop #t)])])))

(define (classify-bareword s)
  (define maybe-num (string->number s))
  (if (number? maybe-num)
      (cons 'number maybe-num)
      (cons 'symbol (string->symbol s))))

;; ------------------------------- Driver -----------------------------------
(define (next-token L)
  (define in (lx-in L))
  (skip-space+comments L)
  (define ch (peek-char in))
  (cond
    [(eof-object? ch)
     (define-values (line col pos) (port-next-location in))
     (token 'eof #f "" (make-srcloc (lx-source L) line col pos 0))]
    [(char=? ch #\") (scan-string L)]
    ;; Special-case '.' that begins a number like .5
    [(char=? ch #\.)
     (define s2 (peek-string 2 0 in))
     (if (and (string? s2)
              (= (string-length s2) 2)
              (char=? (string-ref s2 0) #\.)
              (char-numeric? (string-ref s2 1)))
         (scan-bareword L)
         (scan-delim-or-dot L))]
    [(memq ch '(#\( #\) #\[ #\] #\{ #\} #\.)) (scan-delim-or-dot L)]
    [(memq ch '(#\' #\` #\,)) (scan-quote-like L)]
    [(char=? ch #\#) (scan-sharp-dispatch L)]
    [else (scan-bareword L)]))

;; ------------------------------- Public API -------------------------------
(define (lexer-next L)
  (let ([buf (lx-buf L)])
    (if (pair? buf)
        (begin
          (set-lx-buf! L (cdr buf))
          (car buf))
        (next-token L))))

(define (lexer-peek L)
  (define t (lexer-next L))
  (set-lx-buf! L (cons t (lx-buf L)))
  t)

(define (lexer-unread L t)
  (set-lx-buf! L (cons t (lx-buf L))))

(provide token
         lx make-lexer
         lexer-next lexer-peek lexer-unread
         skip-space+comments
         scan-delim-or-dot scan-quote-like scan-sharp-dispatch
         scan-string scan-bytes scan-bareword scan-boolean scan-char
         next-token
         make-srcloc-from-port)

;; ============================== Tests =====================================

(require rackunit)

(module+ test
  (define (lex-from-string s [source 'string])
    (make-lexer (open-input-string s) source))

  (define (collect-types+lexemes L)
    (let loop ([acc '()])
      (define t  (lexer-next L))
      (define ty (token-type t))
      (define lx (token-lexeme t))
      (define acc* (cons (cons ty lx) acc))
      (if (eq? ty 'eof) (reverse acc*) (loop acc*))))

  (define (kinds s)
    (map car (collect-types+lexemes (lex-from-string s 'k))))

  ;; boolean aliases
  (test-case "boolean aliases"
    (check-equal? (kinds "#true #false #t #f")
                  '(boolean boolean boolean boolean eof)))

  ;; radix/exactness + chaining → numbers
  (test-case "radix/exactness prefixes"
    (check-equal? (kinds "#x1f #o77 #b1010 #d9 #e10 #i3.0 #e#x10")
                  '(number number number number number number number eof))))

