#lang racket/base
;; WebRacket Reader — Lexer (booleans + radix/exactness)
;; - Honors #i (inexact) and #e (exact) by leaving prefixes for string->number
;; - Strings and byte strings treat \x...; correctly (do not include ';')
;; - Booleans, characters, quote-like, delimiters, barewords
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
  (define (emit c)
    (write-char c out-str)
    (when bytes? (write-byte (char->integer c) out-bytes)))
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
       (token (if bytes? 'bytes 'string) val lexeme
              (make-srcloc-from-port in (lx-source L) sl sc sp))]
      [(char=? ch #\\)
       (define esc (read-char in))
       (cond
         [(eof-object? esc)
          (raise-read-error 'scan-string* "dangling escape in string"
                            (make-srcloc (lx-source L) sl sc sp 1))]
         [(member esc '(#\n #\r #\t #\" #\\))
          (emit (case esc
                  [(#\n) #\newline]
                  [(#\r) #\return]
                  [(#\t) #\tab]
                  [(#\") #\"]
                  [(#\\) #\\]))
          (loop)]
         [(char=? esc #\x)
          ;; read hex digits, then require and consume ';'
          (define acc 0)
          (let hex-loop ()
            (define c (peek-char in))
            (cond
              [(eof-object? c)
               (raise-read-error 'scan-string* "unterminated \\x...; escape"
                                 (make-srcloc (lx-source L) sl sc sp 1))]
              [(char=? c #\;)
               (read-char in) ; consume ';' and finish
               (void)]
              [else
               (define v (hex-digit->val c))
               (unless v (raise-read-error 'scan-string* "invalid hex digit in \\x"
                                           (make-srcloc (lx-source L) sl sc sp 1)))
               (read-char in)
               (set! acc (+ (* acc 16) v))
               (hex-loop)]))
          (when (and bytes? (>= acc 256))
            (raise-read-error 'scan-string* "byte value > 255 in byte string"
                              (make-srcloc (lx-source L) sl sc sp 1)))
          (emit (integer->char acc))
          (loop)]
         [else
          (emit esc)
          (loop)])]
      [else
       (emit ch)
       (loop)])))

(define (scan-string L) (scan-string* L))
(define (scan-bytes  L) (scan-string* L #:bytes? #t))

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
    [(and (string? s2) (string=? s2 "#&"))
     (read-char in) (read-char in)
     (token 'box-start #f "#&" (make-srcloc-from-port in (lx-source L) sl sc sp))]
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
  ;; Let Racket honor #e/#i and radix prefixes via string->number
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

(require rackunit
         racket/port
         #;"reader.rkt")

;; Helpers -------------------------------------------------------------------
(define (lex-from-string s [source 'string])
  (define in (open-input-string s))
  (make-lexer in source))

(define (collect-types L)
  (let loop ([acc '()])
    (define t (lexer-next L))
    (define ty (token-type t))
    (define acc* (cons ty acc))
    (if (eq? ty 'eof) (reverse acc*) (loop acc*))))

(define (collect-types+lexemes L)
  (let loop ([acc '()])
    (define t (lexer-next L))
    (define ty (token-type t))
    (define lx (token-lexeme t))
    (define acc* (cons (cons ty lx) acc))
    (if (eq? ty 'eof) (reverse acc*) (loop acc*))))

(define (token-span t)
  (srcloc-span (token-loc t)))

(module+ test
  ;; 1) Empty input → EOF with zero-span location
  (test-case "empty input → eof"
    (define L (lex-from-string "" 'empty))
    (define t (lexer-next L))
    (check-equal? (token-type t) 'eof)
    (check-equal? (srcloc-span (token-loc t)) 0))

  ;; 2) Delimiters, with whitespace/newlines between
  (test-case "delimiters basic"
    (define L (lex-from-string "(\n) [ ] { } ." 'delims))
    (check-equal?
     (collect-types+lexemes L)
     '((lparen . "(") (rparen . ")") (lbracket . "[") (rbracket . "]")
       (lbrace . "{") (rbrace . "}") (dot . ".") (eof . ""))))

  ;; 3) Quote-like tokens
  (test-case "quote-like"
    (define L (lex-from-string "'  ` ,  ,@" 'quotes))
    (check-equal?
     (collect-types+lexemes L)
     '((quote . "'") (quasiquote . "`") (unquote . ",")
       (unquote-splicing . ",@") (eof . ""))))

  ;; 4) Nested block comments are skipped
  (test-case "nested block comments"
    (define L (lex-from-string "#| a #| b |# c |# (" 'blocks))
    (define t (lexer-next L))
    (check-equal? (token-type t) 'lparen)
    (check-equal? (token-lexeme t) "(")
    (check-equal? (token-span t) 1)
    (check-equal? (token-type (lexer-next L)) 'eof))

  ;; 5) BOM + line comment are skipped
  (test-case "bom + line comment"
    (define L (lex-from-string (string #\uFEFF ; BOM
                                       #\; #\c #\o #\m #\m #\e #\n #\t
                                       #\newline
                                       #\() 'bom+line))
    (define t (lexer-next L))
    (check-equal? (token-type t) 'lparen)
    (check-equal? (token-lexeme t) "(")
    (check-equal? (token-span t) 1))

  ;; 6) Span checks for ', and ,@
  (test-case "span of quote-like lexemes"
    (define L (lex-from-string "',@" 'spans))
    (define t1 (lexer-next L))
    (define t2 (lexer-next L))
    (check-equal? (token-type t1) 'quote)
    (check-equal? (token-span t1) 1)
    (check-equal? (token-type t2) 'unquote-splicing)
    (check-equal? (token-span t2) 2))

  ;; 7) Booleans (short and long)
  (test-case "booleans short & long"
    (define L (lex-from-string "#t #f #true #false" 'bools))
    (define t1 (lexer-next L)) (define t2 (lexer-next L))
    (define t3 (lexer-next L)) (define t4 (lexer-next L))
    (check-equal? (map token-type (list t1 t2 t3 t4)) '(boolean boolean boolean boolean))
    (check-equal? (map token-val (list t1 t2 t3 t4)) (list #t #f #t #f))
    (check-equal? (token-type (lexer-next L)) 'eof))

  ;; 8) Characters
  (test-case "characters: literal, name, hex"
    (define L (lex-from-string "#\\a #\\space #\\x41" 'chars))
    (define t1 (lexer-next L)) (define t2 (lexer-next L)) (define t3 (lexer-next L))
    (check-equal? (map token-type (list t1 t2 t3)) '(char char char))
    (check-equal? (token-val t1) #\a)
    (check-equal? (token-val t2) #\space)
    (check-equal? (token-val t3) #\A)
    (check-equal? (token-type (lexer-next L)) 'eof))

  ;; 9) Strings and byte strings
  (test-case "strings & bytes"
    (define L (lex-from-string "\"hi\\n\" #\"A\" #\"\x41;\"" 'strings))
    (define t1 (lexer-next L))
    (define t2 (lexer-next L))
    (define t3 (lexer-next L))
    (check-equal? (token-type t1) 'string)
    (check-equal? (token-val t1) "hi\n")
    (check-equal? (token-type t2) 'bytes)
    (check-equal? (bytes->list (token-val t2)) (bytes->list #"A"))
    (check-equal? (token-type t3) 'bytes)
    (check-equal? (bytes->list (token-val t3)) (bytes->list #"A;"))
    (check-equal? (token-type (lexer-next L)) 'eof))

  ;; 10) Radix/exactness prefixes + dot numbers
  (test-case "radix & exactness prefixes + dotted numbers"
    (define L (lex-from-string "#x10 #o10 #b1011 #d42 #e#x10 #i#o77 3. .5" 'radix))
    (define ts (for/list ([i (in-range 9)]) (lexer-next L)))
    (check-equal? (map token-type ts)
                  '(number number number number number number number number eof))
    (check-equal? (map token-val ts)
                  (list 16 8 11 42 16 63.0 3.0 0.5 #f)))

  ;; 11) Dots as delimiters when isolated
  (test-case "isolated dot remains a delimiter"
    (define L (lex-from-string "1 . 2" 'dots))
    (check-equal? (collect-types L)
                  '(number dot number eof)))

  ;; 12) Sharp dispatch tokens we recognize but do not expand
  (test-case "sharp-dispatch: datum-comment and vector-start"
    (define L (lex-from-string "#; ( #(" 'sharp))
    (define t1 (lexer-next L))
    (check-equal? (token-type t1) 'datum-comment)
    (define t2 (lexer-next L))
    (check-equal? (token-type t2) 'lparen)
    (define t3 (lexer-next L))
    (check-equal? (token-type t3) 'vector-start)
    (check-equal? (token-type (lexer-next L)) 'eof)))
