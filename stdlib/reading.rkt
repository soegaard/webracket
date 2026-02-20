#lang webracket

;; WebRacket Reader — Lexer (booleans + radix/exactness)
;; - Honors #i (inexact) and #e (exact) by leaving prefixes for string->number
;; - Strings and byte strings treat \x...; correctly (do not include ';')
;; - Booleans, characters, quote-like, delimiters, barewords
;; - Skips whitespace, BOM, ';' comments, and nested #| ... |# comments

;; ----------------------------- Exceptions ------------------------------------

(define-values (read read-syntax)
  (let ()

    ;; ----------------------------- Data --------------------------------------
    (struct token (type val lexeme loc) #:transparent)
    (struct lx    (in source buf)       #:transparent #:mutable)

    ;; --------------------------- Construction --------------------------------
    (define (make-lexer in source)
      (port-count-lines! in)
      (lx in source '()))

    ;; ----------------------------- Utilities ---------------------------------
    (define (safe-make-srcloc source line col pos span)
      (define (check-contract who field pred x)
        (unless (or (eq? x #f) (pred x))
          (error who "invalid ~a: ~a" field x)))
      (check-contract 'safe-make-srcloc 'line exact-positive-integer? line)
      (check-contract 'safe-make-srcloc 'column exact-nonnegative-integer? col)
      (check-contract 'safe-make-srcloc 'position exact-nonnegative-integer? pos)
      (check-contract 'safe-make-srcloc 'span exact-nonnegative-integer? span)
      (make-srcloc source line col pos span))

    (define (safe-srcloc-span start-pos end-pos)
      (cond
        [(and (exact-nonnegative-integer? start-pos)
              (exact-nonnegative-integer? end-pos))
         (define span (- end-pos start-pos))
         (when (< span 0)
           (error 'safe-srcloc-span
                  "negative span: start-pos=~a end-pos=~a"
                  start-pos end-pos))
         span]
        [(or (eq? start-pos #f) (eq? end-pos #f)) #f]
        [else
         (error 'safe-srcloc-span
                "invalid positions: start-pos=~a end-pos=~a"
                start-pos end-pos)]))

    (define (make-srcloc-from-port in source start-line start-col start-pos)
      (define-values (end-line end-col end-pos) (port-next-location in))
      (safe-make-srcloc source
                   start-line
                   start-col
                   (and (exact-nonnegative-integer? start-pos) start-pos)
                   (safe-srcloc-span start-pos end-pos)))

    (define (raise-read-error who msg loc)
      (define srclocs
        (cond
          [(list? loc)   loc]
          [(srcloc? loc) (list loc)]
          [(not loc)     '()]
          [else
           (raise-argument-error 'raise-read-error
                                 "(or/c srcloc? (listof srcloc?) #f)" loc)]))
      (define who-str
        (cond
          [(string? who) who]
          [(symbol? who) (symbol->string who)]
          [else          "?"]))
      
      (raise
       (exn:fail:read
        (string-append who-str ": " msg)
        #;(format "~a: ~a" who msg)
        (current-continuation-marks)
        srclocs)))

    (define (hex-digit->val ch)
      (define (offset base) (- (char->integer ch) (char->integer base)))
      (cond [(and (char>=? ch #\0) (char<=? ch #\9))       (offset #\0)]
            [(and (char>=? ch #\a) (char<=? ch #\f)) (+ 10 (offset #\a))]
            [(and (char>=? ch #\A) (char<=? ch #\F)) (+ 10 (offset #\A))]
            [else #f]))

    ;; Character classes -------------------------------------------------------
    (define (delimiter-char? ch)
      (or (eof-object? ch)
          (char-whitespace? ch)
          (member ch '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\.))))

    ;; ------------------------------ Skipper ----------------------------------
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
                          (safe-make-srcloc source sl sc sp 2)))
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

    ;; --------------------------- Trivial scanners ----------------------------
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
          [else (error 'scan-delim-or-dot (format "unexpected char `~a`" ch))]))
      (define loc (make-srcloc-from-port in (lx-source L) sl sc sp))
      (token type #f (string ch) loc))

    (define (scan-quote-like L)
      (define in (lx-in L))
      (define-values (sl sc sp) (port-next-location in))
      (define ch (read-char in))
      (case ch
        [(#\') (token 'quote #f "'"
                      (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(#\`) (token 'quasiquote #f "`"
                      (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(#\,)
         (define nxt (peek-char in))
         (if (and (char? nxt) (char=? nxt #\@))
             (begin
               (read-char in)
               (token 'unquote-splicing #f ",@"
                      (make-srcloc-from-port in (lx-source L) sl sc sp)))
             (token 'unquote #f ","
                    (make-srcloc-from-port in (lx-source L) sl sc sp)))]
        [else (error 'scan-quote-like (format "unexpected char `~a`" ch))]))

    ;; --------------------------- Strings & Bytes -----------------------------
    (define (scan-string* L [bytes? #f])
      (define in (lx-in L))
      (define-values (sl sc sp) (port-next-location in))
      (when bytes? (read-char in))     ; consumed '#'
      (define ch0 (read-char in))      ; opening '"'
      (unless (char=? ch0 #\")
        (error 'scan-string* "internal: expected opening `\"`"))
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
                             (if bytes?
                                 "unterminated byte string"
                                 "unterminated string")
                             (safe-make-srcloc (lx-source L) sl sc sp 1))]
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
                                (safe-make-srcloc (lx-source L) sl sc sp 1))]
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
                                     (safe-make-srcloc (lx-source L) sl sc sp 1))]
                  [(char=? c #\;)
                   (read-char in) ; consume ';' and finish
                   (void)]
                  [else
                   (define v (hex-digit->val c))
                   (unless v (raise-read-error
                              'scan-string* "invalid hex digit in \\x"
                              (safe-make-srcloc (lx-source L) sl sc sp 1)))
                   (read-char in)
                   (set! acc (+ (* acc 16) v))
                   (hex-loop)]))
              (when (and bytes? (>= acc 256))
                (raise-read-error 'scan-string* "byte value > 255 in byte string"
                                  (safe-make-srcloc (lx-source L) sl sc sp 1)))
              (emit (integer->char acc))
              (loop)]
             [else
              (emit esc)
              (loop)])]
          [else
           (emit ch)
           (loop)])))

    (define (scan-string L) (scan-string* L))
    (define (scan-bytes  L) (scan-string* L #t))

    ;; --------------------------- Booleans & Chars ----------------------------
    (define (scan-boolean L)
      (define in (lx-in L))
      (define-values (sl sc sp) (port-next-location in))
      (read-char in) ; '#'
      (define torf (read-char in))
      (define val (case torf [(#\t) #t] [(#\f) #f]
                        [else (error 'scan-boolean "internal")]))
      (token 'boolean val (string-append "#" (string torf))
             (make-srcloc-from-port in (lx-source L) sl sc sp)))

    (define (scan-char L)
      (define in (lx-in L))
      (define-values (sl sc sp) (port-next-location in))
      (define s2 (peek-string 2 0 in))
      (unless (and (string? s2) (string=? s2 "#\\"))
        (error 'scan-char "internal: expected a `#\\`"))
      (read-char in) (read-char in)
      (define ch (peek-char in))
      (cond
        [(eof-object? ch)
         (raise-read-error 'scan-char "unexpected EOF after `#\\`"
                           (safe-make-srcloc (lx-source L) sl sc sp 1))]
        [(delimiter-char? ch)
         ;; Allow delimiter characters as single-character literals, e.g. #\. #\) #\;
         (read-char in)
         (token 'char ch (string-append "#\\" (string ch))
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
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
            ;; `#\x` is the character x/X.
            ;; `#\xNN...` is a hex codepoint literal.
            (define s2x (peek-string 2 0 in))
           (if (and (string? s2x)
                     (= (string-length s2x) 2)
                     (hex-digit->val (string-ref s2x 1)))
                (begin
                  (read-char in)
                  (let ([out 0])
                    (let hex-loop ()
                      (define c (peek-char in))
                      (cond
                        [(eof-object? c) (void)]
                        [(delimiter-char? c) (void)]
                        [else
                         (define v (hex-digit->val c))
                         (unless v (raise-read-error
                                    'scan-char "invalid hex in #\\x..."
                                    (safe-make-srcloc (lx-source L) sl sc sp 1)))
                         (read-char in)
                         (set! out (+ (* out 16) v))
                         (hex-loop)]))
                    (define the (integer->char out))
                    (token 'char the (format "#\\x~x" (char->integer the))
                           (make-srcloc-from-port in (lx-source L) sl sc sp))))
                (let* ([raw-name (read-name)]
                       [name (string-downcase raw-name)]
                       [the
                        (cond [(string=? name "space") #\space]
                              [(string=? name "newline") #\newline]
                              [(string=? name "tab") #\tab]
                              [(string=? name "return") #\return]
                              [(= (string-length raw-name) 1) (string-ref raw-name 0)]
                              [else
                               (raise-read-error 'scan-char
                                                 (format "unknown character name ~a" name)
                                                 (safe-make-srcloc (lx-source L) sl sc sp 1))])])
                  (token 'char the (string-append "#\\" raw-name)
                         (make-srcloc-from-port in (lx-source L) sl sc sp))))]
           [else
            (define raw-name (read-name))
            (define name (string-downcase raw-name))
            (define the
              (cond [(string=? name "space") #\space]
                    [(string=? name "newline") #\newline]
                    [(string=? name "tab") #\tab]
                    [(string=? name "return") #\return]
                    ;; Preserve case for single-character literals:
                    ;; `#\\A` must read as #\\A (not downcased to #\\a).
                    [(= (string-length raw-name) 1) (string-ref raw-name 0)]
                    [else
                     (raise-read-error 'scan-char
                                       (format "unknown character name ~a" name)
                                       (safe-make-srcloc (lx-source L) sl sc sp 1))]))
            (token 'char the (string-append "#\\" raw-name)
                   (make-srcloc-from-port in (lx-source L) sl sc sp))])]))

    ;; --------------------------- Sharp-dispatch -----------------------------
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
         (token 'datum-comment #f "#;"
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(and (string? s2)
              (or (string=? s2 "#(") (string=? s2 "#[") (string=? s2 "#{")))
         (read-char in) (read-char in)
         (token 'vector-start #f s2
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(and (string? s2) (string=? s2 "#&"))
         (read-char in) (read-char in)
         (token 'box-start #f "#&"
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(and (string? s2) (string=? s2 "#\\")) (scan-char L)]
        [(and (string? s2) (string=? s2 "#\"")) (scan-bytes L)]
        ; symbol that starts with #%
        [(and (string? s2) (string=? s2 "#%"))  (scan-bareword L)] 
        [(and (string? s2) (string=? s2 "#:"))  (scan-keyword L)]    
        ;; boolean aliases #true / #false (case-insensitive)
        [(and (string? s5) (string-ci=? s5 "#true") (delim-after? 5))
         (for ([i (in-range 5)]) (read-char in))
         (token 'boolean #t "#true"
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
        [(and (string? s6) (string-ci=? s6 "#false") (delim-after? 6))
         (for ([i (in-range 6)]) (read-char in))
         (token 'boolean #f "#false"
                (make-srcloc-from-port in (lx-source L) sl sc sp))]
        ;; short booleans #t/#f
        [(and (string? s2) (or (string-ci=? s2 "#t") (string-ci=? s2 "#f")))
         (scan-boolean L)]
        ;; radix/exactness prefixes → treat as part of a bareword/number
        [(and (string? s2)
              (member (char-downcase (string-ref s2 1))
                      '(#\x #\o #\b #\d #\e #\i)))
         (scan-bareword L)]
        [else
         (raise-read-error
          'lexer
          (format "unsupported sharp-dispatch starting with ~a" s2)
          (safe-make-srcloc (lx-source L) sl sc sp 1))]))

    ;; --------------------------- Keywords ------------------------------------

    (define (scan-keyword L)
      (define in (lx-in L))
      (define-values (sl sc sp) (port-next-location in))
      (define s2 (peek-string 2 0 in))
      (unless (and (string? s2) (string=? s2 "#:"))
        (error 'scan-keyword "internal: expected `#:`"))
      (read-char in) ; '#'
      (read-char in) ; ':'
      (define name (accum-bareword in))
      (when (zero? (string-length name))
        (raise-read-error 'scan-keyword "expected keyword name after `#:`"
                          (safe-make-srcloc (lx-source L) sl sc sp 2)))
      (token 'keyword (string->keyword name)
             (string-append "#:" name)
             (make-srcloc-from-port in (lx-source L) sl sc sp)))


    ;; --------------------------- Barewords -----------------------------------


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
                (raise-read-error 'scan-bareword
                                  "dangling backslash in bareword"
                                  (safe-make-srcloc (object-name in) #f #f #f 0)))
              (write-char (read-char in) out)
              (loop #f)]
             ;; Treat '.' as part of a number when appropriate
             [(char=? ch #\.)
              (define s2 (peek-string 2 0 in))
              (define sofar (get-output-string out))
              (define prev-digit? (and (positive? (string-length sofar))
                                       (char-numeric?
                                        (string-ref
                                         sofar (sub1 (string-length sofar))))))
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
                                (safe-make-srcloc (object-name in) #f #f #f 0))]
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

    ;; ------------------------------- Driver ----------------------------------
    (define (next-token L)
      (define in (lx-in L))
      (skip-space+comments L)
      (define ch (peek-char in))
      (cond
        [(eof-object? ch)
         (define-values (line col pos) (port-next-location in))
         (token 'eof #f "" (safe-make-srcloc (lx-source L) line col pos 0))]
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

    ;; ------------------------------- Public API ------------------------------
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


    ;; ============================== Parser ===================================

    (define (srcloc-end-position loc)
      (define pos  (srcloc-position loc))
      (define span (srcloc-span loc))
      (and (exact-nonnegative-integer? pos)
           (exact-nonnegative-integer? span)
           (+ pos span)))

    (define (srcloc-join start end)
      (define src     (or (srcloc-source start) (srcloc-source end)))
      (define line    (srcloc-line start))
      (define col     (srcloc-column start))
      (define pos     (srcloc-position start))
      (define end-pos (srcloc-end-position end))
      (define pos*    (and (exact-nonnegative-integer? pos) pos))
      (define span    (safe-srcloc-span pos* end-pos))
      (safe-make-srcloc src line col pos* span))

    (define (closer-for opener-type)
      (case opener-type
        [(lparen)   'rparen]
        [(lbracket) 'rbracket]
        [(lbrace)   'rbrace]
        [else (error 'closer-for (format "unexpected opener `~a`" opener-type))]))

    (define (delimiter->string type)
      (case type
        [(lparen)   "("]
        [(rparen)   ")"]
        [(lbracket) "["]
        [(rbracket) "]"]
        [(lbrace)   "{"]
        [(rbrace)   "}"]
        [else (symbol->string type)]))

    (define (quote-token->sym ty)
      (case ty
        [(quote)            'quote]
        [(quasiquote)       'quasiquote]
        [(unquote)          'unquote]
        [(unquote-splicing) 'unquote-splicing]
        [else
         (error 'quote-token->sym (format "unexpected token `~a`" ty))]))

    (define (raise-unexpected-closing closer)
      (raise-read-error 'read
                        (format "unexpected `~a`" (token-lexeme closer))
                        (token-loc closer)))

    (define (raise-mismatched closer expected opener)
      (raise-read-error 'read
                        (format "expected `~a` to close `~a`, found `~a`"
                                (delimiter->string expected)
                                (token-lexeme opener)
                                (token-lexeme closer))
                        (token-loc closer)))

    (define (parse-quote-like L tok context)
      (define sym (quote-token->sym (token-type tok)))
      (define-values (datum _start end) (parse-datum L 'datum))
      (when (eof-object? datum)
        (raise-read-error 'read
                          (format "unexpected EOF after `~a`" (token-lexeme tok))
                          (token-loc tok)))
      (values (list sym datum) (token-loc tok) end))

    (define (parse-list L opener)
      (define opener-type (token-type opener))
      (define expected    (closer-for opener-type))
      (let loop ([elems '()] [seen-dot? #f] [dot-tail #f])
        (define t  (lexer-next L))
        (define ty (token-type t))
        (case ty
          [(datum-comment)
           (define-values (_1 _2 _3) (parse-datum L 'list))
           (loop elems seen-dot? dot-tail)]
          [(eof)
           (raise-read-error 'read
                             (format "unexpected EOF: expected `~a` to close `~a`"
                                     (delimiter->string expected)
                                     (token-lexeme opener))
                             (token-loc opener))]
          [(dot)
           (cond
             [seen-dot?
              (raise-read-error 'read "multiple `.` in list" (token-loc t))]
             [(null? elems)
              (raise-read-error 'read "`.` cannot appear at start of list"
                                (token-loc t))]
             [else
              (define-values (tail _ts _te) (parse-datum L 'list))
              (when (eof-object? tail)
                (raise-read-error 'read "unexpected EOF after `.`"
                                  (token-loc t)))
              (loop elems #t tail)])]
          [(rparen rbracket rbrace)
           (if (eq? ty expected)
               (let ([result (if seen-dot?
                                 (for/fold ([rest dot-tail])
                                           ([elem (in-list elems)])
                                   (cons elem rest))
                                 (reverse elems))])
                 (values result (token-loc opener) (token-loc t)))
               (raise-mismatched t expected opener))]
          [else
           (lexer-unread L t)
            (when seen-dot?
             (raise-read-error 'read "unexpected datum after `.`" (token-loc t)))
           (define-values (datum _ds _de) (parse-datum L 'list))
           (loop (cons datum elems) seen-dot? dot-tail)])))

    (define (parse-vector L start-token)
      (define start-lexeme (token-lexeme start-token))
      (define expected-type
        (cond [(string=? start-lexeme "#(") 'rparen]
              [(string=? start-lexeme "#[") 'rbracket]
              [(string=? start-lexeme "#{") 'rbrace]
              [else 'rparen]))
      (define expected-char
        (cond [(eq? expected-type 'rparen)   ")"]
              [(eq? expected-type 'rbracket) "]"]
              [(eq? expected-type 'rbrace)   "}"]
              [else ")"]))
      (let loop ([elems '()])
        (define t  (lexer-next L))
        (define ty (token-type t))
        (case ty
          [(datum-comment)
           (define-values (_1 _2 _3) (parse-datum L 'vector))
           (loop elems)]
          [(eof)
           (raise-read-error 'read
                             (string-append "unexpected EOF: expected `"
                                            expected-char
                                            "` to cloce `"
                                            start-lexeme
                                            "`")
                             (token-loc start-token))]
          [(rparen rbracket rbrace)
           (if (eq? ty expected-type)
               (values (list->vector (reverse elems))
                       (token-loc start-token)
                       (token-loc t))
               (raise-read-error 'read
                                 (string-append "expected a `" expected-char
                                                "` to close " start-lexeme
                                                ", found `"  (token-lexeme t)
                                                "`")
                                 (token-loc t)))]
          [(rbracket rbrace)
           (raise-read-error 'read
                             (format "expected a `)` to close a `#(`, found ~a"
                                     (token-lexeme t))
                             (token-loc t))]
          [else
           (lexer-unread L t)
           (define-values (datum _ds _de) (parse-datum L 'vector))
           (loop (cons datum elems))])))

    (define (parse-datum L context)
      (let loop ()
        (define tok (lexer-next L))
        (define ty  (token-type tok))
        (case ty
          [(datum-comment)
           (define-values (_1 _2 _3) (parse-datum L context))
           (loop)]
          [(eof)
           (values eof (token-loc tok) (token-loc tok))]
          [(lparen lbracket lbrace)
           (parse-list L tok)]
          [(rparen rbracket rbrace)
           (raise-unexpected-closing tok)]
          [(dot)
           (raise-read-error 'read "unexpected `.`" (token-loc tok))]
          [(vector-start)
           (parse-vector L tok)]
          [(box-start)
           (raise-read-error 'read "box literals are not supported"
                             (token-loc tok))]
          [(quote quasiquote unquote unquote-splicing)
           (parse-quote-like L tok context)]
          [(string bytes number char boolean symbol keyword)
           (values (token-val tok) (token-loc tok) (token-loc tok))]
          [else
           (raise-read-error 'read
                             (format "unexpected token type `~a`" (list ty (eq? ty 'number) (token-val tok) (token-loc tok) (token-loc tok)))
                             (token-loc tok))])))

    (define (do-read in source syntax?)
      (define L (make-lexer in source))
      (define-values (datum start end) (parse-datum L 'top))
      (if (eof-object? datum)
          datum
          (if syntax?
              (datum->syntax #f datum (srcloc-join start end))
              datum)))

    (define (read [in (current-input-port)])
      (do-read in (object-name in) #f))

    (define (read-syntax source [in (current-input-port)])
      (do-read in source #t))
    
   (values read read-syntax)))


#;(provide token
         lx make-lexer
         lexer-next lexer-peek lexer-unread
         skip-space+comments
         scan-delim-or-dot scan-quote-like scan-sharp-dispatch
         scan-string scan-bytes scan-bareword scan-boolean scan-char
         next-token
         make-srcloc-from-port
         parse-datum
         do-read read read-syntax)

;; ============================== Tests =====================================

#;(define (test)

  #;(require rackunit
             racket/port
             #;"reader.rkt")

  ;; --------------------------  Helpers ---------------------------------------
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

  (define (read-from-string s #:source [source 'string] #:syntax? [syntax? #f])
    (define in (open-input-string s))
    (if syntax?
        (read-syntax source in)
        (read in)))

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
      (define t1 (lexer-next L))
      (define t2 (lexer-next L))
      (define t3 (lexer-next L))
      (define t4 (lexer-next L))
      (check-equal? (map token-type (list t1 t2 t3 t4))
                    '(boolean boolean boolean boolean))
      (check-equal? (map token-val (list t1 t2 t3 t4)) (list #t #f #t #f))
      (check-equal? (token-type (lexer-next L)) 'eof))

    ;; 8) Characters
    (test-case "characters: literal, name, hex"
      (define L (lex-from-string "#\\a #\\space #\\x41" 'chars))
      (define t1 (lexer-next L))
      (define t2 (lexer-next L))
      (define t3 (lexer-next L))
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
      (define L (lex-from-string "#x10 #o10 #b1011 #d42 #e#x10 #i#o77 3. .5"
                                 'radix))
      (define ts (for/list ([i (in-range 9)]) (lexer-next L)))
      (check-equal? (map token-type ts)
                    '(number number number number number number
                             number number eof))
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
      (check-equal? (token-type (lexer-next L)) 'eof))

    (test-case "sharp-dispatch: alternate vector starts"
      (define L (lex-from-string "#[ #{" 'sharp-alt))
      (define t1 (lexer-next L))
      (define t2 (lexer-next L))
      (check-equal? (map token-type (list t1 t2)) '(vector-start vector-start))
      (check-equal? (map token-lexeme (list t1 t2)) '("#[" "#{"))
      (check-equal? (token-type (lexer-next L)) 'eof))
    
    ;; 13) Symbols beginning with #%
    (test-case "symbols starting with #%"
      (define L (lex-from-string "#%foo" 'hashpercent))
      (check-equal? (collect-types+lexemes L)
                    '((symbol . "#%foo") (eof . "")))
      (check-equal? (read-from-string "#%foo") '#%foo))

    ;; 14) Keywords
    (test-case "keywords"
      (define L (lex-from-string "#:foo #:|bar baz| #:1" 'keywords))
      (define t1 (lexer-next L))
      (define t2 (lexer-next L))
      (define t3 (lexer-next L))
      (check-equal? (map token-type (list t1 t2 t3)) '(keyword keyword keyword))
      (check-equal? (map token-val (list t1 t2 t3))
                    (list (string->keyword "foo")
                          (string->keyword "bar baz")
                          (string->keyword "1")))
      (check-equal? (token-type (lexer-next L)) 'eof))
    
    ;; Parser tests ------------------------------------------------------------
    (test-case "parser: basic lists and vectors"
      (check-equal? (read-from-string "()")           '())
      (check-equal? (read-from-string "[]")           '())
      (check-equal? (read-from-string "{}")           '())
      (check-equal? (read-from-string "(1 2)")        '(1 2))
      (check-equal? (read-from-string "[1 2 (3)]")    '(1 2 (3)))
      (check-equal? (read-from-string "{1 2}")        '(1 2))
      (check-equal? (read-from-string "{1 [2] (3)}")  '(1 (2) (3)))
      (check-equal? (read-from-string "#(1 (2) [3])") '#(1 (2) (3)))
      (check-equal? (read-from-string "#[1 (2) {3}]") '#(1 (2) (3)))
      (check-equal? (read-from-string "#{1 (2) [3]}") '#(1 (2) (3))))

    #;(test-case "parser: mismatched brackets track location"
        (check-exn
         (lambda (e)
           (and (exn:fail:read? e)
                (regexp-match? #rx"expected \\)" (exn-message e))
                (let ([loc (car (exn:fail:read-srclocs e))])
                  (and (= (srcloc-line loc) 1)
                       (= (srcloc-column loc) 2)))))
         (lambda () (read-from-string "(]")))
        (check-exn
         (lambda (e)
           (and (exn:fail:read? e)
                (regexp-match? #rx"expected \\}" (exn-message e))
                (let ([loc (car (exn:fail:read-srclocs e))])
                  (and (= (srcloc-line loc) 1)
                       (= (srcloc-column loc) 2)))))
         (lambda () (read-from-string "{)"))))

    (test-case "parser: dotted pairs"
      (check-equal? (read-from-string "(a . b)") (cons 'a 'b))
      (check-equal? (read-from-string "(1 2 . 3)") (cons 1 (cons 2 3))))

    (test-case "parser: dotted pair errors"
      (check-exn
       (lambda (e)
         (and (exn:fail:read? e)
              (regexp-match? #rx"cannot appear" (exn-message e))))
       (lambda () (read-from-string "( . a)")))
      (check-exn
       (lambda (e)
         (and (exn:fail:read? e)
              (regexp-match? #rx"unexpected" (exn-message e))))
       (lambda () (read-from-string "(a . )")))
      (check-exn
       (lambda (e)
         (and (exn:fail:read? e)
              (regexp-match? #rx"unexpected datum" (exn-message e))))
       (lambda () (read-from-string "(a . b c)"))))

    (test-case "parser: quote-like forms"
      (check-equal? (read-from-string "'x") '(quote x))
      (check-equal? (read-from-string "`x") '(quasiquote x))
      (check-equal? (read-from-string ",x") '(unquote x))
      (check-equal? (read-from-string "(,@x)") '((unquote-splicing x)))
      (check-exn
       (lambda (e)
         (and (exn:fail:read? e)
              (regexp-match? #rx"unquote-splicing outside list"
                             (exn-message e))))
       (lambda () (read-from-string ",@x"))))

    (test-case "parser: datum comments"
      (check-equal? (read-from-string "#; 1 2") 2)
      (check-equal? (read-from-string "#; (1 2) 3") 3))

    (test-case "parser: read-syntax carries srcloc"
      (define stx (read-from-string "(1 2 3)" #:source 'src #:syntax? #t))
      (check-true   (syntax? stx))
      (check-equal? (syntax->datum stx) '(1 2 3))
      (check-equal? (syntax-source stx) 'src)
      (check-true   (positive? (syntax-span stx)))
      (check-equal? (syntax-position stx) 1))

    (test-case "parser: datum comment before eof yields eof"
      (define in (open-input-string "#; 1"))
      (check-true (eof-object? (read in)))
      (check-true (eof-object? (read in))))

    (test-case "parser: keywords"
      (check-equal? (read-from-string "#:foo") (string->keyword "foo"))
      (check-equal? (read-from-string "(#:foo #:|bar baz|)")
                    (list (string->keyword "foo")
                          (string->keyword "bar baz")))))
 )
