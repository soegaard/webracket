;; #lang racket/base

;; (require racket/match         
;;          racket/string
;;          (only-in racket/math nan? infinite?)
;;          (only-in racket/list drop-right last))

;; (provide simple-pretty-print
;;          simple-pretty-write
;;          simple-pretty-display
;;          simple-pretty-format
;;          default-pretty-options
;;          fluid-let)

(define-syntax-rule (fluid-let ([x v] ...) body ...)
  (let ([old-x x] ...)
    (set! x v) ...
    (begin0
      body ...
      (set! x old-x) ...)))

;; (define-syntax-rule (fluid-let ([x v] ...) body ...)
;;   (let ([old-x x] ...)
;;     (dynamic-wind
;;      (lambda () (set! x v) ...)
;;      (lambda () body ...)
;;      (lambda () (set! x old-x) ...))))

(define default-pretty-options
  '((columns                . 79)
    (depth                  . #f)
    (newline?               . #t)
    (show-inexactness       . #f)
    (exact-as-decimal       . #f)
    (.-symbol-without-bars  . #f)
    (abbreviate-read-macros . #t)))

(define allowed-option-keys
  '(columns
    depth
    newline?
    show-inexactness
    exact-as-decimal
    .-symbol-without-bars
    abbreviate-read-macros))

(define current-simple-pretty-options default-pretty-options)

(define (simple-pretty-print v [port (current-output-port)] [options '()])
  (simple-pretty-write v port options))

(define (simple-pretty-write v [port (current-output-port)] [options '()])
  (define final-options
    (normalize-options 'simple-pretty-write options))
  (unless (output-port? port)
    (raise-argument-error 'simple-pretty-write "output-port?" port))
  (fluid-let ([current-simple-pretty-options final-options])
    (write-string (render-top v 'write) port)
    (when (option-ref final-options 'newline?)
      (newline port))
    (void)))

(define (simple-pretty-display v [port (current-output-port)] [options '()])
  (define final-options
    (normalize-options 'simple-pretty-display options))
  (unless (output-port? port)
    (raise-argument-error 'simple-pretty-display "output-port?" port))
  (fluid-let ([current-simple-pretty-options final-options])
    (write-string (render-top v 'display) port)
    (when (option-ref final-options 'newline?)
      (newline port))
    (void)))

(define (simple-pretty-format v [options '()])
  (define final-options
    (normalize-options 'simple-pretty-format options))
  (fluid-let ([current-simple-pretty-options final-options])
    (render-top v 'write)))

(define (render-top v mode)
  (define columns (option-ref current-simple-pretty-options 'columns))
  (define depth (option-ref current-simple-pretty-options 'depth))
  (define limit
    (cond
      [(eq? columns 'infinity) +inf.0]
      [else columns]))
  (render v mode 0 limit depth (make-hasheq)))

(define (render v mode indent width depth seen)
  (define (recur x next-indent next-width next-depth)
    (render x mode next-indent next-width next-depth seen))

  (define (atom x)
    (atom->string x mode))

  (define (too-deep?)
    (and depth (not (positive? depth))))

  (define (next-depth)
    (and depth (sub1 depth)))

  (define (cycle-string)
    "#<cycle>")

  (cond
    [(too-deep?) "..."]
    [(atomic? v mode) (atom v)]
    [(hash-ref seen v #f) (cycle-string)]
    [else
     (hash-set! seen v #t)
     (define result
       (cond
         [(and (pair? v)
               (option-ref current-simple-pretty-options
                           'abbreviate-read-macros)
               (read-macro-form? v))
          (render-read-macro v indent width (next-depth) seen mode)]
         [(pair? v)
          (render-pair-like v "(" ")" indent width (next-depth) seen mode)]
         [(vector? v)
          (render-sequence (vector->list v)
                           "#("
                           ")"
                           indent
                           width
                           (next-depth)
                           seen
                           mode)]
         [(box? v)
          (render-box v indent width (next-depth) seen mode)]
         [(hash? v)
          (render-hash v indent width (next-depth) seen mode)]
         [else
          (atom v)]))
     (hash-remove! seen v)
     result]))

(define (atomic? v mode)
  (cond
    [(null? v) #t]
    [(boolean? v) #t]
    [(number? v) #t]
    [(char? v) #t]
    [(string? v) #t]
    [(bytes? v) #t]
    [(symbol? v) #t]
    [(keyword? v) #t]
    [(path? v) #t]
    [(void? v) #t]
    [(eof-object? v) #t]
    [else #f]))

(define (render-read-macro v indent width depth seen mode)
  (match v
    [(list 'quote x)
     (render-prefixed "'" x indent width depth seen mode)]
    [(list 'quasiquote x)
     (render-prefixed "`" x indent width depth seen mode)]
    [(list 'unquote x)
     (render-prefixed "," x indent width depth seen mode)]
    [(list 'unquote-splicing x)
     (render-prefixed ",@" x indent width depth seen mode)]
    [(list 'syntax x)
     (render-prefixed "#'" x indent width depth seen mode)]
    [(list 'quasisyntax x)
     (render-prefixed "#`" x indent width depth seen mode)]
    [(list 'unsyntax x)
     (render-prefixed "#," x indent width depth seen mode)]
    [(list 'unsyntax-splicing x)
     (render-prefixed "#,@" x indent width depth seen mode)]))

(define (render-prefixed prefix v indent width depth seen mode)
  (define flat-body
    (render v mode (+ indent (string-length prefix))
            width
            depth
            seen))
  (define flat (string-append prefix flat-body))
  (cond
    [(fits? flat indent width) flat]
    [else
     (string-append prefix
                    "\n"
                    (spaces (+ indent 2))
                    (render v mode (+ indent 2) width depth seen))]))

(define (render-pair-like v open close indent width depth seen mode)
  (cond
    [(proper-list? v)
     (render-sequence v open close indent width depth seen mode)]
    [else
     (render-improper-list v open close indent width depth seen mode)]))

(define (render-box v indent width depth seen mode)
  (define body
    (render (unbox v) mode (+ indent 2) width depth seen))
  (define flat (string-append "#&" body))
  (cond
    [(fits? flat indent width) flat]
    [else
     (string-append "#&\n"
                    (spaces (+ indent 2))
                    (render (unbox v) mode (+ indent 2) width depth seen))]))

(define (render-hash h indent width depth seen mode)
  (define prefix
    (cond
      [(hash-eq? h) "#hasheq("]
      [(hash-eqv? h) "#hasheqv("]
      [else "#hash("]))
  (define elems
    (map (lambda (kv)
           (match kv
             [(cons k v)
              (list k v)]))
         (hash->list h)))
  (define (render-entry kv entry-indent entry-width)
    (match kv
      [(list k v)
       (render-improper-list (cons k v)
                              "("
                              ")"
                        entry-indent
                        entry-width
                        depth
                        seen
                        mode)]))
  (render-custom-sequence elems
                          prefix
                          ")"
                          render-entry
                          indent
                          width))

(define (render-sequence xs open close indent width depth seen mode)
  (define (render-entry x entry-indent entry-width)
    (render x mode entry-indent entry-width depth seen))
  (render-custom-sequence xs open close render-entry indent width))

(define (split-lines s)
  (let loop ([i 0]
             [start 0]
             [acc '()])
    (cond
      [(= i (string-length s))
       (reverse (cons (substring s start i) acc))]
      [(char=? (string-ref s i) #\newline)
       (loop (add1 i)
             (add1 i)
             (cons (substring s start i) acc))]
      [else
       (loop (add1 i) start acc)])))

(define (render-custom-sequence xs open close render-entry indent width)
  (define open-len (string-length open))

  (define (shift-block s n)
    (define prefix (spaces n))
    (match (split-lines s)
      ['() ""]
      [(cons first rest)
       (string-join
        (cons (string-append prefix first) rest)
        "\n")]))

  (define (append-close s)
    (define parts (split-lines s))
    (cond
      [(null? parts) close]
      [else
       (define init-parts (drop-right parts 1))
       (define last-part (last parts))
       (string-join
        (append init-parts
                (list (string-append last-part close)))
        "\n")]))

  (define flat-pieces
    (for/list ([x (in-list xs)])
      (render-entry x (+ indent open-len) width)))
  (define flat
    (string-append open
                   (string-join flat-pieces " ")
                   close))
  (cond
    [(or (null? xs) (fits? flat indent width)) flat]
    [else
     (define inner-indent (+ indent 2))
     (define first-piece
       (render-entry (car xs) (+ indent open-len) width))
     (define hanging-head
       (string-append open first-piece))
     (cond
       [(fits? hanging-head indent width)
        (cond
          [(null? (cdr xs))
           (string-append hanging-head close)]
          [else
           (define rest-block
             (string-join
              (for/list ([x (in-list (cdr xs))])
                (shift-block
                 (render-entry x inner-indent width)
                 inner-indent))
              "\n"))
           (string-append hanging-head
                          "\n"
                          (append-close rest-block))])]
       [else
        (define block
          (string-join
           (for/list ([x (in-list xs)])
             (shift-block
              (render-entry x inner-indent width)
              inner-indent))
           "\n"))
        (string-append open
                       "\n"
                       (append-close block))])]))

(define (render-improper-list v open close indent width depth seen mode)
  (define-values (xs tail)
    (split-improper-list v))
  (define open-len (string-length open))

  (define (shift-block s n)
    (define prefix (spaces n))
    (match (split-lines s)
      ['() ""]
      [(cons first rest)
       (string-join
        (cons (string-append prefix first) rest)
        "\n")]))

  (define flat-heads
    (for/list ([x (in-list xs)])
      (render x mode (+ indent open-len) width depth seen)))
  (define flat-tail
    (render tail mode (+ indent open-len) width depth seen))
  (define flat
    (string-append open
                   (string-join flat-heads " ")
                   (if (null? xs) "" " ")
                   ". "
                   flat-tail
                   close))
  (cond
    [(fits? flat indent width) flat]
    [else
     (define inner-indent (+ indent 2))
     (define head-block
       (string-join
        (for/list ([x (in-list xs)])
          (shift-block
           (render x mode inner-indent width depth seen)
           inner-indent))
        "\n"))
     (define tail-block
       (shift-block
        (string-append ". "
                       (render tail mode (+ inner-indent 2) width depth seen)
                       close)
        inner-indent))
     (cond
       [(string=? head-block "")
        (string-append open
                       "\n"
                       tail-block)]
       [else
        (string-append open
                       "\n"
                       head-block
                       "\n"
                       tail-block)])]))

(define (read-macro-form? v)
  (match v
    [(list 'quote _) #t]
    [(list 'quasiquote _) #t]
    [(list 'unquote _) #t]
    [(list 'unquote-splicing _) #t]
    [(list 'syntax _) #t]
    [(list 'quasisyntax _) #t]
    [(list 'unsyntax _) #t]
    [(list 'unsyntax-splicing _) #t]
    [_ #f]))

(define (proper-list? v)
  (let loop ([x v])
    (cond
      [(null? x) #t]
      [(pair? x) (loop (cdr x))]
      [else #f])))

(define (split-improper-list v)
  (let loop ([x v] [acc '()])
    (cond
      [(pair? x) (loop (cdr x) (cons (car x) acc))]
      [else (values (reverse acc) x)])))

(define (fits? s indent width)
  (cond
    [(eqv? width +inf.0) #t]
    [else
     (define available (- width indent))
     (and (not (string-contains? s "\n"))
          (<= (string-length s) available))]))

(define (spaces n)
  (make-string (max 0 n) #\space))

(define (atom->string v mode)
  (define show-inexactness
    (option-ref current-simple-pretty-options 'show-inexactness))
  (define exact-as-decimal
    (option-ref current-simple-pretty-options 'exact-as-decimal))
  (define dot-symbol-without-bars
    (option-ref current-simple-pretty-options '.-symbol-without-bars))

  (define (symbol-like->string s)
    (cond
      [(and (eq? s '|.|) dot-symbol-without-bars) "."]
      [else (format "~s" s)]))

  (define (exact-rational? x)
    (and (number? x) (exact? x) (real? x)))
  
  (define (number-like->string n)
    (cond
      [(and exact-as-decimal (exact-rational? n))
       (cond
         [(integer? n) (number->string n)]
         [else
          (let ([as-inexact (exact->inexact n)])
            (if (and (real? as-inexact)
                     (not (nan? as-inexact))
                     (not (infinite? as-inexact)))
                (number->string as-inexact)
                (format "~a" n)))])]
      [show-inexactness (format "~v" n)]
      [else (format "~a" n)]))

  (cond
    [(eq? mode 'display)
     (cond
       [(string? v) v]
       [(bytes? v) (bytes->string/utf-8 v #\?)]
       [(char? v) (string v)]
       [(symbol? v) (symbol->string v)]
       [else (format "~a" v)])]
    [else
     (cond
       [(symbol? v) (symbol-like->string v)]
       [(number? v) (number-like->string v)]
       [else (format "~s" v)])]))

(define (normalize-options who options)
  (unless (list? options)
    (raise-argument-error who "(listof pair?)" options))
  (define merged
    (for/fold ([acc default-pretty-options])
              ([entry (in-list options)])
      (match entry
        [(cons key value)
         (validate-option who key value)
         (alist-set acc key value)]
        [_
         (raise-argument-error who "(listof pair?)" options)])))
  merged)

(define (validate-option who key value)
  (unless (memq key allowed-option-keys)
    (raise-arguments-error who
                           "unknown option"
                           "option" key
                           "allowed options" allowed-option-keys))
  (match key
    ['columns
     (unless (or (integer? value) (eq? value 'infinity))
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "integer or 'infinity"
                              "given" value))]
    ['depth
     (unless (or (not value) (exact-nonnegative-integer? value))
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "exact-nonnegative-integer or #f"
                              "given" value))]
    ['newline?
     (unless (boolean? value)
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "boolean"
                              "given" value))]
    ['show-inexactness
     (unless (boolean? value)
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "boolean"
                              "given" value))]
    ['exact-as-decimal
     (unless (boolean? value)
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "boolean"
                              "given" value))]
    ['.-symbol-without-bars
     (unless (boolean? value)
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "boolean"
                              "given" value))]
    ['abbreviate-read-macros
     (unless (boolean? value)
       (raise-arguments-error who
                              "bad option value"
                              "option" key
                              "expected" "boolean"
                              "given" value))]))

(define (option-ref options key)
  (define p (assq key options))
  (cond
    [p (cdr p)]
    [else
     (error 'option-ref "missing option: ~a" key)]))

(define (alist-set xs key value)
  (define found? #f)
  (define ys
    (for/list ([entry (in-list xs)])
      (match entry
        [(cons k v)
         (cond
           [(eq? k key)
            (set! found? #t)
            (cons k value)]
           [else
            (cons k v)])])))
  (cond
    [found? ys]
    [else (append ys (list (cons key value)))]))

;; (module+ test
;;   (require rackunit
;;            racket/pretty)

;;   (define (string-trim-right s [trim-chars " \t\r\n"])
;;     (define len (string-length s))
;;     (define (trim-char? c)
;;       (for/or ([tc (in-string trim-chars)])
;;         (char=? c tc)))
;;     (define end
;;       (let loop ([i (sub1 len)])
;;         (cond
;;           [(< i 0) 0]
;;           [(trim-char? (string-ref s i)) (loop (sub1 i))]
;;           [else (add1 i)])))
;;     (substring s 0 end))

;;   (define (call->string proc value options)
;;     (define out (open-output-string))
;;     (proc value out options)
;;     (get-output-string out))

;;   (define (pretty-write->string value columns newline?)
;;     (define out (open-output-string))
;;     (define rendered
;;       (parameterize ([pretty-print-columns columns])
;;         (pretty-write value out)
;;         (get-output-string out)))
;;     (cond
;;       [newline? rendered]
;;       [else (string-trim-right rendered "\n")]))

;;   (define (pretty-display->string value columns newline?)
;;     (define out (open-output-string))
;;     (define rendered
;;       (parameterize ([pretty-print-columns columns])
;;         (pretty-display value out)
;;         (get-output-string out)))
;;     (cond
;;       [newline? rendered]
;;       [else (string-trim-right rendered "\n")]))

;;   (define (pretty-format->string value columns)
;;     (string-trim-right
;;      (pretty-write->string value columns #t)
;;      "
;; "))


;;   (define (read-all-from-string s)
;;     (define in (open-input-string s))
;;     (let loop ([acc '()])
;;       (define v (read in))
;;       (cond
;;         [(eof-object? v) (reverse acc)]
;;         [else (loop (cons v acc))])))

;;   (define (check-roundtrip-agrees value
;;                                   proc
;;                                   #:columns [columns 79]
;;                                   #:newline? [newline? #t])
;;     (define rendered
;;       (call->string proc
;;                     value
;;                     `((columns . ,columns)
;;                       (newline? . ,newline?))))
;;     (define expected
;;       (cond
;;         [newline?
;;          (list value)]
;;         [else
;;          (list value)]))
;;     (check-equal? (read-all-from-string rendered) expected))

;;   (define (check-write-agrees value
;;                               #:columns [columns 79]
;;                               #:newline? [newline? #t]
;;                               #:name [name #f])
;;     (test-case (or name (format "pretty-write ~s" value))
;;       (check-equal?
;;        (call->string simple-pretty-write
;;                      value
;;                      `((columns . ,columns)
;;                        (newline? . ,newline?)))
;;        (pretty-write->string value columns newline?))))

;;   (define (check-display-agrees value
;;                                 #:columns [columns 79]
;;                                 #:newline? [newline? #t]
;;                                 #:name [name #f])
;;     (test-case (or name (format "pretty-display ~s" value))
;;       (check-equal?
;;        (call->string simple-pretty-display
;;                      value
;;                      `((columns . ,columns)
;;                        (newline? . ,newline?)))
;;        (pretty-display->string value columns newline?))))

;;   (define (check-format-agrees value
;;                                #:columns [columns 79]
;;                                #:name [name #f])
;;     (test-case (or name (format "pretty-format ~s" value))
;;       (check-equal?
;;        (simple-pretty-format value `((columns . ,columns)))
;;        (pretty-format->string value columns))))


;;   (define sample
;;     '(define (f x)
;;        (cond
;;          [(pair? x)
;;           (map add1 x)]
;;          [else
;;           (list x x x)])))

;;   (test-case "simple atom"
;;     (check-equal? (simple-pretty-format 42) "42"))

;;   (test-case "no trailing newline in format"
;;     (check-equal? (simple-pretty-format '(1 2 3)) "(1 2 3)"))

;;   (test-case "newline option for write"
;;     (check-equal?
;;      (call->string simple-pretty-write '(1 2 3) '((newline? . #f)))
;;      "(1 2 3)"))

;;   (test-case "abbreviated quote"
;;     (check-equal?
;;      (simple-pretty-format '(quote (1 2 3)))
;;      "'(1 2 3)"))

;;   (test-case "width-sensitive line breaking"
;;     (check-equal?
;;      (simple-pretty-format sample '((columns . 20)))
;;      (string-append
;;       "(define\n"
;;       "  (f x)\n"
;;       "  (cond\n"
;;       "    ((pair? x)\n"
;;       "      (map add1 x))\n"
;;       "    (else\n"
;;       "      (list x x x))))")))

;;   (test-case "default output agrees with pretty-print on a simple case"
;;     (check-equal?
;;      (call->string simple-pretty-write '(1 2 (3 4)) '())
;;      (pretty-write->string '(1 2 (3 4)) 79 #t)))

;;   (test-case "common atoms"
;;     (for ([value (in-list
;;                   (list #t
;;                         #f
;;                         '()
;;                         0
;;                         17
;;                         -17
;;                         3.5
;;                         -0.0
;;                         +inf.0
;;                         -inf.0
;;                         +nan.0
;;                         #\a
;;                         #\space
;;                         "hello"
;;                         "hello\nworld"
;;                         #"ABC"
;;                         'hello
;;                         '|two words|
;;                         '#:hello
;;                         '#&42
;;                         '#(1 2 3)
;;                         '#(a (b c) #&d)))])
;;       (check-equal?
;;        (simple-pretty-format value)
;;        (pretty-format->string value 79))))

;;   (test-case "common compact forms at default width"
;;     (for ([value (in-list
;;                   (list
;;                    '(if test then else)
;;                    '(begin (displayln "hi") (add1 x))
;;                    '(begin0 x (set! y 1) (displayln y))
;;                    '(lambda (x y) (list x y))
;;                    '(case-lambda
;;                       [() 0]
;;                       [(x) x]
;;                       [(x y) (+ x y)])
;;                    '(let ([x 1] [y 2]) (+ x y))
;;                    '(let* ([x 1] [y (+ x 1)]) (+ x y))
;;                    '(letrec ([loop (lambda (n)
;;                                      (if (zero? n)
;;                                          0
;;                                          (loop (sub1 n))))])
;;                       (loop 10))
;;                    '(let-values ([(x y) (values 1 2)]) (+ x y))
;;                    '(letrec-values
;;                       ([(even? odd?)
;;                         (values
;;                          (lambda (n)
;;                            (or (zero? n)
;;                                (odd? (sub1 n))))
;;                          (lambda (n)
;;                            (and (not (zero? n))
;;                                 (even? (sub1 n)))))]
;;                        )
;;                       (even? 10))
;;                    '(set! x (+ x 1))
;;                    '(when ready? (displayln "go") (run))
;;                    '(unless ready? (sleep 1) (retry))
;;                    '(and a b c d)
;;                    '(or a b c d)
;;                    '(cond
;;                       [(number? x) (add1 x)]
;;                       [(string? x) (string-length x)]
;;                       [else #f])
;;                    '(case x
;;                       [(1 2) 'small]
;;                       [(3 4) 'medium]
;;                       [else 'large])
;;                    '(match x
;;                       [(list a b) (+ a b)]
;;                       [_ #f])
;;                    '(for/list ([x xs]
;;                                [y ys]
;;                                #:when (positive? x))
;;                       (+ x y))
;;                    '(quasiquote
;;                      (1 2 (unquote x) (unquote-splicing xs)))
;;                    '(quote (a b c))
;;                    '(unquote x)
;;                    '(unquote-splicing xs)
;;                    '(define x 10)
;;                    '(define (f x)
;;                       (define y (+ x 1))
;;                       (+ y y))
;;                    '(struct posn (x y) #:transparent)
;;                    '(parameterize ([current-output-port p])
;;                       (displayln "hi"))
;;                    '(hash 'a 1 'b 2)
;;                    '(hasheq 'a 1 'b 2)
;;                    '(vector 'a 'b 'c)
;;                    '(box 'hello)
;;                    '(1 2 . 3)
;;                    '((lambda (x) x) '(1 2 3))))])
;;       (check-roundtrip-agrees value simple-pretty-write)
;;       (check-roundtrip-agrees value simple-pretty-write #:newline? #f)
;;       (check-equal?
;;        (read (open-input-string (simple-pretty-format value)))
;;        value)))

;;   (test-case "common forms at narrow width"
;;     (for ([value (in-list
;;                   (list
;;                    sample
;;                    '(let loop ([xs xs] [acc '()])
;;                       (cond
;;                         [(null? xs) (reverse acc)]
;;                         [else
;;                          (loop (cdr xs)
;;                                (cons (f (car xs)) acc))]))
;;                    '(for/fold ([acc '()])
;;                               ([x (in-list xs)]
;;                                [i (in-naturals)]
;;                                #:unless (zero? (modulo i 3)))
;;                       (cons (list i x) acc))
;;                    '(match expr
;;                       [`(if ,t ,th ,el)
;;                        (list 'if
;;                              (compile t)
;;                              (compile th)
;;                              (compile el))]
;;                       [_ (error "bad syntax")])
;;                    '(define (render tree)
;;                       (cond
;;                         [(pair? tree)
;;                          (for/list ([x tree])
;;                            (render x))]
;;                         [else tree]))
;;                    '(case-lambda
;;                       [() 'zero]
;;                       [(x) (list 'one x)]
;;                       [(x y z)
;;                        (list 'many x y z)])))])
;;       (check-roundtrip-agrees value simple-pretty-write #:columns 24)
;;       (check-equal?
;;        (read (open-input-string (simple-pretty-format value '((columns . 24)))))
;;        value)))

;;   (test-case "display mode common values"
;;     (for ([value (in-list
;;                   (list
;;                    "hello"
;;                    "hello\nworld"
;;                    'hello
;;                    #\a
;;                    '(1 "two" three)
;;                    '#(1 "two" three)
;;                    '#&"box"))])
;;       (check-equal?
;;        (call->string simple-pretty-display
;;                      value
;;                      '((columns . 20)
;;                        (newline? . #f)))
;;        (pretty-display->string value 20 #f))))


;;   (test-case "hash forms round trip"
;;     (for ([value (in-list
;;                   (list
;;                    '#hash((a . 1) (b . 2))
;;                    '#hasheq((a . 1) (b . 2))))])
;;       (check-roundtrip-agrees value simple-pretty-write)
;;       (check-roundtrip-agrees value simple-pretty-write #:columns 24)
;;       (check-roundtrip-agrees value simple-pretty-write #:newline? #f)
;;       (check-roundtrip-agrees value simple-pretty-display #:columns 24 #:newline? #f)
;;       (check-equal?
;;        (read (open-input-string (simple-pretty-format value)))
;;        value)
;;       (check-equal?
;;        (read (open-input-string (simple-pretty-format value '((columns . 24)))))
;;        value)))

;;   (for ([value (in-list
;;                 (list
;;                  42
;;                  "hello"
;;                  '(1 2 3)
;;                  '#(1 (2 3) 4)
;;                  '(1 2 . 3)))])
;;     (check-write-agrees value)
;;     (check-write-agrees value #:columns 24)
;;     (check-write-agrees value #:newline? #f)
;;     (check-display-agrees value #:columns 24 #:newline? #f)
;;     (check-format-agrees value)
;;     (check-format-agrees value #:columns 24))

;;   (for ([value (in-list
;;                 (list
;;                  '(define (f x)
;;                     (if (pair? x)
;;                         (map add1 x)
;;                         (list x x x)))
;;                  '(let ([x 1] [y 2])
;;                     (list x y (+ x y)))))])
;;     (check-roundtrip-agrees value simple-pretty-write)
;;     (check-roundtrip-agrees value simple-pretty-write #:columns 24)
;;     (check-roundtrip-agrees value simple-pretty-write #:newline? #f)
;;     (check-equal?
;;      (read (open-input-string (simple-pretty-format value)))
;;      value)
;;     (check-equal?
;;      (read (open-input-string (simple-pretty-format value '((columns . 24)))))
;;      value))

;;   (test-case "read-macro abbreviation option"
;;     (check-equal?
;;      (simple-pretty-format '(quote alpha))
;;      "'alpha")
;;     (check-equal?
;;      (simple-pretty-format '(quote alpha)
;;                            '((abbreviate-read-macros . #f)))
;;      "(quote alpha)")
;;     (check-equal?
;;      (simple-pretty-format '(quasiquote (a (unquote x) (unquote-splicing xs))))
;;      "`(a ,x ,@xs)")
;;     (check-equal?
;;      (simple-pretty-format '(quasiquote (a (unquote x) (unquote-splicing xs)))
;;                            '((abbreviate-read-macros . #f)))
;;      "(quasiquote (a (unquote x) (unquote-splicing xs)))"))

;;   (test-case "number formatting options"
;;     (check-equal?
;;      (simple-pretty-format 1/2 '((exact-as-decimal . #t)))
;;      "0.5")
;;     (check-equal?
;;      (simple-pretty-format 1.0 '((show-inexactness . #t)))
;;      "1.0")
;;     (check-equal?
;;      (simple-pretty-format 5 '((show-inexactness . #t)))
;;      "5"))

;;   (test-case "display mode differs from write mode on common atoms"
;;     (check-equal?
;;      (simple-pretty-format "hello\nworld")
;;      "\"hello\\nworld\"")
;;     (check-equal?
;;      (call->string simple-pretty-display
;;                    "hello\nworld"
;;                    '((newline? . #f)))
;;      "hello\nworld")
;;     (check-equal?
;;      (simple-pretty-format #\space)
;;      "#\\space")
;;     (check-equal?
;;      (call->string simple-pretty-display
;;                    #\space
;;                    '((newline? . #f)))
;;      " ")
;;     (check-equal?
;;      (simple-pretty-format #"ABC")
;;      "#\"ABC\"")
;;     (check-equal?
;;      (call->string simple-pretty-display
;;                    #"ABC"
;;                    '((newline? . #f)))
;;      "ABC"))

;;   (test-case "dot symbol option"
;;     (check-equal?
;;      (simple-pretty-format '|.|)
;;      "|.|")
;;     (check-equal?
;;      (simple-pretty-format '|.| '((.-symbol-without-bars . #t)))
;;      "."))

;;   (test-case "option validation"
;;     (check-exn
;;      exn:fail?
;;      (lambda ()
;;        (simple-pretty-format '(1 2 3) '((bogus . #t)))))
;;     (check-exn
;;      exn:fail?
;;      (lambda ()
;;        (simple-pretty-format '(1 2 3) '((columns . nope)))))
;;     (check-exn
;;      exn:fail?
;;      (lambda ()
;;        (simple-pretty-format '(1 2 3) '((depth . -1)))))
;;     (check-exn
;;      exn:fail?
;;      (lambda ()
;;        (simple-pretty-write '(1 2 3) (current-output-port) '((newline? . nope))))))

;;   (test-case "default options and overriding"
;;     (check-equal?
;;      (map car default-pretty-options)
;;      '(columns
;;        depth
;;        newline?
;;        show-inexactness
;;        exact-as-decimal
;;        .-symbol-without-bars
;;        abbreviate-read-macros))
;;     (check-equal?
;;      (simple-pretty-format '(1 2 3)
;;                            '((columns . 10)
;;                              (columns . 79)))
;;      "(1 2 3)")
;;     (check-equal?
;;      (call->string simple-pretty-write
;;                    '(1 2 3)
;;                    '((newline? . #t)
;;                      (newline? . #f)))
;;      "(1 2 3)"))

;;   (test-case "infinite columns keeps compact rendering"
;;     (for ([value (in-list
;;                   (list
;;                    '(define (f x)
;;                       (if (pair? x)
;;                           (map add1 x)
;;                           (list x x x)))
;;                    '(let ([x 1] [y 2])
;;                       (list x y (+ x y)))
;;                    '(letrec-values
;;                       ([(even? odd?)
;;                         (values
;;                          (lambda (n)
;;                            (or (zero? n)
;;                                (odd? (sub1 n))))
;;                          (lambda (n)
;;                            (and (not (zero? n))
;;                                 (even? (sub1 n)))))]
;;                       )
;;                       (even? 10))))])
;;       (check-equal?
;;        (read (open-input-string
;;               (simple-pretty-format value '((columns . infinity)))))
;;        value)
;;       (check-false
;;        (regexp-match? #rx"\n"
;;                       (simple-pretty-format value '((columns . infinity)))))))

;;   (test-case "more round trips at multiple widths"
;;     (for* ([columns (in-list '(12 18 24 40 79))]
;;            [value (in-list
;;                    (list
;;                     '(let/ec k
;;                        (k 42))
;;                     '(local [(define (f x) (+ x 1))
;;                              (define y 10)]
;;                        (f y))
;;                     '(shared ([x (cons 1 x)]) x)
;;                     '(with-handlers ([exn:fail? values])
;;                        (error "boom"))
;;                     '(call-with-values
;;                       (lambda () (values 1 2 3))
;;                       list)
;;                     '(for/vector ([x '(1 2 3)])
;;                        (+ x 10))
;;                     '(let-syntax ([m (syntax-rules ()
;;                                        [(_ x) (list x x)])])
;;                        (m 10))
;;                     '(syntax-case stx ()
;;                        [(head x y) #'(list x y)])
;;                     '(define-values (x y z) (values 1 2 3))
;;                     '(module+ test
;;                        (define check #t)
;;                        (when check (displayln "ok")))))])
;;       (check-roundtrip-agrees value simple-pretty-write #:columns columns)
;;       (check-roundtrip-agrees value simple-pretty-write #:columns columns #:newline? #f)
;;       (check-equal?
;;        (read (open-input-string
;;               (simple-pretty-format value `((columns . ,columns)))))
;;        value)))
;;   ) ; module+
