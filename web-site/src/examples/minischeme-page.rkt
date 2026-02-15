;;;
;;; MiniScheme page (WebRacket site page)
;;;

(define minischeme-sample-program
  (string-append
   "(define (fact n)\n"
   "  (if (= n 0)\n"
   "      1\n"
   "      (* n (fact (- n 1)))))\n"
   "(fact 6)\n"))

(define (minischeme-page)
  `(div (@ (class "page page--minischeme"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Interpreter")
                           (span (@ (class "pill")) "REPL")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "MiniScheme")
                      (p (@ (class "hero-lead"))
                         "A tiny Scheme interpreter running entirely in the browser.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (class "minischeme-shell")
                              (style ,(string-append
                                       "display: grid; gap: 12px; max-width: 920px; margin: 0 auto; "
                                       "padding: 22px; border-radius: 14px; "
                                       "background: rgba(13, 15, 29, 0.74); "
                                       "border: 1px solid rgba(119, 141, 214, 0.25); "
                                       "box-shadow: 0 22px 48px rgba(10, 12, 25, 0.5);")))
                           (p (@ (style "margin: 0; color: rgba(213, 223, 255, 0.85);"))
                              "Type one or more expressions, then click "
                              (code "Run")
                              ". Definitions persist until reset.")
                           (textarea
                            (@ (id "minischeme-input")
                               (rows "12")
                               (spellcheck "false")
                               (style ,(string-append
                                        "display: block; width: 100%; height: 320px; min-height: 320px; resize: vertical; "
                                        "font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace; "
                                        "font-size: 0.96rem; line-height: 1.45; "
                                        "padding: 12px; border-radius: 10px; "
                                        "background: #0A0C18; color: #EEF2FF; "
                                        "border: 1px solid rgba(120, 140, 220, 0.35);")))
                            ,minischeme-sample-program)
                           (div (@ (style "display: flex; gap: 10px; flex-wrap: wrap;"))
                                (button (@ (id "minischeme-run")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #5E7BFF; color: white;")))
                                        "Run")
                                (button (@ (id "minischeme-load-sample")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #20A085; color: white;")))
                                        "Load Sample")
                                (button (@ (id "minischeme-reset")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #E06A5F; color: white;")))
                                        "Reset State"))
                           (pre (@ (id "minischeme-output")
                                   (style ,(string-append
                                            "margin: 0; min-height: 96px; white-space: pre-wrap; word-break: break-word; "
                                            "padding: 12px; border-radius: 10px; "
                                            "font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace; "
                                            "background: #070914; color: #DCE5FF; "
                                            "border: 1px solid rgba(120, 140, 220, 0.26);")))
                                "MiniScheme ready."))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details"))
                           (p "This page ports the existing MiniScheme example into the web-site shell.")
                           (div (@ (class "mathjax-actions"))
                                ,(code-pill (gh-file "examples/minischeme/interpreter.rkt")
                                            "Original interpreter")
                                ,(code-pill (gh-file "web-site/src/examples/minischeme-page.rkt")
                                            "Web-site source")))))
        ,(footer-section)))

(define (minischeme-nullish? x)
  (cond
    [(not x) #t]
    [else
     (define s (js-value->string x))
     (or (string=? s "null")
         (string=? s "undefined"))]))

(define-values (minischeme-reset-state! minischeme-process-input)
  (let ()
    (struct env     (table parent))
    (struct closure (params body env))
    (struct prim    (name proc))
    (struct store   (table next) #:mutable)
    (struct k-apply   (args env))
    (struct k-args    (proc rest env values))
    (struct k-if      (then else env))
    (struct k-begin   (rest env))
    (struct k-set!    (addr name))
    (struct k-define  (addr name))
    (struct k-restore (env))

    (define uninitialized (gensym 'uninitialized))
    (define no-else       (gensym 'no-else))

    (define (make-env parent)
      (env (make-hasheq) parent))

    (define (env-bound-current? e name)
      (hash-has-key? (env-table e) name))

    (define (env-lookup e name)
      (cond
        [(hash-has-key? (env-table e) name)
         (hash-ref (env-table e) name)]
        [(env-parent e)
         (env-lookup (env-parent e) name)]
        [else
         (error 'minischeme "unbound identifier ~a" name)]))

    (define (env-define! e name addr)
      (hash-set! (env-table e) name addr))

    (define (make-store)
      (store (make-hasheq) 0))

    (define (store-alloc! st value)
      (define addr (store-next st))
      (hash-set! (store-table st) addr value)
      (set-store-next! st (add1 addr))
      addr)

    (define (store-ref st addr)
      (define v (hash-ref (store-table st) addr
                          (λ () (error 'minischeme "unknown address ~a" addr))))
      (if (eq? v uninitialized)
          (error 'minischeme "accessing uninitialized binding")
          v))

    (define (store-set! st addr value)
      (hash-set! (store-table st) addr value))

    (define (literal? expr)
      (or (boolean? expr)
          (number? expr)
          (string? expr)
          (char? expr)
          (null? expr)))

    (define (ensure-identifier sym)
      (unless (symbol? sym)
        (error 'minischeme "expected identifier, got ~a" sym)))

    (define (ensure-parameters params)
      (unless (list? params)
        (error 'minischeme "invalid parameter list ~a" params))
      (let loop ([ps params])
        (cond
          [(null? ps) params]
          [(symbol? (car ps)) (loop (cdr ps))]
          [else (error 'minischeme "invalid parameter list ~a" params)])))

    (define (check-numbers name args)
      (for-each (λ (v)
                  (unless (number? v)
                    (error 'minischeme "~a expects numbers, got ~a" name v)))
                args))

    (define (check-arg-count name args expected)
      (unless (= (length args) expected)
        (error 'minischeme "~a expects ~a argument~a"
               name expected (if (= expected 1) "" "s"))))

    (define (check-at-least name args expected)
      (unless (>= (length args) expected)
        (error 'minischeme "~a expects at least ~a argument~a"
               name expected (if (= expected 1) "" "s"))))

    (define (create-initial-state)
      (define base-env   (make-env #f))
      (define base-store (make-store))

      (define all-primitives '())
      (define all-constants  '())
      (define all-keywords   '())

      (define (install name proc)
        (define addr (store-alloc! base-store (prim name proc)))
        (env-define! base-env name addr)
        (set! all-primitives (cons name all-primitives)))

      (define (numeric name f)
        (install name (λ (args)
                        (check-numbers name args)
                        (apply f args))))
      (define (constant name value)
        (define addr (store-alloc! base-store value))
        (env-define! base-env name addr)
        (set! all-constants (cons name all-constants)))

      (constant 'null  '())
      (constant 'empty '())
      (constant 'true  #t)
      (constant 'false #f)

      (numeric '+ +)
      (numeric '* *)
      (install '- (λ (args)
                    (check-at-least '- args 1)
                    (check-numbers '- args)
                    (if (null? (cdr args))
                        (- (car args))
                        (apply - args))))
      (install '/ (λ (args)
                    (check-at-least '/ args 1)
                    (check-numbers '/ args)
                    (if (null? (cdr args))
                        (/ 1 (car args))
                        (apply / args))))
      (install '= (λ (args)
                    (check-numbers '= args)
                    (if (null? args) #t (apply = args))))
      (install '< (λ (args)
                    (check-at-least '< args 2)
                    (check-numbers '< args)
                    (apply < args)))
      (install '<= (λ (args)
                     (check-at-least '<= args 2)
                     (check-numbers '<= args)
                     (apply <= args)))
      (install '> (λ (args)
                    (check-at-least '> args 2)
                    (check-numbers '> args)
                    (apply > args)))
      (install '>= (λ (args)
                     (check-at-least '>= args 2)
                     (check-numbers '>= args)
                     (apply >= args)))
      (install 'cons (λ (args)
                       (check-arg-count 'cons args 2)
                       (cons (car args) (cadr args))))
      (install 'car (λ (args)
                      (check-arg-count 'car args 1)
                      (let ([v (car args)])
                        (unless (pair? v)
                          (error 'minischeme "car expects a non-empty pair"))
                        (car v))))
      (install 'cdr (λ (args)
                      (check-arg-count 'cdr args 1)
                      (let ([v (car args)])
                        (unless (pair? v)
                          (error 'minischeme "cdr expects a non-empty pair"))
                        (cdr v))))
      (install 'list (λ (args) args))
      (install 'list? (λ (args)
                        (check-arg-count 'list? args 1)
                        (list? (car args))))
      (install 'pair? (λ (args)
                        (check-arg-count 'pair? args 1)
                        (pair? (car args))))
      (install 'null? (λ (args)
                        (check-arg-count 'null? args 1)
                        (null? (car args))))
      (install 'symbol? (λ (args)
                          (check-arg-count 'symbol? args 1)
                          (symbol? (car args))))
      (install 'number? (λ (args)
                          (check-arg-count 'number? args 1)
                          (number? (car args))))
      (install 'boolean? (λ (args)
                           (check-arg-count 'boolean? args 1)
                           (boolean? (car args))))
      (install 'not (λ (args)
                      (check-arg-count 'not args 1)
                      (false? (car args))))
      (install 'equal? (λ (args)
                         (check-arg-count 'equal? args 2)
                         (equal? (car args) (cadr args))))
      (constant 'primitives (sort all-primitives (λ (x y) (symbol<? x y))))
      (constant 'constants  (sort all-constants  (λ (x y) (symbol<? x y))))
      (constant 'keywords   (sort all-keywords   (λ (x y) (symbol<? x y))))
      (values base-env base-store))

    (define global-env   #f)
    (define global-store #f)

    (define (reset-state!)
      (define-values (env store) (create-initial-state))
      (set! global-env env)
      (set! global-store store))

    ;; Use a local s-expression parser, since `read` currently parses lists
    ;; incorrectly in this runtime path.
    (define (parse-program s)
      (define len (string-length s))
      (define i 0)
      (define eof-marker (gensym 'eof))

      (define (done?) (>= i len))
      (define (peek) (if (done?) #f (string-ref s i)))
      (define (advance!) (set! i (add1 i)))

      (define (skip-space+comments!)
        (let loop ()
          (cond
            [(done?) (void)]
            [(char-whitespace? (peek))
             (advance!)
             (loop)]
            [(char=? (peek) #\;)
             (let skip-comment ()
               (cond
                 [(done?) (void)]
                 [(char=? (peek) #\newline) (advance!) (loop)]
                 [else (advance!) (skip-comment)]))]
            [else (void)])))

      (define (delimiter? ch)
        (or (char-whitespace? ch)
            (char=? ch #\()
            (char=? ch #\))
            (char=? ch #\')
            (char=? ch #\;)))

      (define (parse-atom token)
        (cond
          [(string=? token "#t") #t]
          [(string=? token "#f") #f]
          [else
           (define n (string->number token))
           (if n n (string->symbol token))]))

      (define (parse-string)
        (advance!) ; opening "
        (define out (open-output-string))
        (let loop ()
          (when (done?)
            (error 'minischeme "unterminated string"))
          (define ch (peek))
          (advance!)
          (cond
            [(char=? ch #\")
             (get-output-string out)]
            [(char=? ch #\\)
             (when (done?)
               (error 'minischeme "unterminated string escape"))
             (define esc (peek))
             (advance!)
             (write-char
              (cond
                [(char=? esc #\\) #\\]
                [(char=? esc #\") #\"]
                [(char=? esc #\n) #\newline]
                [(char=? esc #\t) #\tab]
                [else esc])
              out)
             (loop)]
            [else
             (write-char ch out)
             (loop)])))

      (define (parse-list)
        (advance!) ; opening (
        (define (loop acc)
          (skip-space+comments!)
          (when (done?)
            (error 'minischeme "unterminated list"))
          (if (char=? (peek) #\))
              (begin (advance!) (reverse acc))
              (loop (cons (parse-expr) acc))))
        (loop '()))

      (define (parse-token)
        (define start i)
        (let loop ()
          (when (and (not (done?)) (not (delimiter? (peek))))
            (advance!)
            (loop)))
        (substring s start i))

      (define (parse-expr)
        (skip-space+comments!)
        (cond
          [(done?) eof-marker]
          [(char=? (peek) #\() (parse-list)]
          [(char=? (peek) #\)) (error 'minischeme "unexpected )")]
          [(char=? (peek) #\') (advance!) (list 'quote (parse-expr))]
          [(char=? (peek) #\") (parse-string)]
          [else (parse-atom (parse-token))]))

      (let loop ([acc '()])
        (define e (parse-expr))
        (if (eq? e eof-marker)
            (reverse acc)
            (loop (cons e acc)))))

    (define (value->string v)
      (cond
        [(closure? v) "#<closure>"]
        [(prim? v)    (format "#<primitive ~a>" (prim-name v))]
        [(void? v)    "#<void>"]
        [else         (format "~s" v)]))

    (define (apply-procedure value args env store kont loop)
      (cond
        [(closure? value)
         (define params (closure-params value))
         (when (not (= (length params) (length args)))
           (error 'minischeme "arity mismatch: expected ~a arguments, got ~a"
                  (length params) (length args)))
         (define new-env (make-env (closure-env value)))
         (let bind-loop ([ps params] [as args])
           (unless (null? ps)
             (ensure-identifier (car ps))
             (define addr (store-alloc! store (car as)))
             (env-define! new-env (car ps) addr)
             (bind-loop (cdr ps) (cdr as))))
         (define body (closure-body value))
         (if (null? body)
             (loop 'value (void) env store kont)
             (loop 'eval (car body) new-env store
                   (cons (k-begin (cdr body) new-env)
                         (cons (k-restore env) kont))))]
        [(prim? value)
         (define result ((prim-proc value) args))
         (loop 'value result env store kont)]
        [else
         (error 'minischeme "application of non-procedure: ~a" value)]))

    (define (desugar-let bindings body)
      (define vars '())
      (define vals '())
      (for-each
       (λ (binding)
         (unless (and (pair? binding)
                      (symbol? (car binding))
                      (pair? (cdr binding))
                      (null? (cddr binding)))
           (error 'minischeme "malformed let binding" binding))
         (set! vars (cons (car binding) vars))
         (set! vals (cons (cadr binding) vals)))
       bindings)
      (cons (cons 'lambda (cons (reverse vars) body))
            (reverse vals)))

    (define (cesk-evaluate expr env store)
      (let loop ([mode          'eval]
                 [control       expr]
                 [current-env   env]
                 [current-store store]
                 [kont          '()])
        (define (continue mode control env store kont)
          (loop mode control env store kont))
        (define (apply-now proc args env store kont)
          (apply-procedure proc args env store kont continue))
        (define (eval-sequence forms env store kont)
          (if (null? forms)
              (continue 'value (void) env store kont)
              (continue 'eval (car forms) env store
                        (cons (k-begin (cdr forms) env) kont))))
        (cond
          [(eq? mode 'eval)
           (cond
             [(literal? control)
              (continue 'value control current-env current-store kont)]
             [(symbol? control)
              (define addr (env-lookup current-env control))
              (continue 'value (store-ref current-store addr)
                        current-env current-store kont)]
             [(and (pair? control) (eq? (car control) 'quote))
              (let ([rest (cdr control)])
                (if (and (pair? rest) (null? (cdr rest)))
                    (continue 'value (car rest) current-env current-store kont)
                    (error 'minischeme "malformed quote" control)))]
             [(and (pair? control) (eq? (car control) 'lambda))
              (let ([rest (cdr control)])
                (if (and (pair? rest))
                    (let ([params (car rest)]
                          [body (cdr rest)])
                      (continue 'value (closure (ensure-parameters params)
                                                body current-env)
                                current-env current-store kont))
                    (error 'minischeme "malformed lambda" control)))]
             [(and (pair? control) (eq? (car control) 'if))
              (let* ([rest (cdr control)]
                     [len (length rest)])
                (cond
                  [(or (< len 2) (> len 3))
                   (error 'minischeme "malformed if" control)]
                  [else
                   (define test (car rest))
                   (define then (cadr rest))
                   (define else-expr (if (= len 3) (caddr rest) no-else))
                   (continue 'eval test current-env current-store
                             (cons (k-if then else-expr current-env) kont))]))]
             [(and (pair? control) (eq? (car control) 'begin))
              (eval-sequence (cdr control) current-env current-store kont)]
             [(and (pair? control) (eq? (car control) 'set!))
              (let ([rest (cdr control)])
                (if (and (pair? rest)
                         (symbol? (car rest))
                         (pair? (cdr rest))
                         (null? (cddr rest)))
                    (let ([name (car rest)]
                          [rhs (cadr rest)])
                      (define addr (env-lookup current-env name))
                      (continue 'eval rhs current-env current-store
                                (cons (k-set! addr name) kont)))
                    (error 'minischeme "malformed set!" control)))]
             [(and (pair? control) (eq? (car control) 'define))
              (let ([rest (cdr control)])
                (cond
                  [(and (pair? rest) (symbol? (car rest)))
                   (let ([name (car rest)]
                         [tail (cdr rest)])
                     (unless (and (pair? tail) (null? (cdr tail)))
                       (error 'minischeme "malformed define" control))
                     (define addr
                       (if (env-bound-current? current-env name)
                           (hash-ref (env-table current-env) name)
                           (let ([a (store-alloc! current-store uninitialized)])
                             (env-define! current-env name a)
                             a)))
                     (store-set! current-store addr uninitialized)
                     (continue 'eval (car tail) current-env current-store
                               (cons (k-define addr name) kont)))]
                  [(and (pair? rest)
                        (pair? (car rest))
                        (symbol? (caar rest)))
                   (let* ([head (car rest)]
                          [name (car head)]
                          [params (cdr head)]
                          [body (cdr rest)])
                     (define addr
                       (if (env-bound-current? current-env name)
                           (hash-ref (env-table current-env) name)
                           (let ([a (store-alloc! current-store uninitialized)])
                             (env-define! current-env name a)
                             a)))
                     (store-set! current-store addr uninitialized)
                     (define lambda-expr (cons 'lambda (cons params body)))
                     (continue 'eval lambda-expr current-env current-store
                               (cons (k-define addr name) kont)))]
                  [else (error 'minischeme "malformed define" control)]))]
             [(and (pair? control) (eq? (car control) 'let))
              (let ([rest (cdr control)])
                (if (and (pair? rest))
                    (let ([bindings (car rest)]
                          [body (cdr rest)])
                      (continue 'eval (desugar-let bindings body)
                                current-env current-store kont))
                    (error 'minischeme "malformed let" control)))]
             [(pair? control)
              (define op (car control))
              (define args (cdr control))
              (continue 'eval op current-env current-store
                        (cons (k-apply args current-env) kont))]
             [else
              (error 'minischeme "cannot evaluate expression ~a" control)])]
          [(eq? mode 'value)
           (if (null? kont)
               (values control current-env current-store)
               (let* ([frame (car kont)]
                      [rest (cdr kont)])
                 (cond
                   [(k-apply? frame)
                    (define args (k-apply-args frame))
                    (define call-env (k-apply-env frame))
                    (if (null? args)
                        (apply-now control '() call-env current-store rest)
                        (continue 'eval (car args) call-env current-store
                                  (cons (k-args control (cdr args) call-env '())
                                        rest)))]
                   [(k-args? frame)
                    (define proc (k-args-proc frame))
                    (define rest-args (k-args-rest frame))
                    (define call-env (k-args-env frame))
                    (define collected (cons control (k-args-values frame)))
                    (if (null? rest-args)
                        (apply-now proc (reverse collected)
                                   call-env current-store rest)
                        (continue 'eval (car rest-args) call-env current-store
                                  (cons (k-args proc (cdr rest-args)
                                                call-env collected) rest)))]
                   [(k-if? frame)
                    (define branch-env (k-if-env frame))
                    (define then (k-if-then frame))
                    (define else-expr (k-if-else frame))
                    (if (false? control)
                        (if (eq? else-expr no-else)
                            (continue 'value (void) branch-env current-store rest)
                            (continue 'eval else-expr branch-env current-store rest))
                        (continue 'eval then branch-env current-store rest))]
                   [(k-begin? frame)
                    (define begin-env (k-begin-env frame))
                    (define rest-forms (k-begin-rest frame))
                    (if (null? rest-forms)
                        (continue 'value control begin-env current-store rest)
                        (continue 'eval (car rest-forms) begin-env current-store
                                  (cons (k-begin (cdr rest-forms) begin-env) rest)))]
                   [(k-set!? frame)
                    (store-set! current-store (k-set!-addr frame) control)
                    (continue 'value control current-env current-store rest)]
                   [(k-define? frame)
                    (store-set! current-store (k-define-addr frame) control)
                    (continue 'value (k-define-name frame)
                              current-env current-store rest)]
                   [(k-restore? frame)
                    (continue 'value control (k-restore-env frame)
                              current-store rest)]
                   [else
                    (error 'minischeme "unknown continuation frame ~a" frame)])))]
          [else
           (error 'minischeme "invalid evaluation mode" mode)])))

    (define (evaluate-program exprs)
      (let loop ([forms exprs]
                 [last-value (void)])
        (if (null? forms)
            last-value
            (call-with-values
             (λ () (cesk-evaluate (car forms) global-env global-store))
             (λ (value _env _store)
               (loop (cdr forms) value))))))

    (define (process-input s)
      (unless (and global-env global-store)
        (reset-state!))
      (define-values (exprs read-error)
        (with-handlers
          ([exn:fail:read?
            (λ (e) (values #f (string-append "=> read error: " (exn-message e))))]
           [(λ _ #t)
            (λ (e) (values #f (string-append "=> read error: " (exn-message e))))])
          (values (parse-program s) #f)))
      (if read-error
          read-error
          (with-handlers
            ([exn:fail? (λ (e) (string-append "=> " (exn-message e)))])
            (if (null? exprs)
                "=> ; no input"
                (let ([value (evaluate-program exprs)])
                  (string-append "=> " (value->string value)))))))

    (reset-state!)
    (values reset-state! process-input)))

(define minischeme-page-started? #f)
(define minischeme-run-handler      #f)
(define minischeme-reset-handler    #f)
(define minischeme-load-handler     #f)

(define (init-minischeme-page!)
  (when (not minischeme-page-started?)
    (set! minischeme-page-started? #t)
    (define input-node       (js-get-element-by-id "minischeme-input"))
    (define output-node      (js-get-element-by-id "minischeme-output"))
    (define run-button       (js-get-element-by-id "minischeme-run"))
    (define reset-button     (js-get-element-by-id "minischeme-reset"))
    (define sample-button    (js-get-element-by-id "minischeme-load-sample"))

    (when (or (minischeme-nullish? input-node)
              (minischeme-nullish? output-node)
              (minischeme-nullish? run-button)
              (minischeme-nullish? reset-button)
              (minischeme-nullish? sample-button))
      (error 'minischeme-page "missing expected DOM nodes for MiniScheme page"))

    (define (set-output! text)
      (js-set! output-node "textContent" text))

    (define (run! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (set-output!
                                    (string-append "error: " (exn-message e))))])
        (define source (js-ref input-node "value"))
        (set-output! (minischeme-process-input source))))

    (define (reset! . _)
      (minischeme-reset-state!)
      (set-output! "MiniScheme state reset."))

    (define (load-sample! . _)
      (js-set! input-node "value" minischeme-sample-program)
      (set-output! "Sample program loaded. Click Run to evaluate."))

    (set! minischeme-run-handler   (procedure->external run!))
    (set! minischeme-reset-handler (procedure->external reset!))
    (set! minischeme-load-handler  (procedure->external load-sample!))

    (js-add-event-listener! run-button    "click" minischeme-run-handler)
    (js-add-event-listener! reset-button  "click" minischeme-reset-handler)
    (js-add-event-listener! sample-button "click" minischeme-load-handler)

    (minischeme-reset-state!)
    (set-output! "MiniScheme ready.")))
