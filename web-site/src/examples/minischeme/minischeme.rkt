;;;
;;; MiniScheme Interpreter (shared by minischeme-page)
;;;

(define-values (minischeme-reset-state! minischeme-process-input)
  (let ()
    (struct env       (table parent))
    (struct closure   (params body env))
    (struct prim      (name proc))
    (struct k-apply   (args env))
    (struct k-args    (proc rest env values))
    (struct k-if      (then else env))
    (struct k-begin   (rest env))
    (struct k-set!    (cell name))
    (struct k-define  (cell name))

    (define uninitialized (gensym 'uninitialized))
    (define no-else       (gensym 'no-else))

    (define (make-env parent)
      (env (make-hasheq) parent))

    (define (env-bound-current? e name)
      (hash-has-key? (env-table e) name))

    (define (env-lookup-cell e name)
      (cond
        [(hash-has-key? (env-table e) name)
         (hash-ref (env-table e) name)]
        [(env-parent e)
         (env-lookup-cell (env-parent e) name)]
        [else
         (error 'minischeme "unbound identifier ~a" name)]))

    (define (cell-value cell)
      (define v (unbox cell))
      (if (eq? v uninitialized)
          (error 'minischeme "accessing uninitialized binding")
          v))

    (define (env-lookup e name)
      (cell-value (env-lookup-cell e name)))

    (define (env-define! e name value)
      (hash-set! (env-table e) name (box value)))

    (define (env-define-cell! e name cell)
      (hash-set! (env-table e) name cell))

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
      (define (bad)
        (error 'minischeme "invalid parameter list ~a" params))
      (cond
        [(symbol? params) params]
        [(null? params) params]
        [(pair? params)
         (let loop ([ps params])
           (cond
             [(null? ps) params]
             [(pair? ps)
              (unless (symbol? (car ps)) (bad))
              (loop (cdr ps))]
             [(symbol? ps) params]
             [else (bad)]))]
        [else (bad)]))

    (define (split-formals params)
      (cond
        [(symbol? params) (values '() params)]
        [(null? params) (values '() #f)]
        [else
         (let loop ([ps params] [required '()])
           (cond
             [(null? ps) (values (reverse required) #f)]
             [(pair? ps) (loop (cdr ps) (cons (car ps) required))]
             [else (values (reverse required) ps)]))]))

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
      (define base-env (make-env #f))

      (define all-primitives '())
      (define all-constants  '())
      (define all-keywords   '())

      (define (install name proc)
        (env-define! base-env name (prim name proc))
        (set! all-primitives (cons name all-primitives)))

      (define (numeric name f)
        (install name (λ (args)
                        (check-numbers name args)
                        (apply f args))))
      (define (constant name value)
        (env-define! base-env name value)
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
      base-env)

    (define global-env #f)

    (define (reset-state!)
      (set! global-env (create-initial-state)))

    ;; Parse via WebRacket's `read`.
    (define (parse-program/read s)
      (define in (open-input-string s))
      (let loop ([exprs '()])
        (define datum (read in))
        (if (eof-object? datum)
            (reverse exprs)
            (loop (cons datum exprs)))))

    (define (value->string v)
      (cond
        [(closure? v) "#<closure>"]
        [(prim? v)    (format "#<primitive ~a>" (prim-name v))]
        [(void? v)    "#<void>"]
        [else         (format "~s" v)]))

    (define (apply-procedure value args caller-env kont loop)
      (cond
        [(closure? value)
         (define params (closure-params value))
         (define-values (required rest-id) (split-formals params))
         (define req-count (length required))
         (define arg-count (length args))
         (if rest-id
             (when (< arg-count req-count)
               (error 'minischeme "arity mismatch: expected at least ~a arguments, got ~a"
                      req-count arg-count))
             (when (not (= req-count arg-count))
               (error 'minischeme "arity mismatch: expected ~a arguments, got ~a"
                      req-count arg-count)))
         (define new-env (make-env (closure-env value)))
         (let bind-loop ([ps required] [as args])
           (unless (null? ps)
             (ensure-identifier (car ps))
             (env-define! new-env (car ps) (car as))
             (bind-loop (cdr ps) (cdr as))))
         (when rest-id
           (ensure-identifier rest-id)
           (env-define! new-env rest-id (list-tail args req-count)))
         (define body (closure-body value))
         (if (null? body)
             (loop 'value (void) caller-env kont)
             (loop 'eval (car body) new-env
                   (cons (k-begin (cdr body) new-env) kont)))]
        [(prim? value)
         (define result ((prim-proc value) args))
         (loop 'value result caller-env kont)]
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
           (error 'minischeme "malformed let binding: ~s" binding))
         (set! vars (cons (car binding) vars))
         (set! vals (cons (cadr binding) vals)))
       bindings)
      (cons (cons 'lambda (cons (reverse vars) body))
            (reverse vals)))

    (define (expand-body forms)
      (cond
        [(null? forms) '(begin)]
        [(null? (cdr forms)) (expand-expr (car forms))]
        [else (cons 'begin (map expand-expr forms))]))

    (define (expand-and forms)
      (cond
        [(null? forms) #t]
        [(null? (cdr forms)) (expand-expr (car forms))]
        [else
         (list 'if (expand-expr (car forms))
               (expand-and (cdr forms))
               #f)]))

    (define (expand-or forms)
      (cond
        [(null? forms) #f]
        [(null? (cdr forms)) (expand-expr (car forms))]
        [else
         (define tmp (gensym 'or-tmp))
         (list 'let (list (list tmp (expand-expr (car forms))))
               (list 'if tmp tmp (expand-or (cdr forms))))]))

    (define (desugar-let* bindings body)
      (if (null? bindings)
          (expand-body body)
          (list 'let (list (car bindings))
                (desugar-let* (cdr bindings) body))))

    (define (desugar-cond clauses)
      (cond
        [(null? clauses) '(begin)]
        [else
         (define clause (car clauses))
         (unless (pair? clause)
           (error 'minischeme "malformed cond clause: ~s" clause))
         (define test (car clause))
         (define body (cdr clause))
         (cond
           [(eq? test 'else)
            (unless (null? (cdr clauses))
              (error 'minischeme "cond: else clause must be last: ~s" clauses))
            (expand-body body)]
           [(and (pair? body) (eq? (car body) '=>))
            (error 'minischeme "cond => clauses not supported: ~s" clause)]
           [(null? body)
            (define tmp (gensym 'cond-tmp))
            (list 'let (list (list tmp (expand-expr test)))
                  (list 'if tmp tmp (desugar-cond (cdr clauses))))]
           [else
            (list 'if (expand-expr test)
                  (expand-body body)
                  (desugar-cond (cdr clauses)))])]))

    (define (case-clause-test key clauses)
      (expand-or
       (map (λ (datum) (list 'equal? key (list 'quote datum))) clauses)))

    (define (desugar-case key clauses)
      (define tmp (gensym 'case-key))
      (define (loop cls)
        (cond
          [(null? cls) '(begin)]
          [else
           (define clause (car cls))
           (unless (pair? clause)
             (error 'minischeme "malformed case clause: ~s" clause))
           (define head (car clause))
           (define body (cdr clause))
           (cond
             [(eq? head 'else)
              (unless (null? (cdr cls))
                (error 'minischeme "case: else clause must be last: ~s" clauses))
              (expand-body body)]
             [(list? head)
              (list 'if (case-clause-test tmp head)
                    (expand-body body)
                    (loop (cdr cls)))]
             [else
              (error 'minischeme "malformed case clause: ~s" clause)])]))
      (list 'let (list (list tmp (expand-expr key)))
            (loop clauses)))

    (define (expand-binding b)
      (unless (and (pair? b)
                   (symbol? (car b))
                   (pair? (cdr b))
                   (null? (cddr b)))
        (error 'minischeme "malformed binding: ~s" b))
      (list (car b) (expand-expr (cadr b))))

    (define (expand-expr expr)
      (cond
        [(or (symbol? expr)
             (number? expr)
             (boolean? expr)
             (string? expr)
             (char? expr)
             (null? expr)
             (vector? expr))
         expr]
        [(pair? expr)
         (define head (car expr))
         (define tail (cdr expr))
         (cond
           [(eq? head 'quote) expr]
           [(eq? head 'quasiquote) expr]
           [(eq? head 'unquote) expr]
           [(eq? head 'unquote-splicing) expr]
           [(eq? head 'and) (expand-and tail)]
           [(eq? head 'or) (expand-or tail)]
           [(eq? head 'when)
            (unless (pair? tail)
              (error 'minischeme "malformed when: ~s" expr))
            (list 'if (expand-expr (car tail))
                  (expand-body (cdr tail))
                  '(begin))]
           [(eq? head 'unless)
            (unless (pair? tail)
              (error 'minischeme "malformed unless: ~s" expr))
            (list 'if (expand-expr (car tail))
                  '(begin)
                  (expand-body (cdr tail)))]
           [(eq? head 'cond) (desugar-cond tail)]
           [(eq? head 'case)
            (unless (pair? tail)
              (error 'minischeme "malformed case: ~s" expr))
            (desugar-case (car tail) (cdr tail))]
           [(eq? head 'let*)
            (unless (pair? tail)
              (error 'minischeme "malformed let*: ~s" expr))
            (desugar-let* (car tail) (cdr tail))]
           [(eq? head 'letrec)
            (unless (pair? tail)
              (error 'minischeme "malformed letrec: ~s" expr))
            (list 'letrec (map expand-binding (car tail))
                  (expand-body (cdr tail)))]
           [(eq? head 'let)
            (unless (pair? tail)
              (error 'minischeme "malformed let: ~s" expr))
            (list 'let (map expand-binding (car tail))
                  (expand-body (cdr tail)))]
           [(eq? head 'lambda)
            (if (and (pair? tail) (pair? (cdr tail)))
                (cons 'lambda (cons (car tail) (map expand-expr (cdr tail))))
                expr)]
           [(eq? head 'if)
            (if (and (pair? tail) (pair? (cdr tail)))
                (let ([test (car tail)]
                      [then (cadr tail)]
                      [else-tail (cddr tail)])
                  (if (null? else-tail)
                      (list 'if (expand-expr test) (expand-expr then))
                      (list 'if (expand-expr test)
                            (expand-expr then)
                            (expand-expr (car else-tail)))))
                expr)]
           [(eq? head 'begin) (cons 'begin (map expand-expr tail))]
           [(eq? head 'set!)
            (if (and (pair? tail) (pair? (cdr tail)) (null? (cddr tail)))
                (list 'set! (car tail) (expand-expr (cadr tail)))
                expr)]
           [(eq? head 'define)
            (cond
              [(and (pair? tail) (symbol? (car tail))
                    (pair? (cdr tail)) (null? (cddr tail)))
               (list 'define (car tail) (expand-expr (cadr tail)))]
              [(and (pair? tail) (pair? (car tail)) (symbol? (caar tail)))
               (cons 'define
                     (cons (car tail) (map expand-expr (cdr tail))))]
              [else expr])]
           [else (map expand-expr expr)])]
        [else expr]))

    (define (expand-program exprs)
      (map expand-expr exprs))

    (define (single-arg who form)
      (define rest (cdr form))
      (unless (and (pair? rest) (null? (cdr rest)))
        (error 'minischeme "~a: malformed form: ~s" who form))
      (car rest))

    (define (cek-evaluate expr env)
      (let loop ([mode        'eval]
                 [control     expr]
                 [current-env env]
                 [kont        '()])
        (define (continue mode control env kont)
          (loop mode control env kont))
        (define (eval-now e env)
          (call-with-values
           (λ () (cek-evaluate e env))
           (λ (v _env) v)))
        (define (apply-now proc args env kont)
          (apply-procedure proc args env kont continue))
        (define (eval-sequence forms env kont)
          (if (null? forms)
              (continue 'value (void) env kont)
              (continue 'eval (car forms) env
                        (cons (k-begin (cdr forms) env) kont))))

        (define (qq-general datum depth)
          (cond
            [(pair? datum)
             (define head (car datum))
             (cond
               [(and (symbol? head) (eq? head 'quasiquote))
                (define arg (single-arg 'quasiquote datum))
                (list 'quasiquote (qq-general arg (add1 depth)))]
               [(and (symbol? head) (eq? head 'unquote))
                (define arg (single-arg 'unquote datum))
                (if (= depth 1)
                    (eval-now arg current-env)
                    (list 'unquote (qq-general arg (sub1 depth))))]
               [(and (symbol? head) (eq? head 'unquote-splicing))
                (define arg (single-arg 'unquote-splicing datum))
                (if (= depth 1)
                    (error 'minischeme "unquote-splicing outside list/vector context: ~s" datum)
                    (list 'unquote-splicing (qq-general arg (sub1 depth))))]
               [else
                (qq-list datum depth)])]
            [(vector? datum)
             (qq-vector datum depth)]
            [else
             datum]))

        (define (qq-item datum depth)
          (if (and (pair? datum)
                   (symbol? (car datum))
                   (eq? (car datum) 'unquote-splicing))
              (let ([arg (single-arg 'unquote-splicing datum)])
                (if (= depth 1)
                    (values #t (eval-now arg current-env))
                    (values #f (list 'unquote-splicing
                                     (qq-general arg (sub1 depth))))))
              (values #f (qq-general datum depth))))

        (define (qq-list datum depth)
          (cond
            [(null? datum) '()]
            [(pair? datum)
             (define-values (splice? v) (qq-item (car datum) depth))
             (define rest (qq-list (cdr datum) depth))
             (if splice?
                 (begin
                   (unless (list? v)
                     (error 'minischeme "unquote-splicing expects a list in list context: ~s" v))
                   (append v rest))
                 (cons v rest))]
            [else
             (qq-general datum depth)]))

        (define (qq-vector vec depth)
          (let loopv ([xs (vector->list vec)] [acc '()])
            (if (null? xs)
                (list->vector (reverse acc))
                (let-values ([(splice? v) (qq-item (car xs) depth)])
                  (if splice?
                      (begin
                        (unless (list? v)
                          (error 'minischeme "unquote-splicing expects a list in vector context: ~s" v))
                        (loopv (cdr xs) (append (reverse v) acc)))
                      (loopv (cdr xs) (cons v acc)))))))

        (cond
          [(eq? mode 'eval)
           (cond
             [(literal? control)
              (continue 'value control current-env kont)]
             [(symbol? control)
              (continue 'value (env-lookup current-env control) current-env kont)]
             [(and (pair? control) (eq? (car control) 'quote))
             (let ([rest (cdr control)])
                (if (and (pair? rest) (null? (cdr rest)))
                    (continue 'value (car rest) current-env kont)
                    (error 'minischeme "malformed quote: ~s" control)))]
             [(and (pair? control) (eq? (car control) 'quasiquote))
              (continue 'value (qq-general (single-arg 'quasiquote control) 1)
                        current-env kont)]
             [(and (pair? control) (eq? (car control) 'unquote))
              (error 'minischeme "unquote outside quasiquote: ~s" control)]
             [(and (pair? control) (eq? (car control) 'unquote-splicing))
              (error 'minischeme "unquote-splicing outside quasiquote: ~s" control)]
             [(and (pair? control) (eq? (car control) 'lambda))
              (let ([rest (cdr control)])
                (if (and (pair? rest))
                    (let ([params (car rest)]
                          [body (cdr rest)])
                      (continue 'value (closure (ensure-parameters params)
                                                body current-env)
                                current-env kont))
                    (error 'minischeme "malformed lambda: ~s" control)))]
             [(and (pair? control) (eq? (car control) 'if))
              (let* ([rest (cdr control)]
                     [len (length rest)])
                (cond
                  [(or (< len 2) (> len 3))
                   (error 'minischeme "malformed if: ~s" control)]
                  [else
                   (define test (car rest))
                   (define then (cadr rest))
                   (define else-expr (if (= len 3) (caddr rest) no-else))
                   (continue 'eval test current-env
                             (cons (k-if then else-expr current-env) kont))]))]
             [(and (pair? control) (eq? (car control) 'begin))
              (eval-sequence (cdr control) current-env kont)]
             [(and (pair? control) (eq? (car control) 'set!))
              (let ([rest (cdr control)])
                (if (and (pair? rest)
                         (symbol? (car rest))
                         (pair? (cdr rest))
                         (null? (cddr rest)))
                    (let ([name (car rest)]
                          [rhs (cadr rest)])
                      (define cell (env-lookup-cell current-env name))
                      (continue 'eval rhs current-env
                                (cons (k-set! cell name) kont)))
                    (error 'minischeme "malformed set!: ~s" control)))]
             [(and (pair? control) (eq? (car control) 'define))
              (let ([rest (cdr control)])
                (cond
                  [(and (pair? rest) (symbol? (car rest)))
                   (let ([name (car rest)]
                         [tail (cdr rest)])
                     (unless (and (pair? tail) (null? (cdr tail)))
                       (error 'minischeme "malformed define: ~s" control))
                     (define cell
                       (if (env-bound-current? current-env name)
                           (hash-ref (env-table current-env) name)
                           (let ([c (box uninitialized)])
                             (env-define-cell! current-env name c)
                             c)))
                     (set-box! cell uninitialized)
                     (continue 'eval (car tail) current-env
                               (cons (k-define cell name) kont)))]
                  [(and (pair? rest)
                        (pair? (car rest))
                        (symbol? (caar rest)))
                   (let* ([head (car rest)]
                          [name (car head)]
                          [params (cdr head)]
                          [body (cdr rest)])
                     (define cell
                       (if (env-bound-current? current-env name)
                           (hash-ref (env-table current-env) name)
                           (let ([c (box uninitialized)])
                             (env-define-cell! current-env name c)
                             c)))
                     (set-box! cell uninitialized)
                     (define lambda-expr (cons 'lambda (cons params body)))
                     (continue 'eval lambda-expr current-env
                               (cons (k-define cell name) kont)))]
                  [else (error 'minischeme "malformed define: ~s" control)]))]
             [(and (pair? control) (eq? (car control) 'let))
              (let ([rest (cdr control)])
                (if (and (pair? rest))
                    (let ([bindings (car rest)]
                          [body (cdr rest)])
                      (continue 'eval (desugar-let bindings body)
                                current-env kont))
                    (error 'minischeme "malformed let: ~s" control)))]
             [(and (pair? control) (eq? (car control) 'letrec))
              (let ([rest (cdr control)])
                (if (and (pair? rest))
                    (let ([bindings (car rest)]
                          [body (cdr rest)])
                      (unless (list? bindings)
                        (error 'minischeme "malformed letrec: ~s" control))
                      (define new-env (make-env current-env))
                      (define cells '())
                      (define rhss '())
                      (for-each
                       (λ (binding)
                         (unless (and (pair? binding)
                                      (symbol? (car binding))
                                      (pair? (cdr binding))
                                      (null? (cddr binding)))
                           (error 'minischeme "malformed letrec binding: ~s" binding))
                         (define cell (box uninitialized))
                         (env-define-cell! new-env (car binding) cell)
                         (set! cells (cons cell cells))
                         (set! rhss (cons (cadr binding) rhss)))
                       bindings)
                      (let loopr ([cs (reverse cells)] [es (reverse rhss)])
                        (unless (null? cs)
                          (set-box! (car cs) (eval-now (car es) new-env))
                          (loopr (cdr cs) (cdr es))))
                      (eval-sequence body new-env kont))
                    (error 'minischeme "malformed letrec: ~s" control)))]
             [(pair? control)
              (define op (car control))
              (define args (cdr control))
              (continue 'eval op current-env
                        (cons (k-apply args current-env) kont))]
             [else
              (error 'minischeme "cannot evaluate expression ~a" control)])]
          [(eq? mode 'value)
           (if (null? kont)
               (values control current-env)
               (let* ([frame (car kont)]
                      [rest (cdr kont)])
                 (cond
                   [(k-apply? frame)
                    (define args (k-apply-args frame))
                    (define call-env (k-apply-env frame))
                    (if (null? args)
                        (apply-now control '() call-env rest)
                        (continue 'eval (car args) call-env
                                  (cons (k-args control (cdr args) call-env '())
                                        rest)))]
                   [(k-args? frame)
                    (define proc (k-args-proc frame))
                    (define rest-args (k-args-rest frame))
                    (define call-env (k-args-env frame))
                    (define collected (cons control (k-args-values frame)))
                    (if (null? rest-args)
                        (apply-now proc (reverse collected) call-env rest)
                        (continue 'eval (car rest-args) call-env
                                  (cons (k-args proc (cdr rest-args)
                                                call-env collected) rest)))]
                   [(k-if? frame)
                    (define branch-env (k-if-env frame))
                    (define then (k-if-then frame))
                    (define else-expr (k-if-else frame))
                    (if (false? control)
                        (if (eq? else-expr no-else)
                            (continue 'value (void) branch-env rest)
                            (continue 'eval else-expr branch-env rest))
                        (continue 'eval then branch-env rest))]
                   [(k-begin? frame)
                    (define begin-env (k-begin-env frame))
                    (define rest-forms (k-begin-rest frame))
                    (if (null? rest-forms)
                        (continue 'value control begin-env rest)
                        (continue 'eval (car rest-forms) begin-env
                                  (cons (k-begin (cdr rest-forms) begin-env) rest)))]
                   [(k-set!? frame)
                    (set-box! (k-set!-cell frame) control)
                    (continue 'value control current-env rest)]
                   [(k-define? frame)
                    (set-box! (k-define-cell frame) control)
                    (continue 'value (k-define-name frame) current-env rest)]
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
             (λ () (cek-evaluate (car forms) global-env))
             (λ (value _env)
               (loop (cdr forms) value))))))

    (define (process-input s)
      (unless global-env
        (reset-state!))
      (define-values (exprs read-error)
        (with-handlers
          ([exn:fail:read?
            (λ (e) (values #f (string-append "=> read error: " (exn-message e))))]
           [(λ _ #t)
            (λ (e) (values #f (string-append "=> read error: " (exn-message e))))])
          (values (parse-program/read s) #f)))
      (if read-error
          read-error
          (with-handlers
            ([exn:fail? (λ (e) (string-append "=> " (exn-message e)))])
            (define expanded (expand-program exprs))
            (if (null? expanded)
                "=> ; no input"
                (let ([value (evaluate-program expanded)])
                  (string-append "=> " (value->string value)))))))

    (reset-state!)
    (values reset-state! process-input)))
