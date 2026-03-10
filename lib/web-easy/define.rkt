#lang webracket

(require (for-syntax racket/base racket/list))

;;;
;;; web-easy define/key
;;;

;; Define-like macro for functions that accept optional keyword-style arguments.
;;
;; Exports:
;;   define/key   Define a function with required positional args and optional keyword args.

;; syntax-list : syntax? symbol? -> (listof syntax?)
;;   Convert stx to a proper syntax list or raise a syntax error.
(define-for-syntax (syntax-list stx who)
  (let ([xs (syntax->list stx)])
    (unless xs
      (raise-syntax-error who "expected a proper list" stx))
    xs))

;; parse-keyword-spec : syntax? syntax? symbol? -> (values syntax? syntax?)
;;   Parse one keyword clause body `[id default-expr]`.
(define-for-syntax (parse-keyword-spec spec-stx keyword-stx who)
  (let* ([spec-list (syntax-list spec-stx who)]
         [id-stx    (and (pair? spec-list) (car spec-list))])
    (unless (= (length spec-list) 2)
      (raise-syntax-error
       who
       "expected keyword clause shape `#:kw [id default-expr]`"
       spec-stx
       keyword-stx))
    (unless (identifier? id-stx)
      (raise-syntax-error
       who
       "expected identifier in keyword clause"
       spec-stx
       id-stx))
    (values (car spec-list) (cadr spec-list))))

;; parse-formals : syntax? symbol? -> (values syntax? (listof syntax?) (listof keyword?) (listof syntax?) (listof syntax?))
;;   Parse `(name req ... #:kw [kw-id default] ...)`.
(define-for-syntax (parse-formals formals-stx who)
  (let* ([formals-list (syntax-list formals-stx who)])
    (unless (pair? formals-list)
      (raise-syntax-error who "expected `(name ...)`" formals-stx))
    (let ([name-stx (car formals-list)])
      (unless (identifier? name-stx)
        (raise-syntax-error who "expected function name identifier" formals-stx name-stx))
      (let loop ([xs (cdr formals-list)]
                 [seen-keyword? #f]
                 [req-ids '()]
                 [kw-datums '()]
                 [kw-ids '()]
                 [kw-default-exprs '()])
        (cond
          [(null? xs)
           (values name-stx
                   (reverse req-ids)
                   (reverse kw-datums)
                   (reverse kw-ids)
                   (reverse kw-default-exprs))]
          [else
           (let* ([hd (car xs)]
                  [hd-datum (syntax-e hd)])
             (cond
               [(keyword? hd-datum)
                (unless (pair? (cdr xs))
                  (raise-syntax-error
                   who
                   "missing keyword clause after keyword"
                   formals-stx
                   hd))
                (when (ormap (lambda (k) (eq? k hd-datum)) kw-datums)
                  (raise-syntax-error
                   who
                   "duplicate keyword in define/key header"
                   formals-stx
                   hd))
                (let-values ([(kw-id kw-default-expr)
                              (parse-keyword-spec (cadr xs) hd who)])
                  (loop (cddr xs)
                        #t
                        req-ids
                        (cons hd-datum kw-datums)
                        (cons kw-id kw-ids)
                        (cons kw-default-expr kw-default-exprs)))]
               [else
                (when seen-keyword?
                  (raise-syntax-error
                   who
                   "required positional arguments must appear before keyword clauses"
                   formals-stx
                   hd))
                (unless (identifier? hd)
                  (raise-syntax-error
                   who
                   "expected positional argument identifier or keyword"
                   formals-stx
                   hd))
                (loop (cdr xs)
                      #f
                      (cons hd req-ids)
                      kw-datums
                      kw-ids
                      kw-default-exprs)]))])))))

;; split-required : (listof syntax?) natural syntax? symbol? -> (values (listof syntax?) (listof syntax?))
;;   Split args into required positional prefix and trailing args.
(define-for-syntax (split-required args n use-stx who)
  (let loop ([xs args] [remaining n] [acc '()])
    (cond
      [(zero? remaining)
       (values (reverse acc) xs)]
      [(null? xs)
       (raise-syntax-error who "wrong number of positional arguments" use-stx)]
      [else
       (loop (cdr xs) (sub1 remaining) (cons (car xs) acc))])))

;; last-stx : (listof syntax?) -> syntax?
;;   Return the last syntax object in xs.
(define-for-syntax (last-stx xs)
  (if (null? (cdr xs))
      (car xs)
      (last-stx (cdr xs))))

;; keyword-index : keyword? (listof keyword?) -> (or/c false? natural)
;;   Return zero-based index of kw in allowed, or #f when missing.
(define-for-syntax (keyword-index kw allowed)
  (let loop ([kws allowed] [i 0])
    (cond
      [(null? kws) #f]
      [(eq? kw (car kws)) i]
      [else (loop (cdr kws) (add1 i))])))

;; build-keyword-bindings : (vectorof (or/c false? syntax?)) (listof syntax?) (listof syntax?) -> (listof syntax?)
;;   Build `let*` bindings for keyword temp variables.
(define-for-syntax (build-keyword-bindings provided-values keyword-tmps kw-default-exprs)
  (let loop ([i 0]
             [tmp keyword-tmps]
             [defaults kw-default-exprs]
             [acc '()])
    (cond
      [(null? tmp)
       (reverse acc)]
      [else
       (let* ([provided (vector-ref provided-values i)]
              [rhs (if provided provided (car defaults))])
         (loop (add1 i)
               (cdr tmp)
               (cdr defaults)
               (cons #`[#,(car tmp) #,rhs] acc)))])))

;; expand-define/key-use : symbol? syntax? natural (listof keyword?) (listof syntax?) (listof syntax?) syntax? -> syntax?
;;   Expand one call use of a function defined with `define/key`.
(define-for-syntax (expand-define/key-use who use-stx req-count allowed kw-id-stxes kw-default-exprs core-stx)
  (let* ([all-args (syntax-list (syntax-case use-stx () [(_ arg ...) #'(arg ...)]) who)])
    (let-values ([(positional-args trailing-args)
                  (split-required all-args req-count use-stx who)])
      (unless (even? (length trailing-args))
        (raise-syntax-error
         who
         "expected keyword/value pairs after positional arguments"
         use-stx
         (last-stx trailing-args)))
      (let* ([provided-values (make-vector (length allowed) #f)]
             [seen? (make-vector (length allowed) #f)])
        (let parse-pairs ([xs trailing-args])
          (unless (null? xs)
            (let* ([kw-stx (car xs)]
                   [val-stx (cadr xs)]
                   [kw-datum (syntax-e kw-stx)]
                   [idx (and (keyword? kw-datum)
                             (keyword-index kw-datum allowed))])
              (unless (keyword? kw-datum)
                (raise-syntax-error who "expected keyword" use-stx kw-stx))
              (unless idx
                (raise-syntax-error who "unknown keyword" use-stx kw-stx))
              (when (vector-ref seen? idx)
                (raise-syntax-error who "duplicate keyword argument" use-stx kw-stx))
              (vector-set! seen? idx #t)
              (vector-set! provided-values idx val-stx)
              (parse-pairs (cddr xs)))))
        (let* ([positional-tmps (generate-temporaries positional-args)]
               [keyword-tmps (generate-temporaries kw-id-stxes)]
               [keyword-bindings (build-keyword-bindings
                                  provided-values
                                  keyword-tmps
                                  kw-default-exprs)])
          (with-syntax ([(pos-tmp ...) positional-tmps]
                        [(kw-tmp ...) keyword-tmps]
                        [(pos-arg ...) positional-args]
                        [(kw-binding ...) keyword-bindings]
                        [core core-stx])
            #`(let* ([pos-tmp pos-arg] ...
                     kw-binding ...)
                (core pos-tmp ... kw-tmp ...))))))))

;; define/key : (define/key (name req ... #:kw [kw-id default] ...) body ...+) -> definition
;;   Define a function with optional keyword-style arguments and call-site rewriting.
(define-syntax (define/key stx)
  (let ([who 'define/key])
    (syntax-case stx ()
      [(_ formals body0 body ...)
       (let-values ([(name-stx req-ids kw-datums kw-ids kw-default-exprs)
                     (parse-formals #'formals who)])
         (let* ([core+proc-ids (generate-temporaries (list name-stx name-stx))]
                [core-id (car core+proc-ids)]
                [proc-id (cadr core+proc-ids)]
                [req-count (length req-ids)])
           (with-syntax ([name name-stx]
                         [name-sym (datum->syntax stx (syntax-e name-stx))]
                         [core core-id]
                         [proc proc-id]
                         [(req ...) req-ids]
                         [(kw-id ...) kw-ids]
                         [req-count-datum req-count]
                         [allowed-keywords (datum->syntax stx kw-datums)]
                         [kw-default-exprs-stx #`(#,@kw-default-exprs)])
             #`(begin
                 (define (core req ... kw-id ...)
                   body0
                   body ...)

                 (define (proc req ...)
                   (core req ... #,@kw-default-exprs))

                 (define-syntax (name use-stx)
                   (syntax-case use-stx ()
                     [id
                      (identifier? #'id)
                      #'proc]
                     [(_ . call-args)
                      (expand-define/key-use
                       'name-sym
                       use-stx
                       req-count-datum
                       (syntax->datum #'allowed-keywords)
                       (syntax->list #'(kw-id ...))
                       (syntax->list #'kw-default-exprs-stx)
                       #'core)]
                     [_
                      (raise-syntax-error 'name-sym "bad use of define/key function" use-stx)]))))))]
      [_
       (raise-syntax-error who "expected `(define/key (name ...) body ...+)`" stx)])))
