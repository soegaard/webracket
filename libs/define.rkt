#lang racket/base

(require (for-syntax racket/base racket/list))
(provide define/key call/key)

;;;
;;; webracket define/key
;;;

;; Define-like macro support for positional and keyword-style arguments.
;;
;; Exports:
;;   define/key   Define a function with required/optional positional args,
;;                optional rest arg, and required/optional keyword args.
;;   call/key     Call a procedure using keyword-call syntax in contexts where
;;                the callee is not a syntax transformer (for example aliases).

;; syntax-list : syntax? symbol? -> (listof syntax?)
;;   Convert stx to a proper syntax list or raise a syntax error.
(define-for-syntax (syntax-list stx who)
  (define xs (syntax->list stx))
  (unless xs
    (raise-syntax-error who "expected a proper list" stx))
  xs)

;; split-formals-list : syntax? symbol? -> (values (listof syntax?) (or/c #f syntax?))
;;   Split possibly dotted formals into element list and optional rest-id.
(define-for-syntax (split-formals-list formals-stx who)
  (let loop ([cur formals-stx] [acc '()])
    (syntax-case cur ()
      [()
       (values (reverse acc) #f)]
      [(a . d)
       (if (identifier? #'d)
           (values (reverse (cons #'a acc)) #'d)
           (loop #'d (cons #'a acc)))]
      [_
       (raise-syntax-error who "expected a list of formals" formals-stx)])))

;; parse-opt-positional : syntax? symbol? -> (values syntax? syntax?)
;;   Parse optional positional clause `[id default-expr]`.
(define-for-syntax (parse-opt-positional clause-stx who)
  (define xs (syntax-list clause-stx who))
  (unless (= (length xs) 2)
    (raise-syntax-error who "expected optional positional clause `[id default-expr]`" clause-stx))
  (unless (identifier? (car xs))
    (raise-syntax-error who "expected identifier in optional positional clause" clause-stx (car xs)))
  (values (car xs) (cadr xs)))

;; parse-keyword-clause : syntax? syntax? symbol? -> (values syntax? boolean? (or/c #f syntax?))
;;   Parse keyword clause body: `id` (required) or `[id default]` (optional).
(define-for-syntax (parse-keyword-clause clause-stx kw-stx who)
  (cond
    [(identifier? clause-stx)
     (values clause-stx #t #f)]
    [else
     (define xs (syntax-list clause-stx who))
     (unless (= (length xs) 2)
       (raise-syntax-error
        who
        "expected keyword clause shape `#:kw id` or `#:kw [id default-expr]`"
        clause-stx
        kw-stx))
     (unless (identifier? (car xs))
       (raise-syntax-error
        who
        "expected identifier in keyword clause"
        clause-stx
        (car xs)))
     (values (car xs) #f (cadr xs))]))

;; parse-formals : syntax? symbol? ->
;;   (values syntax? (listof syntax?) (listof syntax?) (listof syntax?)
;;           (or/c #f syntax?)
;;           (listof keyword?) (listof syntax?) (listof boolean?)
;;           (listof (or/c #f syntax?)))
;;   Parse `(name formals ...)` for define/key.
(define-for-syntax (parse-formals formals-stx who)
  (let-values ([(parts maybe-rest-id)
                (split-formals-list formals-stx who)])
    (unless (pair? parts)
      (raise-syntax-error who "expected `(name ...)`" formals-stx))
    (define name-stx (car parts))
    (unless (identifier? name-stx)
      (raise-syntax-error who "expected function name identifier" formals-stx name-stx))
    (let loop ((xs (cdr parts))
               (seen-opt-pos? #f)
               (seen-keyword? #f)
               (req-ids '())
               (opt-ids '())
               (opt-defaults '())
               (kw-datums '())
               (kw-ids '())
               (kw-required? '())
               (kw-defaults '()))
      (cond
        ((null? xs)
         (values name-stx
                 (reverse req-ids)
                 (reverse opt-ids)
                 (reverse opt-defaults)
                 maybe-rest-id
                 (reverse kw-datums)
                 (reverse kw-ids)
                 (reverse kw-required?)
                 (reverse kw-defaults)))
        (else
         (let ((hd (car xs))
               (hd-datum (syntax-e (car xs))))
           (cond
             ((keyword? hd-datum)
              (unless (pair? (cdr xs))
                (raise-syntax-error who "missing keyword clause after keyword" formals-stx hd))
              (when (ormap (lambda (k) (eq? k hd-datum)) kw-datums)
                (raise-syntax-error who "duplicate keyword in define/key header" formals-stx hd))
              (let-values (((kw-id req? kw-default)
                            (parse-keyword-clause (cadr xs) hd who)))
                (loop (cddr xs)
                      seen-opt-pos?
                      #t
                      req-ids
                      opt-ids
                      opt-defaults
                      (cons hd-datum kw-datums)
                      (cons kw-id kw-ids)
                      (cons req? kw-required?)
                      (cons kw-default kw-defaults))))
             (seen-keyword?
              (raise-syntax-error
               who
               "positional formals must appear before keyword clauses"
               formals-stx
               hd))
             ((identifier? hd)
              (if seen-opt-pos?
                  (raise-syntax-error
                   who
                   "required positional argument cannot appear after optional positional argument"
                   formals-stx
                   hd)
                  (loop (cdr xs)
                        #f
                        #f
                        (cons hd req-ids)
                        opt-ids
                        opt-defaults
                        kw-datums
                        kw-ids
                        kw-required?
                        kw-defaults)))
             (else
              (let-values (((opt-id opt-default)
                            (parse-opt-positional hd who)))
                (loop (cdr xs)
                      #t
                      #f
                      req-ids
                      (cons opt-id opt-ids)
                      (cons opt-default opt-defaults)
                      kw-datums
                      kw-ids
                      kw-required?
                      kw-defaults))))))))))

;; kw-token->expr : syntax? -> syntax?
;;   Convert keyword token syntax to quoted keyword expression.
(define-for-syntax (kw-token->expr kw-stx)
  (datum->syntax kw-stx (list 'quote (syntax-e kw-stx)) kw-stx kw-stx))

;; rewrite-call-args : syntax? symbol? -> (listof syntax?)
;;   Rewrite call arguments so keyword tokens become quoted keyword values.
(define-for-syntax (rewrite-call-args args-stx who)
  (map (lambda (a)
         (if (keyword? (syntax-e a))
             (kw-token->expr a)
             a))
       (syntax-list args-stx who)))

;; split-call-args : (listof syntax?) -> (values (listof syntax?) (listof syntax?))
;;   Split at first keyword token into positional prefix and keyword tail.
(define-for-syntax (split-call-args args)
  (let loop ([xs args] [pos '()])
    (cond
      [(null? xs)
       (values (reverse pos) '())]
      [(keyword? (syntax-e (car xs)))
       (values (reverse pos) xs)]
      [else
       (loop (cdr xs) (cons (car xs) pos))])))

;; list-last : (listof syntax?) -> syntax?
;;   Return the final syntax element.
(define-for-syntax (list-last xs)
  (if (null? (cdr xs))
      (car xs)
      (list-last (cdr xs))))

;; keyword-index : keyword? (listof keyword?) -> (or/c #f natural?)
;;   Return zero-based index of kw in allowed, or #f.
(define-for-syntax (keyword-index kw allowed)
  (let loop ([kws allowed] [i 0])
    (cond
      [(null? kws) #f]
      [(eq? kw (car kws)) i]
      [else (loop (cdr kws) (add1 i))])))

;; validate-keyword-tail-shape! : symbol? syntax? syntax? -> void?
;;   Validate keyword/value pair surface syntax.
(define-for-syntax (validate-keyword-tail-shape! who use-stx args-stx)
  (define args (syntax-list args-stx who))
  (let-values ([(pos kw-tail)
                (split-call-args args)])
    (void pos)
    (unless (even? (length kw-tail))
      (raise-syntax-error
       who
       "expected keyword/value pairs after positional arguments"
       use-stx
       (list-last kw-tail)))
    (let loop ([xs kw-tail])
      (unless (null? xs)
        (define kw-stx (car xs))
        (unless (keyword? (syntax-e kw-stx))
          (raise-syntax-error who "expected keyword" use-stx kw-stx))
        (loop (cddr xs))))))

;; validate-define/key-call! : symbol? syntax? syntax? natural? natural? boolean? (listof keyword?) (listof boolean?) -> void?
;;   Validate a direct `(name ... #:kw ...)` call at expansion time.
(define-for-syntax (validate-define/key-call! who use-stx args-stx
                                              req-count opt-count has-rest?
                                              kw-datums kw-required?)
  (define args (syntax-list args-stx who))
  (let-values ([(pos kw-tail)
                (split-call-args args)])
    (define pos-count (length pos))
    (when (< pos-count req-count)
      (raise-syntax-error who "wrong number of positional arguments" use-stx))
    (when (and (not has-rest?)
               (> pos-count (+ req-count opt-count)))
      (raise-syntax-error who "wrong number of positional arguments" use-stx))

    (unless (even? (length kw-tail))
      (raise-syntax-error
       who
       "expected keyword/value pairs after positional arguments"
       use-stx
       (list-last kw-tail)))

    (define seen (make-vector (length kw-datums) #f))
    (let loop ([xs kw-tail])
      (unless (null? xs)
        (define kw-stx (car xs))
        (define kw-datum (syntax-e kw-stx))
        (unless (keyword? kw-datum)
          (raise-syntax-error who "expected keyword" use-stx kw-stx))
        (define idx (keyword-index kw-datum kw-datums))
        (unless idx
          (raise-syntax-error who "unknown keyword" use-stx kw-stx))
        (when (vector-ref seen idx)
          (raise-syntax-error who "duplicate keyword argument" use-stx kw-stx))
        (vector-set! seen idx #t)
        (loop (cddr xs))))

    (let check ([i 0] [flags kw-required?] [kws kw-datums])
      (unless (null? flags)
        (when (and (car flags) (not (vector-ref seen i)))
          (raise-syntax-error
           who
           "missing required keyword argument"
           use-stx
           (datum->syntax use-stx (car kws))))
        (check (add1 i) (cdr flags) (cdr kws))))))

;; call/key : (call/key f arg ... [#:kw v] ...) -> call
;;   Rewrite keyword tokens to keyword values for first-class procedure calls.
(define-syntax (call/key stx)
  (define who 'call/key)
  (syntax-case stx ()
    [(_ f . args)
     (begin
       (validate-keyword-tail-shape! who stx #'args)
       (datum->syntax stx
                      (cons #'f (rewrite-call-args #'args who))
                      stx
                      stx))]
    [_
     (raise-syntax-error who "expected `(call/key f arg ... [#:kw v] ...)`" stx)]))

;; define/key : (define/key (name formals ...) body ...+) -> definition
;;   Define a function with positional and keyword-style formals.
(define-syntax (define/key stx)
  (define who 'define/key)
  (syntax-case stx ()
    [(_ formals body0 body ...)
     (let-values ([(name-stx req-ids opt-ids opt-defaults rest-id-stx
                             kw-datums kw-ids kw-required? kw-defaults)
                   (parse-formals #'formals who)])
       (define proc-id (car (generate-temporaries (list name-stx))))
       (define req-count (length req-ids))
       (define opt-count (length opt-ids))
       (define has-rest? (if rest-id-stx #t #f))

       (define req-bindings
         (let loop ([i 0] [ids req-ids] [acc '()])
           (if (null? ids)
               (reverse acc)
               (loop (add1 i)
                     (cdr ids)
                     (cons #`[#,(car ids) (list-ref pos-values #,i)] acc)))))

       (define opt-bindings
         (let loop ([j 0] [ids opt-ids] [defs opt-defaults] [acc '()])
           (if (null? ids)
               (reverse acc)
               (let ([idx (+ req-count j)])
                 (loop (add1 j)
                       (cdr ids)
                       (cdr defs)
                       (cons #`[#,(car ids)
                                (if (< #,idx pos-count)
                                    (list-ref pos-values #,idx)
                                    #,(car defs))]
                             acc))))))

       (define rest-binding
         (if rest-id-stx
             (list #`[#,rest-id-stx (drop-at-most pos-values #,(+ req-count opt-count))])
             '()))

       (define kw-bindings
         (let loop ([i 0] [ids kw-ids] [req? kw-required?] [defs kw-defaults] [acc '()])
           (if (null? ids)
               (reverse acc)
               (let ([provided #`(vector-ref provided-kw-values #,i)])
                 (loop (add1 i)
                       (cdr ids)
                       (cdr req?)
                       (cdr defs)
                       (cons #`[#,(car ids)
                                #,(if (car req?)
                                      #`(if (vector-ref seen-kw? #,i)
                                            #,provided
                                            (error 'name-sym
                                                   "missing required keyword argument: ~a"
                                                   '#,(list-ref kw-datums i)))
                                      #`(if (vector-ref seen-kw? #,i)
                                            #,provided
                                            #,(car defs)))]
                             acc))))))

       (with-syntax ([name name-stx]
                     [name-sym (datum->syntax stx (syntax-e name-stx))]
                     [proc proc-id]
                     [req-count-datum req-count]
                     [opt-count-datum opt-count]
                     [has-rest?-datum has-rest?]
                     [max-no-rest (+ req-count opt-count)]
                     [(req-bind ...) req-bindings]
                     [(opt-bind ...) opt-bindings]
                     [(rest-bind ...) rest-binding]
                     [(kw-bind ...) kw-bindings]
                     [kw-count (length kw-datums)]
                     [kw-datums-quoted (datum->syntax stx kw-datums)])
         #`(begin
             (define (proc . all-args)
               (define (split-positional+keyword xs)
                 (let loop ([rest xs] [pos '()])
                   (cond
                     [(null? rest)
                      (values (reverse pos) '())]
                     [(keyword? (car rest))
                      (values (reverse pos) rest)]
                     [else
                      (loop (cdr rest) (cons (car rest) pos))])))

               (define (keyword-index/runtime kw allowed)
                 (let loop ([kws allowed] [i 0])
                   (cond
                     [(null? kws) #f]
                     [(eq? kw (car kws)) i]
                     [else (loop (cdr kws) (add1 i))])))

               (define (drop-at-most xs n)
                 (let loop ([ys xs] [k n])
                   (cond
                     [(zero? k) ys]
                     [(null? ys) '()]
                     [else (loop (cdr ys) (sub1 k))])))

               (define-values (pos-values kw-tail)
                 (split-positional+keyword all-args))
               (define pos-count (length pos-values))

               (when (< pos-count req-count-datum)
                 (if has-rest?-datum
                     (error 'name-sym
                            "wrong number of positional arguments (expected at least ~a, got ~a)"
                            req-count-datum
                            pos-count)
                     (error 'name-sym
                            "wrong number of positional arguments (expected ~a to ~a, got ~a)"
                            req-count-datum
                            max-no-rest
                            pos-count)))

               (when (and (not has-rest?-datum)
                          (> pos-count max-no-rest))
                 (error 'name-sym
                        "wrong number of positional arguments (expected ~a to ~a, got ~a)"
                        req-count-datum
                        max-no-rest
                        pos-count))

               (when (odd? (length kw-tail))
                 (error 'name-sym "expected keyword/value pairs after positional arguments"))

               (define provided-kw-values (make-vector kw-count #f))
               (define seen-kw?           (make-vector kw-count #f))

               (let parse-tail ([xs kw-tail])
                 (unless (null? xs)
                   (define kw (car xs))
                   (define v  (cadr xs))
                   (unless (keyword? kw)
                     (error 'name-sym "expected keyword, got ~a" kw))
                   (define idx (keyword-index/runtime kw 'kw-datums-quoted))
                   (unless idx
                     (error 'name-sym "unknown keyword argument: ~a" kw))
                   (when (vector-ref seen-kw? idx)
                     (error 'name-sym "duplicate keyword argument: ~a" kw))
                   (vector-set! seen-kw? idx #t)
                   (vector-set! provided-kw-values idx v)
                   (parse-tail (cddr xs))))

               (let* (req-bind ...
                      opt-bind ...
                      rest-bind ...
                      kw-bind ...)
                 body0
                 body ...))

             (define-syntax (name use-stx)
               (syntax-case use-stx ()
                 [id
                  (identifier? #'id)
                  #'proc]
                 [(_ . args)
                  (begin
                    (validate-define/key-call! 'name-sym
                                               use-stx
                                               #'args
                                               req-count-datum
                                               opt-count-datum
                                               has-rest?-datum
                                               'kw-datums-quoted
                                               '#,kw-required?)
                    (datum->syntax use-stx
                                   (cons #'proc
                                         (rewrite-call-args #'args 'name-sym))
                                   use-stx
                                   use-stx))]
                 [_
                  (raise-syntax-error 'name-sym "bad use of define/key function" use-stx)])))))]
    [_
     (raise-syntax-error who "expected `(define/key (name ...) body ...+)`" stx)]))
