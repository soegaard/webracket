#lang webracket

;;; These primitives need to be added.

(define (procedure-keywords proc) 'todo)

(define (format x) "formatted")

; arity-at-least structure
(define (arity-at-least? x)        'todo)
(define (arity-at-least       . x) 'todo)
(define (arity-at-least-value . x) 'todo)

;;; Code proper

(define-values (struct:keyword-procedure mk-kw-proc keyword-procedure?
                                         keyword-procedure-ref keyword-procedure-set!)
  (make-struct-type 'keyword-procedure #f 4 0 #f
                    (list #;(cons prop:checked-procedure #t)
                          #;(cons prop:impersonator-of keyword-procedure-impersonator-of))
                    (current-inspector)
                    #f
                    '(0 1 2 3)))
(define keyword-procedure-checker  (make-struct-field-accessor keyword-procedure-ref 0))
(define keyword-procedure-proc     (make-struct-field-accessor keyword-procedure-ref 1))
(define keyword-procedure-required (make-struct-field-accessor keyword-procedure-ref 2))
(define keyword-procedure-allowed  (make-struct-field-accessor keyword-procedure-ref 3))

(define-values (struct:keyword-method make-km keyword-method? km-ref km-set!)
  (make-struct-type 'procedure
                    struct:keyword-procedure
                    0 0 #f
                    (list (cons prop:method-arity-error #t))))

#;(define (fmt v)
    ((error-syntax->string-handler) v #f))

(define (fmt v)
  "todo - implement fmt from `kw.rtk` properly")

(define (generate-arity-string proc)
  (let-values ([(req allowed) (procedure-keywords proc)]
               [(a)           (procedure-arity proc)]
               [(keywords-desc)
                (lambda (opt req)
                  (format "~a with keyword~a~a"
                          (if (null? (cdr req))
                              (format "an ~aargument" opt)
                              (format "~aarguments" opt))
                          (if (null? (cdr req))
                              ""
                              "s")
                          (case (length req)
                            [(1) (format " ~a" (fmt (car req)))]
                            [(2) (format " ~a and ~a" (fmt (car req)) (fmt (cadr req)))]
                            [else
                             (let loop ([req req])
                               (if (null? (cdr req))
                                   (format " and ~a" (fmt (car req)))
                                   (format " ~a,~a" (fmt (car req))
                                           (loop (cdr req)))))])))]
               [(method-adjust)
                (lambda (a)
                  (if (or (okm? proc)
                          (keyword-method? proc))
                      (if (zero? a) 0 (sub1 a))
                      a))])

    (string-append
     (cond
       [(number? a) 
        (let ([a (method-adjust a)])
          (format "~a" a))]
       [(arity-at-least? a)
        (let ([a (method-adjust (arity-at-least-value a))])
          (format "at least ~a" a))]
       [else
        "a different number"])
     (if (null? req)
         ""
         (format " plus ~a" (keywords-desc "" req)))
     (if allowed
         (let ([others (let loop ([req req][allowed allowed])
                         (cond
                           [(null? req) allowed]
                           [(eq? (car req) (car allowed))
                            (loop (cdr req) (cdr allowed))]
                           [else
                            (cons (car allowed) (loop req (cdr allowed)))]))])
           (if (null? others)
               ""
               (format " plus ~a"
                       (keywords-desc "optional " others))))
         " plus arbitrary keyword arguments"))))

;; Constructor for a procedure with only optional keywords.
  ;; The `procedure' property dispatches to a procedure in the 
  ;; struct (which has exactly the right arity).
  (define-values (struct:okp make-optional-keyword-procedure okp? okp-ref okp-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      1 0 #f
                      (list (cons prop:arity-string generate-arity-string))
                      (current-inspector) 0))

  ;; A ``method'' (for arity reporting)
  (define-values (struct:okm make-optional-keyword-method okm? okm-ref okm-set!)
    (make-struct-type 'procedure
                      struct:okp
                      0 0 #f
                      (list (cons prop:method-arity-error #t))))

  (define-values (prop:named-keyword-procedure named-keyword-procedure? keyword-procedure-name+fail)
    (make-struct-type-property 'named-keyword-procedure))

  (define (keyword-procedure-name+fail* p)
    (define v (keyword-procedure-name+fail p))
    (if (vector? v) v (v p)))

;; Allows support for new-prop:procedure to extract a field (i.e., this property
;; makes it possible to extract a field for an integer `new-prop:procedure` value):
(define-values (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
  (make-struct-type-property 'procedure (lambda (v info-l)
                                          (if (exact-integer? v)
                                              (make-struct-field-accessor
                                               (list-ref info-l 3)
                                               v)
                                              #f))))

;; Allows keyword application to see into a "method"-style procedure attribute:
(define-values (new-prop:procedure new-procedure? new-procedure-ref)
  (make-struct-type-property 'procedure #f
                             (list
                              ;; Imply normal `prop:procedure`:
                              (cons prop:procedure values)
                              ;; Also imply `prop:procedure-accessor`, in case property
                              ;; value is an integer:
                              (cons prop:procedure-accessor values))
                             ;; Can impersonate:
                             #t))


;; ----------------------------------------
;; Functions and proxies with required keyword arguments

(define-values (struct:keyword-procedure/arity-error make-kp/ae kp/ae? kp/ae-ref kp/ae-set!)
  (make-struct-type 'procedure
                    struct:keyword-procedure
                    0 0 #f
                    (list (cons prop:arity-string generate-arity-string)
                          (cons prop:incomplete-arity #t))))
(define-values (struct:keyword-method/arity-error make-km/ae km/ae? km/ae-ref km/ae-set!)
  (make-struct-type 'procedure
                    struct:keyword-method
                    0 0 #f
                    (list (cons prop:arity-string generate-arity-string)
                          (cons prop:incomplete-arity #t))))

(define-syntax (define-named-variant stx)
  (syntax-case stx ()
    [(_ make-required-keyword-procedure/arity-error
        struct:keyword-procedure/arity-error)
     #'(define-values (struct:rkp/ae make-required-keyword-procedure/arity-error
                                     rkp/ae? rkp/ae-ref rkp/ae-set!)
         (make-struct-type 'procedure
                           struct:keyword-procedure/arity-error
                           3 0 #f
                           (list (cons prop:procedure 0)
                                 (cons prop:object-name 1)
                                 (cons prop:named-keyword-procedure
                                       (lambda (r)
                                         (vector (rkp/ae-ref r 1)
                                                 (rkp/ae-ref r 2)
                                                 (rkp/ae-ref r 0)))))
                           (current-inspector)
                           #f
                           '(0 1 2)))]))  
(define-named-variant make-required-keyword-procedure/arity-error
  struct:keyword-procedure/arity-error)
(define-named-variant make-required-keyword-method/arity-error
  struct:keyword-method/arity-error)

;; Constructor generator for a wrapper on a procedure with a required keyword.
;; The `procedure' property value `fail-proc` is a per-type method that has exactly
;;  the right arity, and that sends all arguments to `missing-kw'.
;; We use a macro so that the `make-struct-type` is visible
;; to the optimizer, which in turn allows it to determine that the first
;; result is a constructor that always succeeds.
;; >> Beware that `name` and `fail-proc` are duplicated in the macro expansion. <<
;; The `name` expresison is expected to be a quoted symbol, and `fail-proc` is
;; expected to be a small procedure, so that duplication is ok.
;; (This macro is used with lift-values-expression, so that the same constructor
;;  is used for each evaluation of a keyword lambda.)
(define-syntax (make-required* stx)
  (syntax-case stx ()
    [(_ struct:km/ae name realm fail-proc)
     #'(make-struct-type name
                         struct:km/ae
                         0 0 #f
                         (list (cons prop:named-keyword-procedure
                                     (vector name realm fail-proc)))
                         (current-inspector)
                         fail-proc)]))

;; To avoid ending up with inferred names that point inside this module, we
;; need to ensure that both 'inferred-name is (void) and there is no source
;; location on the expression
(define-syntax (no-inferred-name stx)
  (syntax-case stx ()
    [(_ e)
     (syntax-property (datum->syntax #'e (syntax-e #'e) #f #'e)
                      'inferred-name (void))]))

(define-for-syntax (hide-binding-name stx)
  (syntax-property stx
                   'inferred-name
                   ;; void hides binding name
                   (void)))

;; Similar to the above, when we copy source locations from an input
;; expression, we need to ensure the source location is copied even if there
;; is no source location on the input, but (quasi)syntax/loc doesn’t copy
;; the source location if it is #f
(begin-for-syntax
  (define-syntaxes (syntax/loc/always quasisyntax/loc/always)
    (let ([mk (lambda (syntax-id)
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ src-expr template)
                     #`(let ([result (#,syntax-id template)])
                         (datum->syntax result (syntax-e result)
                                        src-expr result))])))])
      (values (mk #'syntax) (mk #'quasisyntax)))))



;; (define-values (struct:keyword-procedure/arity-error make-kp/ae kp/ae? kp/ae-ref kp/ae-set!)
;;   (make-struct-type 'procedure
;;                     struct:keyword-procedure
;;                     0 0 #f
;;                     (list (cons prop:arity-string generate-arity-string)
;;                           (cons prop:incomplete-arity #t))))

;; (define-values (struct:keyword-method/arity-error make-km/ae km/ae? km/ae-ref km/ae-set!)
;;   (make-struct-type 'procedure
;;                     struct:keyword-method
;;                     0 0 #f
;;                     (list (cons prop:arity-string generate-arity-string)
;;                           (cons prop:incomplete-arity #t))))

;; (define-values (struct:keyword-procedure-impersonator/arity-error make-kpi/ae kpi/ae? kpi/ae-ref kpi/ae-set!)
;;   (make-struct-type 'procedure
;;                     struct:keyword-procedure-impersonator
;;                     0 0 #f
;;                     (list (cons prop:arity-string generate-arity-string)

;;                           (cons prop:incomplete-arity #t))))
;; (define-values (struct:keyword-method-impersonator/arity-error make-kmi/ae kmi/ae? kmi/ae-ref kmi/ae-set!)
;;   (make-struct-type 'procedure
;;                     struct:keyword-method-impersonator
;;                     0 0 #f
;;                     (list (cons prop:arity-string generate-arity-string)
;;                           (cons prop:incomplete-arity #t))))


;;;
;;; --------------------------------------
;;;

;; Code from earlier experiment

;; WARNING
;;   This file is not in use.
;;   It was an experiment to see how hard porting
;;      collects/racket/private/kw.rkt
;;   directly would be.

;;   The conclusion is that it is better to implement modules first.
;;   The keyword support imports primitive `lambda` and exports
;;   a version that handles keyword arguments.
;;   And `lambda` is not alone, there is also `case-lamdda` and `define` etc.

;; (define-values (prop:keyword-impersonator
;;                 keyword-impersonator?
;;                 keyword-impersonator-ref)
;;   (make-struct-type-property 'keyword-impersonator))

;; (define (keyword-procedure-impersonator-of v)
;;   (cond
;;     [(keyword-impersonator? v) ((keyword-impersonator-ref v) v)]
;;     [else #f]))

;; (define-values (struct:keyword-procedure mk-kw-proc keyword-procedure?
;;                                          keyword-procedure-ref keyword-procedure-set!)
;;   (make-struct-type 'keyword-procedure #f 4 0 #f
;;                     (list (cons prop:checked-procedure #t)
;;                           (cons prop:impersonator-of keyword-procedure-impersonator-of))
;;                     (current-inspector)
;;                     #f
;;                     '(0 1 2 3)))

;; (define keyword-procedure-checker  (make-struct-field-accessor keyword-procedure-ref 0))
;; (define keyword-procedure-proc     (make-struct-field-accessor keyword-procedure-ref 1))
;; (define keyword-procedure-required (make-struct-field-accessor keyword-procedure-ref 2))
;; (define keyword-procedure-allowed  (make-struct-field-accessor keyword-procedure-ref 3))

;; (define-values (struct:keyword-method make-km keyword-method? km-ref km-set!)
;;   (make-struct-type 'procedure
;;                     struct:keyword-procedure
;;                     0 0 #f
;;                     (list (cons prop:method-arity-error #t))))


;; (define (fmt v)
;;   ((error-syntax->string-handler) v #f))

#;(define (generate-arity-string proc)
  (let-values ([(req allowed) (procedure-keywords proc)]
               [(a) (procedure-arity proc)]
               [(keywords-desc)
                (lambda (opt req)
                  (format "~a with keyword~a~a"
                          (if (null? (cdr req))
                              (format "an ~aargument" opt)
                              (format "~aarguments" opt))
                          (if (null? (cdr req))
                              ""
                              "s")
                          (case (length req)
                            [(1) (format " ~a" (fmt (car req)))]
                            [(2) (format " ~a and ~a" (fmt (car req)) (fmt (cadr req)))]
                            [else
                             (let loop ([req req])
                               (if (null? (cdr req))
                                   (format " and ~a" (fmt (car req)))
                                   (format " ~a,~a" (fmt (car req))
                                           (loop (cdr req)))))])))]
               [(method-adjust)
                (lambda (a)
                  (if (or (okm? proc)
                          (keyword-method? proc))
                      (if (zero? a) 0 (sub1 a))
                      a))])

    (string-append
     (cond
       [(number? a) 
        (let ([a (method-adjust a)])
          (format "~a" a))]
       [(arity-at-least? a)
        (let ([a (method-adjust (arity-at-least-value a))])
          (format "at least ~a" a))]
       [else
        "a different number"])
     (if (null? req)
         ""
         (format " plus ~a" (keywords-desc "" req)))
     (if allowed
         (let ([others (let loop ([req req][allowed allowed])
                         (cond
                           [(null? req) allowed]
                           [(eq? (car req) (car allowed))
                            (loop (cdr req) (cdr allowed))]
                           [else
                            (cons (car allowed) (loop req (cdr allowed)))]))])
           (if (null? others)
               ""
               (format " plus ~a"
                       (keywords-desc "optional " others))))
         " plus arbitrary keyword arguments"))
    
    ))


#;(define (procedure-keywords p)
    (cond
     [(keyword-procedure? p)
      (values (keyword-procedure-required p)
              (keyword-procedure-allowed p))]
     [(procedure? p)
      (if (new-procedure? p)
          (let ([v (new-procedure-ref p)])
            (if (procedure? v)
                (procedure-keywords v)
                (let ([a (procedure-accessor-ref p)])
                  (if a
                      (procedure-keywords (a p))
                      (values null null)))))
          (values null null))]
     [else (raise-argument-error* 'procedure-keywords
                                  'racket/primitive
                                  "procedure?"
                                  p)]))


;; Allows support for new-prop:procedure to extract a field (i.e., this property
;; makes it possible to extract a field for an integer `new-prop:procedure` value):
#;(define-values (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
  (make-struct-type-property 'procedure (lambda (v info-l)
                                          (if (exact-integer? v)
                                              (make-struct-field-accessor
                                               (list-ref info-l 3)
                                               v)
                                              #f))))


