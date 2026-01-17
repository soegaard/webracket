#lang racket/base
(require racket/pretty
         (only-in racket/linklet
                  datum->correlated
                  correlated?
                  correlated-e)
         "webify.rkt"
         "known.rkt")

; prim-knowns : hash-table[ symbol -> known-procedure ]
;   Hash table that maps from the name of a primitive
;   to a description of it. See "known.rkt".

; primitives   : (listof (cons symbol procedure?)
;   Association list from name to procedure (as a Racket value)

(define-values (prim-knowns primitives)  
  ;; Register primitives
  ;;   TODO - use primitives from `webracket`
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/unsafe/ops)
      (namespace-require 'racket/flonum)
      (namespace-require 'racket/fixnum))

    ; The base primitives are found in the modules above.
    (define base-primitives
      (for/hasheq ([s (in-list (namespace-mapped-symbols ns))]
                   #:when (with-handlers ([exn:fail? (lambda (x) #f)])
                            (procedure? (eval s ns))))
        (values s (eval s ns))))

    ; Add the constants `eof` and `null`.
    (define primitives (let* ([ht base-primitives]
                              [ht (hash-set ht 'eof eof)]
                              [ht (hash-set ht 'null null)])
                         ht))

    ; In `prim-knowns` the constants are wrapped in `known-literal`.
    ; Similarly, procedures are wrapped in one of:
    ;   - known-procedure/folding
    ;   - known-procedure/folding/limited
    ;   - known-procedure/then-pure/folding-unsafe
    ;   - known-procedure 
    (values
     (for/hasheq ([(s v) (in-hash primitives)])
       (cond
         [(procedure? v)
          (define a (procedure-arity-mask v))
          (values s (case s
                      [(+ - * / integer->char char->integer void)
                       (known-procedure/folding a)]
                      [(fx+ fxlshift)
                       (known-procedure/folding/limited a 'fixnum)]
                      [(expt arithmetic-shift)
                       (known-procedure/folding/limited a 'expt)]
                      [(unsafe-fx+)
                       (known-procedure/then-pure/folding-unsafe a 'fx+)]
                      [else
                       (known-procedure a)]))]
         [else
          (values s (known-literal v))]))
     primitives)))

(define (wrap p)
  (datum->syntax #f p)
  ; p
 
  #;(cond
    [(and (pair? p)
          (eq? (car p) 'define-values))
     ;; expander doesn't use a correalted for id list, so avoid
     ;; adding one here
     (list (car p) (map wrap (cadr p)) (map wrap (cddr p)))]
    [(list? p)
     (datum->correlated (map wrap p))]
    [(pair? p)
     (cons (wrap (car p)) (wrap (cdr p)))]
    [else
     (datum->correlated p)]))

(define (unwrap p)
  (cond
    [(or (correlated? p) (syntax? p)) (unwrap (correlated-e p))]
    [(pair? p)                        (cons (unwrap (car p)) (unwrap (cdr p)))]
    [else                             p]))


(define-values (webified importss exports import-keys imports-abis exports-info)
  (webify-linklet `(linklet 
                    ()
                    (x y [z ext-z] w c1 c2)
                    .
                    ,(map
                      values ; wrap
                      '(
                        (define-values (x) 1)
                        ;; (define y 2)
                        ;; (define z 3)
                        ;; (define w 4)
                        ;; (define c1 5)
                        ;; (define c2 6)
                        ;; (define-values (struct:s make-s s? s-ref s-set!)
                        ;;   (make-struct-type 's #f 2 0 #f))
                        ;; (define-values (y) (make-s (lambda () x) 5))
                        ;; (define-values (x) (lambda () y))
                        ;; (x)
                        ;; (define-values (w) (case-lambda [() (+ 1 7)] [(a) x]))
                        ;; (letrec-values ([(loop) (lambda () (loop))]) (loop))
                        ;; (let-values ([(a) 1] [(b) 2]) (list a b))
                        ;; (let-values ([(a b) (values 1 (+ 2 3))])
                        ;;   (list a
                        ;;         b
                        ;;         (arithmetic-shift 3 1000)
                        ;;         (fx+ 4 5) (fx+ 4 (expt 2 40)) (fx* (fxlshift 1 20) (fxlshift 1 20))
                        ;;         (unsafe-fx+ 4 5) (unsafe-fx+ 4 (expt 2 40))
                        ;;         (integer->char 48)
                        ;;         (char->integer '#\1)
                        ;;         (void (void) eof-object null)))
                        ;; (define-values (adds-unsafe) (lambda (x)
                        ;;                                (list (unsafe-fx+ x 1)
                        ;;                                      (unsafe-fx+ x 2))))
                        ;; (define-values (adds-safe) (lambda (x)
                        ;;                              (list (fx+ x 1)
                        ;;                                    (unsafe-fx+ x 2))))
                        ;; (define-values (adds-still-unsafe) (lambda (x)
                        ;;                                      (list (unsafe-fx+ x 1)
                        ;;                                            (fx+ x 2))))
                        ;; (define-values (done) (z))
                        ;; (define-values (call) (lambda () (values 'c1 'c2)))
                        ;; (define-values (c1 c2) (call))

                        )))
                  
                  #:serializable?-box     #t          
                  #:datum-intern?         #t          
                  #:target                #f          
                  #:allow-set!-undefined? #f          
                  #:unsafe-mode?          #f          
                  #:enforce-constant?     #t          
                  #:allow-inline?         #t          
                  #:no-prompt?            #f          
                  #:prim-knowns           prim-knowns ; hasheq : symbol -> known-procedure (see "known.rkt") 
                  #:primitives            primitives  ; hasheq : symbol -> actual primitive
                  #:compiler-query        #f          
                  #:get-import-knowns     #f          
                  #:import-keys           #f))

'unwrapped-webified
(pretty-print (unwrap webified))
'exports-info
(pretty-print exports-info)

;; The exports info for the examples is:
;;   '#hasheq((c1    . #s(known-constant))
;;            (c2    . #s(known-constant))
;;            (ext-z . #s(known-constant))
;;            (w     . #s(known-constant))
;;            (x     . #s(known-constant))
;;            (y     . #s(known-constant)))

