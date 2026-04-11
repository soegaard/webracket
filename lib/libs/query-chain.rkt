#lang racket/base

(require (for-syntax racket/base racket/list))

(provide (rename-out [chain $chain]))

(begin-for-syntax
  (define (chain-dotted-identifier? stx)
    (and (identifier? stx)
         (let ([name (syntax-e stx)])
           (and (symbol? name)
                (let ([s (symbol->string name)])
                  (and (positive? (string-length s))
                       (char=? (string-ref s 0) #\.)))))))

  (define (chain-split-clauses rest who whole-stx)
    (let loop ([xs rest] [current #f] [clauses '()])
      (cond
        [(null? xs)
         (reverse (if current
                      (cons (reverse current) clauses)
                      clauses))]
        [else
         (define hd (car xs))
         (cond
           [(chain-dotted-identifier? hd)
            (loop (cdr xs)
                  (list hd)
                  (if current
                      (cons (reverse current) clauses)
                      clauses))]
           [current
            (loop (cdr xs) (cons hd current) clauses)]
           [else
            (raise-syntax-error who
                                "expected a dotted method name after the seed expression"
                                whole-stx
                                hd)])])))

  (define (chain-build seed clauses whole-stx)
    (let loop ([current seed] [xs clauses])
      (cond
        [(null? xs) current]
        [else
         (define clause (car xs))
         (define method (car clause))
         (define args (cdr clause))
         (define call-stx
           (quasisyntax/loc whole-stx
             (#,method #,current #,@args)))
         (loop call-stx (cdr xs))]))))

(define-syntax (chain stx)
  (syntax-case stx ()
    [(_ seed rest ...)
     (let* ([rest-list (syntax->list #'(rest ...))]
            [clauses (chain-split-clauses rest-list 'chain stx)])
       (chain-build #'seed clauses stx))]
    [_ (raise-syntax-error '$chain "expected `($chain seed .method arg ... ...)`" stx)]))
