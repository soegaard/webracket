#lang webracket/base

(require (for-syntax racket/base))

(define next-step 0)

;; step : exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Require each initializer to run exactly once in source order.
(define (step expected)
  (unless (= next-step expected)
    (error 'initializer-batches "initializer ran out of order"))
  (set! next-step (add1 next-step))
  expected)

;; define-steps : syntax? -> syntax?
;;   Expand enough complete definitions to cross several initializer batches.
(define-syntax (define-steps stx)
  (syntax-case stx ()
    [(_ count)
     (with-syntax
         ([(definition ...)
           (for/list ([i (in-range (syntax-e #'count))])
             (with-syntax ([name (datum->syntax stx (string->symbol (format "step-~a" i)))]
                           [index (datum->syntax stx i)])
               #'(define name
                   (let ([local-value (step index)])
                     (+ local-value 1)))))])
       #'(begin definition ...))]))

(define-steps 300)

;; These bindings also check forward references through top-level boxes.
(define (read-later) later)
(define later (step 300))
(define-values (left right)
  (let-values ([(a b) (values (step 301) (step 302))])
    (values b a)))
(define-values () (begin (step 303) (values)))
(define recursive-result
  (letrec ([even-loop (lambda (n) (if (= n 0) #t (odd-loop (sub1 n))))]
           [odd-loop (lambda (n) (if (= n 0) #f (even-loop (sub1 n))))])
    (and (even-loop 20) (not (odd-loop 20)))))

(unless (and (= next-step 304)
             (= step-299 300)
             (= (read-later) 300)
             (= left 302)
             (= right 301)
             recursive-result)
  (error 'initializer-batches "initialization did not preserve bindings or values"))

(displayln "initializer-batches-ok")
;; Finish with an expression after all initialization effects.
(+ next-step 1)
