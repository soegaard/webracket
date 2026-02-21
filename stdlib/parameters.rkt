#lang webracket

;;;
;;; Parameters
;;;

;; This implements poor-man's parameters.
;; Ideally, this would use dynamic-wind / continuation marks and thred cells,
;; but we make do with "fluid let" parameters.


(define (make-parameter v [guard #f] [name 'parameter-procedure] [realm 'racket])
  (when guard
    (unless (procedure? guard)
      (error 'make-parameter "expected a guard procedure, got: ~a" guard)))

  (cond
    [guard (let ([value v])
             (case-lambda
               [()   value]
               [(v)  (set! value (guard v))]))]
    [else  (let ([value v])
             (case-lambda
               [()   value]
               [(v)  (set! value v)]))]))

(define-syntax (parameterize stx)
  (syntax-case stx ()
    [(_parameterize ([parameter-expr value-expr] ...) . body)
     (with-syntax ([(p ...)     (generate-temporaries #'(parameter-expr ...))]
                   [(old-v ...) (generate-temporaries #'(parameter-expr ...))])
       (syntax 
        (let-values ([(p ...) (values parameter-expr ...)])
          (let ([restore (λ () (p old-v) ...)])
            ; save old values for parameters
            (let-values ([(old-v ...) (values (p) ...)])
              ; restore in case an exception is thrown
              (with-handlers ([(λ (e)
                                 (restore)
                                 (raise e))])
                ; set parameters to new values
                (p value-expr) ...
                ; run body
                (begin0
                    ((λ () . body))
                  ; set parameters back to old value
                  (restore))))))))]))
