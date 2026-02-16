;;;
;;; error messages
;;;

;; This file tests specific error message text.

;;; Build
;;   racket -l errortrace -t ../webracket.rkt -- -r test-error-messages.rkt
;;
;; On success you will see an s-expression where #t indicates success.

(define (message-matches? thunk predicates)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define msg (exn-message e))
                     (andmap (lambda (p) (p msg)) predicates))])
    (thunk)
    #f))

(define (msg-contains? s)
  (lambda (msg) (string-contains? msg s)))

(struct s (x))
(define s-val (s 1))

(struct p (f) #:property prop:procedure 0)
(define p-val (p 42))

(define-values (prop:foo foo? foo-ref) (make-struct-type-property 'foo))

(list
 (list "application: not a procedure (non-procedure)"
       (message-matches?
        (lambda () (5 1))
        (list (msg-contains? "application: not a procedure")
              (msg-contains? "given: 5"))))
 (list "application: not a procedure (struct without prop:procedure)"
       (message-matches?
        (lambda () (s-val 2))
        (list (msg-contains? "application: not a procedure")
              (msg-contains? "given:"))))
 (list "prop:procedure field not a procedure"
       (message-matches?
        (lambda () (p-val 1))
        (list (msg-contains? "p: arity mismatch")
              (msg-contains? "given: 1"))))
 (list "struct-type-property-accessor arity (too few)"
       (message-matches?
        (lambda () (foo-ref))
        (list (msg-contains? "foo-accessor: arity mismatch")
              (msg-contains? "given: 0"))))
 (list "struct-type-property-accessor arity (too many)"
       (message-matches?
        (lambda () (foo-ref 1 2 3))
        (list (msg-contains? "foo-accessor: arity mismatch")
              (msg-contains? "given: 3"))))
 (list "struct-type-property-accessor contract violation"
       (message-matches?
        (lambda () (foo-ref 1))
        (list (msg-contains? "foo-accessor: contract violation")
              (msg-contains? "expected: foo?")
              (msg-contains? "given: 1"))))
 (list "apply final argument not a list (number)"
       (message-matches?
        (lambda () (apply + 1 2 3 4 5))
        (list (msg-contains? "apply: contract violation")
              (msg-contains? "expected: list?")
              (msg-contains? "given: 5"))))
 (list "apply final argument not a list (string)"
       (message-matches?
        (lambda () (apply + 1 2 3 "x"))
        (list (msg-contains? "apply: contract violation")
              (msg-contains? "expected: list?")
              (msg-contains? "given: x"))))
 (list "apply final argument not a list (improper)"
       (message-matches?
        (lambda () (apply + 1 2 3 (cons 4 5)))
        (list (msg-contains? "apply: contract violation")
              (msg-contains? "expected: list?")
              (msg-contains? "given: (4 . 5)"))))
 (list "exact->inexact non-real"
       (message-matches?
        (lambda () (exact->inexact "x"))
        (list (msg-contains? "exact->inexact: contract violation")
              (msg-contains? "expected: real?")
              (msg-contains? "given: x"))))
 (list "real->double-flonum non-real"
       (message-matches?
        (lambda () (real->double-flonum "x"))
        (list (msg-contains? "real->double-flonum: contract violation")
              (msg-contains? "expected: real?")
              (msg-contains? "given: x")))))
