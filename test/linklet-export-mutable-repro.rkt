(define l
  (make-compiled-linklet
   'l
   '()
   '(x)
   (lambda (self)
     (instance-set-variable-value! self 'x 1))))

(define i (instantiate-linklet l '()))
(instance-set-variable-value! i 'x 2)
(js-log (instance-variable-value i 'x))
