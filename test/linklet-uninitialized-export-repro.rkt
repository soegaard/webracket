(define l
  (make-compiled-linklet
   'l
   '()
   '(x)
   (lambda (self)
     (void))))

(define i (instantiate-linklet l '()))
(js-log (instance-variable-names i))
(js-log (instance-variable-value i 'x 'fallback))
