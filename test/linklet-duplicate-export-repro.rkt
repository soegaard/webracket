(define l
  (make-compiled-linklet
   'l
   '()
   '(x x)
   (lambda (self)
     (instance-set-variable-value! self 'x 1))))

(js-log (linklet-export-variables l))
