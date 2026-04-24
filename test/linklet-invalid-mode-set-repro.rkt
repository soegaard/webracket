(define i (make-instance 'i))
(instance-set-variable-value! i 'x 1 'bogus)
(js-log (instance-variable-value i 'x))
