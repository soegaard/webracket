(define i (make-instance 'i #f 'constant 'x 1))
(instance-set-variable-value! i 'x 2)
(js-log (instance-variable-value i 'x))
