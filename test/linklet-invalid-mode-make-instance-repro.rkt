(define i (make-instance 'i #f 'bogus 'x 1))
(js-log (instance-variable-value i 'x))
