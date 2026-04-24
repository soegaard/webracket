(define l
  (make-compiled-linklet
   'l
   '((x))
   '()
   (lambda (self imported)
     (void))))

(define imported (make-instance 'imported))
(define result (instantiate-linklet l (list imported)))
(js-log (instance? result))
