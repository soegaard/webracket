#lang webracket

(define (caught-with-message? thunk)
  (with-handlers ([(lambda (ex) #t)
                   (lambda (ex)
                     (string? (exn-message ex)))])
    (thunk)
    #f))

(define bs (make-bytes 5 65))

(unless (and (caught-with-message? (lambda () (bytes-ref bs 6)))
             (caught-with-message? (lambda () (bytes-set! bs 6 66))))
  (error 'bytes-index-catchable "failed"))
