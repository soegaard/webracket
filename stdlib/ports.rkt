#lang webracket

;;;
;;; Current Input, Output and Error Ports
;;;

(define current-output-port
  (let ([value (open-output-string)])
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

(define (reset-current-output-port!)
  (current-output-port (open-output-string)))

(define current-error-port
  (let ([value (open-output-string)])
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

(define (reset-current-error-port!)
  (current-output-port (open-output-string)))

(define current-input-port
  (let ([value (open-input-string "")])
    (case-lambda
      [() value]
      [(path)
       (set! value path)
       value])))

(define (reset-current-input-port!)
  (current-input-port (open-input-string "")))
