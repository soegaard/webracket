#lang webracket

;;;
;;; Exceptions
;;;

(struct exn (message continuation-marks)
  ; message is a string
  ; continuation-marks is a continuation-mark-set (not available yet)
  #:extra-constructor-name make-exn
  #:transparent)

(struct exn:fail exn ()
  #:extra-constructor-name make-exn:fail
  #:transparent)


(define error
  (case-lambda
    [(message-sym) (define message
                     (string-append "error: " (symbol->string message-sym)))
                   (raise (make-exn:fail message #f))]
    [(arg0 . args) (cond
                     [(string? arg0)
                      ; Included for compatibility.
                      ; Use `raise-arguments-error` instead. 
                      (define message-str arg0)
                      (define vs          args)
                      (define v-strs
                        (map (λ (v)
                               (let ([old-out (current-output-port)]
                                     [out     (open-output-string)])
                                 (current-output-port out)
                                 (print v)
                                 (let ([str (get-output-string out)])
                                   (current-output-port old-out)
                                   str)))
                             vs))
                      (define message
                        (string-append*
                         (cons message-str (add-between v-strs " "))))
                      (raise (make-exn:fail message #f))]
                     [(symbol? arg0)
                      ; Included for compatibility.
                      ; Use `raise-argument-error` instead. 
                      (define who-sym arg0)
                      (unless (pair? args)
                        (error 'error "format string expected after who-symbol"))
                      (define format-str (car args))
                      (unless (string? format-str)
                        (error 'error "format string expected after who-symbol"))
                      (define vs (cdr args))
                      
                      (define message (apply format
                                             (string-append "~s: " format-str)
                                             who-sym vs))
                      (raise (make-exn:fail message #f))]
                     [else
                      (error 'error "expected: (or/c symbol? string?)")])]))



