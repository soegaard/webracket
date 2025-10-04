; #lang webracket

;;;
;;; TEST SUITE FOR THE STANDARD LIBRARY
;;;

;; The standard library consists of the functions
;; which are implemented in WebRacket in `stdlib/`.

;; Run the test suite like this:
;;
;;   racket -t ../webracket.rkt -- --stdlib -r test-stdlib.rkt 
;;

(list
 (list "ports"
       (let* ([original (current-output-port)]
              [port     (open-output-string)])
         (current-output-port port)
         #;(write-string "ok")  ; todo - make this work
         (write-string "ok" port)
         (let ([after (current-output-port)]
               [text  (get-output-string port)])
           (current-output-port original)
           (and (eq? after port)
                (equal? text "ok"))))

       (let* ([original (current-output-port)]
              [port     (open-output-string)])
         (current-output-port port)
         #;(write-string "x") ; todo - make this work
         (write-string "x" port)         
         (reset-current-output-port!)
         (let* ([after (current-output-port)]
                [text  (get-output-string after)])
           (current-output-port original)
           (and (not (eq? after port))
                (string=? text ""))))

       (let* ([original (current-error-port)]
              [port     (open-output-string)])
         (current-error-port port)
         (write-string "problem" (current-error-port))
         (let ([after (current-error-port)]
               [text  (get-output-string port)])
           (current-error-port original)
           (and (eq? after port)
                (string=? text "problem"))))

       (let* ([original (current-error-port)]
              [port     (open-output-string)])
         (current-error-port port)
         (write-string "discard" (current-error-port))
         (reset-current-error-port!)
         (let* ([after (current-error-port)]
                [text (get-output-string after)])
           (current-error-port original)
           (and (not (eq? after port))
                (string=? text ""))))

       (let* ([original (current-input-port)]
              [port     (open-input-string "hi")])
         (current-input-port port)
         (let ([after (current-input-port)]
               #;[first (read-char)] ; todo - make this work
               [first (read-char port)])
           (current-input-port original)
           (and (eq? after port)
                (char=? first #\h))))

       (let* ([original (current-input-port)]
              [port     (open-input-string "data")])
         (current-input-port port)
         (reset-current-input-port!)
         (let* ([after (current-input-port)]
                [value (read-char after)])
           (current-input-port original)
           (and (not (eq? after port))
                (eof-object? value))))
       )

 
 #;(list "Exceptions"
       (list "struct constructors"
             (let ([value (exn "message" #f)])
               (and (exn? value)
                    (string=? (exn-message value) "message")
                    (eq? (exn-continuation-marks value) #f)))

             (let ([value (make-exn "other" #f)])
               (and (exn? value)
                    (string=? (exn-message value) "other")
                    (eq? (exn-continuation-marks value) #f)))

             (let ([value (exn:fail "boom" #f)])
               (and (exn:fail? value)
                    (exn? value)
                    (string=? (exn-message value) "boom")
                    (eq? (exn-continuation-marks value) #f)))

             (let ([value (make-exn:fail "explode" #f)])
               (and (exn:fail? value)
                    (exn? value)
                    (string=? (exn-message value) "explode")
                    (eq? (exn-continuation-marks value) #f)))))
      
 #;(list "Writing"

       (list "parameters"
             (let ([original (print-pair-curly-braces)])
               (print-pair-curly-braces #t)
               (let ([after (print-pair-curly-braces)])
                 (print-pair-curly-braces original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-mpair-curly-braces)])
               (print-mpair-curly-braces #t)
               (let ([after (print-mpair-curly-braces)])
                 (print-mpair-curly-braces original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-unreadable)])
               (print-unreadable #f)
               (let ([after (print-unreadable)])
                 (print-unreadable original)
                 (and (eq? original #t)
                      (eq? after #f))))
             (let ([original (print-graph)])
               (print-graph #t)
               (let ([after (print-graph)])
                 (print-graph original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-struct)])
               (print-struct #f)
               (let ([after (print-struct)])
                 (print-struct original)
                 (and (eq? original #t)
                      (eq? after #f))))
             (let ([original (print-box)])
               (print-box #f)
               (let ([after (print-box)])
                 (print-box original)
                 (and (eq? original #t)
                      (eq? after #f))))
             (let ([original (print-vector-length)])
               (print-vector-length #t)
               (let ([after (print-vector-length)])
                 (print-vector-length original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-hash-table)])
               (print-hash-table #f)
               (let ([after (print-hash-table)])
                 (print-hash-table original)
                 (and (eq? original #t)
                      (eq? after #f))))
             (let ([original (print-boolean-long-form)])
               (print-boolean-long-form #t)
               (let ([after (print-boolean-long-form)])
                 (print-boolean-long-form original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-reader-abbreviations)])
               (print-reader-abbreviations #t)
               (let ([after (print-reader-abbreviations)])
                 (print-reader-abbreviations original)
                 (and (eq? original #f)
                      (eq? after #t))))
             (let ([original (print-as-expression)])
               (print-as-expression #f)
               (let ([after (print-as-expression)])
                 (print-as-expression original)
                 (and (eq? original #t)
                      (eq? after #f))))
             (let ([original (print-syntax-width)])
               (print-syntax-width 10)
               (let ([after (print-syntax-width)])
                 (print-syntax-width original)
                 (and (= original 40)
                      (= after 10))))
             (let ([original (print-value-columns)])
               (print-value-columns 60)
               (let ([after (print-value-columns)])
                 (print-value-columns original)
                 (and (= original 79)
                      (= after 60))))
             (let ([original (current-write-relative-directory)])
               (current-write-relative-directory 'relative)
               (let ([after (current-write-relative-directory)])
                 (current-write-relative-directory original)
                 (and (eq? original #f)
                      (eq? after 'relative)))))
       
       (list "write"
             (equal? (call-with-output-string (λ (out) (write 42 out)))
                     "42")

             (equal? (call-with-output-string (λ (out) (write "abc" out)))
                     "\"abc\"")

             (let ([original (print-pair-curly-braces)])
               (print-pair-curly-braces #t)
               (let ([text (call-with-output-string (λ (out) (write '(1 2) out)))])
                 (print-pair-curly-braces original)
                 (string=? text "{1 2}")))
             
             (equal? (call-with-output-string (λ (out) (write #"hi" out)))
                     "#\"hi\""))

       (list "display"
             (equal? (call-with-output-string (λ (out) (display "abc" out)))
                     "abc")
             (equal? (call-with-output-string (λ (out) (display '#:sample out)))
                     "#:sample")
             (equal? (call-with-output-string (λ (out) (display '(1 2) out)))
                     "(1 2)"))

       (list "print"
             (equal? (call-with-output-string (λ (out) (print 'sample out)))
                     "'sample")
             
             (equal? (call-with-output-string (λ (out) (print '(1 2) out)))
                     "'(1 2)")
             
             (let ([original (print-as-expression)])
               (print-as-expression #f)
               (let ([text (call-with-output-string (λ (out) (print '(1 2) out)))])
                 (print-as-expression original)
                 (string=? text "(1 2)")))
             
             (equal? (call-with-output-string (λ (out) (print '#(1 2) out)))
                     "'#(1 2)")
             
             (let ()
               (struct hidden-point (x) #:transparent)
               (let* ([value (hidden-point 5)]
                      [port  (open-output-string)]
                      [original (print-struct)])
                 (print-struct #f)
                 (print value port)
                 (let ([text (get-output-string port)])
                   (print-struct original)
                   (string=? text "#<struct:hidden-point>")))))

       (list "newline variants"
             (equal? (call-with-output-string (λ (out) (writeln "abc" out)))
                     "\"abc\"\n")
             (equal? (call-with-output-string (λ (out) (displayln "abc" out)))
                     "abc\n")
             (equal? (call-with-output-string (λ (out) (println '(1 2) out)))
                     "'(1 2)\n"))

       (list "error handling and format"
             (let ([original (error-print-width)])
               (error-print-width 12)
               (let ([after (error-print-width)])
                 (error-print-width original)
                 (and (= original 1024)
                      (= after 12))))
             
             #;(string=? (default-error-value->string-handler '(1 2) 50)
                         "'(1 2)")
             
             (let ([original (error-print-width)])
               (error-print-width 5)
               (let ([text (format "~.a" "abcdef")])
                 (error-print-width original)
                 (string=? text "ab...")))
             
             (let ([original (error-value->string-handler)])
               (error-value->string-handler (λ (value width) "oops"))
               (let ([text (format "~e" 'ignored)])
                 (error-value->string-handler original)
                 (string=? text "oops"))))
       
       (list "fprintf family"
             (equal? (call-with-output-string
                      (λ (out)
                        (fprintf out "~a ~s ~v~%~b ~X ~c"
                                 '(1 2)
                                 "hi"
                                 '(1 2)
                                 5
                                 31
                                 #\A)))
                     "(1 2) \"hi\" '(1 2)\n101 1F A")
             
             #;(let ([port (open-output-string)])
                 (fprintf* port "~a" (list '(1 2)))
                 (string=? (get-output-string port) "(1 2)"))
             
             (let* ([original (current-output-port)]
                    [port (open-output-string)])
               (current-output-port port)
               (printf "hello" #f)
               (let ([text (get-output-string port)])
                 (current-output-port original)
                 (string=? text "hello")))
             
             (let* ([original (current-error-port)]
                    [port (open-output-string)])
               (current-error-port port)
               (eprintf "problem" #f)
               (let ([text (get-output-string port)])
                 (current-error-port original)
                 (string=? text "problem"))))
       )
 )
      

