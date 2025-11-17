#lang webracket

;;;
;;; Exceptions
;;;

;; The exceptions structures are defined in the runtime.
;; Functions that need `format` goes here.

(define-values (primitive-realm
                error
                raise-argument-error
                raise-argument-error*
                raise-arguments-error
                raise-arguments-error*
                raise-range-error
                raise-range-error*
                raise-result-error
                
                error-message->adjusted-string
                error-value->string)
  (let ()
    ;;;
    ;;; HELPERS
    ;;;


    (define indent "   ")

    (define (string-has-newline? str)
      (let ([len (string-length str)])
        (let loop ([i 0])
          (cond
            [(= i len) #f]
            [(char=? (string-ref str i) #\newline) #t]
            [else (loop (add1 i))]))))

    (define (string-starts-with-newline? str)
      (and (> (string-length str) 0)
           (char=? (string-ref str 0) #\newline)))

    (define (indent-lines str)
      (define out (open-output-string))
      (define len (string-length str))
      (when (> len 0)
        (write-string indent out)
        (let loop ([i 0])
          (when (< i len)
            (define ch (string-ref str i))
            (write-char ch out)
            (when (and (char=? ch #\newline)
                       (< (add1 i) len))
              (write-string indent out))
            (loop (add1 i)))))
      (get-output-string out))

    (define (format-field prefix content)
      (if (and (string-has-newline? content)
               (not (string-starts-with-newline? content)))
          (string-append prefix "\n" (indent-lines content))
          (string-append prefix content)))

    (define (value->string v)
      (define handler (error-value->string-handler))
      (define raw (handler v (error-print-width)))
      (cond
        [(string? raw) raw]
        [(bytes? raw) (bytes->string/utf-8 raw)]
        [else "..."]))


    ;;;
    ;;; Exports
    ;;;
    
    (define primitive-realm 'racket/primitive)
    
    (define error
      (procedure-rename
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

                          ; (js-log (equal? format undefined))
                          ; (js-log (procedure? (#%top . format)))

                          ; TODO - find out why `format` alone below doesn't work
                          ;        [it ought to]
                          (define message (apply #;format (#%top . format) ; todo <--
                                                 (string-append "~s: " format-str)
                                                 who-sym
                                                 vs))
                          (raise (make-exn:fail message #f))]
                         [else
                          (error 'error "expected: (or/c symbol? string?)")])])
      'error))


    (define raise-argument-error*
      ; todo: use the realm
      (case-lambda
        [(who realm expected v)
         (raise-argument-error who realm expected v)]
        [(who realm expected bad-pos . vs)
         (apply raise-argument-error who realm expected vs)]))


    (define (error-message->adjusted-string name name-realm message message-realm)
      ; todo: call an error message adjuster
      (if name
          (string-append (format "~a" name) ": " message)
          message))

    (define error-value->string
      (lambda (v)
        (let ([s ((error-value->string-handler)
                  v
                  (error-print-width))])
          (cond
            [(string? s) s]
            #;[(bytes? s)
               ;; Racket BC allows byte strings, and we approximate that here
               (utf8->string s)]
            [else "..."]))))

    ;; Modified from /racket/src/cs/rumble/error.ss

    (define (raise-arguments-error who-in what . more)
      (do-raise-arguments-error 'raise-arguments-error
                                who-in primitive-realm what exn:fail:contract more))

    (define (raise-arguments-error* who-in realm what . more)
      (do-raise-arguments-error 'raise-arguments-error*
                                who-in realm what exn:fail:contract more))
    
    (define do-raise-arguments-error
      (let ()
        (define (string-insert-indentation str i-str)
          (apply
           string-append
           (let loop ([start 0] [i 0])
             (cond
               [(fx= i (string-length str))
                (list (substring str start i))]
               [(eqv? #\newline (string-ref str i))
                (list* (substring str start i)
                       "\n"
                       i-str
                       (loop (fx+ i 1) (fx+ i 1)))]
               [else
                (loop start (fx+ i 1))]))))
        (define (reindent s amt)
          (if (and (string-has-newline? s)
                   (not (string-starts-with-newline? s)))
              (string-insert-indentation s (make-string amt #\space))
              s))

        (define (reindent/newline str)
          (if (and (string-has-newline? str)
                   (not (string-starts-with-newline? str)))
              (string-append "\n   " (string-insert-indentation str "   "))
              str))
        
        (λ (e-who who realm what exn:fail:contract more)
          #;(check e-who symbol? who)
          #;(check e-who symbol? realm)
          #;(check e-who string? what)
          (raise
           (exn:fail:contract
            (error-message->adjusted-string
             who realm
             (apply
              string-append
              what
              (let loop ([more more])
                (cond
                  [(null? more) '()]
                  [(string? (car more))
                   (cond
                     [(null? (cdr more))
                      (raise-arguments-error e-who
                                             "missing value after field string"
                                             "string"
                                             (car more))]
                     [else
                      (cons (string-append "\n  "
                                           (car more) ": "
                                           (let ([val (cadr more)])
                                             (reindent/newline
                                              (if (unquoted-printing-string? val)
                                                  (unquoted-printing-string-value val)
                                                  (error-value->string val)))))
                            (loop (cddr more)))])]
                  [else
                   (raise-argument-error e-who "string?" (car more))])))
             realm)
            (current-continuation-marks))))))


    (define (raise-range-error name type-description index-prefix index in-value lower-bound upper-bound
                               [alt-lower-bound #f])
      (do-raise-range-error name 'racket type-description index-prefix index in-value lower-bound upper-bound
                            alt-lower-bound))

    (define (raise-range-error* name realm type-description index-prefix index in-value lower-bound upper-bound
                                [alt-lower-bound #f])
      (do-raise-range-error name realm type-description index-prefix index in-value lower-bound upper-bound
                            alt-lower-bound))

    (define (do-raise-range-error name realm type-description index-prefix index in-value lower-bound upper-bound
                                  alt-lower-bound)

      (define index-word (string-append index-prefix "index"))
      (define who-str (symbol->string name))
      (define idx-str (number->string index))
      (define lower-str (number->string lower-bound))
      (define upper-str (number->string upper-bound))
      (define range-empty? (< upper-bound lower-bound))
      (define alt-case?
        (and (not range-empty?)
             alt-lower-bound
             (not (eq? alt-lower-bound #f))
             (<= alt-lower-bound index)
             (<= index upper-bound)
             (< index lower-bound)))
      (define type-suffix
        (cond
          [(string=? type-description "") ""]
          [range-empty? (string-append " for empty " type-description)]
          [else (string-append " for " type-description)]))
      (define headline
        (cond
          [alt-case?
           (string-append who-str ": " index-word " " idx-str
                          " is less than the starting index " lower-str
                          type-suffix)]
          [else
           (string-append who-str ": " index-word " " idx-str
                          " out of range" type-suffix)]))
      (define range-line
        (string-append "  range: [" lower-str ", " upper-str "]"))
      (define detail-lines
        (list (string-append "  " index-word ": " idx-str)
              (format-field "  in: " (value->string in-value))
              range-line))
      (define lines
        (if alt-case?
            (append (list headline)
                    detail-lines
                    (list (string-append "  starting index: " lower-str)))
            (cons headline detail-lines)))
      (define message (string-append* (add-between lines "\n")))
      (raise (make-exn:fail:contract
              (error-message->adjusted-string name realm message realm)
              (current-continuation-marks))))


    (define (make-raise-value-error error-name singular plural)
      (define (join-with-newline strs)
        (if (null? strs)
            ""
            (string-append* (add-between strs "\n"))))

      (define (ordinal pos)
        (define n (add1 pos))
        (define tail (modulo n 100))
        (define suffix
          (if (and (>= tail 11) (<= tail 13))
              "th"
              (case (modulo n 10)
                [(1) "st"]
                [(2) "nd"]
                [(3) "rd"]
                [else "th"])))
        (string-append (number->string n) suffix))

      (define (split-at-index lst idx)
        (let loop ([rest lst] [i 0] [before '()])
          (cond
            [(null? rest)
             (error error-name
                    (string-append "bad " singular " position ~a for ~a " plural)
                    idx i)]
            [(= i idx)
             (values (reverse before) (car rest) (cdr rest))]
            [else
             (loop (cdr rest) (add1 i) (cons (car rest) before))])))

      (define (base-lines who expected-str given-str)
        (list (string-append (symbol->string who) ": contract violation")
              (format-field "  expected: " expected-str)
              (format-field "  given: " given-str)))

      (define (other-values-lines others)
        (if (null? others)
            '()
            (list (string-append
                   "  other " plural "...:\n"
                   (indent-lines (join-with-newline
                                  (map value->string others)))))))

      (define (render-message lines)
        (string-append* (add-between lines "\n")))

      (case-lambda
        [(who expected v)
         (define message
           (render-message (base-lines who expected (value->string v))))
         (raise (make-exn:fail:contract message #f))]
        [(who expected bad-pos . vs)
         (unless (and (exact-integer? bad-pos) (>= bad-pos 0))
           (error error-name
                  (string-append "bad " singular " position ~a")
                  bad-pos))
         (define-values (before target after) (split-at-index vs bad-pos))
         (define others (append before after))
         (define lines
           (append (base-lines who expected (value->string target))
                   (list (string-append "  " singular " position: "
                                        (ordinal bad-pos)))
                   (other-values-lines others)))
         (raise (make-exn:fail:contract (render-message lines) #f))]))

    (define raise-argument-error
      (make-raise-value-error 'raise-argument-error "argument" "arguments"))

    (define raise-result-error
      (make-raise-value-error 'raise-result-error "result" "results"))


    (values primitive-realm
            error
            raise-argument-error
            raise-argument-error*
            raise-arguments-error
            raise-arguments-error*
            raise-range-error
            raise-range-error*
            raise-result-error
            
            error-message->adjusted-string
            error-value->string)
    ))


