#lang webracket

;;;
;;; Exceptions
;;;

;; The exceptions structures are defined in the runtime.
;; Functions that need `format` goes here.


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


; A simpler version is available as primitive
(define raise-argument-error
  (let ()
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
           (error 'raise-argument-error
                  "bad argument position ~a for ~a arguments"
                  idx i)]
          [(= i idx)
           (values (reverse before) (car rest) (cdr rest))]
          [else
           (loop (cdr rest) (add1 i) (cons (car rest) before))])))

    (define (base-lines who expected-str given-str)
      (list (string-append (symbol->string who) ": contract violation")
            (format-field "  expected: " expected-str)
            (format-field "  given: " given-str)))

    (define (other-arguments-lines others)
      (if (null? others)
          '()
          (list (string-append
                 "  other arguments...:\n"
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
         (error 'raise-argument-error "bad argument position ~a" bad-pos))
       (define-values (before target after) (split-at-index vs bad-pos))
       (define others (append before after))
       (define lines
         (append (base-lines who expected (value->string target))
                 (list (string-append "  argument position: "
                                      (ordinal bad-pos)))
                 (other-arguments-lines others)))
       (raise (make-exn:fail:contract (render-message lines) #f))])))



