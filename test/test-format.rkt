;; Test suite for format (stdlib)
;; Run with: racket -t ../webracket.rkt -- -r test-format.rkt

(require (for-syntax racket/file))

(define (format-test expected form . vs)
  (string=? (apply format form vs) expected))

(define (format-error? thunk expected-fragment)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (string-contains? (exn-message e) expected-fragment))])
    (thunk)
    #f))

(list
 (list "13.5 Writing"
       (list "format"
             (list "literal output"
                   (and (format-test "hello" "hello")
                        (format-test "~" "~~")
                        (format-test "line1\nline2" "line1~%line2")
                        (format-test "line1\nline2" "line1~nline2")
                        (format-test "ab" "a~\n  b")
                        (format-test "ab" "a~   b")))

             (list "value directives"
                   (and (format-test "sample" "~a" 'sample)
                        (format-test "\"a\"" "~s" "a")
                        (format-test "'(1 2)" "~v" '(1 2))
                        (format-test "(1 a)" "~a" '(1 "a"))
                        (format-test "\"a\"" "~S" "a")
                        (format-test "sample" "~A" 'sample)
                        (format-test "'(1 2)" "~V" '(1 2))))

             (list "numeric directives"
                   (and (format-test "101" "~b" 5)
                        (format-test "11" "~o" 9)
                        (format-test "1f" "~x" 31)
                        (format-test "1F" "~X" 31)
                        (format-test "101" "~B" 5)
                        (format-test "11" "~O" 9)))

             (list "characters"
                   (and (format-test "A" "~c" #\A)
                        (format-test "Z" "~C" #\Z)))

             (list "truncation"
                   (let ([original (error-print-width)])
                     (error-print-width 5)
                     (let ([ok (and (format-test "ab..." "~.a" "abcdef")
                                    (format-test "\"a..." "~.s" "abcdef"))])
                       (error-print-width original)
                       ok)))

             (list "error handler"
                   (let ([original (error-value->string-handler)])
                     (error-value->string-handler (lambda (value width) "error"))
                     (let ([ok (format-test "error" "~e" 'ignored)])
                       (error-value->string-handler original)
                       ok)))

             (list "errors"
                   (and (format-error? (lambda () (format "~"))
                                       "dangling")
                        (format-error? (lambda () (format "~q" 1))
                                       "unknown format directive")
                        (format-error? (lambda () (format "~a"))
                                       "missing argument")
                        (format-error? (lambda () (format "~a" 1 2))
                                       "unused arguments")
                        (format-error? (lambda () (format "~c" 1))
                                       "expects a character")
                        (format-error? (lambda () (format "~b" 1.5))
                                       "exact integer")
                        (format-error? (lambda () (format 123 1))
                                       "expected format string"))))))
