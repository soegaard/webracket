#lang webracket
;;; ============================================================
;;;  ~r  —  number formatter
;;;  A faithful re-implementation of racket/format's ~r
;;; ============================================================

;; This file is prepared for the day, WebRacket gets support for
;; keyword arguments.

(define (~r/precision x precision)
  (unless (number? x)
    (error '~r/precision
           (format "expected a number as first argument, got: ~a" x)))
  (unless (integer? precision)
    (error '~r/precision
           (format "expected an integer as second argument, got: ~a" precision)))
  
  (~r/all x #f #f precision #f #f #f #f #f #f #f))


;; (define (~r x
;;             #:sign            [sign            #f]
;;             #:base            [base            10]
;;             #:precision       [precision        6]
;;             #:notation        [notation        'positional]
;;             #:format-exponent [format-exponent #f]
;;             #:min-width       [min-width        1]
;;             #:pad-string      [pad-string      " "]
;;             #:groups          [groups          '(3)]
;;             #:group-sep       [group-sep       ""]
;;             #:decimal-sep     [decimal-sep     "."])
  
(define (~r/all x
                sign            
                base            
                precision       
                notation        
                format-exponent 
                min-width       
                pad-string      
                groups          
                group-sep       
                decimal-sep)     
  (unless base        (set! base        10))
  (unless precision   (set! precision   6))
  (unless notation    (set! notation    'positional))
  (unless min-width   (set! min-width   1))
  (unless pad-string  (set! pad-string  " "))
  (unless groups      (set! groups      '(3)))
  (unless group-sep   (set! group-sep   ""))
  (unless decimal-sep (set! decimal-sep "."))

  ;; ---- helpers ------------------------------------------------

  (define-values (base-int upper-case?)
    (if (pair? base)
        (values (cadr base) #t)
        (values base        #f)))

  (define (digit->char d)
    (cond [(< d 10)     (integer->char (+ d (char->integer #\0)))]
          [upper-case?  (integer->char (+ (- d 10) (char->integer #\A)))]
          [else         (integer->char (+ (- d 10) (char->integer #\a)))]))

  (define (nat->digits-base10 n)
    (if (zero? n) "0"
        (let loop ([n n] [acc '()])
          (if (zero? n)
              (list->string acc)
              (loop (quotient n 10)
                    (cons (integer->char (+ (remainder n 10) (char->integer #\0))) acc))))))

  (define (nat->digits n)
    (if (zero? n) "0"
        (let loop ([n n] [acc '()])
          (if (zero? n)
              (list->string acc)
              (loop (quotient n base-int)
                    (cons (digit->char (remainder n base-int)) acc))))))

  ;; ---- resolve notation --------------------------------------
  (define actual-notation
    (if (procedure? notation) (notation x) notation))

  ;; ---- sign handling -----------------------------------------
  (define (sign-fixes negative? zero?)
    (cond
      [(eq? sign #f)
       (if negative? (values "-" "") (values "" ""))]
      [(eq? sign '+)
       (cond [negative? (values "-" "")]
             [zero?     (values "" "")]
             [else      (values "+" "")])]
      [(eq? sign '++)
       (if negative? (values "-" "") (values "+" ""))]
      [(eq? sign 'parens)
       (if negative? (values "(" ")") (values "" ""))]
      [(list? sign)
       (let* ([ind (cond [negative? (caddr sign)]
                         [zero?     (cadr  sign)]
                         [else      (car   sign)])]
              [ind (if (list? ind) ind (list ind ""))])
         (values (car ind) (cadr ind)))]
      [else (error "~r: bad #:sign value" sign)]))

  ;; ---- string-trimr helper -----------------------------------
  (define (string-trimr s c)
    (let loop ([i (string-length s)])
      (if (or (zero? i) (not (char=? (string-ref s (- i 1)) c)))
          (substring s 0 i)
          (loop (- i 1)))))

  ;; ---- positional formatting ---------------------------------
  ;; abs-x is a non-negative flonum or exact integer; do NOT floor before calling.
  (define (format-positional abs-x)
    (define-values (exact-prec fixed?)
      (if (pair? precision)
          (values (cadr precision) #t)
          (values precision        #f)))

    (define scale-exact (expt base-int exact-prec))

    ;; For exact integers, avoid flonum conversion which loses precision.
    ;; For flonums, do arithmetic in flonum then convert only the integer result.
    (define-values (int-part frac-scaled)
      (if (exact? abs-x)
          (let* ([scaled (+ (* abs-x scale-exact))])  ; exact arithmetic, no rounding needed
            (values (quotient scaled scale-exact)
                    (remainder scaled scale-exact)))
          (let* ([scale-fl  (exact->inexact scale-exact)]
                 [scaled    (inexact->exact (floor (+ (* (exact->inexact abs-x) scale-fl) 0.5)))]
                 [se        (inexact->exact scale-fl)])
            (values (quotient scaled se)
                    (remainder scaled se)))))

    (define int-str (nat->digits int-part))

    (define frac-raw
      (if (zero? exact-prec) ""
          (let* ([s   (nat->digits frac-scaled)]
                 [pad (- exact-prec (string-length s))])
            (string-append (make-string (max 0 pad) #\0) s))))

    (define frac-str
      (if fixed? frac-raw (string-trimr frac-raw #\0)))

    (values int-str frac-str fixed?))

  ;; ---- group separators on the integer part ------------------
  (define (apply-groups int-str)
    (if (string=? group-sep "")
        int-str
        (let* ([chars  (string->list int-str)]
               [n      (length chars)]
               [gsizes (let loop ([rem n] [gs (reverse groups)] [acc '()])
                         (if (<= rem 0) acc
                             (let ([g (car gs)])
                               (loop (- rem g)
                                     (if (null? (cdr gs)) gs (cdr gs))
                                     (cons (min g rem) acc)))))]
               [slices (let loop ([chars chars] [gsizes gsizes] [acc '()])
                         (if (null? gsizes)
                             (reverse acc)
                             (let ([g (car gsizes)])
                               (loop (list-tail chars g)
                                     (cdr gsizes)
                                     (cons (list->string (take chars g)) acc)))))])
          (string-join slices group-sep))))

  ;; ---- exponential formatting --------------------------------
  (define (format-exponential abs-x)
    ;; Compute exponent; keep divisions in flonum
    (define e-raw
      (cond [(zero? abs-x) 0]
            [else
             (let* ([lg   (/ (log (exact->inexact abs-x)) (log base-int))]
                    [e0   (inexact->exact (floor lg))]
                    [sig0 (/ (exact->inexact abs-x) (exact->inexact (expt base-int e0)))])
               (cond [(>= sig0 base-int) (+ e0 1)]
                     [(<  sig0 1)        (- e0 1)]
                     [else                  e0]))]))

    (define sig-raw (/ (exact->inexact abs-x) (exact->inexact (expt base-int e-raw))))

    ;; Check if rounding will overflow the significand to base; if so bump exponent
    (define-values (exact-prec _fixed?)
      (if (pair? precision)
          (values (cadr precision) #t)
          (values precision        #f)))
    (define scale-fl  (exact->inexact (expt base-int exact-prec)))
    (define sig-scaled (inexact->exact (floor (+ (* sig-raw scale-fl) 0.5))))

    (define-values (e sig)
      (if (>= sig-scaled (inexact->exact (* (exact->inexact base-int) scale-fl)))
          (values (+ e-raw 1)
                  (/ (exact->inexact abs-x) (exact->inexact (expt base-int (+ e-raw 1)))))
          (values e-raw sig-raw)))

    (define-values (sig-int sig-frac fixed?) (format-positional sig))

    (define sig-str
      (if (and (string=? sig-frac "") (not fixed?))
          sig-int
          (string-append sig-int decimal-sep sig-frac)))

    (define exp-str
      (cond
        [(procedure? format-exponent)
         (format-exponent e)]
        [(string? format-exponent)
         (string-append
          format-exponent
          (if (>= e 0) "+" "-")
          (let ([s (nat->digits-base10 (abs e))])
            (if (< (string-length s) 2) (string-append "0" s) s)))]
        [else
         (define marker
           (if (= base-int 10) "e" (format "*~a^" base-int)))
         (string-append
          marker
          (if (>= e 0) "+" "-")
          (let ([s (nat->digits-base10 (abs e))])
            (if (< (string-length s) 2) (string-append "0" s) s)))]))

    (string-append sig-str exp-str))

  ;; ---- assemble the final string ------------------------------
  (define negative? (< x 0))
  (define is-zero?  (zero? x))
  (define abs-x     (if (< x 0) (- x) x))   ; no floor — preserve the fraction

  (define core-str
    (case actual-notation
      [(positional)
       (define-values (int-str frac-str fixed?) (format-positional abs-x))
       (define grouped-int (apply-groups int-str))
       (if (and (string=? frac-str "") (not fixed?))
           grouped-int
           (string-append grouped-int decimal-sep frac-str))]
      [(exponential)
       (format-exponential abs-x)]
      [else (error "~r: bad notation" actual-notation)]))

  (define-values (prefix suffix) (sign-fixes negative? is-zero?))

  (define padded-core
    (let ([deficit (- min-width (string-length core-str))])
      (if (<= deficit 0)
          core-str
          (let* ([reps    (ceiling (/ deficit (string-length pad-string)))]
                 [padding (substring (apply string-append
                                            (make-list reps pad-string))
                                     0 deficit)])
            (string-append padding core-str)))))

  (string-append prefix padded-core suffix))
  
;; (require rackunit)

;; (define-syntax-rule (tc expr expected)
;;   (test-equal? (format "~s" 'expr) expr expected))

;; ;; ~r

;; (tc (~r 0)
;;     "0")
;; (tc (~r pi)
;;     "3.141593")
;; (tc (~r pi #:precision 4)
;;     "3.1416")
;; (tc (~r pi #:precision 0)
;;     "3")
;; (tc (~r 1.5 #:precision 4)
;;     "1.5")
;; (tc (~r 1.5 #:precision '(= 4))
;;     "1.5000")
;; (tc (~r 50 #:precision 2)
;;     "50")
;; (tc (~r 50 #:precision '(= 2))
;;     "50.00")
;; (tc (~r 50 #:precision '(= 0))
;;     "50.")

;; (tc (~r 17)
;;     "17")
;; (tc (~r 17 #:min-width 4)
;;     "  17")
;; (tc (~r -42 #:min-width 4)
;;     "-  42")
;; (tc (~r 1.5 #:min-width 4)
;;     " 1.5")
;; (tc (~r 1.5 #:precision 4 #:min-width 10)
;;     "       1.5")
;; (tc (~r 1.5 #:precision '(= 4) #:min-width 10)
;;     "    1.5000")

;; (tc (~r -42 #:min-width 4 #:pad-string "0")
;;     "-0042")

;; (tc (~r 17 #:min-width 4 #:pad-string "0")
;;     "0017")
;; (tc (~r -42 #:min-width 4 #:pad-string "0")
;;     "-0042")

;; (tc (for/list ([x '(17 0 -42)]) (~r x))
;;     '("17" "0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r x #:sign '+))
;;     '("+17" "0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r x #:sign '++))
;;     '("+17" "+0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r x #:sign 'parens))
;;     '("17" "0" "(42)"))
;; (tc (let ([sign-table '(("" " up") "an even " ("" " down"))])
;;       (for/list ([x '(17 0 -42)]) (~r x #:sign sign-table)))
;;     '("17 up" "an even 0" "42 down"))

;; (tc (~r 100 #:base 7)
;;     "202")
;; (tc (~r 102 #:base 7)
;;     "204")
;; (tc (~r 4.5 #:base 2)
;;     "100.1")
;; (tc (~r 3735928559 #:base 16)
;;     "deadbeef")
;; (tc (~r 3735928559 #:base '(up 16))
;;     "DEADBEEF")
;; (tc (~r (+ 102 1/7 2/49 3/343) #:base 7)
;;     "204.123")

;; (tc (~r 999 #:precision 3)
;;     "999")
;; (tc (~r 1000 #:precision 3)
;;     "1000")

;; ;; ~r #:notation 'positional

;; (tc (~r #:notation 'positional pi)
;;     "3.141593")
;; (tc (~r #:notation 'positional pi #:precision 4)
;;     "3.1416")
;; (tc (~r #:notation 'positional pi #:precision 0)
;;     "3")
;; (tc (~r #:notation 'positional 1.5 #:precision 4)
;;     "1.5")
;; (tc (~r #:notation 'positional 1.5 #:precision '(= 4))
;;     "1.5000")
;; (tc (~r #:notation 'positional 50 #:precision 2)
;;     "50")
;; (tc (~r #:notation 'positional 50 #:precision '(= 2))
;;     "50.00")
;; (tc (~r #:notation 'positional 50 #:precision '(= 0))
;;     "50.")

;; (tc (~r #:notation 'positional 17)
;;     "17")
;; (tc (~r #:notation 'positional 17 #:min-width 4)
;;     "  17")
;; (tc (~r #:notation 'positional -42 #:min-width 4)
;;     "-  42")
;; (tc (~r #:notation 'positional 1.5 #:min-width 4)
;;     " 1.5")
;; (tc (~r #:notation 'positional 1.5 #:precision 4 #:min-width 10)
;;     "       1.5")
;; (tc (~r #:notation 'positional 1.5 #:precision '(= 4) #:min-width 10)
;;     "    1.5000")

;; (tc (~r #:notation 'positional -42 #:min-width 4 #:pad-string "0")
;;     "-0042")

;; (tc (~r #:notation 'positional 17 #:min-width 4 #:pad-string "0")
;;     "0017")
;; (tc (~r #:notation 'positional -42 #:min-width 4 #:pad-string "0")
;;     "-0042")

;; (tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x))
;;     '("17" "0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign '+))
;;     '("+17" "0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign '++))
;;     '("+17" "+0" "-42"))
;; (tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign 'parens))
;;     '("17" "0" "(42)"))
;; (tc (let ([sign-table '(("" " up") "an even " ("" " down"))])
;;       (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign sign-table)))
;;     '("17 up" "an even 0" "42 down"))

;; (tc (~r #:notation 'positional 102 #:base 7)
;;     "204")
;; (tc (~r #:notation 'positional 4.5 #:base 2)
;;     "100.1")
;; (tc (~r #:notation 'positional 3735928559 #:base 16)
;;     "deadbeef")
;; (tc (~r #:notation 'positional 3735928559 #:base '(up 16))
;;     "DEADBEEF")

;; (tc (~r #:notation 'positional 0)
;;     "0")
;; (tc (~r #:notation 'positional 0 #:precision 4)
;;     "0")
;; (tc (~r #:notation 'positional 0 #:precision '(= 4))
;;     "0.0000")

;; ;; ~r #:notation 'exponential

;; (tc (~r 12345 #:precision 3 #:notation 'exponential)
;;     "1.235e+04")
;; (tc (~r 12345 #:precision 2 #:notation 'exponential)
;;     "1.23e+04")
;; (tc (~r 10000 #:precision 2 #:notation 'exponential)
;;     "1e+04")
;; (tc (~r 10000 #:precision '(= 2) #:notation 'exponential)
;;     "1.00e+04")

;; (tc (~r 12345 #:precision 4 #:min-width 12 #:notation 'exponential)
;;     "  1.2345e+04")

;; (tc (~r #:notation 'exponential 1000)
;;     "1e+03")
;; (tc (~r #:notation 'exponential 0.9876)
;;     "9.876e-01")

;; (tc (~r #:notation 'exponential 100 #:base 2)
;;     "1.1001*2^+06")

;; (tc (~r #:notation 'exponential 1234 #:format-exponent "E")
;;     "1.234E+03")

;; (tc (~r #:notation 'exponential 12345 #:precision 3)
;;     ;; note rounding!
;;     "1.235e+04")
;; (tc (~r #:notation 'exponential 12345 #:precision 2)
;;     "1.23e+04")
;; (tc (~r #:notation 'exponential 10000 #:precision 2)
;;     "1e+04")
;; (tc (~r #:notation 'exponential 10000 #:precision '(= 2))
;;     "1.00e+04")

;; (tc (~r #:notation 'exponential 12345 #:min-width 12)
;;     "  1.2345e+04")

;; (tc (~r 3735928559 #:base '(up 16) #:precision 6 #:notation 'exponential)
;;     ;; note rounding!
;;     "D.EADBEF*16^+07")

;; (tc (~r 33.99508664763296 #:precision 1 #:min-width 5)
;;     "   34")
;; (tc (~r 33.99508664763296 #:precision 2 #:min-width 7)
;;     "     34")

;; (tc (~r 33.99508664763296 #:precision 1)
;;     "34")
;; (tc (~r 33.99508664763296 #:precision '(= 1))
;;     "34.0")
;; (tc (~r 33.99508664763296 #:precision '(= 2))
;;     "34.00")
;; (tc (~r 33.99508664763296 #:precision '(= 3))
;;     "33.995")

;; (tc (~r -33.99508664763296 #:precision 1)
;;     "-34")
;; (tc (~r -33.99508664763296 #:precision '(= 1))
;;     "-34.0")
;; (tc (~r -33.99508664763296 #:precision '(= 2))
;;     "-34.00")
;; (tc (~r -33.99508664763296 #:precision '(= 3))
;;     "-33.995")

;; (tc (~r #:notation 'exponential 0)
;;     "0e+00")
;; (tc (~r #:notation 'exponential 0 #:precision 4)
;;     "0e+00")
;; (tc (~r #:notation 'exponential 0 #:precision '(= 4))
;;     "0.0000e+00")
;; (tc (~r #:notation 'exponential 0 #:precision '(= 4) #:decimal-sep ",")
;;     "0,0000e+00")
;; (tc (~r 12345678.123456 #:notation 'exponential #:decimal-sep ",")
;;     "1,234568e+07")
;; (tc (~r 12345678.123456 #:notation 'exponential #:decimal-sep "::")
;;     "1::234568e+07")

;; (tc (~r 123456789.123)
;;     "123456789.123")
;; (tc (~r 1023456789.123 #:groups '(3) #:group-sep "," #:decimal-sep ".")
;;     "1,023,456,789.123")
;; (tc (~r 21231234567890 #:groups '(4 2 3) #:group-sep "_")
;;     "2_1231_2345_67_890")
;; (tc (~r 1234567890.123 #:groups '(3 2) #:group-sep "**" #:decimal-sep "::")
;;     "12**345**678**90::123")


;; ;; some random testing for exponential notation
;; ;;  - only positive numbers
;; ;;  - limited number of digits, exponent range

;; (define (random-in low hi) ;; closed
;;   (+ low (random (- hi low -1))))

;; (define (digits->int digits base)
;;   (for/fold ([acc 0]) ([digit (in-list digits)])
;;     (+ (* base acc) digit)))

;; (define (random-exponential-check #:base [base0 #f] #:precision [precision0 #f])
;;   (define base (or base0 (random-in 2 36)))
;;   (define precision (or precision0 (random-in 0 5)))
;;   (define digit0 (random-in 1 (sub1 base)))
;;   (define digits (for/list ([i (in-range precision)])
;;                    (random-in 0 (sub1 base))))
;;   (define exponent (random-in -50 50))
;;   (define exact-num
;;     (* (digits->int (cons digit0 digits) base)
;;        (expt base (- exponent precision))))
;;   (define inexact-num (exact->inexact exact-num))
;;   (define check-exp (make-exp-checker base precision digit0 digits exponent))
;;   (define (fmt n exactly?)
;;     (~r n #:notation 'exponential #:base base #:format-exponent ";"
;;         #:precision (if exactly? `(= ,precision) precision)))
;;   (check-exp (fmt exact-num #t) #f)
;;   (check-exp (fmt exact-num #f) #f)
;;   (check-exp (fmt inexact-num #t) #t)
;;   (check-exp (fmt inexact-num #f) #f))

;; (define (make-exp-checker base precision digit0 digits exponent)
;;   (lambda (s exactly?)
;;     (with-handlers ([void
;;                      (lambda (e)
;;                        (eprintf "failed on base=~s, prec=~s, digits=~s,~s, exp=~s; got ~s\n"
;;                                 base precision digit0 digits exponent s)
;;                        (raise e))])
;;       (cond [(regexp-match #rx"^([a-z0-9])(?:\\.([a-z0-9]*))?;([+-][0-9]+)$" s)
;;              => (lambda (m)
;;                   (check-pred pair? m)
;;                   ;; Check leading digit is good.
;;                   (check-equal? (second m) (~r digit0 #:base base))
;;                   (define got-digits (map string (string->list (or (third m) ""))))
;;                   (define want-digits (for/list ([d digits]) (~r d #:base base)))
;;                   (check (if exactly? = <=) (length got-digits) (length want-digits))
;;                   ;; Check digits we got are good
;;                   (for ([got got-digits]
;;                         [want want-digits])
;;                     (check-equal? got want))
;;                   ;; If we didn't get as many digits as wanted, check rest are 0
;;                   (for ([want-more (drop want-digits (length got-digits))])
;;                     (check-equal? "0" want-more))
;;                   ;; Check exponent
;;                   (check-equal? (string->number (fourth m)) exponent))]
;;             [else (error 'exp-checker "bad: ~s" s)]))))

;; (for ([i (in-range 10000)])
;;   (random-exponential-check #:base 10 #:precision 10))

;; (for ([i (in-range 10000)])
;;   (random-exponential-check))
