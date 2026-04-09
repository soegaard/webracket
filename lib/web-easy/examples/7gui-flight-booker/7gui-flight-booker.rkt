;;;
;;;7 GUI - Flight Booker 
;;;

;; https://eugenkiss.github.io/7guis/tasks#flight

(include-lib web-easy)


;; ----------------------------------------
;; Dates (replacement for gregor)          
;; ----------------------------------------

(struct flight-date (year month day) #:transparent)

(define (date<? a b)
  (cond
    [(< (flight-date-year  a) (flight-date-year  b)) #t]
    [(> (flight-date-year  a) (flight-date-year  b)) #f]
    [(< (flight-date-month a) (flight-date-month b)) #t]
    [(> (flight-date-month a) (flight-date-month b)) #f]
    [else (< (flight-date-day a) (flight-date-day b))]))

(define (today/date)
  (flight-date 2026 3 16))

(define (tomorrow/date)
  (flight-date 2026 3 17))

(define (leap-year? y)
  (or (zero? (remainder y 400))
      (and (zero? (remainder y 4))
           (not (zero? (remainder y 100))))))

(define (days-in-month y m)
  (case m
    [(1 3 5 7 8 10 12)  31]
    [(4 6 9 11)         30]
    [(2)                (if (leap-year? y) 29 28)]
    [else                0]))

(define (pad2 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (~date d)
  (string-append
   (number->string (flight-date-year d))
   "."
   (pad2 (flight-date-month d))
   "."
   (pad2 (flight-date-day d))))

;;; -- string to date

(define (digit-value ch)
  (define n (- (char->integer ch) (char->integer #\0)))
  (and (<= 0 n 9) n))

(define (parse-2digits s start)
  (and (< (+ start 1) (string-length s))
       (let ([d1 (digit-value (string-ref s    start))]
             [d2 (digit-value (string-ref s (+ start 1)))])
         (and d1 d2 (+ (* d1 10) d2)))))

(define (parse-4digits s start)
  (and (< (+ start 3) (string-length s))
       (let ([d1 (digit-value (string-ref s    start))]
             [d2 (digit-value (string-ref s (+ start 1)))]
             [d3 (digit-value (string-ref s (+ start 2)))]
             [d4 (digit-value (string-ref s (+ start 3)))])
         (and d1 d2 d3 d4
              (+ (* d1 1000)
                 (* d2 100)
                 (* d3 10)
                 d4)))))

(define (string->date s)
  (and (= (string-length s) 10)
       (char=? (string-ref s 4) #\.)
       (char=? (string-ref s 7) #\.)
       (let* ([y  (parse-4digits s 0)]
              [mo (parse-2digits s 5)]
              [d  (parse-2digits s 8)])
         (and y mo d
              (<= 1 mo 12)
              (<= 1 d (days-in-month y mo))
              (flight-date y mo d)))))

;; ----------------------------------------
;; App state
;; ----------------------------------------

(define flight-types
  '((one-way "one-way flight")
    (return  "return flight")))

;                     initial value           name (of the observable)
(define @type    (obs 'one-way                'type))    ; 
(define @t1      (obs (~date (today/date))    't1))      ; time 1 (home -> dest)
(define @t2      (obs (~date (tomorrow/date)) 't2))      ; time 2 (dest -> home)
(define @booked? (obs #f                      'booked?))

(define @valid?
  (obs-combine
   (lambda (type t1 t2)
     (case type
       [(one-way)  (and (string->date t1) #t)]
       [(return)   (define d1 (string->date t1))
                   (define d2 (string->date t2))
                   (and d1 d2 (date<? d1 d2))]))
   @type @t1 @t2))

(define @booking-message
  (obs-combine
   (lambda (type t1 t2)
     (case type
       [(one-way)
        (format "You've booked a one-way flight on ~a." t1)]
       [(return)
        (format "You've booked a return flight on ~a (returning on ~a)." t1 t2)]))
   @type @t1 @t2))

(define @return-disabled
  ; Use #f not "" to remove the 'disabled attribute.
  (@type . ~> . (λ (type) (eq? type 'one-way))))


;; ----------------------------------------
;; View helpers
;; ----------------------------------------

#;(define (input-style valid?)
  (if valid?
      "width: 100%; box-sizing: border-box;"
      "width: 100%; box-sizing: border-box; background-color: #ffdddd;"))

(define (input-style valid?)
  (if valid?
      ""
      "background-color: #ffdddd;")) ; light red


(define (book-button)
  (button "Book"
          (lambda () (:= @booked? #t))
          #:disabled (@valid? . ~> . (λ (valid) (not valid)))))

(define 7gui-flight-booker-app
  (window
   (container #:style "max-width: 320px;"
    (h1 "Flight Booker")
    (group "Book Flight"
           (vpanel
            ; Note: form elments are require to have an #:id or #:name
            (choice #:id "flight-type-choice"
                    flight-types
                    @type
                    (lambda (new-type) (obs-set! @type new-type)))

            ;; Away
            (input #:id "away-date-input"
                   @t1
                   (lambda (text) (obs-set! @t1 text))
                   #:input-attrs
                   (list (cons 'placeholder "yyyy.MM.dd")
                         (cons 'style       (@t1 . ~> . input-style) ; was(input-style (string->date t1))
                               )))
            ;; Return
            (input #:id "return-date-input"
                   @t2
                   (lambda (text) (obs-set! @t2 text))
                   #:style       (@t2 . ~> . (λ (t) (input-style (string->date t))))
                   #:disabled    @return-disabled
                   #:placeholder "yyyy.MM.dd")

            ; (return-date-input)
            (observable-view
             @valid?
             (lambda (valid?)
               (if valid?
                   (text "")
                   (alert "Use yyyy.MM.dd. For return flights, the return date must be later."
                          'danger))))

            ; In a <vpanel> the element stretch to the full width.
            ; We need the natural width of the Book button, so we put it in an <hpanel>.
            (hpanel (spacer) (book-button) (spacer)))))
    
    (dialog @booked?
            (lambda () (obs-set! @booked? #f))            
            (text @booking-message)
            #:title "Flight Booked"
            #:footer
            (hpanel
             (spacer)
             (button "OK" (lambda () (obs-set! @booked? #f)))))))

  
(define app-renderer
  (render 7gui-flight-booker-app))



;;;
;;; Themes
;;;

;; Note: compile.sh copies the required theme CSS files next to the
;;       generated HTML, so these stylesheet paths are relative to
;;       the generated/ output directory.

;; light-theme : theme?
;;   Shared light theme used by this example.
(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
