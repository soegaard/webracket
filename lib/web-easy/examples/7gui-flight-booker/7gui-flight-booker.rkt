;;;
;;;7 GUI - Flight Booker 
;;;

;; https://eugenkiss.github.io/7guis/tasks#flight

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)


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

;; For this program we only need "today" and "tomorrow" initially.
#;(define (today/date)
  (define d (current-date))
  (flight-date (date-year d) (date-month d) (date-day d)))

(define (today/date)
  (flight-date 2026 3 16))

#;(define (tomorrow/date)
  ;; Safe enough for initializing the default value.
  ;; We step via seconds instead of implementing full date arithmetic.
  (define d (seconds->date (+ (current-seconds) (* 24 60 60))))
  (flight-date (date-year d) (date-month d) (date-day d)))

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

(define @type    (obs 'one-way                'type))
(define @t1      (obs (~date (today/date))    't1))
(define @t2      (obs (~date (tomorrow/date)) 't2))
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

;; ----------------------------------------
;; View helpers
;; ----------------------------------------

(define (input-style valid?)
  (if valid?
      "width: 100%;"
      "width: 100%; background-color: #ffdddd;"))

(define (book-button)
  (observable-view
   @valid?
   (lambda (valid?)
     (button "Book"
             (if valid?
                 (lambda () (obs-set! @booked? #t))
                 (lambda () (void)))
             #:attrs
             (if valid?
                 '()
                 '((disabled . "disabled")))))))

(define (return-date-input)
  (observable-view
   @type
   (lambda (type)
     (define valid? (not (not (string->date (obs-peek @t2)))))
     (input @t2
            (lambda (text) (obs-set! @t2 text))
            #:input-attrs
            (append
             (list (cons 'placeholder "yyyy.MM.dd")
                   (cons 'style       (input-style valid?)))
             (if (eq? type 'return)
                 '()
                 '((disabled . "disabled"))))))))


(define 7gui-flight-booker-app
  (window
   (container
    (h1 "Flight Booker")
    (group "Book Flight"
           (container
            (vpanel
             (choice flight-types
                     @type
                     (lambda (new-type) (obs-set! @type new-type)))
             
             (observable-view
              @t1
              (lambda (t1)
                (input t1
                       (lambda (text) (obs-set! @t1 text))
                       #:input-attrs
                       (list (cons 'placeholder "yyyy.MM.dd")
                             (cons 'style       (input-style (string->date t1)))))))
             
             (observable-view
              @type
              (lambda (type)
                (define valid? (string->date (obs-peek @t2)))
                (input @t2
                       (lambda (text) (obs-set! @t2 text))
                       #:input-attrs
                       (append
                        (list (cons 'placeholder "yyyy.MM.dd")
                              (cons 'style       (input-style valid?)))
                        (if (eq? type 'return)
                            '()
                            '((disabled . "disabled")))))))

             ; (return-date-input)

             (observable-view
              @valid?
              (lambda (valid?)
                (if valid?
                    (text "")
                    (alert "Use yyyy.MM.dd. For return flights, the return date must be later."
                           'danger))))

             (book-button))))
    
    (dialog @booked?
            (lambda () (obs-set! @booked? #f))            
            (text @booking-message)
            #:title "Flight Booked"
            #:footer
            (hpanel
             (spacer)
             (button "OK" (lambda () (obs-set! @booked? #f))))))))

  
(define app-renderer
  (render 7gui-flight-booker-app))



;;;
;;; Themes
;;;

;; Note: The paths to the theme css files assume the
;;       web-server was started in lib/web-easy/

;; install-theme-link! : string? -> any/c
;;   Create and attach a stylesheet link element in <head>.
(define (install-theme-link! link-id)
  (define doc  (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id"  link-id)
  (js-set-attribute! link "rel" "stylesheet")
  (js-append-child! head link)
  link)

;; apply-light-theme! : any/c any/c -> void?
;;   Set root class and stylesheet hrefs for Light theme.
(define (apply-light-theme! core-link light-link)
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (js-set-attribute! html-node  "class" "we-theme-light")
  (js-set-attribute! core-link  "href"  "web-easy-core.css")
  (js-set-attribute! light-link "href"  "theme-external-light.css")
  (void))

(define theme-core-link-node  (install-theme-link! "we-theme-core-css"))
(define theme-light-link-node (install-theme-link! "we-theme-external-css"))
(apply-light-theme! theme-core-link-node theme-light-link-node)

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
