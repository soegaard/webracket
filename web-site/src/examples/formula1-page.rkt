;;;
;;; Formula 1 next race demo (WebRacket site page)
;;;

;;------------------------------------------------------------------------------
;; Program: Next F1 race finder (Better F1 Calendar)
;;
;; This page reuses the same parser and date logic as the tutorial file:
;;   web-site/src/examples/formula1/formula1.rkt
;;
;; We intentionally keep the tutorial comments and structure so readers can
;; follow the algorithm step-by-step and compare the standalone example with the
;; website integration.
;;
;; WEBSITE INTEGRATION OVERVIEW:
;; - Parse race events from the bundled ICS text.
;; - Read current UTC time from the browser (`Date.toISOString()`).
;; - Find the next race event.
;; - Render a human-friendly countdown:
;;   * before race day: show days remaining
;;   * on race day: show hours remaining
;;------------------------------------------------------------------------------

; (require (for-syntax racket/base racket/file))


;; event : structure
;; Represents one calendar event we care about, with a summary and a DTSTART.
(struct f1-event (summary start))

;;;
;;; DATES
;;;

;;------------------------------------------------------------------------------
;; utc-date
;;------------------------------------------------------------------------------

(struct utc-date (year month day hour minute second))

;;------------------------------------------------------------------------------
;; ICS Parsing
;;------------------------------------------------------------------------------

;; string-has-suffix? : String String -> Boolean
(define (string-has-suffix? s suffix)
  (define s-len (string-length s))
  (define suf-len (string-length suffix))
  (and (>= s-len suf-len)
       (string=? (substring s (- s-len suf-len) s-len) suffix)))

;; race-summary? : String -> Boolean
;; Keep only full Grand Prix race sessions (exclude sprint events).
(define (race-summary? summary)
  (and (string? summary)
       (string-contains? summary "Race")
       (not (string-contains? summary "Sprint"))))

;; normalize-line : String -> String
;; Strip a trailing CR to handle CRLF ICS files.
(define (normalize-line line)
  (define len (string-length line))
  (if (and (> len 0) (char=? (string-ref line (- len 1)) #\return))
      (substring line 0 (- len 1))
      line))

;; weekend-key : String -> String
;; Normalize a race-weekend summary to its base event name.
(define (weekend-key summary)
  (cond
    [(not (string? summary)) ""]
    [(string-has-suffix? summary " - Sprint Race")
     (substring summary 0 (- (string-length summary)
                             (string-length " - Sprint Race")))]
    [(string-has-suffix? summary " - Race")
     (substring summary 0 (- (string-length summary)
                             (string-length " - Race")))]
    [else summary]))

;; display-weekend-name : String -> String
;; Converts ICS race summaries into a cleaner label for the UI card title.
(define (display-weekend-name summary)
  (define base (weekend-key summary))
  (define no-f1
    (if (string-prefix? base "F1 ")
        (substring base 3)
        base))
  (define sponsor-prefixes
    (list "Qatar Airways "
          "Aramco "
          "Lenovo "
          "Pirelli "
          "Aws "))
  (for/fold ([clean no-f1])
            ([prefix (in-list sponsor-prefixes)])
    (if (string-prefix? clean prefix)
        (substring clean (string-length prefix))
        clean)))

;; parse-events : String -> (Listof f1-event)
;; Parse an ICS calendar string into a list of race events.
(define (parse-events text)
  (define lines (string-split text "\n"))

  ;; Same `for/fold` parser-state style as tutorial example.
  (for/fold ([events  '()]
             [summary #f]
             [start   #f]
             #:result (reverse events))
            ([raw-line (in-list lines)])
    (define line (normalize-line raw-line))
    (cond
      ;; SUMMARY:...
      [(string-prefix? line "SUMMARY:")
       (values events (substring line 8) start)]

      ;; DTSTART:...
      [(string-prefix? line "DTSTART:")
       (values events summary (substring line 8))]

      ;; END:VEVENT => commit if complete + a race
      [(string-prefix? line "END:VEVENT")
       (if (and summary start (string-contains? summary "Race"))
           (values (cons (f1-event summary start) events) #f #f)
           (values events #f #f))]

      ;; otherwise: keep parser state
      [else
       (values events summary start)])))

;;------------------------------------------------------------------------------
;; UTC Parsing
;;------------------------------------------------------------------------------

;; digits-only : String -> String
(define (digits-only s)
  (list->string
   (filter char-numeric?
           (string->list s))))

;; take-n : String Natural -> (values String String)
(define (take-n s n)
  (values (substring s 0 n)
          (substring s n)))

;; utc-string->utc-date : String -> utc-date
;; Accepts:
;;   "20260308T150000Z"          (ICS / iCalendar UTC)
;;   "2026-03-08T15:00:00.000Z"  (JS Date.toISOString)
(define (utc-string->utc-date s)
  (define ds (digits-only s))

  ;; We want at least YYYYMMDDHHMMSS = 14 digits.
  (when (< (string-length ds) 14)
    (error 'utc-string->utc-date
           "expected at least 14 digits in UTC date-time, got: ~a"
           s))

  (define-values (yyyy rest0) (take-n ds 4))
  (define-values (mm   rest1) (take-n rest0 2))
  (define-values (dd   rest2) (take-n rest1 2))
  (define-values (hh   rest3) (take-n rest2 2))
  (define-values (mi   rest4) (take-n rest3 2))
  (define-values (ss   _rest) (take-n rest4 2))

  (utc-date (string->number yyyy)
            (string->number mm)
            (string->number dd)
            (string->number hh)
            (string->number mi)
            (string->number ss)))

;; safe-utc-string->utc-date : String -> (U #f utc-date)
;; Returns #f if the input does not contain a full UTC timestamp.
(define (safe-utc-string->utc-date s)
  (define ds (digits-only s))
  (define ds-len (string-length ds))
  (if (< ds-len 14)
      #f
      (let ()
        (define-values (yyyy rest0) (take-n ds 4))
        (define-values (mm   rest1) (take-n rest0 2))
        (define-values (dd   rest2) (take-n rest1 2))
        (define-values (hh   rest3) (take-n rest2 2))
        (define-values (mi   rest4) (take-n rest3 2))
        (define-values (ss   _rest) (take-n rest4 2))
        (define y (string->number yyyy))
        (define m (string->number mm))
        (define d (string->number dd))
        (define h (string->number hh))
        (define n (string->number mi))
        (define s2 (string->number ss))
        (if (and y m d h n s2)
            (utc-date y m d h n s2)
            #f))))

;;------------------------------------------------------------------------------
;; Ordering and calendar helpers
;;------------------------------------------------------------------------------

;; date< : utc-date utc-date -> Boolean
(define (date< a b)
  (cond
    [(< (utc-date-year a)   (utc-date-year b))   #t]
    [(> (utc-date-year a)   (utc-date-year b))   #f]
    [(< (utc-date-month a)  (utc-date-month b))  #t]
    [(> (utc-date-month a)  (utc-date-month b))  #f]
    [(< (utc-date-day a)    (utc-date-day b))    #t]
    [(> (utc-date-day a)    (utc-date-day b))    #f]
    [(< (utc-date-hour a)   (utc-date-hour b))   #t]
    [(> (utc-date-hour a)   (utc-date-hour b))   #f]
    [(< (utc-date-minute a) (utc-date-minute b)) #t]
    [(> (utc-date-minute a) (utc-date-minute b)) #f]
    [(< (utc-date-second a) (utc-date-second b)) #t]
    [else                                        #f]))

;; same-day? : utc-date utc-date -> Boolean
(define (same-day? a b)
  (and (= (utc-date-year a)  (utc-date-year b))
       (= (utc-date-month a) (utc-date-month b))
       (= (utc-date-day a)   (utc-date-day b))))

;; pad2 : Integer -> String
;; Zero-pads a number to two digits for timestamps.
(define (pad2 n)
  (define s (number->string n))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

;; leap-year? : Integer -> Boolean
(define (leap-year? y)
  (or (and (= (remainder y 4) 0)
           (not (= (remainder y 100) 0)))
      (= (remainder y 400) 0)))

;; days-before-month : Integer Integer -> Integer
(define (days-before-month y m)
  (define base (vector 0 31 59 90 120 151 181 212 243 273 304 334))
  (define d (vector-ref base (- m 1)))
  (if (and (> m 2) (leap-year? y)) (+ d 1) d))

;; days-before-year : Integer -> Integer
(define (days-before-year y)
  (define y1 (- y 1))
  (+ (* y1 365)
     (quotient y1 4)
     (- (quotient y1 100))
     (quotient y1 400)))

;; utc-date->unix-seconds : utc-date -> Real
;; Converts a UTC date to epoch seconds as a flonum to avoid fixnum overflow.
(define (utc-date->unix-seconds d)
  (define days (+ (days-before-year (utc-date-year d))
                  (days-before-month (utc-date-year d) (utc-date-month d))
                  (- (utc-date-day d) 1)))
  (define days-1970 (days-before-year 1970))
  (define days-since-epoch (- days days-1970))
  (+ (* (exact->inexact days-since-epoch) 86400.0)
     (* (exact->inexact (utc-date-hour d)) 3600.0)
     (* (exact->inexact (utc-date-minute d)) 60.0)
     (exact->inexact (utc-date-second d))))

;; utc-date->stamp : utc-date -> String
;; Formats as "YYYY-MM-DD HH:MM UTC" for status display.
(define (utc-date->stamp d)
  (string-append (number->string (utc-date-year d)) "-"
                 (pad2 (utc-date-month d)) "-"
                 (pad2 (utc-date-day d)) " "
                 (pad2 (utc-date-hour d)) ":"
                 (pad2 (utc-date-minute d)) " UTC"))

;;------------------------------------------------------------------------------
;; Current UTC date (browser / WebRacket)
;;------------------------------------------------------------------------------

;; current-utc-date : -> utc-date
(define (current-utc-date)
  (define iso-ext (js-eval "new Date().toISOString()"))
  (define iso     (external-string->string iso-ext))
  (utc-string->utc-date iso))

;;------------------------------------------------------------------------------
;; Race selection + countdown text
;;------------------------------------------------------------------------------

(define f1-ics-data (get-f1-calendar-ics))
(define f1-events   (parse-events f1-ics-data))

;; find-next-race : utc-date -> (U #f f1-event)
(define (find-next-race now)
  (define (event-start ev)
    (safe-utc-string->utc-date (f1-event-start ev)))

  (define (event-after-now? ev)
    (define start (event-start ev))
    (and start
         (or (date< now start)
             (and (same-day? now start)
                  (not (date< start now))))))

  (define (earlier-event? a b)
    (define a-start (event-start a))
    (define b-start (event-start b))
    (and a-start b-start (date< a-start b-start)))

  (for/fold ([best #f])
            ([ev (in-list f1-events)])
    (if (and (race-summary? (f1-event-summary ev))
             (event-after-now? ev))
        (cond
          [(not best) ev]
          [(earlier-event? ev best) ev]
          [else best])
        best)))

;; find-weekend-events : f1-event -> (values (U #f f1-event) f1-event)
;; Returns sprint event (if any) and the race event for the same weekend.
(define (find-weekend-events race-event)
  (define key (weekend-key (f1-event-summary race-event)))
  (define sprint #f)
  (define race race-event)
  (for ([ev (in-list f1-events)])
    (when (string=? (weekend-key (f1-event-summary ev)) key)
      (when (string-contains? (f1-event-summary ev) "Sprint")
        (set! sprint ev))
      (when (race-summary? (f1-event-summary ev))
        (set! race ev))))
  (values sprint race))

;; pluralize : Integer String String -> String
(define (pluralize n singular plural)
  (if (= n 1) singular plural))

;; race-countdown-line : utc-date f1-event -> String
;; New website behavior:
;; - If same day: report hours to race.
;; - Otherwise: report days to race.
(define (race-countdown-line now event)
  (define start-raw (f1-event-start event))
  (define start-date (safe-utc-string->utc-date start-raw))
  (cond
    [(not start-date)
     "Race time unavailable."]
    [else
     (define start-seconds (utc-date->unix-seconds start-date))
     (define now-seconds (utc-date->unix-seconds now))
     (define diff-seconds
       (- start-seconds now-seconds))
     (cond
       [(<= diff-seconds 0)
         "Race started just now."]
       [(same-day? now start-date)
        (define hours (max 1 (inexact->exact (ceiling (/ diff-seconds 3600)))))
        (string-append (number->string hours)
                       " "
                       (pluralize hours "hour" "hours")
                       " to lights out.")]
       [else
        (define days (max 1 (inexact->exact (ceiling (/ diff-seconds 86400)))))
        (string-append (number->string days)
                       " "
                       (pluralize days "day" "days")
                       " to the next race.")])]))

;; race-countdown-parts : utc-date f1-event -> (values String String String)
;; Returns number, unit, and label for the headline countdown.
(define (race-countdown-parts now event)
  (define start-raw (f1-event-start event))
  (define start-date (safe-utc-string->utc-date start-raw))
  (cond
    [(not start-date)
     (values "—" "time" "Race time unavailable.")]
    [else
     (define start-seconds (utc-date->unix-seconds start-date))
     (define now-seconds (utc-date->unix-seconds now))
     (define diff-seconds (- start-seconds now-seconds))
     (cond
       [(<= diff-seconds 0)
        (values "0" "hours" "Race started just now.")]
       [(same-day? now start-date)
        (define hours (max 1 (inexact->exact (ceiling (/ diff-seconds 3600)))))
        (values (number->string hours)
                (pluralize hours "hour" "hours")
                "until lights out")]
       [else
        (define days (max 1 (inexact->exact (ceiling (/ diff-seconds 86400)))))
        (values (number->string days)
                (pluralize days "day" "days")
                "until the next race")])]))

;;------------------------------------------------------------------------------
;; DOM rendering for the website page
;;------------------------------------------------------------------------------

;; render-f1-countdown! : -> Void
;; Pulls `now`, finds next race, then updates 3 nodes in the page.
(define (render-f1-countdown!)
  (define card-node      (js-get-element-by-id "f1-countdown-card"))
  (define title-node     (js-get-element-by-id "f1-next-race-title"))
  (define value-node     (js-get-element-by-id "f1-countdown-value"))
  (define unit-node      (js-get-element-by-id "f1-countdown-unit"))
  (define label-node     (js-get-element-by-id "f1-countdown-label"))
  (define badge-node     (js-get-element-by-id "f1-data-badge"))
  (define status-node    (js-get-element-by-id "f1-countdown-status"))
  (define status-updated-node (js-get-element-by-id "f1-countdown-status-updated"))
  (define status-source-node (js-get-element-by-id "f1-countdown-status-source"))

  (js-set! card-node "className" "card f1-countdown-card is-loading")
  (js-set! badge-node "textContent" "Loading")
  (js-set! badge-node "className" "f1-data-badge is-loading")
  (js-set! status-node "className" "f1-countdown-status is-loading")
  (js-set! status-updated-node "textContent" "Loading calendar…")
  (js-set! status-source-node "textContent" "")

  (define now       (current-utc-date))
  (define next-race (find-next-race now))

  (if next-race
      (let ()
        (define-values (sprint race) (find-weekend-events next-race))
        (define title
          (string-append "Next race weekend: "
                         (display-weekend-name (f1-event-summary race))))
        (define-values (countdown-value countdown-unit countdown-label)
          (race-countdown-parts now race))
        (js-set! title-node "textContent" title)
        (js-set! value-node "textContent" countdown-value)
        (js-set! unit-node "textContent" countdown-unit)
        (js-set! label-node "textContent" countdown-label)
        (js-set! badge-node "textContent" "Cached")
        (js-set! badge-node "className" "f1-data-badge is-cached")
        (js-set! card-node "className" "card f1-countdown-card is-ready")
        (js-set! status-node "className" "f1-countdown-status is-ready")
        (js-set! status-updated-node "textContent"
                 (string-append "Cached · Updated just now (" (utc-date->stamp now) ")"))
        (js-set! status-source-node "textContent" "Source: Better F1 Calendar (ICS snapshot)"))
      (begin
        (js-set! title-node "textContent" "No upcoming race found in the calendar.")
        (js-set! value-node "textContent" "—")
        (js-set! unit-node "textContent" "races")
        (js-set! label-node "textContent" "Check back soon")
        (js-set! badge-node "textContent" "Cached")
        (js-set! badge-node "className" "f1-data-badge is-cached")
        (js-set! card-node "className" "card f1-countdown-card is-error")
        (js-set! status-node "className" "f1-countdown-status is-error")
        (js-set! status-updated-node "textContent" "Cached · Updated never")
        (js-set! status-source-node "textContent" "Source: Better F1 Calendar (ICS snapshot)"))))

;; formula1-page : -> List
(define (formula1-page)
  `(div (@ (class "page page--formula1"))
        ,(navbar)
        (section (@ (class "examples-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Formula 1")
                           (span (@ (class "pill")) "ICS Calendar")
                           (span (@ (class "pill")) "Date parsing"))
                      (h1 (@ (class "hero-title")) "Formula 1 · Next Race")
                      (p (@ (class "hero-lead"))
                         "Uses the Better F1 Calendar ICS to show the next race countdown.")))

        ,(section-block
          "Live countdown"
          "Shows days until the next race, switching to hours on race day."
          (list
           `(div (@ (id "f1-countdown-card") (class "card f1-countdown-card"))
                 (div (@ (class "f1-loading-skeleton") (aria-hidden "true"))
                      (span (@ (class "f1-skel-line f1-skel-title")) "")
                      (span (@ (class "f1-skel-line f1-skel-countdown")) "")
                      (span (@ (class "f1-skel-line f1-skel-meta")) ""))
                 (h3 (@ (id "f1-next-race-title")) "Loading next race…")
                 (div (@ (class "f1-countdown") (aria-live "polite"))
                      (span (@ (id "f1-countdown-value") (class "f1-countdown-value")) "—")
                      " "
                      (span (@ (id "f1-countdown-unit") (class "f1-countdown-unit")) "days"))
                 (p (@ (id "f1-countdown-label") (class "f1-countdown-label"))
                    "until the next race")
                 (div (@ (class "f1-countdown-meta"))
                      (span (@ (id "f1-data-badge") (class "f1-data-badge is-loading") (aria-live "polite")) "Loading")
                      (p (@ (id "f1-countdown-status") (class "f1-countdown-status is-loading") (role "status") (aria-live "polite"))
                         (span (@ (id "f1-countdown-status-updated")) "Loading calendar…")
                         (br)
                         (span (@ (id "f1-countdown-status-source")) "")))))
          #f
          "section--examples")

        ,(section-block
          "Source"
          "This example is based on the Formula 1 parser tutorial."
          (list
           `(div (@ (class "mathjax-actions"))
                 ,(code-pill (gh-file "web-site/src/examples/formula1/formula1.rkt")
                             "Parser tutorial")
                 ,(code-pill (gh-file "web-site/src/examples/formula1-page.rkt")
                             "Website integration example")))
          #f
          "section--examples")

        ,(footer-section)))

;; init-formula1-page! : -> Void
(define (init-formula1-page!)
  (js-set! (js-var "document") "title" "Formula 1 next race")
  (render-f1-countdown!))
