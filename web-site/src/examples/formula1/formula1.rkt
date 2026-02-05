#lang webracket
;;------------------------------------------------------------------------------
;; Program: Next F1 race finder (Better F1 Calendar)
;;
;; This program (eventually) fetches the Better F1 Calendar ICS feed and finds
;; the next upcoming race. This file contains the `event` structure and the
;; `parse-events` function that parses ICS text into race events.
;;
;; ICS FORMAT OVERVIEW:
;; An ICS (iCalendar) file is a plain-text calendar format. It is organized as
;; nested BEGIN:/END: blocks. Events are wrapped like:
;;
;;   BEGIN:VEVENT
;;   SUMMARY:Some Event Name
;;   DTSTART:20260308T150000Z
;;   DTEND:20260308T170000Z
;;   END:VEVENT
;;
;; Each property appears on its own line as KEY:VALUE. Lines may appear in
;; arbitrary order within an event. Some ICS files also support line folding,
;; where a long logical line continues on the next line starting with a space.
;; This parser intentionally ignores folding and focuses on SUMMARY and DTSTART.
;;
;; Notes:
;; - We parse the ICS as plain text, scanning line-by-line.
;; - We collect event fields (SUMMARY and DTSTART) until END:VEVENT.
;; - We keep only events whose SUMMARY contains "Race".
;; - We use `for/fold` to avoid mutating the event list while still threading
;;   parser state (events, summary, start).
;; - We reverse at the end because we cons onto the front during accumulation.
;; - This parser is intentionally simple; it does not handle all iCalendar edge
;;   cases (e.g., folded lines or time zone properties).
;;------------------------------------------------------------------------------

;; event : structure
;; Represents one calendar event we care about, with a summary and a DTSTART.
(struct event (summary start))

;; parse-events : String -> (Listof event)
;; Parse an ICS calendar string into a list of race events.
(define (parse-events text)
  (define lines (string-split text "\n"))

  (for/fold ([events  '()]
             [summary #f]
             [start   #f]
             #:result (reverse events))
            ([line lines])

    (cond
      ;; SUMMARY:...
      [(string-prefix? line "SUMMARY:")
       (values events (substring line 8) start)]

      ;; DTSTART:...
      [(string-prefix? line "DTSTART:")
       (values events summary (substring line 8))]

      ;; END:VEVENT => commit if complete + a race
      [(string-prefix? line "END:VEVENT")
       (if (and summary start
                (string-contains? summary "Race"))
           (values (cons (event summary start) events) #f #f)
           (values events #f #f))]

      ;; otherwise: keep state
      [else
       (values events summary start)])))

;;;
;;; DATES
;;;

;;------------------------------------------------------------------------------
;; utc-date
;;------------------------------------------------------------------------------

(struct utc-date (year month day hour minute second))

;;------------------------------------------------------------------------------
;; Parsing
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

;; parse-2/4 : String -> Natural
(define (parse-2 s) (string->number s))
(define (parse-4 s) (string->number s))

;; utc-string->utc-date : String -> utc-date
;;
;; Accepts:
;;   "20260308T150000Z"          (ICS / iCalendar UTC)
;;   "2026-03-08T15:00:00.000Z"  (JS Date.toISOString)
;; Also works if the trailing "Z" is missing.

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

  (utc-date (parse-4 yyyy)
            (parse-2 mm)
            (parse-2 dd)
            (parse-2 hh)
            (parse-2 mi)
            (parse-2 ss)))

;;------------------------------------------------------------------------------
;; Ordering and equality
;;------------------------------------------------------------------------------

;; date= : utc-date utc-date -> Boolean
(define (date= a b)
  (and (= (utc-date-year a)   (utc-date-year b))
       (= (utc-date-month a)  (utc-date-month b))
       (= (utc-date-day a)    (utc-date-day b))
       (= (utc-date-hour a)   (utc-date-hour b))
       (= (utc-date-minute a) (utc-date-minute b))
       (= (utc-date-second a) (utc-date-second b))))

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

;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

;; pad2 : Natural -> String
(define (pad2 n)
  (define s (number->string n))
  (if (= (string-length s) 1)
      (string-append "0" s)
      s))

;; pad4 : Natural -> String
(define (pad4 n)
  (define s (number->string n))
  (cond
    [(= (string-length s) 4) s]
    [(= (string-length s) 3) (string-append "0" s)]
    [(= (string-length s) 2) (string-append "00" s)]
    [(= (string-length s) 1) (string-append "000" s)]
    [else s]))

;; format-utc-date : utc-date -> String
;; Produces ICS-style UTC string: "YYYYMMDDTHHMMSSZ"
(define (format-utc-date d)
  (string-append
   (pad4 (utc-date-year d))
   (pad2 (utc-date-month d))
   (pad2 (utc-date-day d))
   "T"
   (pad2 (utc-date-hour d))
   (pad2 (utc-date-minute d))
   (pad2 (utc-date-second d))
   "Z"))

;;------------------------------------------------------------------------------
;; Current UTC date (browser / WebRacket)
;;------------------------------------------------------------------------------

;; Uses JS Date.toISOString() via the standard JS bridge.
;;
;; Requires the "standard" JS bridge (available when you build with --ffi dom).
; (require "ffi/dom.ffi")

;; current-utc-date : -> utc-date
(define (current-utc-date)
  ;; js-eval returns an "external string" => convert to a Racket string.
  (define iso-ext (js-eval "new Date().toISOString()"))
  (define iso     (external-string->string iso-ext))
  (utc-string->utc-date iso))

;;------------------------------------------------------------------------------
;; Tiny sanity check
;;------------------------------------------------------------------------------

#;(let ([d (utc-string->utc-date "20260308T150000Z")])
    (displayln (format-utc-date d)))

#;(let ([now (current-utc-date)])
    (displayln (format-utc-date now)))


;;------------------------------------------------------------------------------
;; Tests
;;
;; Simple test using a minimal ICS snippet with two events, only one of which
;; is a race. The parser should return a list containing exactly one event.
;;------------------------------------------------------------------------------

;; test-ics : String
;; Minimal ICS example for testing.

#;(define test-ics
  (string-append
   "BEGIN:VEVENT\n"
   "SUMMARY:Bahrain Grand Prix - Race\n"
   "DTSTART:20260308T150000Z\n"
   "END:VEVENT\n"
   "BEGIN:VEVENT\n"
   "SUMMARY:Bahrain Grand Prix - Practice\n"
   "DTSTART:20260307T110000Z\n"
   "END:VEVENT\n"))

(require (for-syntax racket/base racket/file))

(define-syntax (get-calendar-ics stx)
  (datum->syntax #'here (file->string "better-f1-calendar.ics")))

(define test-ics (get-calendar-ics))


;; test-result : (Listof event)
;; Expected:
;; (list (event "Bahrain Grand Prix - Race" "20260308T150000Z"))

(define test-result (parse-events test-ics))




(displayln "Parsed events:")
(for-each
 (lambda (ev)
   (displayln (event-summary ev))
   (displayln (event-start ev)))
 test-result)
