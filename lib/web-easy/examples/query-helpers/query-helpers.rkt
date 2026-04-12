;;;
;;; Query Helpers
;;;

(include-lib web-easy)
(include-lib query)

(define query-helpers-app
  (window
   (container
    (Style
     ".query-helpers-badge {\n"
     "  display: inline-flex;\n"
     "  align-items: center;\n"
     "  justify-content: center;\n"
     "  min-width: 7rem;\n"
     "  padding: 0.8rem 1rem;\n"
     "  border-radius: 14px;\n"
     "  background: rgba(143, 157, 255, 0.12);\n"
     "  border: 1px solid rgba(143, 157, 255, 0.28);\n"
     "  color: #edf0ff;\n"
     "  transition: transform 140ms ease, box-shadow 140ms ease, background 140ms ease, color 140ms ease;\n"
     "}\n"
     ".query-helpers-badge.highlight {\n"
     "  background: #ffd36c;\n"
     "  color: #281800;\n"
     "  box-shadow: 0 12px 28px rgba(255, 211, 108, 0.28);\n"
     "  transform: translateY(-1px);\n"
     "}\n"
     ".query-helpers-controls {\n"
     "  display: flex;\n"
     "  flex-wrap: wrap;\n"
     "  gap: 0.75rem;\n"
     "}\n")
    (h1 "Query Helpers")
    (P "This example shows .text!, .toggle-class!, .val, .val!, .data, and .data! working together.")
    (vpanel
     (P #:id "query-helpers-status" "Status: Ready")
     (Input #:id "query-helpers-input"
            #:type "text"
            #:value "Hello Web Easy")
     (container #:id "query-helpers-badge"
                #:class "query-helpers-badge"
                #:attrs (list (cons 'data-state "cold"))
       "Badge")
     (P #:id "query-helpers-note" "Data state: cold")
     (hpanel #:class "query-helpers-controls"
      (Button #:id "query-helpers-load" "Load preset")
      (Button #:id "query-helpers-mirror" "Mirror input")
      (Button #:id "query-helpers-toggle" "Toggle highlight")
      (Button #:id "query-helpers-stamp" "Stamp data")
      (Button #:id "query-helpers-read" "Read data"))))))

(define app-renderer
  (render query-helpers-app))

(mount-renderer! app-renderer)

(define status ($ "#query-helpers-status"))
(define note ($ "#query-helpers-note"))
(define field ($ "#query-helpers-input"))
(define badge ($ "#query-helpers-badge"))
(define load-button ($ "#query-helpers-load"))
(define mirror-button ($ "#query-helpers-mirror"))
(define toggle-button ($ "#query-helpers-toggle"))
(define stamp-button ($ "#query-helpers-stamp"))
(define read-button ($ "#query-helpers-read"))

(define (set-status! text)
  ($text! status text))

(define (set-note! text)
  ($text! note text))

($chain load-button
  .on "click"
  (lambda (_evt)
    ($val! field "Query helpers")
    (set-status! "Status: preset loaded")))

($chain mirror-button
  .on "click"
  (lambda (_evt)
    (set-status! (string-append "Status: " ($val field)))))

($chain toggle-button
  .on "click"
  (lambda (_evt)
    ($toggle-class! badge "highlight")
    (set-status! "Status: highlight toggled")))

($chain stamp-button
  .on "click"
  (lambda (_evt)
    (define current ($val field))
    ($data! badge 'state current)
    (set-note! (string-append "Data state: " current))
    (set-status! (string-append "Status: saved " current))))

($chain read-button
  .on "click"
  (lambda (_evt)
    (set-status! (string-append "Status: data state is " ($data badge 'state)))))
