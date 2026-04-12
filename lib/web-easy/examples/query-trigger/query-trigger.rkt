;;;
;;; Query Trigger
;;;

(include-lib web-easy)
(include-lib query)

(define @count (@ 0))

(define query-trigger-app
  (window
   (container
    (h1 "Query Trigger")
    (P "This example uses .trigger to fire a click handler from code.")
    (P "Click the trigger button to programmatically click the counter button.")
    (hpanel
     (Button #:id "query-trigger-target" "Target")
     (Button #:id "query-trigger-fire" "Trigger it")
     (text "Count: ")
     (text (obs-map @count number->string))))))

(define app-renderer
  (render query-trigger-app))

(mount-renderer! app-renderer)

(define target-button ($ "#query-trigger-target"))
(define fire-button ($ "#query-trigger-fire"))

($chain target-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count add1)))

($chain fire-button
  .on "click"
  (lambda (_evt)
    ($trigger "click" target-button)))
