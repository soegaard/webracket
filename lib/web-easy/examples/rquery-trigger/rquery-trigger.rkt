;;;
;;; Query Trigger
;;;

(include-lib web-easy)
(include-lib rquery)

(define @count (@ 0))

(define rquery-trigger-app
  (window
   (container
    (h1 "Query Trigger")
    (P "This example uses .trigger to fire a click handler from code.")
    (P "Click the trigger button to programmatically click the counter button.")
    (hpanel
     (Button #:id "rquery-trigger-target" "Target")
     (Button #:id "rquery-trigger-fire" "Trigger it")
     (text "Count: ")
     (text (obs-map @count number->string))))))

(define app-renderer
  (render rquery-trigger-app))

(mount-renderer! app-renderer)

(define target-button ($ "#rquery-trigger-target"))
(define fire-button ($ "#rquery-trigger-fire"))

($chain target-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count add1)))

($chain fire-button
  .on "click"
  (lambda (_evt)
    ($trigger "click" target-button)))
