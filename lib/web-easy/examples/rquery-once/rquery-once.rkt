;;;
;;; Query Once
;;;

(include-lib web-easy)
(include-lib rquery)

(define @count (@ 0))

(define rquery-once-app
  (window
   (container
    (h1 "Query Once")
    (P "This example uses .once so the button only responds the first time.")
    (P "Click the button repeatedly and the counter still stops at 1.")
    (P "After the first click, the button is disabled to make the one-shot behavior obvious.")
    (hpanel
     (Button #:id "rquery-once-button" "Click once")
     (text "Count: ")
     (text (obs-map @count number->string))))))

(define app-renderer
  (render rquery-once-app))

(mount-renderer! app-renderer)

(define once-button ($ "#rquery-once-button"))

($chain once-button
  .once "click"
  (lambda (_evt)
    (obs-update! @count add1)
    (element-set-attribute! ($first once-button) 'disabled "disabled")))
