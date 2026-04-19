;;;
;;; Query Delegate List
;;;

(include-lib web-easy)
(include-lib rquery)

(define @selected (@ "none"))

(define rquery-delegate-list-app
  (window
   (container
    (h1 "Query Delegate List")
    (P "This example uses .on-delegate to handle clicks from a parent list.")
    (P "Click a fruit button to update the selection.")
    (vpanel
     (hpanel
      (text "Selected: ")
      (text @selected))
     (container #:id "rquery-delegate-list"
       (vpanel
        (Button #:id "rquery-delegate-apple" "Apple")
        (Button #:id "rquery-delegate-banana" "Banana")
        (Button #:id "rquery-delegate-cherry" "Cherry")))))))

(define app-renderer
  (render rquery-delegate-list-app))

(mount-renderer! app-renderer)

(define delegate-list ($ "#rquery-delegate-list"))

($chain delegate-list
  .on-delegate "click"
  "button"
  (lambda (matched _evt)
    (obs-set! @selected (element-text-content matched))))
