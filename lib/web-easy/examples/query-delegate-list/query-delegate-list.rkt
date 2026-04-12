;;;
;;; Query Delegate List
;;;

(include-lib web-easy)
(include-lib query)

(define @selected (@ "none"))

(define query-delegate-list-app
  (window
   (container
    (h1 "Query Delegate List")
    (P "This example uses .on-delegate to handle clicks from a parent list.")
    (P "Click a fruit button to update the selection.")
    (vpanel
     (hpanel
      (text "Selected: ")
      (text @selected))
     (container #:id "query-delegate-list"
       (vpanel
        (Button #:id "query-delegate-apple" "Apple")
        (Button #:id "query-delegate-banana" "Banana")
        (Button #:id "query-delegate-cherry" "Cherry")))))))

(define app-renderer
  (render query-delegate-list-app))

(mount-renderer! app-renderer)

(define delegate-list ($ "#query-delegate-list"))

($chain delegate-list
  .on-delegate "click"
  "button"
  (lambda (matched _evt)
    (obs-set! @selected (element-text-content matched))))
