;;;
;;; Query Counter
;;;

(include-lib web-easy)
(include-lib query)

(define @count (@ 0))

(define query-counter-app
  (window
   (container
    (h1 "Query Counter")
    (P "This example uses query selectors and .on to wire the buttons.")
    (hpanel
     (Button #:id "query-counter-decrement" "-")
     (text (obs-map @count number->string))
     (Button #:id "query-counter-increment" "+")))))

(define app-renderer
  (render query-counter-app))

(mount-renderer! app-renderer)

(define decrement-button ($ "#query-counter-decrement"))
(define increment-button ($ "#query-counter-increment"))

($chain decrement-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count sub1)))

($chain increment-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count add1)))
