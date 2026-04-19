;;;
;;; Query Counter
;;;

(include-lib web-easy)
(include-lib rquery)

(define @count (@ 0))

(define rquery-counter-app
  (window
   (container
    (h1 "Query Counter")
    (P "This example uses query selectors and .on to wire the buttons.")
    (hpanel
     (Button #:id "rquery-counter-decrement" "-")
     (text (obs-map @count number->string))
     (Button #:id "rquery-counter-increment" "+")))))

(define app-renderer
  (render rquery-counter-app))

(mount-renderer! app-renderer)

(define decrement-button ($ "#rquery-counter-decrement"))
(define increment-button ($ "#rquery-counter-increment"))

($chain decrement-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count sub1)))

($chain increment-button
  .on "click"
  (lambda (_evt)
    (obs-update! @count add1)))
