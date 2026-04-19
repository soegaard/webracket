;;;
;;; Query Toggle
;;;

(include-lib web-easy)
(include-lib rquery)

(define @count (@ 0))
(define @increment-enabled? (@ #t))
(define @switch-label
  (obs-map @increment-enabled?
           (lambda (enabled?)
             (if enabled?
                 "Disable +"
                 "Enable +"))))

(define (increment! _evt)
  (obs-update! @count add1))

(define (decrement! _evt)
  (obs-update! @count sub1))

(define rquery-toggle-app
  (window
   (container
    (h1 "Query Toggle")
    (P "This example uses .off to temporarily disable the + button.")
    (P "The handler removed with .off must match the event name and callback used with .on.")
    (hpanel
     (Button #:id "rquery-toggle-decrement" "-")
     (text (obs-map @count number->string))
     (Button #:id "rquery-toggle-increment" "+")
     (Button #:id "rquery-toggle-switch" @switch-label)))))

(define app-renderer
  (render rquery-toggle-app))

(mount-renderer! app-renderer)

(define decrement-button ($ "#rquery-toggle-decrement"))
(define increment-button ($ "#rquery-toggle-increment"))
(define switch-button ($ "#rquery-toggle-switch"))

($chain decrement-button
  .on "click"
  decrement!)

($chain increment-button
  .on "click"
  increment!)

($chain switch-button
  .on "click"
  (lambda (_evt)
    (if (obs-peek @increment-enabled?)
        (begin
          ($chain increment-button
            .off "click"
            increment!)
          (obs-set! @increment-enabled? #f))
        (begin
          ($chain increment-button
            .on "click"
            increment!)
          (obs-set! @increment-enabled? #t)))))
