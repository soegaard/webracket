;;;
;;; Fetch Demo
;;;

(include-lib web-easy)
(include-lib fetch)

(define @url (@ "data:text/plain,Hello from WebRacket"))
(define @header-name (@ "x-demo"))
(define @header-value (@ "fetch"))
(define @status (@ "Ready."))
(define @request-summary (@ "No request built yet."))
(define @headers-summary (@ "No headers built yet."))
(define @fetch-kind (@ "none"))

;; refresh-request-summary! : -> void?
;;   Recompute the request summary from the current observable values.
(define (refresh-request-summary!)
  (define req (make-fetch-request (obs-peek @url)))
  (define hdrs (make-fetch-headers))
  (fetch-headers-set! hdrs (obs-peek @header-name) (obs-peek @header-value))
  (obs-set! @request-summary
            (format "Request: ~a ~a"
                    (fetch-request-method req)
                    (fetch-request-url req)))
  (obs-set! @headers-summary
            (format "Headers: ~a = ~a"
                    (obs-peek @header-name)
                    (fetch-headers-get hdrs (obs-peek @header-name))))
  (obs-set! @status "Built request and headers.")
  (void))

;; start-fetch! : -> void?
;;   Start a sample fetch request and report the kind of result.
(define (start-fetch!)
  (define req (make-fetch-request (obs-peek @url)))
  (define promise (fetch req))
  (obs-set! @fetch-kind (format "fetch returned a ~a" (js-typeof promise)))
  (obs-set! @status "Fetch started.")
  (void))

(define fetch-demo-app
  (window
   (container
    (vpanel
     (h1 "Fetch Demo")
     (text "A small demo for request/headers helpers and the browser fetch entrypoint.")
     (text "URL")
     (input @url
            (lambda (new-value)
              (:= @url new-value)))
     (text "Header name")
     (input @header-name
            (lambda (new-value)
              (:= @header-name new-value)))
     (text "Header value")
     (input @header-value
            (lambda (new-value)
              (:= @header-value new-value)))
     (hpanel
      (button "Build request" refresh-request-summary!)
      (button "Start fetch" start-fetch!))
     (P @status)
     (P @request-summary)
     (P @headers-summary)
     (P @fetch-kind)))))

(define app-renderer
  (render fetch-demo-app))

(mount-renderer! app-renderer)
