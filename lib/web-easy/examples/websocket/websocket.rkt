;;;
;;; WebSocket Echo Demo
;;;

(include-lib web-easy)
(include-lib websocket)

(define websocket-demo-url     "wss://echo-websocket.fly.dev/")
(define websocket-demo-message "Hello from WebRacket")

(define @server-url    (@ websocket-demo-url))
(define @message       (@ websocket-demo-message))
(define @status        (@ "idle"))
(define @socket-state  (@ "closed"))
(define @buffered-amt  (@ "0"))
(define @protocol      (@ "none"))
(define @extensions    (@ "none"))
(define @log           (@ "Ready. Click Connect to open the socket.\n"))

(define current-socket #f)

(define open-handler    #f)
(define message-handler #f)
(define close-handler   #f)
(define error-handler   #f)

;; active-socket? : extern -> boolean?
;;   Check whether ws is still the current live socket.
(define (active-socket? ws)
  (and current-socket (eq? current-socket ws)))

;; append-log! : string? -> void?
;;   Add one line to the log area.
(define (append-log! line)
  (obs-update! @log
               (lambda (text)
                 (string-append text line "\n"))))

;; sync-socket-fields! : extern -> void?
;;   Refresh the state summary from a live WebSocket.
(define (sync-socket-fields! ws)
  (when (active-socket? ws)
    (obs-set! @socket-state  (symbol->string (websocket-ready-state ws)))
    (obs-set! @buffered-amt  (number->string (websocket-buffered-amount ws)))
    (obs-set! @protocol      (let ([p (websocket-protocol ws)])
                               (if (string=? p "") "none" p)))
    (obs-set! @extensions    (let ([e (websocket-extensions ws)])
                               (if (string=? e "") "none" e)))))

;; install-socket-handlers! : extern -> void?
;;   Attach WebSocket event handlers to ws.
(define (install-socket-handlers! ws)
  (set! open-handler
        (lambda (_evt)
          (when (active-socket? ws)
            (obs-set! @status "connected")
            (append-log! "opened")
            (sync-socket-fields! ws))
          (void)))
  (set! message-handler
        (lambda (evt)
          (when (active-socket? ws)
            (define data (js-message-event-data evt))
            (append-log! (format "received: ~a" data))
            (sync-socket-fields! ws))
          (void)))
  (set! close-handler
        (lambda (_evt)
          (when (active-socket? ws)
            (set! current-socket #f)
            (obs-set! @status "closed")
            (obs-set! @socket-state "closed")
            (obs-set! @buffered-amt "0")
            (obs-set! @protocol "none")
            (obs-set! @extensions "none")
            (append-log! "closed"))
          (void)))
  (set! error-handler
        (lambda (_evt)
          (when (active-socket? ws)
            (obs-set! @status "error")
            (append-log! "socket error"))
          (void)))
  (websocket-onopen! ws open-handler)
  (websocket-onmessage! ws message-handler)
  (websocket-onclose! ws close-handler)
  (websocket-onerror! ws error-handler)
  (void))

;; connect! : -> void?
;;   Open a WebSocket connection to the configured URL.
(define (connect!)
  (define url (obs-peek @server-url))
  (when current-socket
    (with-handlers ([exn? (lambda (_e) (void))])
      (websocket-close current-socket 1000 "replaced")))
  (define ws  (websocket-new url))
  (set! current-socket ws)
  (obs-set! @status "connecting")
  (append-log! (format "connecting: ~a" url))
  (install-socket-handlers! ws)
  (sync-socket-fields! ws)
  (void))

;; send-message! : -> void?
;;   Send the current message through the socket.
(define (send-message!)
  (cond
    [current-socket
     (define msg (obs-peek @message))
     (websocket-send current-socket msg)
     (append-log! (format "sent: ~a" msg))
     (sync-socket-fields! current-socket)]
    [else
     (append-log! "cannot send: not connected")]))

;; close-connection! : -> void?
;;   Close the live socket if one exists.
(define (close-connection!)
  (cond
    [current-socket
     (append-log! "closing socket")
     (websocket-close current-socket 1000 "demo closed")]
    [else
     (append-log! "no socket to close")]))

(define websocket-demo-app
  (window
   (container
    (vpanel
     (h1 "WebSocket Echo Demo")
     (text "A tiny demo that connects to a public echo server, sends a message, and logs the responses.")
     (text "URL")
     (input @server-url
            (lambda (new-value)
              (:= @server-url new-value)))
     (text "Message")
     (input @message
            (lambda (new-value)
              (:= @message new-value)))
     (hpanel
      (button "Connect" connect!)
      (button "Send" send-message!)
      (button "Close" close-connection!))
     (hpanel (text "Status: ")       (text @status))
     (hpanel (text "Ready state: ")  (text @socket-state))
     (hpanel (text "Buffered: ")     (text @buffered-amt))
     (hpanel (text "Protocol: ")     (text @protocol))
     (hpanel (text "Extensions: ")   (text @extensions))
     (textarea @log
               (lambda (_new-value) (void))
               #:rows 10
               #:textarea-attrs '((readonly "readonly")))))))

(define app-renderer
  (render websocket-demo-app))

;;;
;;; Themes
;;;

;; Note: compile.sh copies the required theme CSS files next to the
;;       generated HTML, so these stylesheet paths are relative to
;;       the generated/ output directory.

;; light-theme : theme?
;;   Shared light theme used by this example.
(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
