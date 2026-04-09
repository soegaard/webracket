;;;
;;; Audio Buffer Playback Demo
;;;

(include-lib web-easy)
(include-lib audio)

(define @status         (@ "stopped"))
(define @frequency      (@ 440))
(define @gain-percent   (@ 15))
(define @playback-rate  (@ 100))
(define @loop?          (@ #t))
(define @duration-ms    (@ 700))

(define current-context #f)
(define current-source  #f)
(define current-gain    #f)

;; clamp : real? real? real? -> real?
;;   Clamp v to the inclusive range [lo, hi].
(define (clamp v lo hi)
  (max lo (min hi v)))

;; gain-percent->value : exact-integer? -> real?
;;   Convert a 0..100 gain percentage to a linear gain value.
(define (gain-percent->value n)
  (/ n 100.0))

;; playback-rate-percent->value : exact-integer? -> real?
;;   Convert a 50..200 slider value into a playback-rate multiplier.
(define (playback-rate-percent->value n)
  (/ n 100.0))

;; reset-live-graph! : -> void?
;;   Forget the current audio objects after a stop.
(define (reset-live-graph!)
  (set! current-context #f)
  (set! current-source #f)
  (set! current-gain #f))

;; fill-loopable-sine-buffer! : audio-buffer? real? -> void?
;;   Fill the buffer with a sine wave that wraps on a cycle boundary.
(define (fill-loopable-sine-buffer! buffer frequency)
  (define channel-data (audio-buffer-get-channel-data buffer 0))
  (define sample-count (audio-buffer-length buffer))
  (define sample-rate (audio-buffer-sample-rate buffer))
  (define duration (/ sample-count sample-rate))
  (define cycles (max 1 (exact-round (* frequency duration))))
  (for ([i (in-range sample-count)])
    (define phase (/ (* 2 pi cycles i) sample-count))
    (define sample (* 0.35 (sin phase)))
    (js-set! channel-data i sample)))

;; apply-live-controls! : -> void?
;;   Update the live playback node with the current UI values.
(define (apply-live-controls!)
  (when current-gain
    (audio-param-set-value! (audio-gain-node-gain current-gain)
                            (gain-percent->value (obs-peek @gain-percent))))
  (when current-source
    (audio-param-set-value! (audio-buffer-source-node-playback-rate current-source)
                            (playback-rate-percent->value (obs-peek @playback-rate)))
    (audio-buffer-source-node-set-loop! current-source (obs-peek @loop?))))

;; stop-playback! : -> void?
;;   Stop the current buffer source and close the context.
(define (stop-playback!)
  (when current-source
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-buffer-source-node-stop! current-source))
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-context-close current-context)))
  (reset-live-graph!)
  (obs-set! @status "stopped"))

;; start-playback! : -> void?
;;   Create a buffer source, load a generated sine buffer, and start playback.
(define (start-playback!)
  (stop-playback!)
  (define ctx (audio-context-new))
  (define gain-node (audio-context-create-gain ctx))
  (define source (audio-context-create-buffer-source ctx))
  (define sample-rate 44100)
  (define buffer-length
    (max 1
         (exact-round (* sample-rate (/ (obs-peek @duration-ms) 1000.0)))))
  (define buffer (audio-context-create-buffer ctx 1 buffer-length sample-rate))
  (fill-loopable-sine-buffer! buffer (obs-peek @frequency))
  (set! current-context ctx)
  (set! current-gain gain-node)
  (set! current-source source)
  (audio-buffer-source-node-set-buffer! source buffer)
  (audio-node-connect source gain-node)
  (audio-node-connect gain-node (audio-context-destination ctx))
  (apply-live-controls!)
  (audio-buffer-source-node-start! source)
  (audio-context-resume ctx)
  (obs-set! @status "playing"))

;; regenerate-buffer! : -> void?
;;   Restart playback so the buffer content is regenerated with new settings.
(define (regenerate-buffer!)
  (when current-source
    (start-playback!)))

(define audio-buffer-playback-app
  (window
   (container #:style "max-width: 820px;"
    (vpanel
     (h1 "Audio Buffer Playback Demo")
     (text "A small buffer-source playground: generate a sine buffer, loop it, and change playback settings live.")
     (text "Click Start Playback to create a fresh AudioContext and play the buffer.")

     (group "Transport"
            (vpanel
             (hpanel
              (button "Start Playback" start-playback!)
              (button "Stop Playback" stop-playback!)
              (button "Regenerate Buffer" regenerate-buffer!))
             (hpanel
              (text "Status: ")
              (text @status))
             (hpanel
              (text "Loop: ")
              (text (@loop? . ~> . (lambda (flag) (if flag "on" "off"))))))))

     (group "Buffer"
            (vpanel
             (text "Frequency")
             (slider @frequency
                     (lambda (new-value)
                       (:= @frequency new-value)
                       (when current-source
                         (start-playback!)))
                     80
                     1200)
             (text (@frequency . ~> . (lambda (hz) (string-append (number->string hz) " Hz"))))
             (text "Duration")
             (slider @duration-ms
                     (lambda (new-value)
                       (:= @duration-ms new-value))
                     150
                     1500)
             (text (@duration-ms . ~> . (lambda (ms) (string-append (number->string ms) " ms"))))
             (text "This demo writes a loop-friendly sine wave into an AudioBuffer and reuses it from an AudioBufferSourceNode.")))

     (group "Playback"
            (vpanel
             (text "Gain")
             (slider @gain-percent
                     (lambda (new-value)
                       (:= @gain-percent new-value)
                       (apply-live-controls!))
                     0
                     100)
             (text (@gain-percent . ~> . (lambda (pct) (string-append (number->string pct) "%"))))
             (text "Playback rate")
             (slider @playback-rate
                     (lambda (new-value)
                       (:= @playback-rate new-value)
                       (apply-live-controls!))
                     50
                     200)
             (text (@playback-rate . ~> . (lambda (pct)
                                             (string-append (number->string (/ pct 100.0)) "x"))))
             (hpanel
              (checkbox @loop?
                        (lambda (new-value)
                          (:= @loop? new-value)
                          (apply-live-controls!)))
              (text "Loop buffer")))))))

(define app-renderer
  (render audio-buffer-playback-app))

(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

(mount-renderer! app-renderer)
