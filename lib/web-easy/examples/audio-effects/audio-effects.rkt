;;;
;;; Audio Effects Chain Demo
;;;

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)
(include-lib audio)

(define @status      (@ "stopped"))
(define @frequency   (@ 220))
(define @gain-percent (@ 10))
(define @canvas-scale (@ 12))
(define @pan-percent (@ 50))
(define @threshold   (@ -24))
(define @ratio       (@ 12))
(define @filter-type (@ "lowpass"))
(define @filter-freq (@ 1200))
(define @waveform    (@ "sawtooth"))
(define @meter       (@ 0))

(define current-context  #f)
(define current-osc      #f)
(define current-gain     #f)
(define current-filter   #f)
(define current-panner   #f)
(define current-compress #f)
(define current-analyser  #f)
(define meter-interval-id #f)
(define meter-bytes       (js-new (js-Uint8Array) (vector 128)))
(define canvas-width      720.)
(define canvas-height     160.)
(define canvas-name       "audio-effects-canvas")

;; gain-percent->value : exact-integer? -> real?
;;   Convert a 0..100 gain percentage to a linear gain value.
;;   The top of the range is one quarter of the old mapping.
(define (gain-percent->value n)
  (/ n 400.0))

;; pan-percent->value : exact-integer? -> real?
;;   Convert a 0..100 slider value into the stereo pan range -1..1.
(define (pan-percent->value n)
  (/ (- n 50) 50.0))

;; reset-live-graph! : -> void?
;;   Forget the current live graph references.
(define (reset-live-graph!)
  (set! current-context #f)
  (set! current-osc #f)
  (set! current-gain #f)
  (set! current-filter #f)
  (set! current-panner #f)
  (set! current-compress #f)
  (set! current-analyser #f))

;; set-param-if-audio! : (or/c #f audio-param?) real? -> void?
;;   Update an AudioParam only when the accessor returned one.
(define (set-param-if-audio! param value)
  (when (audio-param? param)
    (audio-param-set-value! param value)))

;; stop-meter! : -> void?
;;   Stop analyser polling and clear the visual meter.
(define (stop-meter!)
  (when meter-interval-id
    (js-window-clear-interval meter-interval-id))
  (set! meter-interval-id #f)
  (:= @meter 0))

;; get-canvas : -> extern?
;;   Return the waveform canvas element, if present.
(define (get-canvas)
  (define doc (js-var "document"))
  (js-send/extern/nullish doc "getElementById" (vector canvas-name)))

;; get-drawing-context : -> extern?
;;   Return the 2D drawing context for the waveform canvas.
(define (get-drawing-context)
  (define canvas (get-canvas))
  (and canvas
       (js-canvas-get-context canvas "2d" (js-var "undefined"))))

;; clear-canvas! : extern? -> void?
;;   Paint the canvas background and center line.
(define (clear-canvas! ctx)
  (js-set-canvas2d-fill-style! ctx "#111827")
  (js-canvas2d-fill-rect ctx 0. 0. canvas-width canvas-height)
  (js-set-canvas2d-stroke-style! ctx "#334155")
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-move-to ctx 0. (/ canvas-height 2.))
  (js-canvas2d-line-to ctx canvas-width (/ canvas-height 2.))
  (js-canvas2d-stroke ctx (void)))

;; meter-peak->percent : external? -> exact-integer?
;;   Convert analyser bytes into a 0..100 meter value.
(define (meter-peak->percent bs)
  (define count (js-ref bs "length"))
  (define peak
    (for/fold ([best 0]) ([i (in-range count)])
      (max best (abs (- (js-index bs i) 128)))))
  (exact-round (* 100 (/ peak 128.0))))

;; draw-meter! : external? -> void?
;;   Draw the analyser byte buffer as a waveform.
(define (draw-meter! bs)
  (define ctx (get-drawing-context))
  (when ctx
    (clear-canvas! ctx)
    (js-set-canvas2d-stroke-style! ctx "#8f9dff")
    (js-canvas2d-begin-path ctx)
    (define count (js-ref bs "length"))
    (define mid (/ canvas-height 2.))
    (define scale (* (/ canvas-height 2.1) (obs-peek @canvas-scale)))
    (for ([i (in-range count)])
      (define x (* (/ (exact->inexact i) (max 1.0 (exact->inexact (- count 1))))
                   canvas-width))
      (define sample (/ (- (js-index bs i) 128) 128.0))
      (define y (- mid (* sample scale)))
      (if (zero? i)
          (js-canvas2d-move-to ctx x y)
          (js-canvas2d-line-to ctx x y)))
    (js-canvas2d-stroke ctx (void))
    (js-set-canvas2d-stroke-style! ctx "#1d4ed8")
    (js-canvas2d-begin-path ctx)
    (js-canvas2d-move-to ctx 0. mid)
    (js-canvas2d-line-to ctx canvas-width mid)
    (js-canvas2d-stroke ctx (void))))

;; update-meter! : -> void?
;;   Refresh the analyser meter and waveform.
(define (update-meter!)
  (when current-analyser
    (audio-analyser-node-get-byte-time-domain-data! current-analyser meter-bytes)
    (:= @meter (meter-peak->percent meter-bytes))
    (draw-meter! meter-bytes)))

;; start-meter! : -> void?
;;   Begin polling the analyser node.
(define (start-meter!)
  (stop-meter!)
  (when current-analyser
    (update-meter!)
    (set! meter-interval-id
          (js-window-set-interval
           (procedure->external update-meter!)
           50.))))

;; apply-live-controls! : -> void?
;;   Push the current control values into the active graph.
(define (apply-live-controls!)
  (when current-osc
    (set-param-if-audio! (audio-oscillator-node-frequency current-osc)
                         (obs-peek @frequency))
    (audio-oscillator-node-set-type! current-osc (obs-peek @waveform)))
  (when current-gain
    (set-param-if-audio! (audio-gain-node-gain current-gain)
                         (gain-percent->value (obs-peek @gain-percent))))
  (when current-panner
    (set-param-if-audio! (audio-stereo-panner-node-pan current-panner)
                         (pan-percent->value (obs-peek @pan-percent))))
  (when current-compress
    (set-param-if-audio! (audio-dynamics-compressor-node-threshold current-compress)
                         (obs-peek @threshold))
    (set-param-if-audio! (audio-dynamics-compressor-node-ratio current-compress)
                         (obs-peek @ratio)))
  (when current-filter
    (audio-biquad-filter-node-set-type! current-filter (obs-peek @filter-type))
    (set-param-if-audio! (audio-biquad-filter-node-frequency current-filter)
                         (obs-peek @filter-freq))))

;; stop-demo! : -> void?
;;   Stop and close the current graph.
(define (stop-demo!)
  (stop-meter!)
  (when current-osc
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-oscillator-node-stop! current-osc))
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-context-close current-context)))
  (reset-live-graph!)
  (obs-set! @status "stopped"))

;; start-demo! : -> void?
;;   Build the audio-effects chain and begin playback.
(define (start-demo!)
  (stop-demo!)
  (define ctx (audio-context-new))
  (define osc (audio-context-create-oscillator ctx))
  (define gain-node (audio-context-create-gain ctx))
  (define filter-node (audio-context-create-biquad-filter ctx))
  (define panner (audio-context-create-stereo-panner ctx))
  (define compressor (audio-context-create-dynamics-compressor ctx))
  (define analyser (audio-context-create-analyser ctx))
  (audio-analyser-node-set-fft-size! analyser 256)
  (audio-analyser-node-set-smoothing-time-constant! analyser 0.0)
  (set! current-context ctx)
  (set! current-osc osc)
  (set! current-gain gain-node)
  (set! current-filter filter-node)
  (set! current-panner panner)
  (set! current-compress compressor)
  (set! current-analyser analyser)
  (audio-node-connect osc filter-node)
  (audio-node-connect filter-node gain-node)
  (audio-node-connect gain-node panner)
  (audio-node-connect panner compressor)
  (audio-node-connect compressor analyser)
  (audio-node-connect analyser (audio-context-destination ctx))
  (apply-live-controls!)
  (audio-oscillator-node-start! osc)
  (audio-context-resume ctx)
  (start-meter!)
  (obs-set! @status "playing"))

;; reset-controls! : -> void?
;;   Restore the default effect chain values.
(define (reset-controls!)
  (:= @frequency 220)
  (:= @gain-percent 10)
  (:= @canvas-scale 12)
  (:= @pan-percent 50)
  (:= @threshold -24)
  (:= @ratio 12)
  (:= @filter-type "lowpass")
  (:= @filter-freq 1200)
  (:= @waveform "sawtooth")
  (apply-live-controls!))

(define audio-effects-app
  (window
   (container #:style "max-width: 900px;"
    (vpanel
     (h1 "Audio Effects Chain Demo")
     (text "A compact graph playground with oscillator, filter, gain, pan, and compressor stages.")
     (text "Use the sliders to hear the chain change in real time.")

     (group "Transport"
            (vpanel
             (hpanel
              (button "Start" start-demo!)
              (button "Stop" stop-demo!)
              (button "Reset Controls" reset-controls!))
             (hpanel
              (text "Status: ")
              (text @status))))
     (group "Meter"
            (vpanel
             (text "Live analyser")
             (progress @meter #:min 0 #:max 100)
             (text (@meter . ~> . (lambda (n) (string-append (number->string n) "%"))))
             (text "Canvas amplitude")
             (slider @canvas-scale
                     (lambda (new-value)
                       (:= @canvas-scale new-value))
                     1
                     24)
             (text (@canvas-scale . ~> . (lambda (n)
                                           (string-append (number->string n) "x"))))
             (Canvas #:id canvas-name
                     #:width canvas-width
                     #:height canvas-height)))

     (group "Source"
            (vpanel
             (text "Waveform")
             (choice '("sine" "square" "sawtooth" "triangle")
                     @waveform
                     (lambda (new-value)
                       (:= @waveform new-value)
                       (apply-live-controls!)))
             (text "Frequency")
             (slider @frequency
                     (lambda (new-value)
                       (:= @frequency new-value)
                       (apply-live-controls!))
                     80
                     1200)
             (text (@frequency . ~> . (lambda (hz) (string-append (number->string hz) " Hz"))))
             (text "Gain")
             (slider @gain-percent
                     (lambda (new-value)
                       (:= @gain-percent new-value)
                       (apply-live-controls!))
                     0
                     100)
             (text (@gain-percent . ~> . (lambda (pct) (string-append (number->string pct) "%"))))))

     (group "Filter"
            (vpanel
             (text "Type")
             (choice '("lowpass" "highpass" "bandpass" "notch")
                     @filter-type
                     (lambda (new-value)
                       (:= @filter-type new-value)
                       (apply-live-controls!)))
             (text "Frequency")
             (slider @filter-freq
                     (lambda (new-value)
                       (:= @filter-freq new-value)
                       (apply-live-controls!))
                     80
                     5000)
             (text (@filter-freq . ~> . (lambda (hz) (string-append (number->string hz) " Hz"))))
             (text "Q is fixed in this demo to keep the browser bridge simple.")))

     (group "Space / Dynamics"
            (vpanel
             (text "Pan")
             (slider @pan-percent
                     (lambda (new-value)
                       (:= @pan-percent new-value)
                       (apply-live-controls!))
                     0
                     100)
             (text (@pan-percent . ~> . (lambda (pct)
                                           (string-append "pan " (number->string (pan-percent->value pct))))))
             (text "Compressor threshold")
             (slider @threshold
                     (lambda (new-value)
                       (:= @threshold new-value)
                       (apply-live-controls!))
                     -60
                     0)
             (text (@threshold . ~> . (lambda (db) (string-append (number->string db) " dB"))))
             (text "Compressor ratio")
             (slider @ratio
                     (lambda (new-value)
                       (:= @ratio new-value)
                       (apply-live-controls!))
                     1
                     20)
             (text (@ratio . ~> . (lambda (r) (string-append (number->string r) ":1"))))
             (text "The chain runs oscillator -> biquad filter -> gain -> stereo panner -> compressor -> destination.")))))))

(define app-renderer
  (render audio-effects-app))

(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

(mount-renderer! app-renderer)
