;;;
;;; Audio Oscillator Demo
;;;

(include-lib web-easy)
(include-lib audio)

;; Demo state.
(define @status          (@ "stopped"))
(define @frequency       (@ 440))
(define @detune          (@ 0))
(define @gain-percent    (@ 12))
(define @waveform        (@ "sine"))
(define @pan-percent     (@ 50))
(define @threshold       (@ -24))
(define @ratio           (@ 12))
(define @meter           (@ 0))

;; Live audio graph references.
(define current-context    #f)
(define current-oscillator #f)
(define current-gain       #f)
(define current-panner     #f)
(define current-compressor #f)
(define current-analyser   #f)
(define meter-interval-id  #f)
(define meter-bytes        (make-bytes 64))

;; clamp : real? real? real? -> real?
;;   Clamp v to the inclusive range [lo, hi].
(define (clamp v lo hi)
  (max lo (min hi v)))

;; gain-percent->value : exact-integer? -> real?
;;   Convert a 0..100 gain percentage to a linear gain value.
(define (gain-percent->value n)
  (/ n 100.0))

;; pan-percent->value : exact-integer? -> real?
;;   Convert a 0..100 pan percentage to the stereo pan range -1..1.
(define (pan-percent->value n)
  (/ (- n 50) 50.0))

;; reset-live-graph! : -> void?
;;   Drop the current audio graph references.
(define (reset-live-graph!)
  (set! current-context #f)
  (set! current-oscillator #f)
  (set! current-gain #f)
  (set! current-panner #f)
  (set! current-compressor #f)
  (set! current-analyser #f))

;; stop-meter! : -> void?
;;   Stop the analyser polling timer.
(define (stop-meter!)
  (when meter-interval-id
    (js-window-clear-interval meter-interval-id))
  (set! meter-interval-id #f)
  (:= @meter 0))

;; meter-peak->percent : bytes? -> exact-integer?
;;   Turn a byte time-domain snapshot into a 0..100 meter value.
(define (meter-peak->percent bs)
  (define peak
    (for/fold ([best 0]) ([i (in-range (bytes-length bs))])
      (max best (abs (- (bytes-ref bs i) 128)))))
  (exact-round (* 100 (/ peak 128.0))))

;; update-meter! : -> void?
;;   Poll the analyser node and update the level meter.
(define (update-meter!)
  (when current-analyser
    (audio-analyser-node-get-byte-time-domain-data! current-analyser meter-bytes)
    (:= @meter (clamp (meter-peak->percent meter-bytes) 0 100))))

;; start-meter! : -> void?
;;   Start polling the analyser node, if there is one.
(define (start-meter!)
  (stop-meter!)
  (when current-analyser
    (update-meter!)
    (set! meter-interval-id
          (js-window-set-interval
           (procedure->external update-meter!)
           60))))

;; apply-live-controls! : -> void?
;;   Push the current control values into the live audio graph.
(define (apply-live-controls!)
  (when current-oscillator
    (audio-param-set-value! (audio-oscillator-node-frequency current-oscillator)
                            (obs-peek @frequency))
    (audio-param-set-value! (audio-oscillator-node-detune current-oscillator)
                            (obs-peek @detune))
    (audio-oscillator-node-set-type! current-oscillator
                                     (obs-peek @waveform)))
  (when current-gain
    (audio-param-set-value! (audio-gain-node-gain current-gain)
                            (gain-percent->value (obs-peek @gain-percent))))
  (when current-panner
    (audio-param-set-value! (audio-stereo-panner-node-pan current-panner)
                            (pan-percent->value (obs-peek @pan-percent))))
  (when current-compressor
    (audio-param-set-value! (audio-dynamics-compressor-node-threshold current-compressor)
                            (obs-peek @threshold))
    (audio-param-set-value! (audio-dynamics-compressor-node-ratio current-compressor)
                            (obs-peek @ratio))))

;; stop-tone! : -> void?
;;   Stop and close the current audio graph if one exists.
(define (stop-tone!)
  (stop-meter!)
  (when current-oscillator
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-oscillator-node-stop! current-oscillator))
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-context-close current-context)))
  (reset-live-graph!)
  (obs-set! @status "stopped"))

;; start-tone! : -> void?
;;   Create a fresh audio context and start the live playground graph.
(define (start-tone!)
  (stop-tone!)
  (define ctx (audio-context-new))
  (define gain-node (audio-context-create-gain ctx))
  (define osc (audio-context-create-oscillator ctx))
  (define panner (audio-context-create-stereo-panner ctx))
  (define compressor (audio-context-create-dynamics-compressor ctx))
  (define analyser (audio-context-create-analyser ctx))
  (set! current-context ctx)
  (set! current-gain gain-node)
  (set! current-oscillator osc)
  (set! current-panner panner)
  (set! current-compressor compressor)
  (set! current-analyser analyser)
  (audio-node-connect osc gain-node)
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
;;   Restore the playground controls to the default tone setup.
(define (reset-controls!)
  (:= @frequency 440)
  (:= @detune 0)
  (:= @gain-percent 12)
  (:= @waveform "sine")
  (:= @pan-percent 50)
  (:= @threshold -24)
  (:= @ratio 12)
  (apply-live-controls!))

(define audio-oscillator-app
  (window
   (container #:style "max-width: 860px;"
    (vpanel
      (h1 "Audio Playground")
      (text "A small sandbox for the browser audio wrappers: oscillator, gain, pan, compressor, and analyser.")
      (text "Click Start Tone to open an AudioContext, then tweak the controls live.")

      (group "Transport"
             (vpanel
              (hpanel
               (button "Start Tone" start-tone!)
               (button "Stop Tone" stop-tone!)
               (button "Reset Controls" reset-controls!))
              (hpanel
               (text "Status: ")
               (text @status))
              (text "Meter")
              (progress @meter #:min 0 #:max 100)
              (text (@meter . ~> . (lambda (n) (string-append "level " (number->string n) "%"))))))

      (group "Tone"
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
                      110
                      880)
              (text (@frequency . ~> . (lambda (hz) (string-append (number->string hz) " Hz"))))
              (text "Detune")
              (slider @detune
                      (lambda (new-value)
                        (:= @detune new-value)
                        (apply-live-controls!))
                      -1200
                      1200)
              (text (@detune . ~> . (lambda (cents) (string-append (number->string cents) " cents"))))
              (text "Gain")
              (slider @gain-percent
                      (lambda (new-value)
                        (:= @gain-percent new-value)
                        (apply-live-controls!))
                      0
                      100)
              (text (@gain-percent . ~> . (lambda (pct) (string-append (number->string pct) "%"))))
              (text "The live graph uses GainNode -> StereoPannerNode -> DynamicsCompressorNode -> AnalyserNode.")))

      (group "Spatial / Dynamics"
             (vpanel
              (text "Pan")
              (slider @pan-percent
                      (lambda (new-value)
                        (:= @pan-percent new-value)
                        (apply-live-controls!))
                      0
                      100)
              (text (@pan-percent . ~> . (lambda (pct)
                                           (string-append "pan "
                                                          (number->string (/ (- pct 50) 50.0))))))
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
              (text (@ratio . ~> . (lambda (r) (string-append (number->string r) ":1")))))
      )))))

(define app-renderer
  (render audio-oscillator-app))

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
