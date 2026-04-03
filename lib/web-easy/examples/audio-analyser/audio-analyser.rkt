;;;
;;; Audio Analyser Demo
;;;

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)
(include-lib audio)

(define @status        (@ "stopped"))
(define @frequency     (@ 440))
(define @gain-percent  (@ 75))
(define @canvas-scale   (@ 12))
(define @waveform      (@ "square"))
(define @meter         (@ 0))

(define current-context   #f)
(define current-oscillator #f)
(define current-gain      #f)
(define current-analyser   #f)
(define meter-interval-id  #f)
(define meter-bytes        (js-new (js-Uint8Array) (vector 128)))
(define canvas-width       720.)
(define canvas-height      180.)
(define canvas-name        "audio-analyser-canvas")

;; clamp : real? real? real? -> real?
;;   Clamp v to the inclusive range [lo, hi].
(define (clamp v lo hi)
  (max lo (min hi v)))

;; gain-percent->value : exact-integer? -> real?
;;   Convert a 0..100 gain percentage to a linear gain value.
;;   The top of the range is one quarter of the old mapping.
(define (gain-percent->value n)
  (/ n 400.0))

;; reset-live-graph! : -> void?
;;   Forget the current graph references after stopping.
(define (reset-live-graph!)
  (set! current-context #f)
  (set! current-oscillator #f)
  (set! current-gain #f)
  (set! current-analyser #f))

;; stop-meter! : -> void?
;;   Stop polling the analyser meter.
(define (stop-meter!)
  (when meter-interval-id
    (js-window-clear-interval meter-interval-id))
  (set! meter-interval-id #f)
  (:= @meter 0))

;; get-canvas : -> extern?
;;   Return the waveform canvas element, if it is mounted.
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
;;   Clear the waveform canvas.
(define (clear-canvas! ctx)
  (js-set-canvas2d-fill-style! ctx "#111827")
  (js-canvas2d-fill-rect ctx 0. 0. canvas-width canvas-height)
  (js-set-canvas2d-stroke-style! ctx "#334155")
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-move-to ctx 0. (/ canvas-height 2.))
  (js-canvas2d-line-to ctx canvas-width (/ canvas-height 2.))
  (js-canvas2d-stroke ctx (void)))

;; draw-waveform! : bytes? -> void?
;;   Draw the current analyser bytes as a waveform.
(define (draw-waveform! bs)
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

;; meter-peak->percent : bytes? -> exact-integer?
;;   Convert time-domain bytes into a 0..100 level value.
(define (meter-peak->percent bs)
  (define peak
    (for/fold ([best 0]) ([i (in-range (js-ref bs "length"))])
      (max best (abs (- (js-index bs i) 128)))))
  (exact-round (* 100 (/ peak 128.0))))

;; update-meter! : -> void?
;;   Poll the analyser node and refresh the level indicator.
(define (update-meter!)
  (when current-analyser
    (audio-analyser-node-get-byte-time-domain-data! current-analyser meter-bytes)
    (:= @meter (clamp (meter-peak->percent meter-bytes) 0 100))
    (draw-waveform! meter-bytes)))

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
;;   Push the current UI values into the live nodes.
(define (apply-live-controls!)
  (when current-oscillator
    (audio-param-set-value! (audio-oscillator-node-frequency current-oscillator)
                            (obs-peek @frequency))
    (audio-oscillator-node-set-type! current-oscillator (obs-peek @waveform)))
  (when current-gain
    (audio-param-set-value! (audio-gain-node-gain current-gain)
                            (gain-percent->value (obs-peek @gain-percent)))))

;; stop-demo! : -> void?
;;   Stop the live graph and close the audio context.
(define (stop-demo!)
  (stop-meter!)
  (when current-oscillator
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-oscillator-node-stop! current-oscillator))
    (with-handlers ([exn? (lambda (_e) (void))])
      (audio-context-close current-context)))
  (reset-live-graph!)
  (obs-set! @status "stopped"))

;; start-demo! : -> void?
;;   Create an oscillator, analyser, and gain stage and start playback.
(define (start-demo!)
  (stop-demo!)
  (define ctx (audio-context-new))
  (define osc (audio-context-create-oscillator ctx))
  (define gain-node (audio-context-create-gain ctx))
  (define analyser (audio-context-create-analyser ctx))
  (audio-analyser-node-set-fft-size! analyser 256)
  (audio-analyser-node-set-smoothing-time-constant! analyser 0.0)
  (set! current-context ctx)
  (set! current-oscillator osc)
  (set! current-gain gain-node)
  (set! current-analyser analyser)
  (audio-node-connect osc gain-node)
  (audio-node-connect gain-node analyser)
  (audio-node-connect analyser (audio-context-destination ctx))
  (apply-live-controls!)
  (audio-oscillator-node-start! osc)
  (audio-context-resume ctx)
  (start-meter!)
  (obs-set! @status "playing"))

;; reset-controls! : -> void?
;;   Restore the default analyser demo values.
(define (reset-controls!)
  (:= @frequency 440)
  (:= @gain-percent 12)
  (:= @canvas-scale 12)
  (:= @waveform "sine")
  (apply-live-controls!))

(define audio-analyser-app
  (window
   (container #:style "max-width: 820px;"
    (vpanel
     (h1 "Audio Analyser Demo")
     (text "A compact analyser playground with an oscillator source and a live time-domain meter.")
     (text "Click Start Tone to open an AudioContext and watch the meter respond.")

     (group "Transport"
            (vpanel
             (hpanel
              (button "Start Tone" start-demo!)
              (button "Stop Tone" stop-demo!)
              (button "Reset Controls" reset-controls!))
             (hpanel
              (text "Status: ")
              (text @status))
             (text "Level")
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
             (text (@gain-percent . ~> . (lambda (pct) (string-append (number->string pct) "%"))))
             (text "The analyser polls the time-domain byte buffer every 50 ms.")))))))

(define app-renderer
  (render audio-analyser-app))

(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

(mount-renderer! app-renderer)
