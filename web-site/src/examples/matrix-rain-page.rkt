;;;
;;; Matrix Rain demo page (WebRacket site page)
;;;

(define matrix-rain-xterm-css
  "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css")

(define matrix-rain-xterm-js
  "https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js")

(define matrix-rain-xterm-fit-addon-js
  "https://cdn.jsdelivr.net/npm/@xterm/addon-fit@0.10.0/lib/addon-fit.min.js")

(define (matrix-rain-page)
  `(div (@ (class "page page--matrix-rain"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "XtermJS")
                           (span (@ (class "pill")) "Animation")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "Matrix Rain")
                      (p (@ (class "hero-lead"))
                         "A browser terminal rendering a digital-rain animation in real time.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (id "matrix-root")
                              (style "width: 100%; height: 70vh; border-radius: 14px; overflow: hidden; border: 1px solid rgba(80, 110, 80, 0.5); background: radial-gradient(circle at center, #050 0%, #000 60%);")))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details"))
                           (p "This page is adapted from the repository example in "
                              (code "examples/matrix-rain/matrix-rain.rkt")
                              ". Type in the terminal to trigger bursts in nearby columns.")
                           (div (@ (class "mathjax-actions"))
                                ,(code-pill (gh-file "examples/matrix-rain/matrix-rain.rkt") "Example source")
                                ,(code-pill (gh-file "web-site/src/examples/matrix-rain-page.rkt") "Page layout")))))
        ,(footer-section)))

(define matrix-rain-started? #f)

(define (matrix-rain-init-terminal)
  (when (not matrix-rain-started?)
    (set! matrix-rain-started? #t)

    (define container (js-get-element-by-id "matrix-root"))
    (define terminal-options
      (js-object
       (vector
        (vector "theme"
                (js-object
                 (vector (vector "background" "#000000")))))))
    (define terminal (xterm-terminal-new terminal-options))

    (define fit-addon
      (let* ([win               (js-window-window)]
             [addon-namespace   (js-ref/extern win "FitAddon")]
             [addon-constructor (js-ref/extern addon-namespace "FitAddon")])
        (js-new addon-constructor (vector))))

    (xterm-terminal-load-addon terminal fit-addon)
    (xterm-terminal-open terminal container)
    (xterm-fit-addon-fit fit-addon)
    (xterm-terminal-focus terminal)
    (xterm-terminal-write terminal "\u001b[2J\u001b[?25l" (void))

    (define columns (inexact->exact (xterm-terminal-cols terminal)))
    (define rows    (inexact->exact (xterm-terminal-rows terminal)))

    (struct drop (position speed trail last-row) #:mutable)

    (define (->f x)
      (if (exact? x) (exact->inexact x) x))

    (define glyphs
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛ")
    (define glyph-count (string-length glyphs))
    (define (random-glyph)
      (string-ref glyphs (random glyph-count)))

    (define (randf)
      (->f (random)))

    (define (random-between low high)
      (+ low (* (randf) (- high low))))

    (define fade-rate 2.6)
    (define base-spawn-rate 0.32)
    (define min-speed 14.)
    (define max-speed 32.)
    (define min-trail 8.)
    (define max-trail 22.)
    (define burst-speed-min 32.)
    (define burst-speed-max 48.)
    (define burst-trail-min 14.)
    (define burst-trail-max 26.)

    (define intensities (make-vector rows #f))
    (define characters (make-vector rows #f))
    (for ([row (in-range rows)])
      (vector-set! intensities row (make-vector columns 0.0))
      (vector-set! characters row (make-vector columns #\space)))

    (define column-drops (make-vector columns #f))

    (define (boost-intensity! row col target)
      (when (and (>= row 0) (< row rows))
        (define row-int (vector-ref intensities row))
        (define current (vector-ref row-int col))
        (when (< current target)
          (vector-set! row-int col (min 1. target)))))

    (define (set-cell! row col intensity char [head? #f])
      (when (and (>= row 0) (< row rows))
        (define row-int (vector-ref intensities row))
        (define row-char (vector-ref characters row))
        (vector-set! row-int col (min 1. intensity))
        (vector-set! row-char col char)
        (when head?
          (boost-intensity! (sub1 row) col 0.65))))

    (define (spawn-drop! col [position (- (random-between 0. rows))]
                         [speed    (random-between min-speed max-speed)]
                         [trail    (random-between min-trail max-trail)])
      (vector-set! column-drops col
                   (drop position speed trail (inexact->exact (floor position)))))

    (define (fade-grid! dt)
      (define decay (* dt fade-rate))
      (for ([row (in-range rows)])
        (define row-int (vector-ref intensities row))
        (define row-char (vector-ref characters row))
        (for ([col (in-range columns)])
          (define value (vector-ref row-int col))
          (when (> value 0.)
            (define new-value (max 0. (- value decay)))
            (if (<= new-value 0.05)
                (begin
                  (vector-set! row-int col 0.)
                  (vector-set! row-char col #\space))
                (begin
                  (vector-set! row-int col new-value)
                  (when (< (randf) (* dt 0.08))
                    (vector-set! row-char col (random-glyph)))))))))

    (define (update-drops! dt)
      (for ([col (in-range columns)])
        (define current-drop (vector-ref column-drops col))
        (when current-drop
          (define new-pos (+ (drop-position current-drop)
                             (* (drop-speed current-drop) dt)))
          (set-drop-position! current-drop new-pos)
          (define head-row (inexact->exact (floor new-pos)))
          (define last-row (drop-last-row current-drop))
          (when (> head-row last-row)
            (for ([row (in-range (add1 last-row) (add1 head-row))])
              (when (and (>= row 0) (< row rows))
                (set-cell! row col 1.0 (random-glyph) (= row head-row))
                (boost-intensity! (sub1 row) col 0.55)))
            (set-drop-last-row! current-drop head-row))
          (when (>= new-pos (+ rows (drop-trail current-drop)))
            (vector-set! column-drops col #f)))))

    (define (maybe-spawn-drops! dt)
      (for ([col (in-range columns)])
        (when (not (vector-ref column-drops col))
          (when (< (randf) (* base-spawn-rate dt))
            (spawn-drop! col)))))

    (define (boost-column! col)
      (for ([row (in-range rows)])
        (define row-int (vector-ref intensities row))
        (define value (vector-ref row-int col))
        (when (> value 0.)
          (vector-set! row-int col (min 1. (+ value 0.3))))))

    (define (flare-column! col)
      (for ([row (in-range (min rows 4))])
        (set-cell! row col (+ 0.5 (* (randf) 0.5)) (random-glyph))))

    (define (disturb! data)
      (when (> columns 0)
        (define center
          (modulo
           (+ (for/fold ([acc 0]) ([ch (in-list (string->list data))])
                (+ acc (char->integer ch)))
              columns)
           columns))
        (for ([offset '(-2 -1 0 1 2)])
          (define col (modulo (+ center offset columns) columns))
          (spawn-drop! col (random-between -2. 0.25)
                       (random-between burst-speed-min burst-speed-max)
                       (random-between burst-trail-min burst-trail-max))
          (boost-column! col)
          (flare-column! col))))

    (define (render!)
      (define payload
        (call-with-output-string
         (λ (out)
           (display "\u001b[H" out)
           (for ([row (in-range rows)])
             (display "\u001b[0m" out)
             (define row-int (vector-ref intensities row))
             (define row-char (vector-ref characters row))
             (for ([col (in-range columns)])
               (define value (vector-ref row-int col))
               (define char  (vector-ref row-char col))
               (if (<= value 0.)
                   (write-char #\space out)
                   (let* ([i (max 0. (min 1. value))]
                          [g (inexact->exact (round (+ 80. (* i 175.))))]
                          [r (inexact->exact (round (* i 40.)))]
                          [b (inexact->exact (round (* i 70.)))])
                     (fprintf out "\u001b[38;2;~a;~a;~am~a" r g b char))))
             (display "\u001b[0m" out)
             (when (< row (sub1 rows))
               (newline out))))))
      (xterm-terminal-write terminal payload (void)))

    (define last-time #f)
    (define tick-external #f)

    (define (update! dt)
      (fade-grid! dt)
      (update-drops! dt)
      (maybe-spawn-drops! dt))

    (define (tick timestamp)
      (define delta (if last-time (- timestamp last-time) 16.))
      (define dt (min 0.1 (* 0.001 delta)))
      (set! last-time timestamp)
      (update! dt)
      (render!)
      (js-window-request-animation-frame tick-external))

    (define on-data-external
      (procedure->external
       (λ (data)
         (when (string? data)
           (disturb! data))
         (void))))

    (js-send terminal "onData" (vector on-data-external))

    (for ([col (in-range columns)])
      (when (< (randf) 0.6)
        (spawn-drop! col (random-between (- rows) 0.))))

    (render!)
    (set! tick-external (procedure->external tick))
    (js-window-request-animation-frame tick-external)
    (void)))

(define (nullish? x)
  (cond
    [(not x) #t]
    [else
     (define s (js-value->string x))
     (or (string=? s "null")
         (string=? s "undefined"))]))

(define (ensure-matrix-rain-assets!)
  (define head (js-document-head))

  (define (maybe-init-terminal)
    (when (and (js-ref (js-var "window") "Terminal")
               (js-ref (js-var "window") "FitAddon"))
      (matrix-rain-init-terminal)))

  (define style-id "matrix-rain-xterm-css")
  (define style-existing (js-get-element-by-id style-id))
  (when (nullish? style-existing)
    (define link (js-create-element "link"))
    (js-set-attribute! link "id" style-id)
    (js-set-attribute! link "rel" "stylesheet")
    (js-set-attribute! link "href" matrix-rain-xterm-css)
    (js-append-child! head link))

  (define script-id "matrix-rain-xterm-js")
  (define script-existing (js-get-element-by-id script-id))
  (define fit-script-id "matrix-rain-xterm-fit-addon-js")
  (define fit-script-existing (js-get-element-by-id fit-script-id))

  (define maybe-init-external
    (procedure->external (λ (_) (maybe-init-terminal) (void))))

  (cond
    [(not (nullish? script-existing))
     (js-add-event-listener! script-existing "load" maybe-init-external)]
    [else
     (define script (js-create-element "script"))
     (js-set-attribute! script "id" script-id)
     (js-set-attribute! script "src" matrix-rain-xterm-js)
     (js-add-event-listener! script "load" maybe-init-external)
     (js-append-child! head script)])

  (cond
    [(not (nullish? fit-script-existing))
     (js-add-event-listener! fit-script-existing "load" maybe-init-external)]
    [else
     (define script (js-create-element "script"))
     (js-set-attribute! script "id" fit-script-id)
     (js-set-attribute! script "src" matrix-rain-xterm-fit-addon-js)
     (js-add-event-listener! script "load" maybe-init-external)
     (js-append-child! head script)])

  (maybe-init-terminal))

(define (init-matrix-rain-page!)
  (js-set! (js-var "document") "title" "Matrix Rain")
  (ensure-matrix-rain-assets!))
