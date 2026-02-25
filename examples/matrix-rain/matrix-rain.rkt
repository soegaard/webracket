;;;
;;; MATRIX RAIN
;;;

;; An attempt to recreate the famous screen saver from the Matrix movies.
;; This doubles as an example of how to use xtermjs.

;; Compile this demo with:

;;    racket -t ../../webracket.rkt -- --ffi xtermjs --ffi dom --ffi standard -b matrix-rain.rkt

;;;
;;; Utilities
;;;

; Ensure inexactness
(define (inexact x)
  (if (exact? x) (exact->inexact x) x))

(define (exact x)
  (if (inexact? x) (inexact->exact x) x))

(define (randf)
  (inexact (random)))

(define (random-between low high)
  (+ low (* (randf) (- high low))))

;;;
;;; Glyphs
;;;

(define glyphs (string-append "0123456789"
                              "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              "abcdefghijklmnopqrstuvwxyz"
                              "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛ"))  
(define glyph-count (string-length glyphs))

(define (random-glyph) (string-ref glyphs (random glyph-count)))


;; We need to load external scripts first (here xterm from cdn).
;; Then we can begin the actual program.
;; Using the `load` event listener, we get alerted when the external
;; scripts are loaded.

(define (load-and-start)
  ; - load the style sheet in parallel to the script
  ; - don't wait for the style sheet to load

  (define head (js-document-head))
  
  ;; Use the xtermjs stylesheet
  (define link (js-create-element "link"))
  (js-set-attribute! link "rel" "stylesheet")
  (js-set-attribute! link "href" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css")
  (js-append-child! head link)

  ;; Load xtermjs from cdn
  (define script (js-create-element "script"))
  (js-set-attribute!      script "src" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js")
  (js-add-event-listener! script "load" (procedure->external init-terminal))
  (js-append-child! head  script))


(define (init-terminal . _)
  (define body (js-document-body))
  (js-set-attribute! body "style"
                     (string-append
                      "margin: 0; height: 100vh; background: #000; "
                      "display: flex; align-items: center; justify-content: center; "
                      "font-family: 'Fira Code', 'Source Code Pro', monospace;"))

  (define container (js-create-element "div"))
  (js-set-attribute! container "id" "matrix-root")
  (js-set-attribute! container "style"
                     (string-append
                      "width: 100vw; height: 100vh; display: flex; "
                      "align-items: center; justify-content: center; "
                      "background: radial-gradient(circle at center, #050 0%, #000 60%);"))
  (js-append-child! body container)
  
  ;;;
  ;;; SETUP TERMINAL
  ;;;
  (define terminal (xterm-terminal-new (void)))
  (xterm-terminal-open  terminal container)
  (xterm-terminal-focus terminal)
  ; erase display and hide cursor
  (xterm-terminal-write terminal "\u001b[2J\u001b[?25l" (void))

  (define columns (exact (xterm-terminal-cols terminal)))
  (define rows    (exact (xterm-terminal-rows terminal)))

  ;;;
  ;;; CONFIGURATION
  ;;;
  
  (define fade-rate        2.6)
  (define base-spawn-rate  0.32)
  (define min-speed       14.)
  (define max-speed       32.)
  (define min-trail        8.)
  (define max-trail       22.)
  (define burst-speed-min 32.)
  (define burst-speed-max 48.)
  (define burst-trail-min 14.)
  (define burst-trail-max 26.)
  
  ;;;
  ;;; SCREEN
  ;;;

  ;; The screen is modelled as an 2d array of characters
  ;; and intensities.

  ;; Clear screen
  (define intensities (make-vector rows #f))
  (define characters  (make-vector rows #f))
  (for ([row (in-range rows)])
    (vector-set! intensities row (make-vector columns 0.0))
    (vector-set! characters  row (make-vector columns #\space)))

  ;;;
  ;;; TRAILS
  ;;;

  ; A `drop` represents the leading, falling character in a trail.
  
  (struct drop (position speed trail last-row) #:mutable)  
  (define column-drops (make-vector columns #f))

  
  (define (boost-intensity! row col target)
    (when (and (>= row 0) (< row rows))
      (define row-int (vector-ref intensities row))
      (define current (vector-ref row-int col))
      (when (< current target)
        (vector-set! row-int col (min 1. target)))))

  (define (set-cell! row col intensity char [head? #f])
    (when (and (>= row 0) (< row rows))
      (define row-int  (vector-ref intensities row))
      (define row-char (vector-ref characters  row))
      (vector-set! row-int  col (min 1. intensity))
      (vector-set! row-char col char)
      (when head?
        (boost-intensity! (sub1 row) col 0.65))))

  (define (spawn-drop! col
                       [position (- (random-between 0. rows))]
                       [speed    (random-between min-speed max-speed)]
                       [trail    (random-between min-trail max-trail)])
    (vector-set! column-drops col
                 (drop position speed trail (exact (floor position)))))

  (define (fade-grid! dt)
    (define decay (* dt fade-rate))
    (for ([row (in-range rows)])
      (define row-int  (vector-ref intensities row))
      (define row-char (vector-ref characters  row))
      (for ([col (in-range columns)])
        (define value (vector-ref row-int col))
        (when (> value 0.)
          (define new-value (max 0. (- value decay)))
          (if (<= new-value 0.05)
              (begin
                (vector-set! row-int  col 0.)
                (vector-set! row-char col #\space))
              (begin
                (vector-set! row-int  col new-value)
                (when (< (randf) (* dt 0.08))
                  (vector-set! row-char col (random-glyph)))))))))

  (define (update-drops! dt)
    (for ([col (in-range columns)])
      (define current-drop (vector-ref column-drops col))
      (when current-drop
        (define new-pos (+ (drop-position current-drop)
                           (* (drop-speed current-drop) dt)))
        (set-drop-position! current-drop new-pos)
        (define head-row (exact (floor new-pos)))
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
      (unless (vector-ref column-drops col)
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

  (define (intensity->rgb intensity)
    (define i (max 0. (min 1. intensity)))
    (define g (exact (round (+ 80. (* i 175.)))))
    (define r (exact (round (* i 40.))))
    (define b (exact (round (* i 70.))))
    (values r g b))

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
                 (let-values ([(r g b) (intensity->rgb value)])
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
  (void))


;;;
;;; Run
;;;

(load-and-start)
