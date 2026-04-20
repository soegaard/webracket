;;;
;;; Web Pict Support
;;;

;; Shared color, alpha, font, and canvas helpers used by the public
;; `web-pict` surface.
;;;
;;; Pict
;;;

;; This is a port of `pict` to WebRacket.
;; The port started from this file:
;;
;;     https://github.com/racket/pict/blob/master/pict-lib/pict/main.rkt
;;
;; Which contains definitions for the `pict` structure.
;;
;; Since we are rendering to a browser canvas, we needed to replace
;; the rendering. Also, since we haven't got support for the Racket
;; object system yet, we need to provide an alternative way of
;; working with fonts and colors (instead of font% and color%).


;;;
;;; Color
;;;

(struct rgb-color (r g b a) #:transparent)
(struct color     (value)   #:transparent)
; value is either a string or an `rgb-color`.

(define make-rgb-color
  (case-lambda
    [(r g b)   (rgb-color r g b 255)]
    [(r g b a) (rgb-color r g b a)]))

(define make-color
  (case-lambda
    [(s)       (color s)]
    [(r g b)   (color (make-rgb-color r g b))]
    [(r g b a) (color (make-rgb-color r g b a))]))

(define css-color-keywords
  '("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure"
    "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet"
    "brown" "burlywood" "cadetblue" "chartreuse" "chocolate"
    "coral" "cornflowerblue" "cornsilk" "crimson" "cyan"
    "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen"
    "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen"
    "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen"
    "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise"
    "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey"
    "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia"
    "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green"
    "greenyellow" "grey" "honeydew" "hotpink" "indianred"
    "indigo" "ivory" "khaki" "lavender" "lavenderblush"
    "lawngreen" "lemonchiffon" "lightblue" "lightcoral"
    "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen"
    "lightgrey" "lightpink" "lightsalmon" "lightseagreen"
    "lightskyblue" "lightslategray" "lightslategrey" "lightsteelblue"
    "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon"
    "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple"
    "mediumseagreen" "mediumslateblue" "mediumspringgreen"
    "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream"
    "mistyrose" "moccasin" "navajowhite" "navy" "oldlace"
    "olive" "olivedrab" "orange" "orangered" "orchid"
    "palegoldenrod" "palegreen" "paleturquoise" "palevioletred"
    "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue"
    "purple" "rebeccapurple" "red" "rosybrown" "royalblue"
    "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell"
    "sienna" "silver" "skyblue" "slateblue" "slategray"
    "slategrey" "snow" "springgreen" "steelblue" "tan" "teal"
    "thistle" "tomato" "turquoise" "violet" "wheat" "white"
    "whitesmoke" "yellow" "yellowgreen" "transparent"
    "currentcolor"))

(define color->string
  (let ()
    (define (string-prefix-ci? s prefix)
      (let ([len (string-length s)]
            [plen (string-length prefix)])
        (and (>= len plen)
             (string=? (string-downcase (substring s 0 plen))
                       (string-downcase prefix)))))

    (define (string-suffix-ci? s suffix)
      (let ([len (string-length s)]
            [slen (string-length suffix)])
        (and (>= len slen)
             (string=? (string-downcase (substring s (- len slen) len))
                       (string-downcase suffix)))))

    (define (string-suffix? s suffix)
      (let ([len (string-length s)]
            [slen (string-length suffix)])
        (and (>= len slen)
             (string=? (substring s (- len slen) len) suffix))))

    (define (string-index-of s ch)
      (let ([len (string-length s)])
        (let loop ([i 0])
          (cond
            [(= i len) #f]
            [(char=? (string-ref s i) ch) i]
            [else (loop (add1 i))]))))

    (define (split-by-commas s)
      (let ([len (string-length s)])
        (let loop ([i 0] [start 0] [parts null])
          (if (= i len)
              (reverse (cons (string-trim (substring s start len)) parts))
              (let ([ch (string-ref s i)])
                (if (char=? ch #\,)
                    (loop (add1 i)
                          (add1 i)
                          (cons (string-trim (substring s start i)) parts))
                    (loop (add1 i) start parts)))))))

    (define (split-by-spaces s)
      (let ([len (string-length s)])
        (let loop ([i 0] [parts null])
          (if (>= i len)
              (reverse parts)
              (let find-start ([k i])
                (cond
                  [(>= k len) (reverse parts)]
                  [(char-whitespace? (string-ref s k))
                   (loop (add1 k) parts)]
                  [else
                   (let find-end ([j k])
                     (if (or (>= j len) (char-whitespace? (string-ref s j)))
                         (loop j (cons (substring s k j) parts))
                         (find-end (add1 j))))]))))))

    (define (hex-digit? ch)
      (or (and (char>=? ch #\0) (char<=? ch #\9))
          (and (char>=? ch #\a) (char<=? ch #\f))
          (and (char>=? ch #\A) (char<=? ch #\F))))

    (define (css-hex-color-string? s)
      (let ([len (string-length s)])
        (and (>= len 2)
             (char=? (string-ref s 0) #\#)
             (let ([digits (- len 1)])
               (and (or (= digits 3) (= digits 4) (= digits 6) (= digits 8))
                    (let loop ([i 1])
                      (cond
                        [(= i len) #t]
                        [(hex-digit? (string-ref s i)) (loop (add1 i))]
                        [else #f])))))))

    (define (css-color-keyword? s)
      (let ([lower (string-downcase s)])
        (let loop ([lst css-color-keywords])
          (cond
            [(null? lst) #f]
            [(string=? lower (car lst)) #t]
            [else (loop (cdr lst))]))))

    (define (parse-real-number s)
      (let ([n (string->number s)])
        (and n (real? n) n)))

    (define (all-true? items pred)
      (let loop ([lst items])
        (cond
          [(null? lst) #t]
          [(pred (car lst)) (loop (cdr lst))]
          [else #f])))

    (define (parse-rgb-component s)
      (cond
        [(string-suffix? s "%")
         (let ([n (parse-real-number (substring s 0 (sub1 (string-length s))))])
           (and n (<= 0 n) (<= n 100)))]
        [else
         (let ([n (parse-real-number s)])
           (and n (<= 0 n) (<= n 255)))]))

    (define (parse-alpha-component s)
      (cond
        [(string-suffix? s "%")
         (let ([n (parse-real-number (substring s 0 (sub1 (string-length s))))])
           (and n (<= 0 n) (<= n 100)))]
        [else
         (let ([n (parse-real-number s)])
           (and n (<= 0 n) (<= n 1)))]))

    (define (parse-angle-component s)
      (let ([trimmed (string-trim s)])
        (let ([len (string-length trimmed)])
          (cond
            [(zero? len) #f]
            [else
             (let loop ([units '("deg" "grad" "rad" "turn")])
               (cond
                 [(null? units)
                  (parse-real-number trimmed)]
                 [(string-suffix-ci? trimmed (car units))
                  (let* ([unit (car units)]
                         [u-len (string-length unit)]
                         [num (string-trim (substring trimmed 0 (- len u-len)))])
                    (and (not (string=? num "")) (parse-real-number num)))]
                 [else (loop (cdr units))]))]))))

    (define (parse-percentage-component s)
      (and (string-suffix? s "%")
           (let ([n (parse-real-number (substring s 0 (sub1 (string-length s))))])
             (and n (<= 0 n) (<= n 100)))))

    (define (rgb-body-valid-comma? body require-alpha?)
      (let ([parts (split-by-commas body)])
        (let ([len (length parts)])
          (cond
            [(or (< len 3) (> len 4)) #f]
            [else
             (and (all-true? (list (car parts) (cadr parts) (caddr parts))
                             parse-rgb-component)
                  (cond
                    [(= len 3) (not require-alpha?)]
                    [(= len 4) (parse-alpha-component (list-ref parts 3))]
                    [else #f]))]))))

    (define (rgb-body-valid-space? body require-alpha?)
      (let* ([trimmed (string-trim body)]
             [len (string-length trimmed)]
             [slash (string-index-of trimmed #\/)])
        (if slash
            (let ([color-part (string-trim (substring trimmed 0 slash))]
                  [alpha-part (string-trim (substring trimmed (add1 slash) len))])
              (and (not (string=? color-part ""))
                   (not (string=? alpha-part ""))
                   (parse-alpha-component alpha-part)
                   (let ([items (split-by-spaces color-part)])
                     (and (= (length items) 3)
                          (all-true? items parse-rgb-component)))))
            (let ([items (split-by-spaces trimmed)])
              (cond
                [(= (length items) 3)
                 (and (all-true? items parse-rgb-component)
                      (not require-alpha?))]
                [(= (length items) 4)
                 (and (all-true? (list (car items) (cadr items) (caddr items))
                                 parse-rgb-component)
                      (parse-alpha-component (cadddr items)))]
                [else #f])))))

    (define (rgb-body-valid? body require-alpha?)
      (let ([trimmed (string-trim body)])
        (and (not (string=? trimmed ""))
             (or (rgb-body-valid-comma? trimmed require-alpha?)
                 (rgb-body-valid-space? trimmed require-alpha?)))))

    (define (hsl-body-valid-comma? body require-alpha?)
      (let ([parts (split-by-commas body)])
        (let ([len (length parts)])
          (cond
            [(or (< len 3) (> len 4)) #f]
            [else
             (and (parse-angle-component (car parts))
                  (parse-percentage-component (cadr parts))
                  (parse-percentage-component (caddr parts))
                  (cond
                    [(= len 3) (not require-alpha?)]
                    [(= len 4) (parse-alpha-component (list-ref parts 3))]
                    [else #f]))]))))

    (define (hsl-body-valid-space? body require-alpha?)
      (let* ([trimmed (string-trim body)]
             [len (string-length trimmed)]
             [slash (string-index-of trimmed #\/)])
        (if slash
            (let ([color-part (string-trim (substring trimmed 0 slash))]
                  [alpha-part (string-trim (substring trimmed (add1 slash) len))])
              (and (not (string=? color-part ""))
                   (not (string=? alpha-part ""))
                   (parse-alpha-component alpha-part)
                   (let ([items (split-by-spaces color-part)])
                     (and (= (length items) 3)
                          (parse-angle-component (car items))
                          (parse-percentage-component (cadr items))
                          (parse-percentage-component (caddr items))))))
            (let ([items (split-by-spaces trimmed)])
              (cond
                [(= (length items) 3)
                 (and (parse-angle-component (car items))
                      (parse-percentage-component (cadr items))
                      (parse-percentage-component (caddr items))
                      (not require-alpha?))]
                [(= (length items) 4)
                 (and (parse-angle-component (car items))
                      (parse-percentage-component (cadr items))
                      (parse-percentage-component (caddr items))
                      (parse-alpha-component (cadddr items)))]
                [else #f])))))

    (define (hsl-body-valid? body require-alpha?)
      (let ([trimmed (string-trim body)])
        (and (not (string=? trimmed ""))
             (or (hsl-body-valid-comma? trimmed require-alpha?)
                 (hsl-body-valid-space? trimmed require-alpha?)))))

    (define (function-body s prefix)
      (let ([len (string-length s)]
            [plen (string-length prefix)])
        (if (and (>= len (+ plen 1))
                 (string-prefix-ci? s prefix)
                 (char=? (string-ref s (sub1 len)) #\)))
            (substring s plen (sub1 len))
            #f)))

    (define (css-rgb-function-string? s prefix require-alpha?)
      (let ([body (function-body s prefix)])
        (and body (rgb-body-valid? body require-alpha?))))

    (define (css-hsl-function-string? s prefix require-alpha?)
      (let ([body (function-body s prefix)])
        (and body (hsl-body-valid? body require-alpha?))))

    (define (css-rgb-like-string? s)
      (or (css-rgb-function-string? s "rgb(" #f)
          (css-rgb-function-string? s "rgba(" #t)))

    (define (css-hsl-like-string? s)
      (or (css-hsl-function-string? s "hsl(" #f)
          (css-hsl-function-string? s "hsla(" #t)))

    (define css-color-function-prefixes
      '("hwb(" "lab(" "lch(" "oklab(" "oklch(" "color(" "device-cmyk("
               "color-mix(" "var(" "env(" "calc(" "attr("))

    (define (css-generic-color-function? s)
      (let loop ([lst css-color-function-prefixes])
        (cond
          [(null? lst) #f]
          [else
           (let ([body (function-body s (car lst))])
             (if body
                 (not (string=? (string-trim body) ""))
                 (loop (cdr lst))))])))

    (define (css-color-string? s)
      (let ([trimmed (string-trim s)])
        (and (not (string=? trimmed ""))
             (or (css-color-keyword? trimmed)
                 (css-hex-color-string? trimmed)
                 (css-rgb-like-string? trimmed)
                 (css-hsl-like-string? trimmed)
                 (css-generic-color-function? trimmed)))))

    (define (ensure-byte who val name)
      (cond
        [(and (exact-integer? val) (<= 0 val) (<= val 255)) val]
        [(and (inexact? val) (<= 0 val) (<= val 255) (= val (round val)))
         (inexact->exact (round val))]
        [else
         (error who "expected ~a in range 0-255, got: ~a" name val)]))

    (define (alpha-value->string val)
      (define clamped (max 0 (min val 1)))
      (define scaled (inexact->exact (round (* 1000 (exact->inexact clamped)))))
      (define ip (quotient scaled 1000))
      (define frac (remainder scaled 1000))
      (define base (number->string ip))
      (if (= frac 0)
          base
          (let* ([digits (number->string frac)]
                 [padded (case (string-length digits)
                           [(1) (string-append "00" digits)]
                           [(2) (string-append "0" digits)]
                           [else digits])]
                 [trimmed
                  (let loop ([i (sub1 (string-length padded))])
                    (cond
                      [(< i 0) ""]
                      [(char=? (string-ref padded i) #\0)
                       (loop (sub1 i))]
                      [else (substring padded 0 (add1 i))]))])
            (if (string=? trimmed "")
                base
                (string-append base "." trimmed)))))

    (define (normalize-alpha who a)
      (cond
        [(or (not a) (eq? a #f)) #f]
        [(and (exact-integer? a) (<= 0 a) (<= a 255)) (/ a 255)]
        ; [(and (rational? a) (<= 0 a) (<= a 1)) a] ; todo
        [(and (real? a) (<= 0 a) (<= a 1)) a]
        [else (error who "expected alpha in range 0-255 or 0-1, got: ~a" a)]))

    (define (color->string c)
      (cond
        [(or (string? c) (external? c)) c]
        [else
         (let ([val (color-value c)])
           (cond
             [(string? val)
              (let ([trimmed (string-trim val)])
                (unless (css-color-string? trimmed)
                  (error 'color->string "expected a CSS color string, got: ~a" val))
                trimmed)]
             [(rgb-color? val)
              (let* ([who 'color->string]
                     [r     (ensure-byte     who (rgb-color-r val) "red")]
                     [g     (ensure-byte     who (rgb-color-g val) "green")]
                     [b     (ensure-byte     who (rgb-color-b val) "blue")]
                     [alpha (normalize-alpha who (rgb-color-a val))])
                (if alpha
                    (string-append "rgba(" (number->string r) ", "
                                   (number->string g) ", " (number->string b)
                                   ", " (alpha-value->string alpha) ")")
                    (string-append "rgb(" (number->string r) ", "
                                   (number->string g) ", " (number->string b) ")")))]
             [else (error 'color->string "expected a color value, got: ~a" val)]))]))

    color->string))

;;;
;;; Canvas alpha compositing
;;;

;; The drawing context in `racket/gui` has operations `begin-alpha` and `end-alpha`
;; which doesn't exist in a JavaScript canvas. The implementation below
;; uses an offscreen canvas to draw the new layer, and them composites it
;; to the canvas.

(struct alpha-prev  (opacity global-alpha transform image-smoothing?) #:transparent)
(struct alpha-group (canvas ctx prev) #:transparent)

(define (alpha-smoothing->flag smoothing?)
  (if smoothing? 1 0))

(define (alpha-bool v)
  (not (zero? v)))

(define (alpha-identity-transform ctx)
  (js-canvas2d-set-transform! ctx 1.0 0.0 0.0 1.0 0.0 0.0))

(define (alpha-offscreen-canvas width height)
  (define offscreen-ctor (js-var "OffscreenCanvas"))
  (if (string=? (js-typeof offscreen-ctor) "undefined")
      (let ([canvas (js-create-element "canvas")])
        (js-set-canvas-width! canvas width)
        (js-set-canvas-height! canvas height)
        canvas)
      (js-new offscreen-ctor (vector width height))))

(define (begin-alpha ctx opacity)
  (define base-canvas       (js-canvas2d-canvas ctx))
  (define width             (js-canvas-width  base-canvas))
  (define height            (js-canvas-height base-canvas))
  (define off               (alpha-offscreen-canvas width height))
  (define options           (js-object (vector (vector "alpha" #t))))
  (define octx              (js-canvas-get-context off "2d" options))
  (define prev-global-alpha (js-canvas2d-global-alpha ctx))
  (define prev-transform    (js-canvas2d-get-transform ctx))
  (define prev-smoothing?   (alpha-bool (js-canvas2d-image-smoothing-enabled ctx)))
  (js-canvas2d-set-transform-matrix! octx prev-transform)
  (js-set-canvas2d-image-smoothing-enabled! octx (alpha-smoothing->flag prev-smoothing?))
  (js-set-canvas2d-global-alpha! octx 1.0)
  (alpha-group off
               octx
               (alpha-prev (exact->inexact opacity)
                           prev-global-alpha
                           prev-transform
                           prev-smoothing?)))

(define (end-alpha ctx group)
  (match group
    [(alpha-group off _ (alpha-prev prev-opacity prev-global-alpha _ _))
     (js-canvas2d-save ctx)
     (alpha-identity-transform ctx)
     (define old-alpha (js-canvas2d-global-alpha ctx))
     (js-set-canvas2d-global-alpha! ctx
                                    (exact->inexact (* prev-global-alpha prev-opacity)))
     (js-canvas2d-draw-image ctx off 0.0 0.0)
     (js-set-canvas2d-global-alpha! ctx old-alpha)
     (js-canvas2d-restore ctx)]
    [_
     (error 'end-alpha "expected an alpha-group, got: ~a" group)]))


;;;
;;; Font
;;;

(struct font (family size style variant weight line-height) #:transparent)

(define the-default-text-size 16)
(define the-default-font
  (font 'sans-serif the-default-text-size 'normal 'normal 'normal #f))
  
; a <font family> is either a <family-name> (a string) or a <generic-name> (a symbol).

; https://drafts.csswg.org/css-fonts/#generic-font-families

; https://developer.mozilla.org/en-US/docs/Web/CSS/font-family
; https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
; https://developer.mozilla.org/en-US/docs/Web/CSS/font-style
; https://developer.mozilla.org/en-US/docs/Web/CSS/font-variants
; https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight

(define generic-font-families
  '(serif sans-serif cursive fantasy monospace
          system-ui math ui-serif ui-sans-serif ui-monospace
          ui-rounded))

; Original
; (define families '(default decorative roman script swiss modern symbol system))
(define families generic-font-families)

(define (font-family? x)
  (or (string? x) (memq x generic-font-families)))

(define (font-family->string x)
  (cond
    [(not x)     "sans-serif"]
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [else
     (error 'font-family->string "expected a font-family, got: ~a" x)]))

(define (make-font family
                   [size        12]
                   [style       'normal]
                   [variant     #f]
                   [weight      'normal]
                   [line-height #f])
  ;; (unless (font-family? family)
  ;;   (error 'make-font "expected a font-family, got: ~a" family))
  ;; (unless (font-size? size)
  ;;   (error 'make-font "expected a font-size, got: ~a" size))
  ;; (unless (font-style? style)
  ;;   (error 'make-font "expected a font-style, got: ~a" style))
  ;; (unless (font-style? variant)
  ;;   (error 'make-font "expected a font-variant, got: ~a" variant))
  ;; (unless (font-style? weight)
  ;;   (error 'make-font "expected a font-weight, got: ~a" weight))
  ;; (unless (line-height? line-height)
  ;;   (error 'make-font "expected a line-height, got: ~a" line-height))
  (font (font-family->string  family)
        (font-size->string    size)
        (and style       (font-style->string   style))
        (and variant     (font-variant->string variant))
        (and weight      (font-weight->string  weight))
        (and line-height (line-height->string  line-height))))

; a <font size>  is a string

(define (font-size? x)
  (or (string? x)
      (and (number? x) (positive? x))))

(define (font-size->string x)
  (cond
    [(not x)     "12"]
    [(string? x) x]
    [(number? x) (number->string x)]
    [else
     (error 'font-size->string "expected a font-size, got: ~a" x)]))

; a <font style> is a a string or one of the symbols below

(define font-styles ; aka family
  '(normal italic oblique))  ; oblique = slant

(define (font-style? x)
  (or (string? x) (memq x font-styles)))

(define (font-style->string x)
  (cond
    [(not x)     "normal"]
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [else
     (error 'font-style->string "expected a font-style, got: ~a" x)]))

; a <font variant> is a string
; (controls ligatures, caps etc.)

(define (font-variant? x)
  (or (string? x) (symbol? x)))

(define (font-variant->string x)
  (cond
    [(not x) "normal"]
    [(string? x) x]
    [(symbol? x) x]
    [else
     (error 'font-variant->string "expected a font-variant, got: ~a" x)]))

; a <font weight> is a string, a number, or one of the symbols below.

(define font-weights
  '(normal bold lighter bolder)) ; or a number

(define (font-weight? x)
  (or (string? x)
      (and (number? x) (positive? x))
      (member x font-weights)))

(define (font-weight->string x)
  (cond
    [(not x)     "normal"]
    [(string? x) x]
    [(symbol? x) (symbol->string x)]   
    [(number? x) (number->string x)]
    [else
     (error 'font-weight->string "expected a font-weight, got: ~a" x)]))


(define (line-height? x)
  (or (string? x)
      (symbol? x)
      (and (number? x) (positive? x))))

(define (line-height->string x)
  (cond
    [(not x)     "normal"]
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [else
     (error 'line-height->string "expected a line-height, got: ~a" x)]))

(define (font->string f)
  (define (value->string v)
    (cond
      [(not v) #f]
      [(string? v) v]
      [(symbol? v) (symbol->string v)]
      [(number? v) (number->string v)]
      [else (error 'font->string "unexpected font value: ~a" v)]))

  (define parts '())

  (define (add-part s)
    (set! parts (append parts (list s))))

  (define (maybe-add-part v default)
    (let ([s (value->string v)])
      (cond
        [(not s) (void)]
        [(and default (string=? s default)) (void)]
        [else (add-part s)])))

  (maybe-add-part (font-style   f) "normal")
  (maybe-add-part (font-variant f) "normal")
  (maybe-add-part (font-weight  f) "normal")

  (define size-raw (or (value->string (font-size f)) "medium"))
  (define size
    (let ([n (string->number size-raw)])
      (if n
          (string-append (number->string n) "px")
          size-raw)))

  (define line-height-raw (value->string (font-line-height f)))
  (define line-height
    (cond
      [(not line-height-raw) #f]
      [(string=? line-height-raw "normal") #f]
      [else line-height-raw]))

  (add-part (if line-height
                (string-append size "/" line-height)
                size))

  (define (string-length=? s n)
    (= (string-length s) n))

  (define (string-any? s pred)
    (let loop ([i 0])
      (if (= i (string-length s))
          #f
          (if (pred (string-ref s i))
              #t
              (loop (add1 i))))))

  (define (valid-family-first-char? c)
    (or (char-alphabetic? c)
        (char=? c #\-)
        (char=? c #\_)))

  (define (valid-family-char? c)
    (or (char-alphabetic? c)
        (char-numeric? c)
        (char=? c #\-)
        (char=? c #\_)))

  (define (valid-unquoted-family? s)
    (and (not (string-length=? s 0))
         (valid-family-first-char? (string-ref s 0))
         (let loop ([i 1])
           (if (= i (string-length s))
               #t
               (and (valid-family-char? (string-ref s i))
                    (loop (add1 i)))))))

  (define (quote-family s)
    (define out (open-output-string))
    (write-char #\' out)
    (let loop ([i 0])
      (if (< i (string-length s))
          (let ()
            (define c (string-ref s i))
            (cond
              [(char=? c #\\)
               (write-char #\\ out)
               (write-char #\\ out)]
              [(char=? c #\')
               (write-char #\\ out)
               (write-char #\' out)]
              [else (write-char c out)])
            (loop (add1 i)))
          (void)))
    (write-char #\' out)
    (get-output-string out))

  (define (font-family-token v)
    (let ([s (value->string v)])
      (cond
        [(not s) "sans-serif"]
        [(string-any? s (lambda (c) (char=? c #\,))) s]
        [(memq (string->symbol s) generic-font-families) s]
        [(valid-unquoted-family? s) s]
        [else (quote-family s)])))

  (add-part (font-family-token (font-family f)))

  (if (null? parts)
      ""
      (let loop ([rest (cdr parts)]
                 [acc (car parts)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc " " (car rest)))))))

;;;
;;; Canvas
;;;

;; The function `canvas-context->dc` produces a function that
;; receives drawing commands and executes then in the given context.

;; Example:
;;    (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))  
;;    (define dc (canvas-context->dc ctx))
;;    (dc 'move-to x y)

;; The drawing commands are the same as the method names used in JavaScript.
;; See "MDN Canvas" for documentation.

(define (canvas-context->dc ctx)
  (define (to-string x)
    (cond
      [(string? x)   x]
      [(symbol? x)   (symbol->string x)]
      [(color?  x)   (color->string x)]
      [(font?   x)   (font->string x)]
      [(external? x) x]
      [else          (error 'to-string "got: ~a" x)]))
  
  (define (to-real x)
    (cond
      [(flonum? x) x]
      [(fixnum? x) (exact->inexact x)]
      [else        (error 'to-real "got: ~a" x)]))
  
  (define (to-real-01 x)
    (to-real (max (min x 1.0) 0.0)))
  
  (define (to-boolean x)
    (if x 1 0))
  
  (define (maybe-string x)
    (if (or (void? x) (not x))
        (void)
        (to-string x)))

  (define (maybe-real x)
    (if (or (void? x) (not x))
        (void)
        (to-real x)))

  (define (to-extern x who)
    (cond
      [(external? x) x]
      [else (error who "expected an external JS value, got: ~a" x)]))

  (define (to-array x)
    (cond
      [(external? x) x]
      [(vector? x)
       (js-array/extern
        (list->vector (map to-real (vector->list x))))]
      [(list? x)
       (js-array/extern (list->vector (map to-real x)))]
      [else
       (error 'to-array "expected a list, vector, or external JS value, got: ~a" x)]))

  (define (maybe-extern x)
    (if (or (void? x) (not x))
        (void)
        (to-extern x 'canvas-context->dc)))

  (define (maybe-radii x)
    (cond
      [(or (void? x) (not x)) (void)]
      [(or (list? x) (vector? x) (external? x)) (to-array x)]
      [else (to-real x)]))
  
  (λ args
    (match args
      [(list 'context)
       ctx]
      [(list 'direction dir)
       (js-set-canvas2d-direction! ctx (to-string dir))]
      [(list 'fill-style)
       (js-canvas2d-fill-style ctx)]
      [(list 'fill-style val)
       ; <color>, CanvasGradient, CanvasPattern
       (js-set-canvas2d-fill-style! ctx (to-string val))]
      [(list 'font)
       (js-canvas2d-font ctx)]
      [(list 'font val)
       (js-set-canvas2d-font! ctx (to-string val))]
      [(list 'font-kerning val)
       (js-set-canvas2d-font-kerning! ctx (to-string val))]
      ; deprecated:
      ;   [(list 'font-stretch val)
      ;    (js-set-canvas2d-font-stretch! ctx (to-string val))]
      ; missing in Safari:
      ;   [(list 'font-variant-caps val)
      ;    (js-set-canvas2d-font-variant-caps! ctx (to-string val))]      
      [(list 'global-alpha)
       (js-canvas2d-global-alpha ctx)]
      [(list 'global-alpha val)
       (js-set-canvas2d-global-alpha! ctx (to-real-01 val))]
      [(list 'global-composite-operation val)
       (js-set-canvas2d-global-composite-operation! ctx (to-string val))]
      [(list 'image-smoothing-enabled val)
       (js-set-canvas2d-image-smoothing-enabled! ctx (to-boolean val))]
      ;; image-smooting-quality  -- currently missing from Firefox
      ;;   [(list 'image-smoothing-quality val)
      ;;    (js-set-canvas2d-image-smoothing-quality! ctx (to-string val))]      
      [(list 'letter-spacing val)
       (js-set-canvas2d-letter-spacing! ctx (to-string val))]
      [(list 'line-cap val)
       (js-set-canvas2d-line-cap! ctx (to-string val))]
      [(list 'line-dash-offset val)
       (js-set-canvas2d-line-dash-offset! ctx (to-real val))]

      [(list 'line-join)
       (js-canvas2d-line-join ctx)]
      [(list 'line-join val)
       (js-set-canvas2d-line-join! ctx (to-string val))]
      [(list 'line-width)
       (js-canvas2d-line-width ctx)]
      [(list 'line-width val)
       (js-set-canvas2d-line-width! ctx (to-real val))]
      [(list 'miter-limit)
       (js-canvas2d-miter-limit ctx)]
      [(list 'miter-limit val)
       (js-set-canvas2d-miter-limit! ctx (to-real val))]
      [(list 'shadow-blur)
       (js-canvas2d-shadow-blur ctx)]
      [(list 'shadow-blur val)
       (js-set-canvas2d-shadow-blur! ctx (to-real val))]
      [(list 'shadow-color)
       (js-canvas2d-shadow-color ctx)]
      [(list 'shadow-color val)
       (js-set-canvas2d-shadow-color! ctx (to-string val))]
      [(list 'shadow-offset-x)
       (js-canvas2d-shadow-offset-x ctx)]
      [(list 'shadow-offset-x val)
       (js-set-canvas2d-shadow-offset-x! ctx (to-real val))]
      [(list 'shadow-offset-y)
       (js-canvas2d-shadow-offset-y ctx)]
      [(list 'shadow-offset-y val)
       (js-set-canvas2d-shadow-offset-y! ctx (to-real val))]
      [(list 'stroke-style)
       (js-canvas2d-stroke-style ctx)]
      [(list 'stroke-style val)
       (js-set-canvas2d-stroke-style! ctx (to-string val))]
      [(list 'text-align)
       (js-canvas2d-text-align ctx)]
      [(list 'text-align val)
       (js-set-canvas2d-text-align! ctx (to-string val))]
      [(list 'text-baseline)
       (js-canvas2d-text-baseline ctx)]
      [(list 'text-baseline val)
       (js-set-canvas2d-text-baseline! ctx (to-string val))]
      [(list 'text-rendering)
       (js-canvas2d-text-rendering ctx)]
      [(list 'text-rendering val)
       (js-set-canvas2d-text-rendering! ctx (to-string val))]
      [(list 'word-spacing)
       (js-canvas2d-word-spacing ctx)]
      [(list 'word-spacing val)
       (js-set-canvas2d-word-spacing! ctx (to-string val))]
      [(list 'arc x y radius start-angle end-angle)
       (js-canvas2d-arc ctx
                         (to-real x)
                         (to-real y)
                         (to-real radius)
                         (to-real start-angle)
                         (to-real end-angle)
                         (to-boolean #f))]
      [(list 'arc x y radius start-angle end-angle ccw)
       (js-canvas2d-arc ctx
                         (to-real x)
                         (to-real y)
                         (to-real radius)
                         (to-real start-angle)
                         (to-real end-angle)
                         (to-boolean ccw))]
      [(list 'arc-to x1 y1 x2 y2 radius)
       (js-canvas2d-arc-to ctx
                            (to-real x1)
                            (to-real y1)
                            (to-real x2)
                            (to-real y2)
                            (to-real radius))]
      [(list 'begin-path)
       (js-canvas2d-begin-path ctx)]
      [(list 'bezier-curve-to cp1x cp1y cp2x cp2y x y)
       (js-canvas2d-bezier-curve-to ctx
                                    (to-real cp1x)
                                    (to-real cp1y)
                                    (to-real cp2x)
                                    (to-real cp2y)
                                    (to-real x)
                                    (to-real y))]
      [(list 'clear-rect x y w h)
       (js-canvas2d-clear-rect ctx
                               (to-real x)
                               (to-real y)
                               (to-real w)
                               (to-real h))]
      [(list 'clip)
       (js-canvas2d-clip ctx (void) (void))]
      [(list 'clip arg)
       (if (or (string? arg) (symbol? arg))
           (js-canvas2d-clip ctx (void) (to-string arg))
           (js-canvas2d-clip ctx (maybe-extern arg) (void)))]
      [(list 'clip path rule)
       (if (or (string? path) (symbol? path))
           (js-canvas2d-clip ctx (void) (maybe-string rule))
           (js-canvas2d-clip ctx (maybe-extern path) (maybe-string rule)))]
      [(list 'close-path)
       (js-canvas2d-close-path ctx)]
      [(list 'create-image-data width height)
       (js-canvas2d-create-image-data ctx (to-real width) (to-real height))]
      [(list 'create-image-data-from other)
       (js-canvas2d-create-image-data-from ctx (to-extern other 'create-image-data-from))]
      [(list 'create-linear-gradient x0 y0 x1 y1)
       (js-canvas2d-create-linear-gradient ctx
                                           (to-real x0)
                                           (to-real y0)
                                           (to-real x1)
                                           (to-real y1))]
      [(list 'create-pattern image repetition)
       (js-canvas2d-create-pattern ctx
                                   (to-extern image 'create-pattern)
                                   (to-string repetition))]
      [(list 'create-radial-gradient x0 y0 r0 x1 y1 r1)
       (js-canvas2d-create-radial-gradient ctx
                                           (to-real x0)
                                           (to-real y0)
                                           (to-real r0)
                                           (to-real x1)
                                           (to-real y1)
                                           (to-real r1))]
      [(list 'create-conic-gradient angle x y)
       (js-canvas2d-create-conic-gradient ctx
                                          (to-real angle)
                                          (to-real x)
                                          (to-real y))]
      [(list 'draw-image image dx dy)
       (js-canvas2d-draw-image ctx
                               (to-extern image 'draw-image)
                               (to-real dx)
                               (to-real dy))]
      [(list 'draw-image image dx dy dw dh)
       (js-canvas2d-draw-image-5 ctx
                                  (to-extern image 'draw-image)
                                  (to-real dx)
                                  (to-real dy)
                                  (to-real dw)
                                  (to-real dh))]
      [(list 'draw-image image sx sy sw sh dx dy dw dh)
       (js-canvas2d-draw-image-9 ctx
                                  (to-extern image 'draw-image)
                                  (to-real sx)
                                  (to-real sy)
                                  (to-real sw)
                                  (to-real sh)
                                  (to-real dx)
                                  (to-real dy)
                                  (to-real dw)
                                  (to-real dh))]
      [(list 'ellipse x y radius-x radius-y rotation start-angle end-angle)
       (js-canvas2d-ellipse ctx
                            (to-real x)
                            (to-real y)
                            (to-real radius-x)
                            (to-real radius-y)
                            (to-real rotation)
                            (to-real start-angle)
                            (to-real end-angle)
                            (to-boolean #f))]
      [(list 'ellipse x y radius-x radius-y rotation start-angle end-angle ccw)
       (js-canvas2d-ellipse ctx
                            (to-real x)
                            (to-real y)
                            (to-real radius-x)
                            (to-real radius-y)
                            (to-real rotation)
                            (to-real start-angle)
                            (to-real end-angle)
                            (to-boolean ccw))]

      [(list 'fill)
       (js-canvas2d-fill ctx (void) (void))]
      [(list 'fill arg)
       (if (or (string? arg) (symbol? arg))
           (js-canvas2d-fill ctx (void) (to-string arg))
           (js-canvas2d-fill ctx (maybe-extern arg) (void)))]
      [(list 'fill path rule)
       (if (or (string? path) (symbol? path))
           (js-canvas2d-fill ctx (void) (maybe-string rule))
           (js-canvas2d-fill ctx (maybe-extern path) (maybe-string rule)))]

      [(list 'fill-rect x y w h)
       (js-canvas2d-fill-rect ctx
                              (to-real x)
                              (to-real y)
                              (to-real w)
                              (to-real h))]
      [(list 'fill-text text x y)
       (js-canvas2d-fill-text ctx (to-string text) (to-real x) (to-real y) (void))]
      [(list 'fill-text text x y max-width)
       (js-canvas2d-fill-text ctx (to-string text) (to-real x) (to-real y) (maybe-real max-width))]

      [(list 'get-image-data sx sy sw sh)
       (js-canvas2d-get-image-data ctx
                                   (to-real sx)
                                   (to-real sy)
                                   (to-real sw)
                                   (to-real sh)
                                   (void))]
      [(list 'get-image-data sx sy sw sh settings)
       (js-canvas2d-get-image-data ctx
                                   (to-real sx)
                                   (to-real sy)
                                   (to-real sw)
                                   (to-real sh)
                                   (maybe-extern settings))]
      [(list 'get-transform)
       (js-canvas2d-get-transform ctx)]
      [(list 'is-point-in-path x y)
       (js-canvas2d-is-point-in-path ctx (void) (to-real x) (to-real y) (void))]
      [(list 'is-point-in-path path x y)
       (js-canvas2d-is-point-in-path ctx (maybe-extern path) (to-real x) (to-real y) (void))]
      [(list 'is-point-in-path path x y rule)
       (js-canvas2d-is-point-in-path ctx (maybe-extern path) (to-real x) (to-real y) (maybe-string rule))]
      [(list 'is-point-in-stroke x y)
       (js-canvas2d-is-point-in-stroke ctx (void) (to-real x) (to-real y))]
      [(list 'is-point-in-stroke path x y)
       (js-canvas2d-is-point-in-stroke ctx (maybe-extern path) (to-real x) (to-real y))]
      [(list 'line-to x y)
       (js-canvas2d-line-to ctx (to-real x) (to-real y))]
      [(list 'measure-text text)
       (js-canvas2d-measure-text ctx (to-string text))]
      [(list 'move-to x y)
       (js-canvas2d-move-to ctx (to-real x) (to-real y))]
      [(list 'put-image-data data dx dy)
       (js-canvas2d-put-image-data ctx
                                   (to-extern data 'put-image-data)
                                   (to-real dx)
                                   (to-real dy)
                                   (void)
                                   (void)
                                   (void)
                                   (void))]
      [(list 'put-image-data data dx dy dirty-x dirty-y dirty-width dirty-height)
       (js-canvas2d-put-image-data ctx
                                   (to-extern data 'put-image-data)
                                   (to-real dx)
                                   (to-real dy)
                                   (maybe-real dirty-x)
                                   (maybe-real dirty-y)
                                   (maybe-real dirty-width)
                                   (maybe-real dirty-height))]
      [(list 'quadratic-curve-to cpx cpy x y)
       (js-canvas2d-quadratic-curve-to ctx
                                       (to-real cpx)
                                       (to-real cpy)
                                       (to-real x)
                                       (to-real y))]
      [(list 'rect x y w h)
       (js-canvas2d-rect ctx
                         (to-real x)
                         (to-real y)
                         (to-real w)
                         (to-real h))]
      [(list 'reset)
       (js-canvas2d-reset ctx)]
      [(list 'reset-transform)
       (js-canvas2d-reset-transform ctx)]
      [(list 'restore)
       (js-canvas2d-restore ctx)]
      [(list 'rotate angle)
       (js-canvas2d-rotate ctx (to-real angle))]
      [(list 'round-rect x y w h radii)
       (js-canvas2d-round-rect ctx
                               (to-real x)
                               (to-real y)
                               (to-real w)
                               (to-real h)
                               (maybe-radii radii))]
      [(list 'save)
       (js-canvas2d-save ctx)]
      [(list 'scale x y)
       (js-canvas2d-scale ctx (to-real x) (to-real y))]
      [(list 'set-line-dash segments)
       (js-canvas2d-set-line-dash ctx (to-array segments))]
      [(list 'set-transform! a b c d e f)
       (js-canvas2d-set-transform! ctx
                                   (to-real a)
                                   (to-real b)
                                   (to-real c)
                                   (to-real d)
                                   (to-real e)
                                   (to-real f))]
      [(list 'set-transform-matrix! matrix)
       (js-canvas2d-set-transform-matrix! ctx (to-extern matrix 'set-transform-matrix!))]
      [(list 'stroke)
       (js-canvas2d-stroke ctx (void))]
      [(list 'stroke path)
       (js-canvas2d-stroke ctx (maybe-extern path))]
      [(list 'stroke-rect x y w h)
       (js-canvas2d-stroke-rect ctx
                                 (to-real x)
                                 (to-real y)
                                 (to-real w)
                                 (to-real h))]
      [(list 'stroke-text text x y)
       (js-canvas2d-stroke-text ctx (to-string text) (to-real x) (to-real y) (void))]
      [(list 'stroke-text text x y max-width)
       (js-canvas2d-stroke-text ctx (to-string text) (to-real x) (to-real y) (maybe-real max-width))]

      [(list 'transform)
       (js-canvas2d-get-transform ctx)]
      [(list 'transform extern-matrix)
       (cond
         [(external? extern-matrix)
          (js-canvas2d-set-transform-matrix! ctx extern-matrix)]
         [(vector? extern-matrix)
          (match extern-matrix
            [(vector a b c d e f)
             (js-canvas2d-transform ctx
                              (to-real a)
                              (to-real b)
                              (to-real c)
                              (to-real d)
                              (to-real e)
                              (to-real f))])]
         [else (error 'tranform "expected an extern (matrix) or a vector")])]
      [(list 'transform a b c d e f)
       (js-canvas2d-transform ctx
                              (to-real a)
                              (to-real b)
                              (to-real c)
                              (to-real d)
                              (to-real e)
                              (to-real f))]
      [(list 'translate x y)
       (js-canvas2d-translate ctx (to-real x) (to-real y))]
      [else
       (error 'canvas-context->dc "unknown canvas command: ~a" args)])))
