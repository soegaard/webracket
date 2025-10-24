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
       (js-canvas2d-set-transform-matrix! ctx extern-matrix)]      
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


;;;
;;; Pict
;;;

(struct pict (draw       ; drawing instructions
              width      ; total width
              height     ; total height >= ascent + desecnt
              ascent     ; portion of height above top baseline
              descent    ; portion of height below bottom baseline
              children   ; list of child records
              panbox     ; panorama box, computed on demand
              last)      ; a descendent for the bottom-right
  #:extra-constructor-name make-pict
  #:mutable #:transparent)


(struct child (pict dx dy sx sy sxy syx)
  #:extra-constructor-name make-child
  #:transparent)

(struct bbox  (x1 y1 x2 y2 ay dy)
  #:extra-constructor-name make-bbox
  #:transparent)


;; The `draw` field of `pict` contains the drawing information in an internal format.
;; To draw a `pict` the internal format is converted into a "pict drawer", which
;; is a function that receives a drawing context `dc` and a point (dx,dy) which
;; is the upper, left corner to place the pict.

(define (draw-pict p dc dx dy)
  ((make-pict-drawer p) dc dx dy))

(define (make-pict-drawer p)
  (let ([cmds (pict->command-list p)]
        [h    (pict-height p)])
    ; (js-log (format "cmds: ~a" cmds))
    (lambda (dc dx dy)
      (render dc (+ h dy)
              cmds
              dx 0))))

;; The list of commands in the internal format originate
;; from the LaTeX picture environment.

;;     https://latexref.xyz/picture.html

;;  (picture   width height command ...)                                    
;;  (local     command ....)                                                
;;  (begin     commands commands ...)                                       
;;  (color     color command ...)         ; pen color                       
;;  (thickness width command ...)         ;                                 
;;  (put       x y command ...)           ; place contents at (x,y)         
;;  (qbezier   num x1 y1 x2 y2 x3 y3)     ; quadratic Bezier curve          
;;  (line      x_run y_rise travel)       ; line segment                    
;;  (line      dx dy #f)                  ; line segment                    
;;  (vector    x_run y_rise travel)       ; arrow                           
;;  (vector    dx dy)                     ; arrow                           
;;  (circle    x y)                       ; circle                          
;;  (circle*   x y)                       ; filled circle                   
;;  (make-box  w h position text)         ;                                 
;;  (frame     commdands)                 ; puts contents in a box          
;;  (colorbox  box commands)              ; same, but background is colored 
;;  (oval      w h commands)              ; rounded rectangle               
;;  (prog      f commands)                                                  
;;                                                                          
;; where                                                                    
;;   position is one of t, b, l, r                                          
;;   f is a function: (-> (is-a?/c dc<%>) real? real? any)

;; The pict `blank` draws nothing when rendered, but takes up space
;; when combined with other picts.

(define blank 
  (case-lambda
   [()        (blank 0 0 0)]
   [(s)       (blank s s)]
   [(w h)     (blank w h 0)]
   [(w a d)   (make-pict `(picture ,w ,(+ a d)) w (+ a d) a d null #f #f)]
   [(w h a d) (make-pict `(picture ,w ,h)       w h       a d null #f #f)]))

;; As we will see, most pict constructors use `extend-pict` (defined below),
;; but `blank` isn't the only constructor, that uses `make-pict` directly.

;; The functions `lift-above-baseline`, `drop-below-ascent` and `baseless`
;; all 

;; (drop-below-ascent p n)
;;     For backwards compatibility.
;;     Use drop-top-relative-to-ascent instead.
;;
;;     Drops pict relative to its ascent line, extending
;;     the bounding box height if necessary.
(define (drop-below-ascent p n)
  (let* ([dh    (- (max 0 (- n (pict-ascent p))))]
         [do-d? (= (pict-height p)
                   (+ (pict-ascent p) (pict-descent p)))]
         [h2    (+ dh (pict-height p))]
         [a2    (max 0 (- (pict-ascent p) n))])
    (make-pict (pict-draw p)         
               (pict-width p)
               h2     
               a2
               (if do-d?
                   (- h2 a2)
                   (pict-descent p))
               (pict-children p)
               #f
               (pict-last p))))

;; (lift-above-baseline p n)
;;   For backwards compatibility.
;;   Use lift-bottom-relative-to-baseline instead.
(define (lift-above-baseline p n)
  (let* ([dh (- (max 0 (- n (pict-descent p))))]
         [do-a? (= (pict-height p)
                   (+ (pict-ascent p) (pict-descent p)))]
         [h2 (+ dh (pict-height p))]
         [d2 (max 0 (- (pict-descent p) n))])
    (make-pict (pict-draw p)
               (pict-width p)
               h2
               (if do-a?
                   (- h2 d2)
                   (pict-ascent p))
               d2
               (map (lambda (c)
                      (make-child
                       (child-pict c)
                       (child-dx c)
                       (+ dh (child-dy c))
                       1 1
                       0 0))
                    (pict-children p))
               #f
               (pict-last p))))


;; (baseless p)
;;   Makes the descent 0 and the ascent the same as the height.
(define (baseless p)
  (let ([p (lift-above-baseline p (pict-descent p))])
    (drop-below-ascent p (- (pict-ascent p) (pict-height p)))))


;; Draw a graphical representation of a record.
;;   - i.e. this is unrelated to serialization

(define recordseplinespace 4)

(define (record title . fields)
  (let* ([totalwidth  (apply max (pict-width title) (map pict-width fields))]
         [linespace   (if (null? fields) 0 recordseplinespace)]
         [totalheight (+ (pict-height title) (apply + (map pict-height fields))
                         linespace)]
         [title-y     (- totalheight (pict-height title))]
         [field-ys    (let loop ([pos (- totalheight (pict-height title) linespace)]
                                 [fields fields])
                        (if (null? fields)
                            null
                            (let* ([p (- pos (pict-height (car fields)))])
                              (cons p
                                    (loop p (cdr fields))))))])
    (make-pict
     `(picture
       ,totalwidth ,totalheight
       (put 0 0            (line 1 0 ,totalwidth))  ; note: x_run, y_rize, travel (i.e. LaTeX convention)
       (put 0 ,totalheight (line 1 0 ,totalwidth))  ; note: x_run, y_rize, travel (i.e. LaTeX convention)
       (put 0 0            (line 0 1 ,totalheight)) ; note: x_run, y_rize, travel (i.e. LaTeX convention)
       (put ,totalwidth 0  (line 0 1 ,totalheight)) ; note: x_run, y_rize, travel (i.e. LaTeX convention)
       (put 0 ,title-y     ,(pict-draw title))
       ,@(if (null? fields)
             '()
             `((put 0 ,(- totalheight (pict-height title) (/ linespace 2))
                    (line 1 0 ,totalwidth))))
       ,@(map (lambda (f p) `(put 0 ,p ,(pict-draw f)))
              fields field-ys))
     totalwidth totalheight
     totalheight 0
     (cons
      (make-child title 0 title-y 1 1 0 0)
      (map (lambda (child child-y) (make-child child 0 child-y 1 1 0 0)) fields field-ys))
     #f
     #f)))


;; To extend an existing pict `box` and place it into a larger pict,
;; we can use `extend-pict`.
;; The dimension of the extended box add `dw`, `da` and `dd`
;; to the width, ascent and descent (and adjust the height as well
;; based on ascent and descent.
;; The old picture `box` is placed at the coordinate (dx,dy) relative
;; to the upper, left corner of the extended box.

;; Note: For pictures without children, panbox and last (child),
;;       it can be more convenient to use `extend-pict` than
;;       `make-pict` directly. You will see quite a few functions
;;       that use all zeros for the size adjustments, but
;;       still use `extend-pict`.

(define (extend-pict box dx dy dw da dd draw)
  (let ([w (pict-width   box)]
        [h (pict-height  box)]
        [d (pict-descent box)]
        [a (pict-ascent  box)])
    (make-pict (if draw draw (pict-draw box))        ; draw
               (+ w dw) (+ h da dd)                  ; width, height
               (max 0 (+ a da)) (max 0 (+ d dd))     ; descent, ascent
               (list (make-child box dx dy 1 1 0 0)) ; children
               #f                                    ; panbox
               (pict-last box))))                    ; last

; Creates a container picture that doesn’t draw the child picture,
; but uses the child’s size.
(define (ghost box)
  (let ([w (pict-width box)]
        [h (pict-height box)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h))))

; Truncates pict’s bounding box by removing the descent part.
(define (clip-descent b)
  (let* ([w (pict-width   b)]
         [h (pict-height  b)]
         [d (pict-descent b)])
    (extend-pict
     b 0 (- d)
     0 0 (- d)
     `(picture ,w ,(- h d)
               (put 0 ,(- d) ,(pict-draw b))))))

; Truncates pict’s bounding box by removing the ascent part.
(define (clip-ascent b)
  (let* ([w (pict-width b)]
         [h (pict-height b)]
         [a (pict-ascent b)])
    (extend-pict
     b 0 a
     0 (- a) 0
     `(picture ,w ,(- h a)
               (put 0 0 ,(pict-draw b))))))

;; Extends pict’s bounding box by adding the given amounts to the
;; corresponding sides; ascent and descent are extended, too.
;; The actual contents is drawn in the same place as before.

(define inset
  (case-lambda
   [(box a)       (inset box a a a a)]
   [(box h v)     (inset box h v h v)]
   [(box l t r b) (let ([w (+ l r (pict-width box))]
                        [h (+ t b (pict-height box))])
                    (extend-pict
                     box l b
                     (+ l r) t b
                     `(picture
                       ,w ,h
                       (put ,l ,b ,(pict-draw box)))))]))

;; Unless in black-and-white mode, `colorize` will change
;; the stroke and fill of a pict.

(define black-and-white
  (make-parameter #f
                  (lambda (x)
                    (and x #t))))

(define (colorize p color)
  (unless (or (string? color)
              (color?  color)
              (and (list? color) (= 3 (length color)) (andmap byte? color)))
    (error 'colorize "expected a color, given ~e" color))
  (let ([color (if (list? color) (apply make-color color) color)])
    (if (black-and-white)
        p
        (extend-pict 
         p 0 0 0 0 0
         `(color ,color ,(pict-draw p))))))


;; In `thickness` we see `extend-pict` used to adjust the draw
;; function. It was also possible to use `make-pict` directly.

; (thickness mode sub-pict)
(define (thickness mode b)
  (let* ([w (pict-width  b)]
         [h (pict-height b)])
    (extend-pict
     b 0 0 0 0 0
     `(picture ,w ,h
               (thickness ,mode ,(pict-draw b))))))

;; The thinkness modes are inherited from LaTeX.
;; Here b is a box (a pict) and n is a number.

;; (define (thick b)            (thickness 'thicklines b))
;; (define (thin  b)            (thickness 'thinlines  b))
;; (define (line-thickness n b) (thickness n b))        
;; (define (line-style     n s) (thickness n s))

;;;
;;; Lines (vertical and horizontal)
;;;

;; Lines are either solid or dashed.

(define default-seg 5) ; default segment length for dashes

; (dash-line width height rotate seg)
;   For horizontal lines `rotate` is the identity.
;   For veritical  lines `rotate` swap x and y / or swap width and height.
;   The segment length of the dashes is given by `seg`.
(define (dash-line width height rotate seg)
  (let ([vpos (/ height 2)])
    (make-pict     
     `(picture
       ,@(rotate width height)       
       ,@(if (>= seg width)
             `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,width)))
             (let ()
               (define seg*2 (* seg 2))
               (define count (inexact->exact (truncate (/ width seg*2))))
               (define remain/2 (/ (- width (* seg*2 count)) 2))
               `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,remain/2))
                 ,@(let loop ([count count] [pos remain/2])
                     (if (zero? count)
                         null
                         (cons `(put ,@(rotate (+ pos seg) vpos) 
                                     (line ,@(rotate 1 0) ,seg))
                               (loop (sub1 count) (+ pos seg*2)))))
                 (put ,@(rotate (- width remain/2) vpos)
                      (line ,@(rotate 1 0) ,remain/2))))))
     (car  (rotate width height)) ; width
     (cadr (rotate width height)) ; height
     (cadr (rotate 0 height)) 0   ; ascent, descent
     null                         ; children
     #f                           ; panbox
     #f)))                        ; last

(define (rlist b a) (list a b))

(define (hline width height)
  (dash-line width height list width))

(define (vline width height)
  (dash-line height width rlist height))

(define dash-hline
  (case-lambda 
    [(width height)     (dash-hline width height default-seg)]
    [(width height seg) (dash-line  width height list seg)]))

(define dash-vline
  (case-lambda 
    [(width height)     (dash-vline width  height default-seg)]
    [(width height seg) (dash-line  height width  rlist seg)]))

;;;
;;; Frames
;;;

;; A frame draws a border around an existing (child) pict.
;; A the border can be drawn as a solid line or a dashed line.
;; Actually, a solid border is the same as a dashed border --
;; with wery long segments.

(define dash-frame
  (case-lambda
    [(box) (dash-frame box default-seg)]
    [(box seg)
     (let* ([w (pict-width box)]
            [h (pict-height box)])
       (extend-pict
        box 0 0 0 0 0
        `(picture
          ,w ,h
          (put  0  0 ,(pict-draw box))
          (put  0  0 ,(pict-draw (dash-hline w 0 seg)))
          (put  0 ,h ,(pict-draw (dash-hline w 0 seg)))
          (put  0  0 ,(pict-draw (dash-vline 0 h seg)))
          (put ,w  0 ,(pict-draw (dash-vline 0 h seg))))))]))

(define (frame box)
  (let ([box (pict-convert box)])
    (dash-frame box (max (pict-width box) (pict-height box)))))


;;;
;;; Pict Convertible
;;; 

;; In full Racket the `pict` library supports structures
;; that are "pict convertible". They are structs, that
;; contain a "methods" that renders them as a pict.

;; Our functions that renders, combines, adjust picts
;; must therefore be aware of such "picts".

;; For now, pict convertible structs aren't supported
;; in this WebRacket implentation, but we keep the
;; basic definitions in order to add them later.

;; For our dummy definition below, we must remember
;; that picts also counts as "pict convertible".

(struct converted-pict pict (parent))

(define (pict-convertible? x)    (pict? x))  ; TODO
(define (pict-convertible-ref x) x)          ; TODO

(define (pict-convert v)
  (pict-convert/who v 'pict-convert))

(define (pict-convert/who v who)
  (cond [(pict? v) v]
        [(not (pict-convertible? v))
         (raise-type-error who "pict-convertible" v)]
        [else
         (define converted ((pict-convertible-ref v) v))
         (converted-pict
          (pict-draw converted)
          (pict-width converted)
          (pict-height converted)
          (pict-ascent converted)
          (pict-descent converted)
          (pict-children converted)
          (pict-panbox converted)
          (pict-last converted)
          v)]))

;;;
;;; Picture Paths
;;;

;; A "picture path" is basically either a pict or a list of picts.
;; Or more precisely: a pictue path is either a pict, a pict convertible struct
;; or a list of picts and pict convertible structs.

(define (pict-path? p)
  (or (pict-convertible? p)  
      (and (pair? p)
           (list? p)
           (andmap pict-convertible? p))))

;; When pict convertible structs are to be compared we use
;; their picts for the comparison.

(define (pict-path-element=? a b)
  (or (eq? a b)
      (if (converted-pict? a)
          (if (converted-pict? b)
              (eq? (converted-pict-parent a) (converted-pict-parent b))
              (eq? (converted-pict-parent a) b))
          (if (converted-pict? b)
              (eq? (converted-pict-parent b) a)
              #f))))
;;;
;;; Picture Finders
;;;

;; With picture paths in place, we can now look at picture finders.

;; (single-pict-offset who pict subbox dx dy nth)
;;   A helper routine called by `who` (used for error messages).
;;   Finds `subbox` inside `pict` and reports the location.

;; What complicates matters is that the coordinate system of
;; a child can be transformed relative to its parent.
;; The transform between parent and child are the reason
;; children picts are wrapped in a `child` struct:

;;    (struct child (pict dx dy sx sy sxy syx))

;; The gransform is an affine transform and can express
;; compositions of rotation, scaling and translation.

(define (transform dx dy                      ; point to tranform
                   tdx tdy tsx tsy tsxy tsyx) ; affine transform
  (values (+ (* tsx dx) (* tsxy dy) tdx)
          (+ (* tsy dy) (* tsyx dx) tdy)))

(define (single-pict-offset who pict subbox dx dy nth)
  (let floop ([box pict]
              [nth nth]
              [found values]
              [not-found (lambda (nth)
                           (error who
                                  "sub-pict not found\n  sub-pict: ~.v\n  in: ~.v"
                                  subbox pict))])
    (if (pict-path-element=? subbox box)
        (if (or (not nth) (zero? nth))
            (found dx dy)
            (not-found (and nth (sub1 nth))))
        (let loop ([c (pict-children box)] [nth nth] [fx #f] [fy #f])
          (if (null? c)
              (if fx
                  (found fx fy)
                  (not-found nth))
              (let ([rst (cdr c)])
                (floop (child-pict (car c))
                       nth
                       (lambda (dx dy)
                         (let ([c (car c)])
                           (let-values ([(dx dy)
                                         (transform
                                          dx dy
                                          (child-dx  c) (child-dy  c)
                                          (child-sx  c) (child-sy  c)
                                          (child-sxy c) (child-syx c))])
                             (cond
                               [nth (found dx dy)]
                               [(not fx) (loop rst #f dx dy)]
                               [(and (= fx dx) (= fy dy)) (loop rst #f dx dy)]
                               [else (error who
                                            "sub-pict location is ambiguous\n  sub-pict: ~.v\n  in: ~.v"
                                            subbox pict)]))))
                       (lambda (nth)
                         (loop rst nth fx fy)))))))))

;; The functions below finds the location of a subbox-path
;; (in most cases) a sub-pict and returns the location (x,y)
;; of the sub-pict.

(define (find-lbx who pict subbox-path dx dy nth)
  (if (pict-convertible? subbox-path)
      (single-pict-offset who pict subbox-path dx dy nth)
      (let loop ([l (cons pict subbox-path)])
        (if (null? (cdr l))
            (values dx dy)
            (let-values ([(dx dy) (loop (cdr l))])
              (single-pict-offset who (car l) (cadr l) dx dy
                                  (if (null? (cddr l)) nth 0)))))))

(define (raise-type-error . _) (error 'todo "implement in stdlib"))

(define-values (find-lt
                find-lc
                find-lb
                find-ltl
                find-lbl
                find-ct
                find-cc
                find-cb
                find-ctl
                find-cbl
                find-rt
                find-rc
                find-rb
                find-rtl
                find-rbl

                lt-find
                lc-find
                lb-find
                ltl-find
                lbl-find
                ct-find
                cc-find
                cb-find
                ctl-find
                cbl-find
                rt-find
                rc-find
                rb-find
                rtl-find
                rbl-find)
  (let ([lb    (lambda (x sx w d a) x)]
        [c     (lambda (x sx w d a) (+ x (* sx (/ w 2))))]
        [rt    (lambda (x sx w d a) (+ x (* sx w)))]
        [tline (lambda (x sx w d a) (+ x (* sx (- w a))))]
        [bline (lambda (x sx w d a) (+ x (* sx d)))]
        [find  (lambda (who get-x get-y flip?)
                 (procedure-rename
                  (lambda (pict pict-path [nth 0])
                    (let ([p (let loop ([path pict-path])
                               (cond
                                 [(pict? path) path]
                                 [(pict-convertible? path) (pict-convert path)]
                                 [(null? (cdr path)) (loop (car path))]
                                 [else (loop (cdr path))]))])
                      (let ([w (pict-width p)]
                            [h (pict-height p)]
                            [d (pict-descent p)]
                            [a (pict-ascent p)])
                        (define-values (x y)
                          (find-lbx who pict pict-path
                                    (get-x 0 1 w 0 0)
                                    (get-y 0 1 h d a)
                                    (if (eq? nth 'unique)
                                        #f
                                        nth)))
                        (if flip?
                            (values x (- (pict-height pict) y))
                            (values x y)))))
                  who))])
    (values (find 'find-lt  lb rt    #f)
            (find 'find-lc  lb c     #f)
            (find 'find-lb  lb lb    #f)
            (find 'find-ltl lb tline #f)
            (find 'find-lbl lb bline #f)
            (find 'find-ct   c rt    #f)
            (find 'find-cc   c c     #f)
            (find 'find-cb   c lb    #f)
            (find 'find-ctl  c tline #f)
            (find 'find-cbl  c bline #f)
            (find 'find-rt  rt rt    #f)
            (find 'find-rc  rt c     #f)
            (find 'find-rb  rt lb    #f)
            (find 'find-rtl rt tline #f)
            (find 'find-rbl rt bline #f)

            (find 'lt-find  lb rt    #t)
            (find 'lc-find  lb c     #t)
            (find 'lb-find  lb lb    #t)
            (find 'ltl-find lb tline #t)
            (find 'lbl-find lb bline #t)
            (find 'ct-find   c rt    #t)
            (find 'cc-find   c c     #t)
            (find 'cb-find   c lb    #t)
            (find 'ctl-find  c tline #t)
            (find 'cbl-find  c bline #t)
            (find 'rt-find  rt rt    #t)
            (find 'rc-find  rt c     #t)
            (find 'rb-find  rt lb    #t)
            (find 'rtl-find rt tline #t)
            (find 'rbl-find rt bline #t))))

;;;
;;; Parent / Child interaction
;;;

;; (refocus pict sub-pict)
;;   Assuming that sub-pict can be found within pict, shifts the overall
;;   bounding box to that of sub-pict (but preserving all the drawing of
;;   pict). The last element, as reported by pict-last is also set to (or
;;   (pict-last sub-pict) sub-pict).

;; Note: After a refocus, the bounding box of a child might be larger
;;       than its parent.

(define (refocus p c)
  (let-values ([(x y) (find-lt p c)])
    (let ([p (inset p
                    (- x) (- y (pict-height p))
                    (- (- (pict-width p) x (pict-width c)))
                    (- (pict-height c) y))])
      (make-pict (pict-draw p)
                 (pict-width  c) (pict-height  c)
                 (pict-ascent c) (pict-descent c)
                 (pict-children p)
                 #f
                 (last* c)))))

; Shifts the given pict’s bounding box to enclose the bounding boxes
; of all sub-picts (even laundered picts).

(define (panorama p)
  (let-values ([(x1 y1 x2 y2 ay dy) (panorama-box! p)])
    (let ([h (- y2 y1)])
      (place-over (blank (- x2 x1) h (- h ay) dy)
                  (- x1) (- y2 (pict-height p))
                  p))))

(define (panorama-box! p*)
  (let* ([p  (pict-convert p*)]
         [bb (pict-panbox p)])
    (if bb
        (values (bbox-x1 bb) (bbox-y1 bb) (bbox-x2 bb) (bbox-y2 bb)
                (bbox-ay bb) (bbox-dy bb))
        (let loop ([x1 0][y1 0][x2 (pict-width p)][y2 (pict-height p)]
                   [ay (- (pict-height p) (pict-ascent p))][dy (pict-descent p)]
                   [l (pict-children p)])
          (if (null? l)
              (begin
                (set-pict-panbox! p (make-bbox x1 y1 x2 y2 ay dy))
                (values x1 y1 x2 y2 ay dy))
              (let ([c (car l)])
                (let-values ([(cx1 cy1 cx2 cy2 cay cdy) (panorama-box! (child-pict c))])
                  (loop (min x1 (+ (* cx1 (child-sx c)) (child-dx c)))
                        (min y1 (+ (* cy1 (child-sy c)) (child-dy c)))
                        (max x2 (+ (* cx2 (child-sx c)) (child-dx c)))
                        (max y2 (+ (* cy2 (child-sy c)) (child-dy c)))
                        (max ay (+ (* cay (child-sy c)) (child-dy c)))
                        (min dy (+ (* cdy (child-sy c)) (child-dy c)))
                        (cdr l)))))))))

;;;
;;; Laundering
;;;

;; Using a pict finder, we can find a child.
;; However, sometimes we may want to conceal children.
;; The function `launder` hides children so they are not found by
;; the pict finders.

(define (launder box*)
  ;; we might be given a pict-convertable
  ;; but set-pict-foo! isn't defined on those
  (define box
    (if (pict? box*)
        box*
        (pict-convert box*)))
  (unless (pict-panbox box)
    (panorama-box! box))
  (let ([b (extend-pict box 0 0 0 0 0 #f)])
    (set-pict-children! b null)
    (set-pict-panbox! b (pict-panbox box))
    ;; After laundering, we can't find the last-pos box.
    ;; So create a new last-position box to preserve the
    ;; original shape:
    (let ([l (pict-last box)])
      (set-pict-last! box #f) ; preserve invariants
      (cond
       [(not l) b]
       [else
        (let-values ([(x y) (lt-find box l)]
                     [(l) (let loop ([l l])
                            (if (pair? l)
                                (if (null? (cdr l))
                                    (car l)
                                    (loop (cdr l)))
                                l))])
          (let ([b2 (blank (pict-width l) (pict-height l)
                           (pict-ascent l) (pict-descent l))])
            (use-last/unchecked
             (pin-over b x y b2)
             b2)))]))))


;;;
;;; The last child 
;;;

;; The `last` field indicates a pict within the children list
;; (transitively) that can be treated as the last element of the last
;; line in the pict. A #f value means that the pict is its own last
;; sub-pict.

;; The last pict is useful when rendering multi line contents.
;; In order to place text, the baseline must be know.


(define (use-last* p sub-p)
  (use-last p (last* sub-p)))

(define (last* sub-p)
  ;; Either use `sub-p' for last or create a path though `sub-p'
  ;; to the last of `sub-p' (in case the last of `sub-p' is also
  ;; in other places within `p')
  (let ([l (pict-last sub-p)])
    (cond
     [(not l) sub-p]
     [(pair? l) (if (pict-path-element=? (car l) sub-p)
                    l
                    (cons sub-p l))]
     [(pict-path-element=? l sub-p) sub-p]
     [else (list sub-p l)])))

;; (use-last pict sub-pict)
;;     Returns a pict like `pict`, but with the last element (as reported by pict-last)
;;     set to `sub-pict`. The sub-pict must exist as a sub-pict
;;     (or path of sub-picts) within pict.
(define (use-last p sub-p)
  (if (let floop ([p p] [sub-p sub-p])
        (or
         (if (not (pair? sub-p))
             (pict-path-element=? p sub-p)
             (and (not (pair? (car sub-p)))
                  (pict-path-element=? p (car sub-p))
                  (or (null? (cdr sub-p))
                      (floop p (cdr sub-p)))))
            (ormap (lambda (c) (floop (child-pict c) sub-p))
                   (pict-children p))))
      (use-last/unchecked p sub-p)
      (error 'use-last
             "given new last-pict path: ~e not in the base pict: ~e"
             sub-p
             p)))

(define (use-last/unchecked p sub-p)
  (make-pict (pict-draw p)
             (pict-width  p) (pict-height  p)
             (pict-ascent p) (pict-descent p)
             (list (make-child p 0 0 1 1 0 0))
             #f
             sub-p))

;;;
;;; Oval (the old name for rounded rectangle)
;;; 

;; See https://latexref.xyz/_005coval.html
;; for an explanation of the corner arguments.

(define (oval box)
  (let ([w (pict-width box)]
        [h (pict-height box)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h
       (put 0 0 ,(pict-draw box))
       (put ,(/ w 2) ,(/ h 2) (oval "" ,w ,h))))))

(define (oval/radius box r)
  (let* ([w (pict-width box)]
         [h (pict-height box)]
         [rr (* 2 r)]
         [lw (- w rr)]
         [lh (- h rr)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h
       (put  0 0 ,(pict-draw box))
       (put ,r       ,r       (oval "[bl]" ,rr ,rr))  (put ,r  0 (line 1 0 ,lw))
       (put ,(- w r) ,r       (oval "[br]" ,rr ,rr))  (put ,w ,r (line 0 1 ,lh))
       (put ,r       ,(- h r) (oval "[tl]" ,rr ,rr))  (put ,r ,h (line 1 0 ,lw))
       (put ,(- w r) ,(- h r) (oval "[tr]" ,rr ,rr))  (put ,0 ,r (line 0 1 ,lh))))))

;;;
;;; Pict Combiners
;;;

;; The append-functions below place picts either on-top or besides each other.
;; The v-functions (v for vertical)   place the picts in a column.
;; The h-functions (h for horizontal) place the picts in a row.
;; The l/c/r stands for left, center, right.
;; The t/c/b stands for top, center, bottom.
;; They indicate how to align the subpicts.

;; The optional d argument specifies amount of space to insert between each
;; pair of pictures in making the column or row.

;; Different procedures align pictures in the orthogonal direction in
;; different ways. For example, vl-append left-aligns all of the
;; pictures.

;; The descent of the result corresponds to baseline that is lowest in
;; the result among all of the picts’ descent-specified baselines;
;; similarly, the ascent of the result corresponds to the highest
;; ascent-specified baseline. If at least one pict is supplied, then the
;; last element (as reported by pict-last) for the result is (or
;; (pict-last pict) pict) for the using last supplied pict.

(define-values (vl-append 
                vc-append 
                vr-append
                
                ht-append
                hc-append
                hb-append
                
                htl-append
                hbl-append)
  (let ([make-append-boxes 
         (lambda (wcomb hcomb fxoffset fyoffset rxoffset ryoffset 
                        combine-ascent combine-descent)
           (let ([do-append
                  (lambda (sep args)
                    (let append-boxes ([args args])
                      (cond
                       [(null? args) (blank)]
                       [(null? (cdr args)) (pict-convert (car args))]
                       [else
                        (let* ([first (pict-convert (car args))]
                               [rest (append-boxes (cdr args))]
                               [w (wcomb (pict-width first) (pict-width rest) sep first rest)]
                               [h (hcomb (pict-height first) (pict-height rest) sep first rest)]
                               [fw (pict-width first)]
                               [fh (pict-height first)]
                               [rw (pict-width rest)]
                               [rh (pict-height rest)]
                               [fd1 (pict-ascent first)]
                               [fd2 (pict-descent first)]
                               [rd1 (pict-ascent rest)]
                               [rd2 (pict-descent rest)]
                               [dx1 (fxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
                               [dy1 (fyoffset fw fh rw rh sep fd1 fd2 rd1 rd2 h)]
                               [dx2 (rxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
                               [dy2 (ryoffset fw fh rw rh sep fd1 fd2 rd1 rd2 h)])
                          (make-pict
                           `(picture 
                             ,w ,h
                             (put ,dx1
                                  ,dy1
                                  ,(pict-draw first))
                             (put ,dx2
                                  ,dy2
                                  ,(pict-draw rest)))
                           w h
                           (combine-ascent fd1 rd1 fd2 rd2 fh rh h (+ dy1 fh) (+ dy2 rh))
                           (combine-descent fd2 rd2 fd1 rd1 fh rh h (- h dy1) (- h dy2))
                           (list (make-child first dx1 dy1 1 1 0 0)
                                 (make-child rest dx2 dy2 1 1 0 0))
                           #f
                           (last* rest)))])))])
             (let ([*-append (case-lambda
                              [() (do-append 0 null)]
                              [(sep . args)
                               (if (number? sep)
                                   (do-append sep args)
                                   (do-append 0 (cons sep args)))])])
               *-append)))]
        [2max    (lambda (a b c . rest) (max a b))]
        [zero    (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 . args) 0)]
        [fv      (lambda (a b . args) a)]
        [sv      (lambda (a b . args) b)]
        [min2    (lambda (a b . args) (min a b))]
        [max2    (lambda (a b . args) (max a b))]
        [3+      (lambda (a b c . args) (+ a b c))]
        [a-max   (lambda (a b c first rest)
                   (+ (max (pict-ascent first) (pict-ascent rest))
                      (max (- (pict-height first) (pict-ascent first))
                           (- (pict-height rest) (pict-ascent rest)))))]
        [d-max   (lambda (a b c first rest)
                   (+ (max (pict-descent first) (pict-descent rest))
                      (max (- (pict-height first) (pict-descent first))
                           (- (pict-height rest) (pict-descent rest)))))]
        [min-ad  (lambda (a b oa ob ah bh h da db)
                   (- h (max oa ob) (max (- ah oa a)
                                         (- bh ob b))))]
        [xmin-ad (lambda (a b oa ob ah bh h da db)
                   (min (+ (- h da) a) (+ (- h db) b)))])
    (values
     (make-append-boxes 2max 3+ 
                        zero (lambda (fw fh rw rh sep . a) (+ sep rh))
                        zero zero 
                        fv sv)
     (make-append-boxes 2max 3+ 
                        (lambda (fw fh rw rh sep . a) (/ (- (max fw rw) fw) 2))
                        (lambda (fw fh rw rh sep . a) (+ sep rh))
                        (lambda (fw fh rw rh sep . a) (/ (- (max fw rw) rw) 2))
                        zero 
                        fv sv)
     (make-append-boxes 2max 3+ 
                        (lambda (fw fh rw rh sep . a) (- (max fw rw) fw))
                        (lambda (fw fh rw rh sep . a) (+ sep rh))
                        (lambda (fw fh rw rh sep . a) (- (max fw rw) rw))
                        zero 
                        fv sv)
     (make-append-boxes 3+ 2max
                        zero
                        (lambda (fw fh rw rh sep . a) (- (max fh rh) fh))
                        (lambda (fw fh rw rh sep . a) (+ sep fw))
                        (lambda (fw fh rw rh sep . a) (- (max fh rh) rh))
                        xmin-ad xmin-ad)
     (make-append-boxes 3+ 2max
                        zero
                        (lambda (fw fh rw rh sep . a) (/ (- (max fh rh) fh) 2))
                        (lambda (fw fh rw rh sep . a) (+ sep fw))
                        (lambda (fw fh rw rh sep . a) (/ (- (max fh rh) rh) 2))
                        xmin-ad xmin-ad)
     (make-append-boxes 3+ 2max 
                        zero zero
                        (lambda (fw fh rw rh sep . a) (+ sep fw)) zero
                        xmin-ad xmin-ad)
     (make-append-boxes 3+ a-max
                        zero
                        (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
                          (- h fh (- (max fd1 rd1) fd1)))
                        (lambda (fw fh rw rh sep . a) (+ sep fw))
                        (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
                          (- h rh (- (max fd1 rd1) rd1)))
                        max2 min-ad)
     (make-append-boxes 3+ d-max
                        zero
                        (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
                          (- (max fd2 rd2) fd2))
                        (lambda (fw fh rw rh sep . a) (+ sep fw))
                        (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
                          (- (max fd2 rd2) rd2))
                        min-ad max2))))

;; The functions below creates a new picture by superimposing a set of pictures.
;; The name prefixes are alignment indicators:
;;   horizontal alignment then vertical alignment.

;; The descent of the result corresponds to baseline that is lowest in
;; the result among all of the picts’ descent-specified baselines;
;; similarly, the ascent of the result corresponds to the highest
;; ascent-specified baseline. The last element (as reported by pict-last)
;; for the result is the lowest, right-most among the last-element picts
;; of the pict arguments, as determined by comparing the last-element
;; bottom-right corners.

(define-values (lt-superimpose
                lb-superimpose
                lc-superimpose

                ltl-superimpose
                lbl-superimpose

                rt-superimpose
                rb-superimpose
                rc-superimpose

                rtl-superimpose
                rbl-superimpose

                ct-superimpose
                cb-superimpose
                cc-superimpose

                ctl-superimpose
                cbl-superimpose)
  (let ([make-superimpose
         (lambda (get-h get-v get-th name)
           (lambda boxes*
             (when (null? boxes*)
               (error name "expected at least one argument, got none"))
             (define boxes
               (map
                (lambda (p)
                  (cond
                    [(pict? p)             p]
                    [(pict-convertible? p) (pict-convert p)]
                    [else (raise-argument-error
                           name "all picts as arguments"
                           (apply string-append (add-between (map (λ (x) (format "~e" x)) boxes*) " ")))]))
                boxes*))
             (let ([max-w            (apply max (map pict-width boxes))]
                   [max-h            (apply max (map pict-height boxes))]
                   [max-a            (apply max (map pict-ascent boxes))]
                   [max-a-complement (apply max (map (lambda (b) (- (pict-height b) (pict-ascent b)))
                                                     boxes))]
                   [max-d            (apply max (map pict-descent boxes))]
                   [max-d-complement (apply max (map (lambda (b) (- (pict-height b) (pict-descent b)))
                                                     boxes))])
               (let ([p (picture max-w (get-th max-h max-a max-d max-a-complement max-d-complement)
                                 (map (lambda (box)
                                        `(place ,(get-h max-w (pict-width box))
                                                ,(get-v max-h (pict-height box)
                                                        max-d (pict-descent box)
                                                        max-a-complement (pict-ascent box))
                                                ,box))
                                      boxes))])
                 ;; Figure out top and bottom baselines by finding the picts again, etc:
                 (let ([ys (map (lambda (box)
                                  (let-values ([(x y) (find-lt p box)])
                                    y))
                                boxes)])
                   (let ([min-a (apply min (map (lambda (box y)
                                                  (+ (- (pict-height p) y) (pict-ascent box)))
                                                boxes ys))]
                         [min-d (apply min (map (lambda (box y)
                                                  (+ (- y (pict-height box)) (pict-descent box)))
                                                boxes ys))])
                     (make-pict (pict-draw p)
                                (pict-width p) (pict-height p)
                                min-a min-d
                                (pict-children p)
                                #f
                                ;; Find bottomost, rightmost of old last picts to be the
                                ;;  new last pict.
                                (let ([subs (map (lambda (box)
                                                   (let ([last (last* box)])
                                                     (let-values ([(x y) (rb-find p last)])
                                                       (list last x y))))
                                                 boxes)])
                                  (if (null? subs)
                                      #f
                                      (caar (sort subs
                                                  (lambda (a b)
                                                    (let ([ay (caddr a)]
                                                          [by (caddr b)])
                                                      (cond
                                                       [(ay . > . by) #t]
                                                       [(= ay by) ((cadr a) . > . (cadr b))]
                                                       [else #f]))))))))))))))]
        [norm  (lambda (h a d ac dc)    h)]
        [tbase (lambda (h a d ac dc)    (+ a ac))] 
        [bbase (lambda (h a d ac dc)    (+ d dc))] 
        [lb    (lambda (m v . rest)     0)]
        [rt    (lambda (m v . rest)     (- m v))]
        [tline (lambda (m v md d mac a) (- mac (- v a)))]
        [bline (lambda (m v md d mac a) (- md d))]
        [c     (lambda (m v . rest)     (/ (- m v) 2))])
    (values
     (make-superimpose lb rt    norm  'lt-superimpose)
     (make-superimpose lb lb    norm  'lb-superimpose)
     (make-superimpose lb c     norm  'lc-superimpose)
     (make-superimpose lb tline tbase 'ltl-superimpose)
     (make-superimpose lb bline bbase 'lbl-superimpose)
     (make-superimpose rt rt    norm  'rt-superimpose)
     (make-superimpose rt lb    norm  'rb-superimpose)
     (make-superimpose rt c     norm  'rc-superimpose)
     (make-superimpose rt tline tbase 'rtl-superimpose)
     (make-superimpose rt bline bbase 'rbl-superimpose)
     (make-superimpose c  rt    norm  'ct-superimpose)
     (make-superimpose c  lb    norm  'cb-superimpose)
     (make-superimpose c  c     norm  'cc-superimpose)
     (make-superimpose c  tline tbase 'ctl-superimpose)
     (make-superimpose c  bline bbase 'cbl-superimpose))))

;;;
;;; Table
;;;

;; Given `superimpose` we can make a table.

(define table
  (case-lambda
    [(ncol cells col-aligns row-aligns col-seps row-seps)
     (let ([count (length cells)])
       (unless (zero? (remainder count ncol))
         (error 'table "cell count isn't divisble by the provided column count"))
       (let* ([w ncol]
              [h (/ count w)]
              [cells (let rloop ([r h][cells cells][r-acc null])
                       (if (zero? r)
                           (list->vector (reverse r-acc))
                           (let loop ([c w][cells cells][one-acc null])
                             (if (zero? c)
                                 (rloop (sub1 r) cells (cons (list->vector (reverse one-acc)) r-acc))
                                 (loop (sub1 c) (cdr cells) (cons (pict-convert (car cells)) one-acc))))))]
              [imp-list->vector (lambda (l n)
                                  (let ([v (make-vector n)])
                                    (let loop ([l l][p 0])
                                      (unless (= n p)
                                        (vector-set! v
                                                     p
                                                     (if (pair? l)
                                                         (car l)
                                                         l))
                                        (loop (if (pair? l) (cdr l) l) (add1 p))))
                                    v))]
              [ralign (imp-list->vector row-aligns h)]
              [calign (imp-list->vector col-aligns w)]
              [rsep (imp-list->vector row-seps h)]
              [csep (imp-list->vector col-seps w)]
              [get-cell (lambda (c r) (vector-ref (vector-ref cells r) c))]
              [nmap (lambda (f w)
                      (let loop ([n w][acc null])
                        (if (zero? n)
                            acc
                            (loop (sub1 n) (cons (f (sub1 n)) acc)))))]
              [rowmap (lambda (f) (nmap f h))]
              [colmap (lambda (f) (nmap f w))]
              [superimposed-rows (list->vector
                                  (rowmap (lambda (r)
                                            (apply
                                             (vector-ref ralign r)
                                             (colmap (lambda (c) (get-cell c r)))))))]
              [superimposed-cols (list->vector
                                  (colmap (lambda (c)
                                            (apply
                                             (vector-ref calign c)
                                             (rowmap (lambda (r) (get-cell c r)))))))])
         ; No space after the last row/col
         (vector-set! rsep (sub1 h) 0)
         (vector-set! csep (sub1 w) 0)

         (apply
          vl-append
          0
          (rowmap
           (lambda (r)
             (vl-append
              0
              (apply
               ht-append
               0
               (colmap (lambda (c)
                         (ht-append
                          0
                          (let* ([cell (get-cell c r)]
                                 [sc (vector-ref superimposed-cols c)]
                                 [sr (vector-ref superimposed-rows r)]
                                 [w (pict-width sc)]
                                 [h (pict-height sr)])
                            (let-values ([(x __) (find-lb sc cell)]
                                         [(_  y) (find-lb sr cell)])
                              (picture
                               w h
                               `((place ,x ,y ,cell)))))
                          (blank (vector-ref csep c) 0)))))
              (blank 0 (vector-ref rsep r))))))))]))

;;;
;;; Connectors and Placement
;;;

;; For the following picture constructors, we want to add a few
;; commands to our set of drawing commands.

;; The function `picture*` below does just that.
;; It adds various connect commands, the place command
;; and the curve command.

; (place x y p)
;    Place sub-pict `p` at (x,y).
;    Make p a child.


;; Note:
;;   In the code below `~connect` doesn't use the `exact` nor the `close-enough`
;;   arguments. I think, `~connect` is a holdover from the previous LaTeX backend.
;;   The code for `picture*` could be simplified by having a single, simple
;;   `connect` function.

; Connect two points with either a line segment or an arrow.
(define connect
  (case-lambda
    [(x1 y1 x2 y2)        (connect            x1 y1 x2 y2 #f)]
    [(x1 y1 x2 y2 arrow?) (~connect 'r +inf.0 x1 y1 x2 y2 arrow?)]))

(define ~connect 
  (case-lambda
    [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
    [(exact close-enough x1 y1 x2 y2 arrow?)
     `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,(- x2 x1) ,(- y2 y1) #f)))]))


(define (picture* w h a d commands)
  (let loop ([commands   commands]
             [translated null]
             [children   null])
    (if (null? commands)
        (make-pict
         `(picture ,w ,h
                   ,@(reverse translated))
         w h a d
         children
         #f
         #f)
        (let ([c    (car commands)]
              [rest (cdr commands)])
          (unless (and (pair? c) (symbol? (car c)))
            (error 'picture "bad command: ~a" c))
          (case (car c)
            [(connect) (loop rest
                             (append (apply connect (cdr c))
                                     translated)
                             children)]
            [(dconnect) (loop rest
                              (let ([x (cadr c)]
                                    [y (caddr c)]
                                    [dx (cadddr c)]
                                    [dy (list-ref c 4)])
                                (append (connect x y (+ x dx) (+ y dy)
                                                 (if (null? (list-tail c 5))
                                                     #t
                                                     (list-ref c 5)))
                                        translated))
                              children)]
            [(connect~y) (loop rest
                               (append (apply ~connect 'x (cdr c))
                                       translated)
                               children)]
            [(connect~x) (loop rest
                               (append (apply ~connect 'y (cdr c))
                                       translated)
                               children)]
            [(connect~xy) (loop rest
                                (append (apply ~connect 'r (cdr c))
                                        translated)
                                children)]
            [(curve) (loop rest
                           (let ([x1 (cadr c)]
                                 [y1 (caddr c)]
                                 [x2 (cadddr c)]
                                 [y2 (list-ref c 4)]
                                 [xm (list-ref c 5)]
                                 [ym (list-ref c 6)]
                                 [d (if (null? (list-tail c 7))
                                        1.0
                                        (list-ref c 7))])
                             (let ([p (if (and d (>= d 0))
                                          (inexact->exact
                                           (floor (* d (sqrt (+ (expt (- x2 x1) 2)
                                                                (expt (- y2 y1) 2))))))
                                          #f)])
                               (if (and (= x1 x2) (= y1 y2))
                                   translated
                                   (cons `(qbezier ,p ,x1 ,y1 ,xm ,ym ,x2 ,y2)
                                         translated))))
                           children)]
            [(place) (let ([x (cadr c)]
                           [y (caddr c)]
                           [p (cadddr c)])
                       (loop rest
                             (cons
                              `(put ,x ,y ,(pict-draw p))
                              translated)
                             (cons
                              (make-child p x y 1 1 0 0)
                              children)))]
            [else (loop rest (cons c translated) children)])))))

(define (picture w h commands)
  (picture* w h h 0 commands))

(define (cons-picture p commands)
  (picture
   (pict-width p) (pict-height p)
   (cons
    `(place 0 0 ,p)
    commands)))

(define (cons-picture* p commands)
  (picture*
   (pict-width p) (pict-height p)
   (pict-ascent p) (pict-descent p)
   (cons
    `(place 0 0 ,p)
    commands)))


(define (place-it who flip? base dx dy target)
  (let-values ([(dx dy)
                (cond
                 [(and (number? dx) (number? dy))
                  (values dx (- (pict-height base) dy))]
                 [(and (pict-path? dx)
                       (procedure? dy)
                       (procedure-arity-includes? dy 2))
                  (if flip?
                      (let-values ([(dx dy) (dy base dx)])
                        (values dx (- (pict-height base) dy)))
                      (dy base dx))]
                 [else
                  (error who
                         "expects two numbers or a sub-pict and a find procedure")])])
    (use-last/unchecked (cons-picture*
                         base
                         `((place ,dx ,(- dy (pict-height target)) ,target)))
                        (last* base))))

(define (place-over base dx dy target)
  (place-it 'place-over #f base dx dy target))

(define (place-under base dx dy target)
  (cc-superimpose
   (place-it 'place-under #f (ghost base) dx dy target)
   base))

(define (pin-over base dx dy target)
  (place-it 'pin-over #t base dx dy target))

(define (pin-under base dx dy target)
  (cc-superimpose
   (place-it 'pin-under #t (ghost base) dx dy target)
   (launder base)))





(define (optimize s)
  (let o-loop ([s s][dx 0][dy 0])
    (if (string? s)
        s
        (let ([tag (car s)])
          (case tag
            [(picture)
             (list* 'picture (cadr s) (caddr s)
                    (map optimize (cdddr s)))]
            [(color)
             (let ([next (caddr s)])
               (if (and (pair? next) (eq? (car next) 'color))
                   (optimize next)
                   (list* 'color (cadr s) 
                          (list 'put dx dy (optimize next)))))]
            [(thickness)
             (let ([t (cadr s)]
                   [p (caddr s)])
               (list 'put dx dy 
                     (list 'thickness t 
                           (optimize p))))]
            [(put)
             (let ([x (cadr s)]
                   [y (caddr s)]
                   [next (cadddr s)])
               (if (and (pair? next) (eq? (car next) 'picture))
                                        ; optmize put-picture to just contents ...
                   (cons 'begin (map (lambda (s) (o-loop s (+ x dx) (+ y dy))) (cdddr next)))
                                        ; normal
                   (list 'put (+ x dx) (+ y dy) (optimize next))))]
            [(qbezier)
             (let ([x1 (list-ref s 2)]
                   [y1 (list-ref s 3)]
                   [xm (list-ref s 4)]
                   [ym (list-ref s 5)]
                   [x2 (list-ref s 6)]
                   [y2 (list-ref s 7)]
                   [p (list-ref s 1)])
               (list 'qbezier p
                     (+ x1 dx) (+ y1 dy)
                     (+ xm dx) (+ ym dy)
                     (+ x2 dx) (+ y2 dy)))]
            [(frame)
             (list 'frame (optimize (cadr s)))]
            [(colorbox)
             (list 'colorbox (cadr s) (optimize (caddr s)))]
            [(line vector dirline dirvector circle circle* make-box oval prog) s]
            [else (error 'optimize "bad tag: ~s" tag)])))))

(define (fixup-top s)
  (cond
   [(and (pair? s) (eq? (car s) 'color))
    ;; Drop initial put
    (list* 'color (cadr s) (caddr (cdddr s)))]
   [(and (pair? s) (eq? (car s) 'put))
    ;; Wrap initial put (from thickness) in a pair of braces
    `(local ,(cadddr s))]
   [else
    ;; Do nothing
    s]))

(define (prepare-for-output s)
  (fixup-top (optimize (pict-draw s))))

(define (pict->command-list s)
  (let output ([s (prepare-for-output s)])
    (if (string? s)
        (list s)
        (let ([tag (car s)])
          (case tag
            [(local)
             (output (cadr s))]
            [(begin)
             (apply append (map output (cdr s)))]
            [(picture)
             (apply append (map output (cdddr s)))]
            [(color)
             `((with-color ,(cadr s) ,(output (cddr s))))]
            [(thickness)
             `((with-thickness ,(cadr s) ,(output (caddr s))))]
            [(put)
             `((offset ,(cadr s) ,(caddr s) ,(output (cadddr s))))]
            [(qbezier)
             `((bezier ,@(cddr s)))]
            [(line vector)
             `((,tag ,(cadr s) ,(caddr s) ,(cadddr s)))]
            [(circle circle*)
             `((,tag ,(cadr s)))]
            [(frame)
             `((frame ,(output (cadr s))))]
            [(colorbox)
             `((colorbox ,(cadr s) ,(output (caddr s))))]
            [(oval)
             `((oval ,(caddr s) ,(cadddr s) ,(cadr s)))]
            [(make-box)
             `((make-box ,(cadr s) ,(caddr s) ,(cadddr s) ,(car (cddddr s))))]
            [(prog)
             `((prog ,(cadr s) ,(caddr s)))]
            [else (error 'pict->commands "bad tag: ~s" tag)])))))


(define convert-bounds-padding
  (make-parameter
   (list 3 3 3 3)
   (lambda (x)
     (unless (and (list? x) (= 4 (length x)) (andmap real? x)
                  (andmap (lambda (i) (not (negative? i))) x))
       (raise-argument-error 'convert-bounds-padding
                             "(list/c (>=/c 0) (>=/c 0) (>=/c 0) (>=/c 0))"
                             x))
     x)))

;;;
;;; Basic Pict Constructors
;;;

;; Note: These are without keywords for now.

(define (dc f w h [a h] [d 0])
  (make-pict `(prog ,f ,h) w h a d null #f #f))

(define (circle diameter [border-color #f] [border-width #f])
  (define d  diameter)
  (define r  (/ d 2.))
  (dc (lambda (dc x y)
        (define cx (+ x r))
        (define cy (+ y r))
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))
       
        (dc 'begin-path)
        (dc 'arc cx cy r 0 (* 2. pi))
        (dc 'stroke)
        
        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width)))
      d d))

(define (ellipse width height [border-color #f] [border-width #f])
  (define dx width)
  (define dy height)
  (define rx (/ dx  2.))
  (define ry (/ dy 2.))
  (define rotation 0.)
  (dc (lambda (dc x y)
        (define cx (+ x rx))
        (define cy (+ y ry))        
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))
        (dc 'begin-path)
        (dc 'ellipse cx cy rx ry rotation 0. (* 2. pi))
        (dc 'stroke)
        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width)))
      dx dy))

(define (filled-ellipse width height
                        [draw-border? #t] 
                        [border-color #f]
                        [border-width #f])
  (define dx width)
  (define dy height)
  (define rx (/ dx  2.))
  (define ry (/ dy 2.))
  (define rotation 0.)
  (dc (lambda (dc x y)
        (define cx (+ x rx))
        (define cy (+ y ry))
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))

        (dc 'begin-path)
        (dc 'ellipse cx cy rx ry rotation 0. (* 2. pi))
        (dc 'fill)
        
        (when draw-border?
          (dc 'begin-path)
          (dc 'ellipse cx cy rx ry rotation 0. (* 2. pi))
          (dc 'stroke))
        
        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width)))
      dx dy))

(define (disk diameter
              [draw-border? #t]
              [border-color #f]
              [border-width #f])
  (filled-ellipse diameter diameter
                  draw-border?
                  border-color
                  border-width))

(define (rectangle width height [border-color #f] [border-width #f])
  (define w width)
  (define h height)
  (dc (lambda (dc x y)
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))

        (dc 'stroke-rect x y w h)

        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width))
        )      
      w h))

(define (filled-rectangle width height
                          [draw-border? #t]
                          [border-color #f]
                          [border-width #f])
  (define w width)
  (define h height)
  (dc (lambda (dc x y)
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))

        (dc 'fill-rect x y w h)
        (when draw-border?
          (dc 'stroke-rect x y w h))

        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width)))
      w h))

(define (rounded-rectangle width height
                           [corner-radius -0.25]
                           [border-color  #f]
                           [border-width  #f])
  (define w  width)
  (define h  height)
  (define cr corner-radius)
  (define r  (cond
               [(> cr 0)       cr]
               [(< -0.5 cr 0.) (* (- cr) (min w h))]
               [else
                (error 'rounded-rectangle "expected radiues, got: ~a" cr)]))
  (dc (lambda (dc x y)
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))

        (dc 'begin-path)
        (dc 'round-rect x y w h r)
        (dc 'stroke)

        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width))
        )      
      w h))

(define (filled-rounded-rectangle width height
                                  [corner-radius -0.25]
                                  [draw-border? #t]
                                  [border-color #f]
                                  [border-width #f])
  (define w  width)
  (define h  height)
  (define cr corner-radius)
  (define r  (cond
               [(> cr 0)       cr]
               [(< -0.5 cr 0.) (* (- cr) (min w h))]
               [else
                (error 'rounded-rectangle "expected radiues, got: ~a" cr)]))
  (dc (lambda (dc x y)
        (define old-stroke (and border-color (dc 'stroke-style)))
        (define old-width  (and border-width (dc 'line-width)))
        (when border-color
          (dc 'stroke-style (color->string border-color)))
        (when border-width
          (dc 'line-width border-width))

        (dc 'begin-path)
        (dc 'round-rect x y w h r)
        (dc 'fill)

        (when draw-border?
          (dc 'begin-path)
          (dc 'round-rect x y w h r)
          (dc 'stroke))

        (when border-color
          (dc 'stroke-style old-stroke))
        (when border-width
          (dc 'line-width old-width))
        )      
      w h))

(define (bitmap image)
  ; `image` is an external containing an ImageBitmap.  
  (define w (js-ref image 'width))
  (define h (js-ref image 'height))
  (dc (lambda (dc x y)
        (dc 'draw-image image x y))
      w h))

;;;
;;; Pict Drawing Adjusters
;;;


;; Scales a pict drawing, as well as its bounding box, by multiplying it
;; current size by factor (if two arguments are supplied) or by
;; multiplying the current width by w-factor and current height by
;; h-factor (if three arguments are supplied).

(define scale
  (case-lambda
    [(p x-factor y-factor)
     (define drawer (make-pict-drawer p))
     (define new
       (dc
        (λ (dc x y)
          (define t (dc 'transform))
          (dc 'scale x-factor y-factor)
          (drawer dc
                  (/ x x-factor)
                  (/ y y-factor))          
          (dc 'transform t))
        (* (pict-width   p) x-factor)
        (* (pict-height  p) y-factor)
        (* (pict-ascent  p) y-factor)
        (* (pict-descent p) y-factor)))
     (make-pict (pict-draw    new)
                (pict-width   new)
                (pict-height  new)
                (pict-ascent  new)
                (pict-descent new)
                (list (make-child p 0 0 x-factor y-factor 0 0))
                #f
                (pict-last p))]
    [(p factor) (scale p factor factor)]))

;; ;; The adjusters flip-x and flip-y are easier to define,
;; ;; if there are general transformations available.

;; (define (compose-trans t1 t2)
;;   (define-values (a d b e c f) (apply values t1))
;;   (define-values (g j h k i l) (apply values t2))
;;   (list (+ (* a g) (* b j))   (+ (* d g) (* e j))
;;         (+ (* a h) (* b k))   (+ (* d h) (* e k))
;;         (+ (* a i) (* b l) c) (+ (* d i) (* e l) f)))
  
;; (define (compose-trans* t0 . ts)
;;   (foldl (λ (t acc) (compose-trans acc t)) t0 ts))

;; (define (make-translate h k)
;;   (list 1 0 0 1 h k))

;; (define (make-flip-x) ; around y-axis
;;   (list -1 0 0 1 0 0))

(define (flip-x p)
  (define w      (pict-width   p))
  (define h      (pict-height  p))
  (define a      (pict-ascent  p))
  (define d      (pict-descent p))
  (define drawer (make-pict-drawer p))
  (define new    (dc (λ (dc x y)
                       ;; ( x, y) is the top-left corner
                       ;; (cx,cy) is the center of the pict
                       (define cx    (+ x (/ w 2.)))
                       (define cy    (+ y (/ h 2.)))
                       (define old-t (dc 'transform))
                       (dc 'translate cx cy)
                       (dc 'transform -1 0 0 1 0 0)
                       (dc 'translate (- cx) (- cy))
                       (drawer dc x y)
                       (dc 'transform old-t))
                     w h a d))
  ; Embed the child
  (make-pict (pict-draw    new)
             w h a d
             (list (make-child p 1 0 0 1 0 0))
             #f
             (pict-last p)))
  

(define (flip-y p)
  (define w      (pict-width   p))
  (define h      (pict-height  p))
  (define a      (pict-ascent  p))
  (define d      (pict-descent p))
  (define drawer (make-pict-drawer p))
  (define new    (dc (λ (dc x y)
                       ;; ( x, y) is the top-left corner
                       ;; (cx,cy) is the center of the pict
                       (define cx (+ x (/ w 2.)))
                       (define cy (+ y (/ h 2.)))
                       (define old-t (dc 'transform))
                       (dc 'translate cx cy)
                       (dc 'transform 1 0 0 -1 0 0)
                       (dc 'translate (- cx) (- cy))
                       (drawer dc x y)
                       (dc 'transform old-t))
                     ; Note: The ascent and descent are swapped on purpose here.
                     ;       The baseline also flips when the pict is flipped.
                     w h d a))
  ; `dc` doesn't embed the subpict, so we replant
  (make-pict (pict-draw    new)
             (pict-width   new)
             (pict-height  new)
             (pict-ascent  new)
             (pict-descent new)
             (list (make-child p 1 0 0 1 0 0))
             #f
             (pict-last p)))


(define (translate p dx dy [extend-bb? #f])
  (define bb? extend-bb?)
  (define nw (if (not bb?) (pict-width p)  (+ (pict-width  p) (abs dx))))
  (define nh (if (not bb?) (pict-height p) (+ (pict-height p) (abs dy))))
  (define drawer
    (dc
     (lambda (dc dx1 dy1)
       (draw-pict p dc
                  (+ dx1
                     (if bb?
                         (max 0 dx)
                         dx))
                  (+ dy1
                     (if bb?
                         (max 0 dy)
                         dy))))
     nw nh))
  (make-pict (pict-draw drawer)
             nw nh
             (if (and bb? (dy . < . 0))
                 (- (pict-ascent p) dy)
                 (pict-ascent p))
             (if (and bb? (dy . > . 0))
                 (+ (pict-descent p) dy)
                 (pict-descent p))
             (list
              (make-child p
                          dx (- dy) 
                          1 1
                          0 0))
             #f
             (pict-last p)))

(define (shear p shear-x shear-y)
  (define drawer  (make-pict-drawer p))
  (define x-shift (* shear-x (pict-height p)))
  (define y-shift (* shear-y (pict-width  p)))
  (define new
    (dc
     (λ (dc dx dy)
       (define t (dc 'transform))
       (dc 'transform 1 shear-y shear-x 1 (- dx (min 0 x-shift)) (- dy (min 0 y-shift)))
       (drawer dc 0 0)
       (dc 'transform t))
     (+ (pict-width  p) (abs x-shift))
     (+ (pict-height p) (abs y-shift))
     (pict-ascent  p)
     (pict-descent p)))
  (define rx-shift
    (if (shear-x . < . 0)
        (* -3/2 x-shift)
        (* -1/2 x-shift)))
  (make-pict (pict-draw    new)
             (pict-width   new)
             (pict-height  new)
             (pict-ascent  new)
             (pict-descent new)
             (list (make-child p rx-shift 0 1 1 shear-x shear-y))
             #f
             (pict-last p)))


(define (rotate p theta)
    (let ([w      (pict-width  p)]
          [h      (pict-height p)]
          [drawer (make-pict-drawer p)])
      (let ([dl (min 0 (* w    (cos theta)) (* h (sin theta)) (+ (* w    (cos theta)) (* h (sin theta))))]
            [dr (max 0 (* w    (cos theta)) (* h (sin theta)) (+ (* w    (cos theta)) (* h (sin theta))))]
            [dt (min 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]
            [db (max 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]            
            [da (- (*    (pict-ascent p)                    (cos theta))   (* (sin theta) w 1/2))]
            [dd (- (* (- (pict-height p) (pict-descent p))  (cos theta))   (* (sin theta) w 1/2))])
        (let ([new (dc
                    (lambda (dc x y)
                      (let ([t (dc 'transform)])
                        (dc 'translate (- x dl) (- y dt))
                        (dc 'rotate theta)
                        (drawer dc 0 0)
                        (dc 'transform t)))
                    (- dr dl) (- db dt) 
                    (min (- da dt) (- (- db dt) (- db dd)))
                    (min (- db da) (- db dd)))])
          (make-pict (pict-draw    new)
		     (pict-width   new)
		     (pict-height  new)
		     (pict-ascent  new)
		     (pict-descent new)
		     (list (make-child p 
                                       (- (* h (sin theta)) dl) 
                                       (max 0 (- db (* h (cos theta))))
                                       (cos theta) (cos theta) 
                                       (- (sin theta)) (sin theta)))
		     #f
                     (pict-last p))))))

(define (cellophane p alpha-factor [composite? #t]) 
  (let ([p (pict-convert p)])
    (cond
      [(= 1.0 alpha-factor)
       (inset p 0)]
      [(zero? alpha-factor)
       (ghost p)]
      [else
       (let ([drawer (make-pict-drawer p)])
         (let ([new
                (dc
                 (if composite?
                     (lambda (dc x y)
                       (define ctx       (dc 'context))
                       (define group     (begin-alpha ctx alpha-factor))
                       (define group-ctx (alpha-group-ctx group))
                       (define group-dc  (canvas-context->dc group-ctx))
                       (drawer group-dc x y)
                       (end-alpha ctx group))
                     (lambda (dc x y)
                       (let ([a (dc 'global-alpha)])
                         (dc 'global-alpha (* a alpha-factor))
                         (drawer dc x y)
                         (dc 'global-alpha a))))
                 (pict-width   p)
                 (pict-height  p)
                 (pict-ascent  p)
                 (pict-descent p))])
           (make-pict (pict-draw    new)
                      (pict-width   new)
                      (pict-height  new)
                      (pict-ascent  new)
                      (pict-descent new)
                      (list (make-child p 0 0 1 1 0 0))
                      #f
                      (pict-last p))))])))

;; Selects a specific pen width for drawing, which applies to pen drawing
;; for pict that does not already use a specific pen width. A #f value
;; for w makes the pen transparent (in contrast to a zero value, which
;; means “as thin as possible for the target device”).

(define (linewidth w p)
  (unless (and (number? w) (real? w) (not (negative? w)))
    (error 'linewidth "expected a non-negative width, got: ~a" w))
  (define drawer (make-pict-drawer p))
  (define new    (dc (lambda (dc x y)
                       (cond
                         ; zero width means transparent
                         [(zero? w)
                          (define old-s (dc 'stroke-style))
                          (dc 'stroke-style "transparent")
                          (drawer dc x y)
                          (dc 'stroke-style old-s)]
                         ; w is positive
                         [else
                          (define old-w (dc 'line-width))
                          (dc 'line-width w)
                          (drawer dc x y)
                          (dc 'line-width old-w)]))
                     (pict-width   p)
                     (pict-height  p)
                     (pict-ascent  p)
                     (pict-descent p)))
  ; p needs to be a sub-pict, so `new` is transplanted
  (make-pict (pict-draw    new)
             (pict-width   new)
             (pict-height  new)
             (pict-ascent  new)
             (pict-descent new)
             (list (make-child p 0 0 1 1 0 0))
             #f
             (pict-last p)))

(define old-linestyle-symbols
  '(transparent
    solid
    ; xor hilite
    dot long-dash short-dash dot-dash
    ; xor-dot xor-long-dash xor-short-dash xor-dot-dash
                ))
(define (old-linestyle? x)
  (and (symbol? x)
       (memq x old-linestyle-symbols)))

(define dash-style-symbols
  '(dot long-dash short-dash dot-dash))
(define (dash-style-symbol? x)
  (and (symbol? x)
       (memq x dash-style-symbols)))

(define (dash-style->segments linestyle)
  ;; (define long-dash-pen-style  (make-pen-style '(5 4)))
  ;; (define short-dash-pen-style (make-pen-style '(3 2)))
  ;; (define dot-pen-style        (make-pen-style '(1 2)))
  ;; (define dot-dash-pen-style   (make-pen-style '(1 3 4 3)))
  (case linestyle
    [(long-dash)  (vector 5 4)]
    [(short-dash) (vector 3 2)]
    [(dot)        (vector 1 2)]
    [(dot-dash)   (vector 1 3 4 3)]
    [else         #f]))
  
(define (linestyle style p)
  ; For `style` we accept:
  ;   - a color
  ;   - a gradient (external)
  ;   - a pattern  (external)
  ; See
  ;   https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeStyle

  ; For backwards compatibility the symbols:
  ;   transparent solid xor hilite dot long-dash short-dash
  ;   dot-dash xor-dot xor-long-dash xor-short-dash xor-dot-dash
  ; are converted to a style as above.
  
  (define drawer (make-pict-drawer p))
  (define new    (dc (lambda (dc x y)
                       (cond
                         ; transparent
                         [(eq? style 'transparent)
                          (define old-s (dc 'stroke-style))
                          (dc 'stroke-style "transparent")
                          (drawer dc x y)
                          (dc 'stroke-style old-s)]
                         
                         [(eq? style 'solid)
                          (define old-s (dc 'stroke-style))
                          (dc 'stroke-style "") ; choose default
                          (drawer dc x y)
                          (dc 'stroke-style old-s)]

                         [(dash-style-symbol? style)
                          (define segments (dash-style->segments style))
                          (dc 'set-line-dash segments)
                          (drawer dc x y)
                          (dc 'set-line-dash #())]

                         [(external? style)
                          ; external color, gradient or pattern
                          (define old-s (dc 'stroke-style))                          
                          (dc 'stroke-style style)
                          (drawer dc x y)
                          (dc 'stroke-style old-s)]
                         
                         [else
                          (define col (color->string style))
                          (cond
                            [col
                             (define old-s (dc 'stroke-style))
                             (dc 'stroke-style col)
                             (drawer dc x y)
                             (dc 'stroke-style old-s)]
                            [else
                             (error 'linestyle
                                    "expected a line style, got: ~a"
                                    style)])]))
                     (pict-width   p)
                     (pict-height  p)
                     (pict-ascent  p)
                     (pict-descent p)))
  ; p needs to be a sub-pict, so `new` is transplanted
  (make-pict (pict-draw    new)
             (pict-width   new)
             (pict-height  new)
             (pict-ascent  new)
             (pict-descent new)
             (list (make-child p 0 0 1 1 0 0))
             #f
             (pict-last p)))

; (inset/clip pict l-amt t-amt r-amt b-amt)
;    Insets and clips the pict’s drawing to its bounding box.
;    Usually, the inset amounts are negative.
(define inset/clip
  (let ()
    (define (->real v)
      (cond
        [(flonum? v) v]
        [(real? v)   (exact->inexact v)]
        [else (error 'inset/clip "expected a real number, got: ~a" v)]))
    (case-lambda
      [(p a)   (inset/clip p a a a a)]
      [(p h v) (inset/clip p h v h v)]
      [(p l t r b)
       (let* ([p      (inset p l t r b)]
              [drawer (make-pict-drawer p)]
              [w      (pict-width  p)]
              [h      (pict-height p)])

         (define new
           (dc
            (λ (dc x y)
              (dc 'save) ; only way to store clipping region
              ; (define rgn <make-new-path>) ; new Path2d()   
              ; <set-reg-to-rectangle>       ; rgn.rectangle(x, y, w, h)
              (define rgn (js-new (js-var "Path2D") (vector))) ; new Path2D()
              (js-send rgn "rect"
                       (vector (->real x)
                               (->real y)
                               (->real w)
                               (->real h)))
              (dc 'clip rgn)
              (drawer dc x y)
              (dc 'restore))
            w h
            (pict-ascent p) (pict-descent p)))
         
         (make-pict (pict-draw    new)
                    (pict-width   new)
                    (pict-height  new)
                    (pict-ascent  new)
                    (pict-descent new)
                    (list (make-child p 0 0 1 1 0 0))
                    #f
                    (pict-last p)))])))
  
(define (clip p)
  (inset/clip p 0))

(define (hyperlinkize r)
    (colorize (inset
	       (place-over r
			   0 (pict-height r)
			   (linewidth 2 (hline (pict-width r) 1)))
	       0 0 0 2)
	      "blue"))



(define (freeze p
                [_inset      0]  ; todo #:inset
                [extra-scale 1]) ; todo #:scale
  (define inset-list
    (cond
      [(real? _inset) (list _inset)]
      [else           _inset]))
  (define p*      (pict-convert p))
  (define sized   (scale (apply inset p* inset-list) extra-scale))
  (define frozen  (bitmap (pict->bitmap sized)))
  (define unsized (apply inset (scale frozen (/ extra-scale)) (map - inset-list)))
  (struct-copy pict p* [draw (pict-draw unsized)]))


    
;;;
;;; Animation combinators
;;;

(define single-pict (lambda (p) (if (list? p) (last p) p)))

;; "Morph" from one pict to another. Use `combine' to align
;; the picts relative to another. Only the bounding box is
;; actually morphed; the drawing part transitions by fading
;; the original `a' out and the new `b' in. The `n' argument
;; ranges from 0.0 (= `a') to 1.0 (= `b').
(define (fade-pict n a b ; a and b are picts
                   [combine cc-superimpose]    ; todo #:combine?
                   [composite? #t])            ; todo #:composite?
  ;; Combine ghosts of scaled pictures:
  (let ([orig (combine (cellophane a (- 1.0 n) composite?)
                       (cellophane b n         composite?))])
    (cond
     [(zero? n) (refocus orig a)]
     [(= n 1.0) (refocus orig b)]
     [else
      (let-values ([(atx aty) (ltl-find orig a)]
                   [(abx aby) (rbl-find orig a)]
                   [(btx bty) (ltl-find orig b)]
                   [(bbx bby) (rbl-find orig b)])
        (let ([da (+ aty (* (- bty aty) n))]
              [dd (- (pict-height orig)
                     (+ aby (* (- bby aby) n)))]
              [orig 
               ;; Generate intermediate last-pict
               (let ([ap (or (pict-last a) a)]
                     [bp (or (pict-last b) b)])
                 (let-values ([(al at) (lt-find orig (if (pair? ap) (cons a ap) (list a ap)))]
                              [(bl bt) (lt-find orig (if (pair? bp) (cons b bp) (list b bp)))]
                              [(ae) (single-pict ap)]
                              [(be) (single-pict bp)])
                   (let ([ar (+ al (pict-width ae))]
                         [ab (+ at (pict-height ae))]
                         [br (+ bl (pict-width be))]
                         [bb (+ bt (pict-height be))])
                     (let ([atl (+ at (pict-ascent ae))]
                           [abl (- ab (pict-descent ae))]
                           [btl (+ bt (pict-ascent be))]
                           [bbl (- bb (pict-descent be))]
                           [btw (lambda (a b)
                                  (+ a (* (- b a) n)))])
                       (let ([t (btw at bt)]
                             [l (btw al bl)])
                         (let ([b (max t (btw ab bb))]
                               [r (max l (btw ar br))])
                           (let ([tl (max t (min (btw atl btl) b))]
                                 [bl (max t (min (btw abl bbl) b))])
                             (let ([p (blank (- r l) (- b t)
                                             (- tl t) (- b bl))])
                               (let ([orig+p (pin-over orig l t p)])
                                 (use-last orig+p p))))))))))])
          (let ([p (make-pict (pict-draw orig)
                              (pict-width orig)
                              (pict-height orig)
                              da
                              dd
                              (list (make-child orig 0 0 1 1 0 0))
                              #f
                              (pict-last orig))])
            (let ([left (+ atx (* (- btx atx) n))]
                  [right (+ abx (* (- bbx abx) n))])
              (let ([hp (inset p
                               (- left)
                               0
                               (- right (pict-width p))
                               0)])
                (let-values ([(atx aty) (lt-find hp a)]
                             [(abx aby) (lb-find hp a)]
                             [(btx bty) (lt-find hp b)]
                             [(bbx bby) (lb-find hp b)])
                  (let ([top (+ aty (* (- bty aty) n))]
                        [bottom (+ aby (* (- bby aby) n))])
                    (inset hp
                           0
                           (- top)
                           0
                           (- bottom (pict-height hp))))))))))])))

(define (fail-gracefully t)
  (with-handlers ([exn:fail? (lambda (x) (values 0 0))])
    (t)))

;; Pin `p' into `base', sliding from `p-from' to `p-to'
;;  (which are picts within `base') as `n' goes from 0.0 to 1.0.
;; The `p-from' and `p-to' picts are typically ghosts of
;;   `p' within `base', but they can be any picts within
;;   `base'. The top-left locations of `p-from' and `p-to'
;;   determine the placement of the top-left of `p'.
(define (slide-pict base p p-from p-to n)
  (let-values ([(x1 y1) (fail-gracefully (lambda () (lt-find base p-from)))]
               [(x2 y2) (fail-gracefully (lambda () (lt-find base p-to)))])
    (pin-over base
              (+ x1 (* (- x2 x1) n))
              (+ y1 (* (- y2 y1) n))
              p)))

(define (slide-pict/center base p p-from p-to n)
  (let-values ([(x1 y1) (fail-gracefully (lambda () (cc-find base p-from)))]
               [(x2 y2) (fail-gracefully (lambda () (cc-find base p-to)))])
    (pin-over base
              (- (+ x1 (* (- x2 x1) n)) (/ (pict-width p) 2))
              (- (+ y1 (* (- y2 y1) n)) (/ (pict-height p) 2))
              p)))

(define (fade-around-pict n base evolved [composite? #t])
  (define tg1 (launder (ghost base)))
  (define tg2 (launder (ghost base)))
  (slide-pict
   (fade-pict n
              tg1
              (evolved tg2)
              composite?)
   base
   tg1
   tg2
   n))

;; Concatenate a sequence of animations
(define (sequence-animations . l)
  (let ([len (length l)])
    (lambda (n)
      (cond
       [(zero? n)
        ((car l) 0.0)]
       [(= n 1.0)
        ((list-ref l (sub1 len)) n)]
       [else
        (let ([pos (inexact->exact (floor (* n len)))])
          ((list-ref l pos) (* len (- n (* pos (/ len))))))]))))

;; Reverse a sequence of animations
(define (reverse-animations . l)
  (let ([s (apply sequence-animations l)])
    (lambda (n)
      (s (- 1 n)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [0,1] -> [0,1] functions

(define (fast-start n)
  (- 1 (* (- 1 n) (- 1 n))))

(define (fast-end n)
  (* n n))

(define (fast-edges n)
  (if (n . < . 0.5)
      (- 0.5 (fast-middle (- 0.5 n)))
      (+ 0.5 (fast-middle (- n 0.5)))))

(define (fast-middle n)
  (- 0.5 (/ (cos (* n pi)) 2)))

(define (split-phase opt-n)
  (values (* 2 (min opt-n 0.5))
          (* 2 (- (max opt-n 0.5) 0.5))))

;;;
;;; Standard Fish
;;;

#;(define standard-fish 
  (lambda (w h [direction 'left] [c "blue"] [ec #f] [mouth-open #f])
    (define (ensure-color who value)
      (cond
        [(color? value)      value]
        [(rgb-color? value) (color value)]
        [(string? value)    (make-color value)]
        [(list? value)      (apply make-color value)]
        [else
         (error who "expected a color, got: ~a" value)]))

    (define (scale-color factor col)
      (define normalized (ensure-color 'standard-fish col))
      (define raw        (color-value normalized))
      (cond
        [(rgb-color? raw)
         (define (scale-channel v)
           (define scaled  (* (exact->inexact v) factor))
           (define clipped (max 0 (min 255 scaled)))
           (inexact->exact (round clipped)))
         (color (rgb-color (scale-channel (rgb-color-r raw))
                           (scale-channel (rgb-color-g raw))
                           (scale-channel (rgb-color-b raw))
                           (rgb-color-a raw)))]
        [else normalized]))

    (unless (memq direction '(left right))
      (error 'standard-fish "expected direction 'left or 'right, got: ~a" direction))

    (define base-color        (ensure-color 'standard-fish c))
    (define outline-color     (scale-color 0.7  base-color))
    (define fin-color         (scale-color 1.1  base-color))
    (define tail-color        (scale-color 0.85 base-color))
    (define body-color-str    (color->string base-color))
    (define outline-color-str (color->string outline-color))
    (define fin-color-str     (color->string fin-color))
    (define tail-color-str    (color->string tail-color))

    (js-log (list body-color-str outline-color-str fin-color-str tail-color-str))

    (define eye-spec
      (cond
        [(eq? ec 'x) 'x]
        [(not ec)    #f]
        [else        (ensure-color 'standard-fish ec)]))

    (define (clamp-01 v)
      (cond
        [(<= v 0.0) 0.0]
        [(>= v 1.0) 1.0]
        [else v]))

    (define mouth-open-amt
      (cond
        [(number? mouth-open) (clamp-01 (exact->inexact mouth-open))]
        [mouth-open           1.0]
        [else                 0.0]))

    (define mouth-open? (> mouth-open-amt 0.0))
    
    (dc (lambda (dc x y)
          ; (define lw (max 1.0 (/ (max (exact->inexact w) (exact->inexact h)) 60.0)))
          (define lw            3.0)
          (define body-rx       (* w 0.45))
          (define body-ry       (* h 0.42))
          (define body-cx       body-rx)
          (define body-cy       (* h 0.5))
          (define tail-base-x   (* w 0.7))
          (define tail-top-y    (* h 0.15))
          (define tail-bottom-y (* h 0.85))
          (define fin-top-y     (* h 0.18))
          (define fin-bottom-y  (* h 0.82))
          (define mouth-x       (* w 0.05))
          (define mouth-length  (* w 0.18))
          (define mouth-span    (* h 0.3 mouth-open-amt))
          (define eye-radius    (* (min w h) 0.075))
          (define eye-cx        (* w 0.27))
          (define eye-cy        (* h 0.35))
          
          (dc 'save)
          (dc 'translate x y)
          (when (eq? direction 'right)
            (dc 'translate w 0)
            (dc 'scale -1 1))

          ;; Tail
          (dc 'begin-path)
          (dc 'move-to tail-base-x tail-top-y)
          (dc 'line-to w (* h 0.5))
          (dc 'line-to tail-base-x tail-bottom-y)
          (dc 'close-path)
          (dc 'fill-style tail-color-str)
          (dc 'fill)
          (dc 'stroke-style outline-color-str)
          (dc 'line-width lw)
          (dc 'stroke)

          ;; Fins (top and bottom)
          (define fin-width  (* w 0.22))
          (define fin-offset (* w 0.45))
          (define (draw-fin top?)
            (define base-y (if top? fin-top-y fin-bottom-y))
            (define tip-y  (if top? 0 h))
            (dc 'begin-path)
            (dc 'move-to (- fin-offset (* fin-width 0.6)) base-y)
            (dc 'line-to (+ fin-offset (* fin-width 0.4)) tip-y)
            (dc 'line-to (+ fin-offset (* fin-width 0.8)) base-y)
            (dc 'close-path)
            (dc 'fill-style fin-color-str)
            (dc 'fill)
            (dc 'stroke-style outline-color-str)
            (dc 'line-width lw)
            (dc 'stroke))
          (draw-fin #t)
          (draw-fin #f)

          ;; Body
          (dc 'begin-path)
          (dc 'ellipse body-cx body-cy body-rx body-ry 0 0 (* 2. pi))
          (dc 'fill-style body-color-str)
          (dc 'fill)
          (dc 'stroke-style outline-color-str)
          (dc 'line-width lw)
          (dc 'stroke)

          ;; Accent stripes
          (for-each
           (lambda (offset)
             (dc 'begin-path)
             (dc 'move-to (+ (* w 0.38) offset) (* h 0.25))
             (dc 'line-to (+ (* w 0.32) offset) (* h 0.75))
             (dc 'stroke-style outline-color-str)
             (dc 'line-width (/ lw 1.4))
             (dc 'stroke))
           (list 0.0 (* w 0.07) (* w 0.14)))

          ;; Mouth
          (dc 'stroke-style outline-color-str)
          (dc 'line-width (/ lw 1.2))
          (if mouth-open?
              (begin
                (dc 'begin-path)
                (dc 'move-to mouth-x (- (* h 0.5) mouth-span))
                (dc 'line-to (+ mouth-x mouth-length) (* h 0.5))
                (dc 'line-to mouth-x (+ (* h 0.5) mouth-span))
                (dc 'close-path)
                (dc 'fill-style "white")
                (dc 'fill)
                (dc 'stroke))
              (begin
                (dc 'begin-path)
                (dc 'move-to mouth-x (* h 0.5))
                (dc 'line-to (+ mouth-x mouth-length) (* h 0.5))
                (dc 'stroke)))

          ;; Eye
          #;(when eye-spec
            (if (eq? eye-spec 'x)
                (let ([diag (* eye-radius 0.9)])
                  (dc 'begin-path)
                  (dc 'move-to (- eye-cx diag) (- eye-cy diag))
                  (dc 'line-to (+ eye-cx diag) (+ eye-cy diag))
                  (dc 'move-to (- eye-cx diag) (+ eye-cy diag))
                  (dc 'line-to (+ eye-cx diag) (- eye-cy diag))
                  (dc 'stroke-style outline-color-str)
                  (dc 'line-width (/ lw 1.1))
                  (dc 'stroke))
                (begin
                  (dc 'begin-path)
                  (dc 'ellipse eye-cx eye-cy (* eye-radius 1.1) (* eye-radius 1.1) 0 0 (* 2. pi))
                  (dc 'fill-style (color->string eye-spec))
                  (dc 'fill)
                  (dc 'stroke-style outline-color-str)
                  (dc 'line-width (/ lw 1.3))
                  (dc 'stroke)
                  (dc 'begin-path)
                  (dc 'ellipse (+ eye-cx (* eye-radius 0.25))
                      (- eye-cy (* eye-radius 0.2))
                      (* eye-radius 0.35)
                      (* eye-radius 0.35)
                      0 0 (* 2. pi))
                  (dc 'fill-style outline-color-str)
                  (dc 'fill))))

          (dc 'restore))
        w h)))



;;;
;;; Stop converting here
;;;


;;;
;;; Explainers
;;;

(define (explain p         
                 #;#:border     [border     "Firebrick"]
                 #;#:ascent     [ascent     "MediumSeaGreen"]
                 #;#:baseline   [baseline   "DodgerBlue"]
                 #;#:scale      [scale      5]
                 #;#:line-width [line-width 1])
  (define b  border)
  (define a  ascent)
  (define d  baseline)
  (define s  scale)
  (define lw line-width)  
  (explain-child* p p b a d s lw))

(define (explain-child
         p
         ; #;#:border     [border     "Firebrick"]
         ; #;#:ascent     [ascent     "MediumSeaGreen"]
         ; #;#:baseline   [baseline   "DodgerBlue"]
         ; #;#:scale      [scale      5]
         ; #;#:line-width [line-width 1])
         . child-path)
  (define b "Firebrick")
  (define a "MediumSeaGreen")
  (define d "DodgerBlue")
  (define s 5)
  (define lw 1)
  
  (scale
   (for/fold ([p p])
             ([c (in-list child-path)])
     (explain-child* p c b a d 1 lw))
   s))

(define (explain-child*
         p
         child-path
         b a d s lw)
  (define t     (get-child-transformation p child-path))
  (define child (last (flatten child-path)))
  (define cw    (pict-width child))
  (define ch    (pict-height child))
  (define nw    (+ (* 2 lw) (pict-width p)))
  (define nh    (+ (* 2 lw) (pict-height p)))
  (define ncw   (+ (* 2 lw) cw))
  (define nch   (+ (* 2 lw) ch))
  (define lw/2  (/ lw 2))

  (define annotations
    (dc
     (lambda (dc dx dy)
       (define oldt (send dc get-transformation))
       (define p    (send dc get-pen))
       (define br   (send dc get-brush))
       (dc 'fill-style "rgb(255 255 255 / 0% )") ; white, fully transparent
       (dc 'translate dx dy)
       (dc 'transform t)
       (when b
         (define t2 (send dc get-transformation))
         (send dc scale (/ ncw cw) (/ nch ch))
         (define path (new dc-path%))
         (send dc set-pen b lw 'solid)
         (send path move-to lw/2 lw/2)
         (send path line-to lw/2 (- ch lw/2))
         (send path line-to (- cw lw/2) (- ch lw/2))
         (send path line-to (- cw lw/2) lw/2)
         (send path close)
         (send dc draw-path path 0 0)
         (send dc set-transformation t2))
       (when a
         (define line (pict-ascent child))
         (send dc set-pen a lw 'solid)
         (send dc draw-line lw/2 line
               (+ lw lw/2 cw) line))
       (when d
         (define line (- (pict-height child) (pict-descent child)))
         (send dc set-pen d lw 'solid)
         (send dc draw-line lw/2 line
               (+ lw lw/2 cw) line))
       (send dc set-transformation oldt)
       (send dc set-pen p)
       (send dc set-brush br))
     nw nh))
  
  (scale
   (cc-superimpose
    p
    annotations)
   s))

;;;
;;;
;;;


(define prog-picture dc)


(define (memq* a l)
  (if (pair? l)
      (or (eq? (car l) a)
          (memq* a (cdr l)))
      #f))

(define text
  (case-lambda
    [(string)                  (text string '() the-default-text-size)]
    [(string style/size)       (if (number? style/size)
                                   (text string '()        style/size            0)
                                   (text string style/size the-default-text-size 0))]
    [(string style size)       (text string style size 0)]
    [(string style size angle) (not-caps-text string style size angle)
                               ; todo: we need to measure the size of the text
                               ;       also we need to a way to represent fonts.

                               #;(if (il-memq 'caps style)
                                     (begin
                                       (unless (zero? angle) 
                                         (error 'text "the style cannot include 'caps with a non-zero angle"))
                                       (caps-text str (il-remq 'caps style) size))
                                     (not-caps-text str style size angle))]))


(define (il-memq sym s)
  (and (pair? s)
       (or (eq? sym (car s))
           (il-memq sym (cdr s)))))

(define (il-remq sym s)
  (if (pair? s)
      (if (eq? sym (car s))
          (cdr s)
          (cons (car s) (il-remq sym (cdr s))))
      s))

(define the-dc-used-for-text-size #f)

(define (dc-for-text-size)
  (cond
    [the-dc-used-for-text-size the-dc-used-for-text-size]
    [else
     (define canvas (js-create-element "canvas"))
     (js-set-canvas-width!  canvas 1024)
     (js-set-canvas-height! canvas 1024)
     (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))
     (define dc (canvas-context->dc ctx))
     dc]))

(define (not-caps-text string orig-style size angle)
  ; Notes:
  ;   strokeText - draws text outline directly
  ;   fillText   - draws test filled  directly
  ; They accept a string to draw and an x,y.
  ; Use ctx.measureText(text)
  ; then the relevant properties are:
  ;   - actualBoundingBoxLeft
  ;   - actualBoundingBoxRight
  ;   - actualBoundingBoxAscent
  ;   - actualBoundingBoxDescent

  ; Note: We need the font at pict creation time in order
  ;       to measure the bounding box of the text.
  ;       That is, we can't get the font from `ctx` since
  ;       the drawing context isn't available until 
  ;       render time.  
  (let ([font
         ; Find font based on `orig-style`.
         (let loop ([style orig-style])
           (cond
             [(null? style) ; null means default
              (font 'sans-serif (or size the-default-text-size) 'normal 'normal 'normal #f)]
             [(font? style)
              font]
             [(memq style families)
              (font style (or size 24) 'normal 'normal 'normal #f)]
             [(string? style)
              style] ; assume it is a legal css font name
             ; TODO
             #;[(and (pair? style)
                     (string? (car style))
                     (memq (cdr style) families))              
                (send the-font-list find-or-create-font
                      size (car style) (cdr style) 'normal 'normal #f 'default #t 'unaligned)]
             ; todo
             #;[(and (pair? style)
                     (or (memq (car style)
                               '(superscript
                                 subscript
                                 large-script
                                 bold italic
                                 aligned unaligned))
                         (and (pair? (car style))
                              (eq? (caar style) 'weight))))
                
                (let ([font (loop (cdr style))]
                      [style (car style)])
                  (cond
                    [(eq? style 'bold)
                     (extend-font font
                                  (send font get-point-size)
                                  (send font get-style)
                                  'bold
                                  (send font get-hinting))]
                    [(and (pair? style)
                          (eq? (car style) 'weight))
                     (extend-font font
                                  (send font get-point-size)
                                  (send font get-style)
                                  (cdr style)
                                  (send font get-hinting))]
                    [(eq? style 'italic)
                     (extend-font font
                                  (send font get-point-size)
                                  'italic
                                  (send font get-weight)
                                  (send font get-hinting))]
                    [(or (eq? style 'aligned)
                         (eq? style 'unaligned))
                     (extend-font font
                                  (send font get-point-size)
                                  (send font get-style)
                                  (send font get-weight)
                                  style)]
                    [else font]))]
             #;[(and (pair? style)
                     (memq (car style) '(combine no-combine outline)))
                (loop (cdr style))]
             #;[(and (pair? style)
                     (is-a? (car style) color%))
                (loop (cdr style))]
             [else (raise-type-error 'text "style" orig-style)]))]
        ; combine? (i.e. use kerning or ligatures?)
        #;[combine?      (let loop ([style orig-style])
                           (cond
                             [(eq? style 'modern)           #f]
                             [(not (pair? style))           #t]
                             [(eq? (car style) 'combine)    #t]
                             [(eq? (car style) 'no-combine) #f]
                             [else
                              (loop (cdr style))]))]
        #;[sub?          (memq* 'subscript    orig-style)]
        #;[sup?          (memq* 'superscript  orig-style)]
        #;[large-script? (memq* 'large-script orig-style)]
        #;[outline?      (memq* 'outline      orig-style)]
        #;[color         (let loop ([style orig-style])
                           (cond
                             [(not (pair? style)) #f]
                             [(is-a? (car style) color%) 
                              (resolve-color (car style))]
                             [else (loop (cdr style))]))])
    (let (#;[s-font (if (or sub? sup?)
                        (extend-font font
                                     (floor (* (if large-script?
                                                   85/100
                                                   6/10)
                                               (send font get-point-size)))
                                     (send font get-style)
                                     (send font get-weight)
                                     (send font get-hinting))
                        font)]
          )      
      (let ([dc (dc-for-text-size)])
        (let-values
            ([(w h   ; width, height
                 d   ; distance from base line to bottom of descender
                 s   ; extra vertical space (included in height, often zero)
                 as  ; ascent
                 de) ; descent
              (let ()
                ; 1. measure text
                (define old-font (dc 'font))
                (dc 'font (font->string font))
                (define angle? (and angle (not (zero? angle))))
                (define m #f)
                (cond
                  [angle?
                   (dc 'rotate (- angle))
                   (set! m (dc 'measure-text string))
                   (dc 'rotate angle)]
                  [else
                   (set! m (dc 'measure-text string))])
                (dc 'font old-font)

                ; get these values from m:
                ;   - actualBoundingBoxLeft
                ;   - actualBoundingBoxRight
                ;   - actualBoundingBoxAscent
                ;   - actualBoundingBoxDescent
                (define (metric field [default 0.0])
                  (let ([v (js-ref m field)])
                    (cond
                      [(real? v) (if (inexact? v) v (exact->inexact v))]
                      [else      default])))

                ;; (define left    (metric "actualBoundingBoxLeft"))
                ;; (define right   (metric "actualBoundingBoxRight"))
                ;; (define ascent  (metric "actualBoundingBoxAscent"))
                ;; (define descent (metric "actualBoundingBoxDescent"))

                (define left    (metric "actualBoundingBoxLeft"))
                (define right   (metric "actualBoundingBoxRight"))
                (define ascent  (metric "fontBoundingBoxAscent"))
                (define descent (metric "fontBoundingBoxDescent"))
                
                ; (define width   (abs (- right left)))
                (define width   (metric "width"))
                #;(define width   (let ([w (metric "width")])
                                    (if (positive? (+ left right))
                                        (max w (+ left right))
                                        w)))
                (define height  (+ ascent descent))
                (values width height descent 0.0 ascent descent))])

          (define (make-draw adj-x adj-y angle)
            ; Notes:
            ;  The methods `strokeText` and `fillText` uses (x,y)
            ;  to as the point where the baseline starts.
            ;  The `pict` constructor `text` on the other hand uses (x,y)
            ;  as the upper, left point of the text.            
            
            ; todo get path from text
            #;(define p 
                (and outline?
                     (let ([p (new dc-path%)])
                       (send p text-outline
                             s-font string 0 0 combine?)
                       (unless (zero? angle)
                         (send p rotate angle))
                       p)))
            
            (lambda (dc x y)
              (let ([old-font (dc 'font)])
                (define dest-x    (adj-x x))
                (define dest-y (+ (adj-y y) as))
                ; TODO: check for outline? here
                (dc 'font (font->string font))
                (define angle? (and angle (not (zero? angle))))
                (cond
                  [angle?
                   (dc 'translate dest-x dest-y)
                   (dc 'rotate (- angle))
                   (dc 'fill-text string 0 0)
                   (dc 'rotate angle)
                   (dc 'translate (- dest-x) (- dest-y))]
                  [else
                   (dc 'fill-text string dest-x dest-y)])
                (dc 'font old-font))))
          
          #;(let ([f (send dc get-font)])
              (define dest-x (adj-x x))
              (define dest-y (adj-y y))
              (cond
                [outline?
                 (define pn (and color (send dc get-pen)))
                 (when color (send dc set-pen color (send pn get-width) (send pn get-style)))
                 (send dc draw-path p dest-x dest-y)
                 (when color (send dc set-pen pn))]
                [else
                 (define fg (and color (send dc get-text-foreground)))
                 (when color (send dc set-text-foreground color))
                 (send dc set-font s-font)
                 (send dc draw-text string
                       dest-x dest-y
                       combine? 0 angle)
                 (when fg (send dc set-text-foreground fg))
                 (send dc set-font f)]))
          
          (cond
            ;; Normal case: no rotation
            [(zero? angle)
             (prog-picture (make-draw (lambda (x) x)
                                      (lambda (y) y)
                                      angle)
                           w h (- h d) d)]
            [else
             ;; Rotation case. Need to find the bounding box.
             ;; Calculate the four corners, relative to top left as origin:
             (let* ([tlx 0]
                    [tly 0]
                    [ca  (cos angle)]
                    [sa  (sin angle)]
                    [trx        (* w ca)]
                    [try     (- (* w sa))]
                    [brx (+ trx (* h sa))]
                    [bry (- try (* h ca))]
                    [blx        (* h sa)]
                    [bly     (- (* h ca))]
                    ;;min-x and min-y must be non-positive,
                    ;; since tlx and tly are always 0
                    [min-x (min tlx trx blx brx)]
                    [min-y (min tly try bly bry)])
               (let ([pw (- (max tlx trx blx brx) min-x)]
                     [ph (- (max tly try bly bry) min-y)]
                     [dx (cond
                           [(and (positive? ca) (positive? sa)) 0]
                           [(positive? ca)                         (- (* h sa))]
                           [(positive? sa)                         (- (* w ca))]
                           [else                                (+ (- (* w ca))
                                                                   (- (* h sa)))])]
                     [dy (cond
                           [(and (positive? ca) (negative? sa)) 0]
                           [(positive? ca)                            (* w sa)]
                           [(negative? sa)                         (- (* h ca))]
                           [else                                (+ (- (* h ca))
                                                                   (* w sa))])])
                 (prog-picture (make-draw (lambda (x) (+ x dx))
                                          (lambda (y) (+ y dy))
                                          angle)
                               pw ph ph 0)))])

          #;(if (or sub? sup?)
                (let-values ([(ww wh wd ws) (with-text-scale
                                              dc
                                              (lambda ()
                                                (send dc get-text-extent "Wy" font)))])
                  ; note: prog-picture = (dc draw w h [a d])
                  (prog-picture (make-draw
                                 (lambda (x) x)
                                 (lambda (y) (if sub?
                                                 (+ y
                                                    (if large-script?
                                                        (+ (* (- wh wd ws) 4/10)
                                                           (- ws s))
                                                        (- wh h)))
                                                 (+ y
                                                    (if large-script?
                                                        (* (- wh wd ws) -3/10)
                                                        0))))
                                 0)
                                w wh (- wh wd) wd))
                (if (zero? angle)
                    ;; Normal case: no rotation
                    (prog-picture (make-draw (lambda (x) x)
                                             (lambda (y) y)
                                             0)
                                  w h (- h d) d)
                    ;; Rotation case. Need to find the bounding box.
                    ;; Calculate the four corners, relative to top left as origin:
                    (let* ([tlx 0]
                           [tly 0]
                           [ca (cos angle)]
                           [sa (sin angle)]
                           [trx (* w ca)]
                           [try (- (* w sa))]
                           [brx (+ trx (* h sa))]
                           [bry (- try (* h ca))]
                           [blx (* h sa)]
                           [bly (- (* h ca))]
                           ;;min-x and min-y must be non-positive,
                           ;; since tlx and tly are always 0
                           [min-x (min tlx trx blx brx)]
                           [min-y (min tly try bly bry)])
                      (let ([pw (- (max tlx trx blx brx) min-x)]
                            [ph (- (max tly try bly bry) min-y)]
                            [dx (cond
                                  [(and (positive? ca) (positive? sa)) 0]
                                  [(positive? ca) (- (* h sa))]
                                  [(positive? sa) (- (* w ca))]
                                  [else (+ (- (* w ca)) (- (* h sa)))])]
                            [dy (cond
                                  [(and (positive? ca) (negative? sa)) 0]
                                  [(positive? ca) (* w sa)]
                                  [(negative? sa) (- (* h ca))]
                                  [else (+ (- (* h ca)) (* w sa))])])
                        (prog-picture (make-draw (lambda (x) (+ x dx))
                                                 (lambda (y) (+ y dy))
                                                 angle)
                                      pw ph ph 0))))))))))





(define (render dc h+top l dx dy)
  (define b&w? #f)
  
  (let loop ([dx dx] [dy dy] [l l])   ; l is a list of commands
    (unless (null? l)
      ; first command
      (let ([x (car l)]) ; x is the first command
        (js-log (format "render: ~a" x))        
        (if (string? x)
            (error 'draw-pict "how did a string get here?: ~s" x)
            (case (car x)
              [(offset) (loop (+ dx (cadr x))
                              (+ dy (caddr x))
                              (cadddr x))]
              ; The format of the old LaTeX line command is:
              ;     (line x_run y_rise travel)
              ; or  (line dx dy #f)

              ; The pair (x_run, y_rise) gives the slope and travel is delta-x.
              [(line vector)
               
               (let ([xs  (cadr x)]
                     [ys  (caddr x)]
                     [len (cadddr x)])
                 (let ([mx  (if len (abs (if (zero? xs) ys xs)) 1)]
                       [len (or len 1)])
                   (let ([x0 dx]
                         [y0 (- h+top dy)]
                         [x1 (+ dx (* (/ xs mx) len))]
                         [y1 (- h+top (+ dy (* (/ ys mx) len)))])
                     (case (car x)
                       [(line)   (begin
                                   (dc 'begin-path)
                                   (dc 'move-to x0 y0)
                                   (dc 'line-to x1 y1)
                                   (dc 'stroke))]
                       [(vector) (begin ; todo - add arrow
                                   (dc 'begin-path)
                                   (dc 'move-to x0 y0)
                                   (dc 'line-to x1 y1)
                                   (dc 'stroke))]))))]
              
              [(circle circle*)
               (let ([size (cadr x)])
                 (dc 'ellipse (- dx (/ size 2)) (- h+top dy (/ size 2))
                     size size))]
              [(oval) ; unfilled rounded rectangle
               ; This a rectangle with rounded corners
               ; roundRect(x, y, width, height, radii)
               (let ([w (cadr  x)]
                     [h (caddr x)])
                 (let ([rx (-       dx (/ w 2))]
                       [ry (- h+top dy (/ h 2))])
                   (dc 'roundRect dx dy w h 5)))] ; todo - what should go here instead of 5?

                 ;; (send cr set-rectangle
                 ;;       (+ rx (* l (cadr x)))
                 ;;       (+ ry (* t (caddr x)))
                 ;;       (* (- r l) (cadr x))
                 ;;       (* (- b t) (caddr x)))
               
                 ;; (let ([b  (get-brush)]
                 ;;       [rx (- dx (/ (cadr x) 2))]
                 ;;       [ry (- h+top dy (/ (caddr x) 2))])
                 ;;   (set-brush (find-or-create-brush "BLACK" 'transparent))
                 ;;   (let ([part (cadddr x)]
                 ;;         [cr (send dc get-clipping-region)]
                 ;;         [set-rect (lambda (l t r b)
                 ;;                     (let ([cr (make-object region% dc)])
                 ;;                       (send cr set-rectangle
                 ;;                             (+ rx (* l (cadr x)))
                 ;;                             (+ ry (* t (caddr x)))
                 ;;                             (* (- r l) (cadr x))
                 ;;                             (* (- b t) (caddr x)))
                 ;;                       cr))])
                 ;;     (send dc set-clipping-region
                 ;;           (cond
                 ;;             [(string=? part "[l]")
                 ;;              (set-rect 0 0 0.5 1.0)]
                 ;;             [(string=? part "[tl]")
                 ;;              (set-rect 0 0 0.5 0.5)]
                 ;;             [(string=? part "[tr]")
                 ;;              (set-rect 0.5 0 1.0 0.5)]
                 ;;             [(string=? part "[r]")
                 ;;              (set-rect 0.5 0 1.0 1.0)]
                 ;;             [(string=? part "[bl]")
                 ;;              (set-rect 0 0.5 0.5 1.0)]
                 ;;             [(string=? part "[br]")
                 ;;              (set-rect 0.5 0.5 1.0 1.0)]
                 ;;             [else cr]))
                 ;;     (send dc draw-rounded-rectangle
                 ;;           rx ry
                 ;;           (cadr x) (caddr x)
                 ;;           (if (string=? part "") -0.2 -0.5))
                 ;;     (send dc set-clipping-region cr)
                 ;;     (set-brush b)))]
              [(bezier)
               ; note:  [num] (x0,y0) (x1,y1) (x2,y2)
               ;   num is the number of points to compute
               ;   curve starts in (x0,y0) and ends in (x2,y2)
               ;   qbezier = quadratic bezier
               (let ([x1 (list-ref x 1)]
                     [y1 (list-ref x 2)]
                     [x2 (list-ref x 3)]
                     [y2 (list-ref x 4)]
                     [x3 (list-ref x 5)]
                     [y3 (list-ref x 6)])
                 (dc 'begin-path)
                 (dc 'move-to (+ dx x1) (- h+top (+ dy y1)))
                 (dc 'quadratic-curve-to 
                     (+ dx x2) (- h+top (+ dy y2))
                     (+ dx x3) (- h+top (+ dy y3)))
                 (dc 'stroke))]
              [(with-color)
               (js-log "render: with-color")
               (if b&w?
                   (loop dx dy (caddr x))
                   (let ([c1 (cadr x)]) ; todo rename back to c and have the define this leads to a problem
                     (js-log (format "1.  ~a" c))        ; was 0
                     (js-log (format "1.5 ~a" (cadr x))) ; was/should be "red"
                     (define c (if (string? c1) c1 (color->string c1)))
                     (let ([old-stroke     (dc 'stroke-style)]
                           [old-fill       (dc 'fill-style)])
                       ; we set
                       ;  1) stroke color
                       ;  2) brush to solid color
                       ;  3) text color
                       (dc 'stroke-style c)
                       (dc 'fill-style   c)
                       (loop dx dy (caddr x))
                       ; reset colors
                       (dc 'stroke-style old-stroke)
                       (dc 'fill-style   old-fill))))]
              [(with-thickness)
               (let ([w (second x)])
                 (let ([old-w (dc 'line-width)])
                   (dc 'line-width w)
                   (loop dx dy (caddr x))
                   (dc 'line-width old-w)))]
              [(prog)
               ((cadr x) dc dx (- h+top dy (caddr x)))]
              [else (error 'render "unknown command: ~a\n" x)])))
      ; remaining commands
      (loop dx dy (cdr l)))))

;;;
;;; https://github.com/racket/pict/blob/master/pict-lib/pict/private/utils.rkt
;;;

; (re-pict 

(define (re-pict box naya)
  (let ([w (pict-width   box)]
        [h (pict-height  box)]
        [d (pict-descent box)]
        [a (pict-ascent  box)])
    (make-pict (pict-draw naya)
               w h
               a d
               (list (make-child box 0 0 1 1 0 0))
               #f
               (pict-last box))))

(define (re-pict box naya)
    (let ([w (pict-width box)]
	  [h (pict-height box)]
	  [d (pict-descent box)]
	  [a (pict-ascent box)])
      (make-pict (pict-draw naya)
		 w h
		 a d
		 (list (make-child box 0 0 1 1 0 0))
		 #f
                 (pict-last box))))
  
(define cons-colorized-picture
  (lambda (p color cmds)
    (re-pict
     p
     (cc-superimpose
      p
      (colorize
       (cons-picture
        (ghost (launder p))
        cmds)
       color)))))

(define (round-frame p radius)
  (re-pict
   p
   (cc-superimpose
    p
    (let ([w (pict-width p)]
          [h (pict-height p)])
      (dc (lambda (dc2 x y)
            (dc2 'begin-path)
            (dc2 'round-rect x y w h radius)
            (dc2 'stroke))
          (pict-width p) (pict-height p))))))



;; FIXME: abstract common part of color-frame, etc.

(define color-frame
  (case-lambda
    [(p color w)
     (re-pict
      p
      (cc-superimpose
       p
       (let ([p2 (colorize (frame (ghost (launder p))) color)])
         (if w
             (linewidth w p2)
             p2))))]
    [(p color) (color-frame p color #f)]))

(define color-round-frame
  (case-lambda
    [(p radius color)   (color-round-frame p radius color #f)]
    [(p radius color w)
     (re-pict
      p
      (cc-superimpose
       p
       (let ([p2 (colorize (round-frame (ghost (launder p)) radius) color)])
         (if w
             (linewidth w p2)
             p2))))]
    ))  

(define color-dash-frame
  (case-lambda
    [(p seg-length color w)
     (re-pict
      p
      (cc-superimpose
       p
       (let ([p2 (colorize (dash-frame (ghost (launder p)) seg-length) color)])
         (if w
             (linewidth w p2)
             p2))))]
    [(p seg-length color) (color-dash-frame p seg-length color #f)]))  

;;;
;;; Rendering
;;;

#;(define (pict->bitmap p
                      [smoothing   'aligned]
                      [make-bitmap make-bitmap])
  (define w  (pict-width p))
  (define h  (pict-height p))
  (define bm (make-bitmap (max 1 (inexact->exact (ceiling w)))
                          (max 1 (inexact->exact (ceiling h)))))
  (unless (send bm ok?)
    (error 'pict->bitmap
           (string-append "bitmap creation failed\n"
                          "  possible reason: out of memory\n"
                          "  pict width: ~a\n"
                          "  pict height: ~a")
           w h))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-smoothing smoothing)
  (draw-pict p dc 0 0)
  bm)

(define (make-bitmap w h)
  (alpha-offscreen-canvas w h))

(define (pict->bitmap p
                      [smoothing   'aligned]
                      [make-bitmap make-bitmap])
  (define (normalize-smoothing style)
    (cond
      [(boolean? style)                   style]
      [(memq style '(smoothed unaligned)) #t]
      [(memq style '(aligned unsmoothed)) #f]
      [else (error 'pict->bitmap "unknown smoothing option: ~a" style)]))
  (define width  (max 1 (inexact->exact (ceiling (pict-width p)))))
  (define height (max 1 (inexact->exact (ceiling (pict-height p)))))
  (define bm     (make-bitmap width height))
  (unless (external? bm)
    (error 'pict->bitmap "expected make-bitmap to produce a canvas, got: ~a" bm))
  (define ctx (js-canvas-get-context bm "2d" (js-undefined)))
  (define dc  (canvas-context->dc ctx))
  (dc 'image-smoothing-enabled (normalize-smoothing smoothing))
  (dc 'clear-rect 0 0 width height)
  (draw-pict p dc 0 0)
  bm)


;;;
;;; Bitmaps
;;;

(define bitmap-urls
  (list "https://i.imgur.com/dI22MrB.png"  ; Nemo fish
        "https://i.imgur.com/BRJ0UpN.png"  ; Tropical fish
        ))

(define loaded-bitmaps  (make-hash))
(define bitmaps-to-load (length bitmap-urls))
(define bitmaps-loaded  0)
(define main-started?   #f)

(define (bitmap-ref url [default #f])
  (hash-ref loaded-bitmaps url (λ () default)))

(define (register-bitmap! url image)
  (hash-set! loaded-bitmaps url image))

(define (maybe-start-main)
  (when (and (not main-started?) (= bitmaps-loaded bitmaps-to-load))
    (set! main-started? #t)
    (start-main)))

(define ((bitmap-ready url image) . _)
  (register-bitmap! url image)
  (set! bitmaps-loaded (add1 bitmaps-loaded))
  (maybe-start-main))

(define ((bitmap-error url) . _)
  (js-log (format "Failed to load bitmap: ~a" url))
  (set! bitmaps-loaded (add1 bitmaps-loaded))
  (maybe-start-main))

(define (register-bitmap-events! image url)
  (define ready-callback  (procedure->external (bitmap-ready url image)))
  (define error-callback  (procedure->external (bitmap-error url)))
  (js-add-event-listener! image "load"  ready-callback)
  (js-add-event-listener! image "error" error-callback))

(define (load-bitmap url)
  (define image (js-create-element "img"))
  (js-set-attribute! image "crossorigin" "anonymous")
  (js-set-attribute! image "style" "display:none")
  (register-bitmap-events! image url)
  (js-set-attribute! image "src" url)
  (js-append-child! (js-document-body) image))

(define (load-bitmaps)
  (for-each load-bitmap bitmap-urls))


;;;
;;; Example
;;;

(define (main)
  (define canvas (js-create-element "canvas"))
  (js-set-canvas-width!  canvas 1024)
  (js-set-canvas-height! canvas 1024)
  (js-append-child! (js-document-body) canvas)

  (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))  
  (define dc  (canvas-context->dc ctx))

  (define nemo-image     (bitmap-ref (first  bitmap-urls)))
  (define tropical-image (bitmap-ref (second bitmap-urls)))

  (displayln 
   (with-handlers ([(λ _ #t) (λ (e) e)])

     (draw-pict (scale (bitmap tropical-image) 0.5)
                dc 800 100)

     (draw-pict (flip-x (scale (bitmap tropical-image) 0.5))
                dc 800 475)

     (draw-pict (flip-x (flip-x (scale (bitmap tropical-image) 0.5)))
                dc 800 700)


     (draw-pict (flip-y (scale (bitmap tropical-image) 0.2))
                dc 100 900)

     (draw-pict (flip-y (flip-y (scale (bitmap tropical-image) 0.2)))
                dc 200 900)

     (draw-pict (shear (scale (bitmap tropical-image) 0.2) -0.5 0.3)
                dc 300 900)

     (draw-pict (rotate (frame (scale (bitmap tropical-image) 0.2)) (/ 3.1415 4))                 
                 dc 525 800)
     
     #;(draw-pict (hline 40 30) dc 200 200)
     #;(draw-pict (linewidth 10 (hline 40 30)) dc 200 200)
     #;(draw-pict (colorize (hline 40 30) "blue")
                  dc 200 200)
     
     #;(draw-pict (colorize (linewidth 10 (hline 40 30))
                            (make-color "blue"))
                  dc 200 200)

     #;(draw-pict (colorize (linewidth 10 (dash-vline 5 40 5) ) "blue")
                  dc 200 200)
     #;(draw-pict (text "Hello World")
                  dc 200 200)
     #;(draw-pict (frame (text "Hello World"))
                  dc 200 200)
     (draw-pict (frame (text "Hello World" '() 24 (/ 3.14 4)))
                dc 200 200)

     (draw-pict (scale (frame (text "Hello" '() 16 (/ 3.14 8))) 2)
                dc 600 200)

     (draw-pict (scale (frame (text "Hello" '() 16)) 2)
                dc 700 200)

     (draw-pict (hc-append (frame (blank 30))  (frame (blank 60)))
                dc 300 300)

     (draw-pict (cc-superimpose (frame (blank 30)) (frame (blank 60)))
                dc 400 200)

     
     (draw-pict (table 4
                       (map (λ (x) (text (format "~a" x)))
                            (list 1 2 3 4
                                  5 6 7 8
                                  9000 10 11 12))
                       cc-superimpose
                       cc-superimpose
                       10
                       10)
                dc 500 500)
     
     (draw-pict  (round-frame (blank 80) 10)
                 dc 300 500)

     (draw-pict (colorize (round-frame (blank 80) 10) "red")
                dc 300 500)

     (draw-pict (color-frame (blank 80) "blue" 1)
                dc 200 500)

     (draw-pict (color-round-frame (blank 80) 10 "green" 1)
                dc 100 500)
     
     (draw-pict (color-dash-frame (blank 80) 5 "gold" 1)
                dc 400 500)

     (draw-pict (circle 80)
                dc 400 600)

     (draw-pict (ellipse 80 40)
                dc 300 600)

     (draw-pict (filled-ellipse 80 40)
                dc 200 600)

     (draw-pict (color-frame (blank 80) "red" 1)
                dc 100 600)

     (draw-pict (color-round-frame (blank 80) 10 "purple")
                dc 100 700)

     (js-log (color->string "purple"))

     (draw-pict (circle 80 "darkblue")
                dc 200 700)

     (draw-pict (circle 80 "darkblue" 2)
                dc 300 700)

     (draw-pict (colorize (filled-ellipse 80 40 #t "darkblue" 5)
                          "lightblue")
                dc 400 700)

     (draw-pict (rectangle 80 80)
                dc 500 700)

     (draw-pict (rectangle 80 80  "red")
                dc 600 700)

     (draw-pict (rectangle 80 80 "gold" 5)
                dc 700 700)

     (draw-pict (filled-rectangle 80 80)
                dc 100 800)

     (draw-pict (colorize (filled-rectangle 80 80  #t "red") "gold")
                dc 200 800)

     (draw-pict (colorize (filled-rectangle 80 80 #t "gold" 5) "red")
                dc 300 800)


     (draw-pict (rounded-rectangle 80 80)
                dc 100 400)

     (draw-pict (rounded-rectangle 80 80 10)
                dc 200 400)

     (draw-pict (rounded-rectangle 80 80 10 "blue")
                dc 300 400)

     (draw-pict (filled-rounded-rectangle 80 80)
                dc 400 400)

     (draw-pict (colorize (filled-rounded-rectangle 80 80 10) "gold")
                dc 500 400)

     (draw-pict (colorize (filled-rounded-rectangle 80 80 10 #t "blue" 5) "lightblue")
                dc 600 400)

     (draw-pict (cc-superimpose (colorize (filled-rectangle 70 45) "darkcyan")
                                (cellophane (disk 40) 0.2 #f))
                dc 700 400)

     (draw-pict (cc-superimpose (colorize (filled-rectangle 70 45) "darkcyan")
                                (cellophane (disk 40) 0.8 #f))
                dc 800 400)

     (draw-pict (cc-superimpose (colorize (filled-rectangle 70 45) "darkcyan")
                                (cellophane (disk 40) 0.2))
                dc 700 500)

     (draw-pict (cc-superimpose (colorize (filled-rectangle 70 45) "darkcyan")
                                (cellophane (disk 40) 0.8))
                dc 800 500)

     (draw-pict (vc-append 5
                           (linewidth 3 (hline 80 1))
                           (linewidth 0 (hline 80 1))
                           (linewidth 6 (hline 80 1)))
                dc 500 600)

     (draw-pict (inset/clip (colorize (filled-rectangle 40 40) "forestgreen") -10)
                dc 500 300)

     (draw-pict (inset/clip (colorize (filled-rectangle 40 40) "forestgreen") -10 -5)
                dc 550 300)

     (draw-pict (inset/clip (colorize (filled-rectangle 40 40) "forestgreen") -2 -4 -8 -16)
                dc 600 300)

     (draw-pict (inset (colorize (filled-rectangle 40 40) "thistle") -10)
                dc 650 300)

     (draw-pict (clip (inset (colorize (filled-rectangle 40 40) "thistle") -10))
                dc 700 300)

     (draw-pict (hyperlinkize (text "Hello"))
                dc 750 300)

     (draw-pict (frame (scale (inset (text (string #\U1F41F)) 0 -4) 4)) ; fish
                dc 500 200)

     
     (define styles '(transparent solid dot long-dash short-dash dot-dash ))
     (define dash-pict (apply vl-append
                              5
                              (for/list ([style (in-list styles)])
                                (vl-append 5
                                           (text (symbol->string style))
                                           (linewidth 1 (linestyle style (hline 80 1)))))))
     (draw-pict dash-pict dc 600 800)

     (draw-pict (let ()
                  (define ss '("0" "0.2" "0.4" "0.6" "0.8" "1"))
                  (define (do-fade n)
                    (fade-pict n (rectangle 30 30) (disk 30)))
                  (apply ht-append 10
                         (for/list ([n (in-range 0 1.2 0.2)]
                                    [s (in-list ss)])
                           (vc-append (text s 12)
                                      (do-fade n)))))
                dc 600 600)

     #;(draw-pict (standard-fish 80 40)
                  dc 100 300)

     (draw-pict (let ()
                  (define txt
                    (colorize (text "Freeze!" null 25) "deepskyblue"))
                  (vl-append ; 5
                             (scale (frame         txt)  2.5)
                             (scale (frame (freeze txt)) 2.5)))
                dc 100 300)
     
     ))
  (flush)
   
  (dc 'fill-style "red")   
  (dc 'font       "64px 'Arial'")
  (dc 'fill-text  "webracket/pict" 300 100)

  (define (draw-star ctx cx cy spikes outerR innerR)
    (define step (/ pi spikes))
    (define rot  (/ pi 2))
    (dc 'begin-path)
    (dc 'move-to cx (+ cy outerR))
    (for ([i (in-range 1 (+ spikes 1))])
      (define x1 (+ cx (* (flcos rot) outerR)))
      (define y1 (+ cy (* (flsin rot) outerR)))
      (dc 'line-to x1 y1)
      (set! rot (+ rot step))
      (define x2 (+ cx (* (flcos rot) innerR)))
      (define y2 (+ cy (* (flsin rot) innerR)))
      (dc 'line-to x2 y2)
      (set! rot (+ rot step)))
    (dc 'close-path)
    (dc 'fill (void) "nonzero")
    (dc 'stroke))

  (draw-star ctx 150. 150. 5 100. 40.))


(define (flush)
  (js-log (get-output-string (current-output-port))))

(define (start-main)
  (define result
    (with-handlers ([(λ _ #t) (λ (e) e)])
      ; (error 'font-family->string "expected a font-family, got: ~a" 'normal)
      #; (make-font 'sans-serif 12  "normal" "normal" "normal" 'normal)
      #;(hline 40 5)
      (main)
      (displayln "Hello")))
  (unless (void? result)
    (displayln result))
  (flush))

(load-bitmaps)
(maybe-start-main)

