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
    #;(define (string-trim-left s)
      (let ([len (string-length s)])
        (let loop ([i 0])
          (if (or (= i len) (not (char-whitespace? (string-ref s i))))
              (substring s i len)
              (loop (add1 i))))))

    #;(define (string-trim-right s)
      (let ([len (string-length s)])
        (let loop ([i len])
          (if (zero? i)
              ""
              (let ([pos (sub1 i)])
                (if (char-whitespace? (string-ref s pos))
                    (loop pos)
                    (substring s 0 i)))))))

    #;(define (string-trim s)
      (string-trim-right (string-trim-left s)))

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
          [else (error 'color->string "expected a color value, got: ~a" val)])))

    color->string))



;;;
;;; Font
;;;

(struct font (family size  style variant weight line-height)
  #:transparent)

  
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
  #:mutable
  #:extra-constructor-name make-pict
  #:transparent)

(struct child (pict dx dy sx sy sxy syx)
  #:extra-constructor-name make-child
  #:transparent)

(struct bbox  (x1 y1 x2 y2 ay dy)
  #:extra-constructor-name make-bbox
  #:transparent)


(struct converted-pict pict (parent))


(define (pict-convertible? x)    #f)  ; TODO
(define (pict-convertible-ref x) #f)  ; TODO

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


(define (pict-path-element=? a b)
  (or (eq? a b)
      (if (converted-pict? a)
          (if (converted-pict? b)
              (eq? (converted-pict-parent a) (converted-pict-parent b))
              (eq? (converted-pict-parent a) b))
          (if (converted-pict? b)
              (eq? (converted-pict-parent b) a)
              #f))))

(define (pict-path? p)
  (or (pict-convertible? p)
      (and (pair? p)
           (list? p)
           (andmap pict-convertible? p))))


;;;
;;;
;;;

(define default-seg        5) ; default segment length for dashes
(define recordseplinespace 4)


(define blank 
  (case-lambda
   [()        (blank 0 0 0)]
   [(s)       (blank s s)]
   [(w h)     (blank w h 0)]
   [(w a d)   (make-pict `(picture ,w ,(+ a d)) w (+ a d) a d null #f #f)]
   [(w h a d) (make-pict `(picture ,w ,h)       w h       a d null #f #f)]))


(define (extend-pict box dx dy dw da dd draw)
  (let ([w (pict-width   box)]
        [h (pict-height  box)]
        [d (pict-descent box)]
        [a (pict-ascent  box)])
    (make-pict (if draw draw (pict-draw box))
               (+ w dw) (+ h da dd) 
               (max 0 (+ a da)) (max 0 (+ d dd))
               (list (make-child box dx dy 1 1 0 0))
               #f
               (pict-last box))))

(define (transform dx dy tdx tdy tsx tsy tsxy tsyx)
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
                                          (child-dx c) (child-dy c)
                                          (child-sx c) (child-sy c)
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

(define (lift-above-baseline p n)
  (let* ([dh (- (max 0 (- n (pict-descent p))))]
         [do-a? (= (pict-height p)
                   (+ (pict-ascent p) (pict-descent p)))]
         [h2 (+ dh (pict-height p))]
         [d2 (max 0 (- (pict-descent p) n))])
    (make-pict (pict-draw p)
               (pict-width p) h2
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

(define (drop-below-ascent p n)
  (let* ([dh (- (max 0 (- n (pict-ascent p))))]
         [do-d? (= (pict-height p)
                   (+ (pict-ascent p) (pict-descent p)))]
         [h2 (+ dh (pict-height p))]
         [a2  (max 0 (- (pict-ascent p) n))])
    (make-pict (pict-draw p)
               (pict-width p) h2
               a2
               (if do-d?
                   (- h2 a2)
                   (pict-descent p))
               (pict-children p)
               #f
               (pict-last p))))

(define (baseless p)
  (let ([p (lift-above-baseline p (pict-descent p))])
    (drop-below-ascent p (- (pict-ascent p) (pict-height p)))))

(define (refocus p c)
  (let-values ([(x y) (find-lt p c)])
    (let ([p (inset p
                    (- x) (- y (pict-height p))
                    (- (- (pict-width p) x (pict-width c)))
                    (- (pict-height c) y))])
      (make-pict (pict-draw p)
                 (pict-width c) (pict-height c)
                 (pict-ascent c) (pict-descent c)
                 (pict-children p)
                 #f
                 (last* c)))))

(define (panorama-box! p*)
  (let* ([p (pict-convert p*)]
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

(define (panorama p)
  (let-values ([(x1 y1 x2 y2 ay dy) (panorama-box! p)])
    (let ([h (- y2 y1)])
      (place-over (blank (- x2 x1) h (- h ay) dy)
                  (- x1) (- y2 (pict-height p))
                  p))))

(define (clip-descent b)
  (let* ([w (pict-width b)]
         [h (pict-height b)]
         [d (pict-descent b)])
    (extend-pict
     b 0 (- d) 
     0 0 (- d)
     `(picture ,w ,(- h d)
               (put 0 ,(- d) ,(pict-draw b))))))

(define (clip-ascent b)
  (let* ([w (pict-width b)]
         [h (pict-height b)]
         [a (pict-ascent b)])
    (extend-pict
     b 0 a
     0 (- a) 0
     `(picture ,w ,(- h a)
               (put 0 0 ,(pict-draw b))))))

(define (thickness mode b)
  (let* ([w (pict-width b)]
         [h (pict-height b)])
    (extend-pict
     b 0 0 0 0 0
     `(picture ,w ,h
               (thickness ,mode ,(pict-draw b))))))

(define (thick b)            (thickness 'thicklines b))
(define (thin  b)            (thickness 'thinlines  b))
(define (line-thickness n b) (thickness n b))
(define (line-style     n s) (thickness n s))

(define inset
  (case-lambda
   [(box a) (inset box a a a a)]
   [(box h v) (inset box h v h v)]
   [(box l t r b)
    (let ([w (+ l r (pict-width box))]
          [h (+ t b (pict-height box))])
      (extend-pict
       box l b
       (+ l r) t b
       `(picture
         ,w ,h
         (put ,l ,b ,(pict-draw box)))))]))

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
     (car (rotate width height))
     (cadr (rotate width height))
     (cadr (rotate 0 height)) 0
     null
     #f
     #f)))

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
       (put 0 0 ,(pict-draw box))
       (put ,r ,r (oval "[bl]" ,rr ,rr))
       (put ,r 0 (line 1 0 ,lw))
       (put ,(- w r) ,r (oval "[br]" ,rr ,rr))
       (put ,w ,r (line 0 1 ,lh))
       (put ,r ,(- h r) (oval "[tl]" ,rr ,rr))
       (put ,r ,h (line 1 0 ,lw))
       (put ,(- w r) ,(- h r) (oval "[tr]" ,rr ,rr))
       (put ,0 ,r (line 0 1 ,lh))))))

(define (big-circle d)
  (let ([r (/ d 2)])
    (picture
     d d
     `((curve 0 ,r ,r 0 0 0)
       (curve ,r 0 ,d ,r ,d 0)
       (curve ,d ,r ,r ,d ,d ,d)
       (curve ,r ,d 0 ,r 0 ,d)))))

(define (ghost box)
  (let ([w (pict-width box)]
        [h (pict-height box)])
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h))))

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
        [2max (lambda (a b c . rest) (max a b))]
        [zero (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 . args) 0)]
        [fv (lambda (a b . args) a)]
        [sv (lambda (a b . args) b)]
        [min2 (lambda (a b . args) (min a b))]
        [max2 (lambda (a b . args) (max a b))]
        [3+ (lambda (a b c . args) (+ a b c))]
        [a-max (lambda (a b c first rest)
                 (+ (max (pict-ascent first) (pict-ascent rest))
                    (max (- (pict-height first) (pict-ascent first))
                         (- (pict-height rest) (pict-ascent rest)))))]
        [d-max (lambda (a b c first rest)
                 (+ (max (pict-descent first) (pict-descent rest))
                    (max (- (pict-height first) (pict-descent first))
                         (- (pict-height rest) (pict-descent rest)))))]
        [min-ad (lambda (a b oa ob ah bh h da db)
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
                    [(pict? p) p]
                    [(pict-convertible? p)
                     (pict-convert p)]
                    [else
                     (raise-argument-error
                      name "all picts as arguments"
                      (apply string-append (add-between (map (λ (x) (format "~e" x)) boxes*) " ")))]))
                boxes*))
             (let ([max-w (apply max (map pict-width boxes))]
                   [max-h (apply max (map pict-height boxes))]
                   [max-a (apply max (map pict-ascent boxes))]
                   [max-a-complement (apply max (map (lambda (b) (- (pict-height b) (pict-ascent b)))
                                                     boxes))]
                   [max-d (apply max (map pict-descent boxes))]
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
        [norm (lambda (h a d ac dc) h)]
        [tbase (lambda (h a d ac dc) (+ a ac))] 
        [bbase (lambda (h a d ac dc) (+ d dc))] 
        [lb (lambda (m v . rest) 0)]
        [rt (lambda (m v . rest) (- m v))]
        [tline (lambda (m v md d mac a) (- mac (- v a)))]
        [bline (lambda (m v md d mac a) (- md d))]
        [c (lambda (m v . rest) (/ (- m v) 2))])
    (values
     (make-superimpose lb rt norm 'lt-superimpose)
     (make-superimpose lb lb norm 'lb-superimpose)
     (make-superimpose lb c norm 'lc-superimpose)
     (make-superimpose lb tline tbase 'ltl-superimpose)
     (make-superimpose lb bline bbase 'lbl-superimpose)
     (make-superimpose rt rt norm 'rt-superimpose)
     (make-superimpose rt lb norm 'rb-superimpose)
     (make-superimpose rt c norm 'rc-superimpose)
     (make-superimpose rt tline tbase 'rtl-superimpose)
     (make-superimpose rt bline bbase 'rbl-superimpose)
     (make-superimpose c rt norm 'ct-superimpose)
     (make-superimpose c lb norm 'cb-superimpose)
     (make-superimpose c c norm 'cc-superimpose)
     (make-superimpose c tline tbase 'ctl-superimpose)
     (make-superimpose c bline bbase 'cbl-superimpose))))

; TODO: element on stack fallthru error
#;(define table
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

(define (record title . fields)
  (let* ([totalwidth (apply max (pict-width title) (map pict-width fields))]
         [linespace (if (null? fields) 0 recordseplinespace)]
         [totalheight (+ (pict-height title) (apply + (map pict-height fields))
                         linespace)]
         [title-y (- totalheight (pict-height title))]
         [field-ys (let loop ([pos (- totalheight (pict-height title) linespace)]
                              [fields fields])
                     (if (null? fields)
                         null
                         (let* ([p (- pos (pict-height (car fields)))])
                           (cons p
                                 (loop p (cdr fields))))))])
    (make-pict
     `(picture
       ,totalwidth ,totalheight
       (put 0 0 (line 1 0 ,totalwidth))
       (put 0 ,totalheight (line 1 0 ,totalwidth))
       (put 0 0 (line 0 1 ,totalheight))
       (put ,totalwidth 0 (line 0 1 ,totalheight))
       (put 0 ,title-y ,(pict-draw title))
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

(define (picture* w h a d commands)
  (error 'picture* "todo") ; TODO 
  #;(let loop ([commands commands][translated null][children null])
    (if (null? commands)
        (make-pict
         `(picture ,w ,h
                   ,@(reverse translated))
         w h a d
         children
         #f
         #f)
        (let ([c (car commands)]
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
                                          (inexact->exact (floor (* d (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))
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

(define black-and-white
  (make-parameter #f
                  (lambda (x)
                    (and x #t))))


(define (colorize p color)
  (unless (or (string? color)
              (color?  color)
              (and (list? color) (= 3 (length color)) (andmap byte? color)))
    (error 'colorize "expected a color, given ~e" color))
  (let ([color (if (list? color)
                   (apply make-color color) 
                   color)])
    (if (black-and-white)
        p
        (extend-pict 
         p 0 0 0 0 0
         `(color ,color ,(pict-draw p))))))

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

(define (dc f w h [a h] [d 0])
  (make-pict `(prog ,f ,h) w h a d null #f #f))

(define prog-picture dc)


(define (memq* a l)
  (if (pair? l)
      (or (eq? (car l) a)
          (memq* a (cdr l)))
      #f))


(define text
  (case-lambda
   [(string)               (text string '() 12)]
   [(string style)         (text string style 12)]
   [(string style size)    (text string style size 0)]
   [(str style size angle) (not-caps-text str style size angle)
    ; todo: we need to measure the size of the text
    ;       also we need to a way to represent fonts.

    #;(if (il-memq 'caps style)
        (begin
          (unless (zero? angle) 
            (error 'text "the style cannot include 'caps with a non-zero angle"))
          (caps-text str (il-remq 'caps style) size))
        (not-caps-text str style size angle))]))


(define families '(default decorative roman script swiss modern symbol system))

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

(define (not-caps-text string orig-style size angle)
  (let (#;[font
         ; Find font based on `orig-style`.
         (let loop ([style orig-style])
           (cond
             [(null? style) 
              (send the-font-list find-or-create-font
                    size 'default 'normal 'normal #f 'default #t 'unaligned)]
             [(is-a? style font%)
              style]
             [(memq style families)
              (send the-font-list find-or-create-font
                    size style 'normal 'normal #f 'default #t 'unaligned)]
             [(string? style)
              (send the-font-list find-or-create-font
                    size style 'default 'normal 'normal #f 'default #t 'unaligned)]
             [(and (pair? style)
                   (string? (car style))
                   (memq (cdr style) families))
              (send the-font-list find-or-create-font
                    size (car style) (cdr style) 'normal 'normal #f 'default #t 'unaligned)]
             [(and (pair? style)
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
             [(and (pair? style)
                   (memq (car style) '(combine no-combine outline)))
              (loop (cdr style))]
             [(and (pair? style)
                   (is-a? (car style) color%))
              (loop (cdr style))]
             [else (raise-type-error 'text
                                     "style"
                                     orig-style)]))]
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
          #;[dc (dc-for-text-size)])
      #;(unless dc
        (error 'text "no dc<%> object installed for sizing"))
      (let-values ([(w h   ; width, height
                       d   ; distance from base line to bottom of descender
                       s)  ; extra vertical space (included in height, often zero)
                    (values 11 22 0 0) ; todo
                    #;(with-text-scale
                      dc
                      (lambda ()
                        (send dc get-text-extent string s-font combine?)))])
        
        (define (make-draw adj-x adj-y angle)          
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
            (displayln (list "draw-text: " string))
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
                 (send dc set-font f)]))))

        ;; Normal case: no rotation
        (prog-picture (make-draw (lambda (x) x)
                                 (lambda (y) y)
                                 0)
                      w h (- h d) d)

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
                                  pw ph ph 0)))))))))


(define (linewidth n p) (line-thickness n p))
#;(define (linestyle n p) 
  (unless (memq n '(transparent solid xor hilite 
                                dot long-dash short-dash dot-dash 
                                xor-dot xor-long-dash xor-short-dash 
                                xor-dot-dash))
    (raise-type-error 'linestyle "style symbol" n))
  (line-style n p))

#;(define connect
  (case-lambda
   [(x1 y1 x2 y2) (connect x1 y1 x2 y2 #f)]
   [(x1 y1 x2 y2 arrow?) (~connect 'r +inf.0 x1 y1 x2 y2 arrow?)]))

#;(define ~connect 
  (case-lambda
   [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
   [(exact close-enough x1 y1 x2 y2 arrow?)
    `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,(- x2 x1) ,(- y2 y1) #f)))]))


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
                 (dc 'ellipse
                     (- dx (/ size 2)) (- h+top dy (/ size 2))
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
                 (js-log (format "bezier: ~a" x))
                 (dc 'begin-path)
                 (dc 'move-to (+ dx x1) (- h+top (+ dy y1)))
                 (dc 'quadratic-curve-to 
                     (+ dx x2) (- h+top (+ dy y2))
                     (+ dx x3) (- h+top (+ dy y3)))
                 (dc 'stroke))]
              [(with-color)
               (if b&w?
                   (loop dx dy (caddr x))
                   (let ([c (second x)])
                     ; (define c (if (string? c) c (color->string c)))
                     (define c "blue")
                     (let ([old-stroke     (dc 'stroke-style)]
                           [old-fill       (dc 'fill-style)])                           
                       ; we set
                       ;  1) stroke color
                       ;  2) brush to solid color
                       ;  3) text color
                       (dc 'stroke-style c)
                       (dc 'fill-style   c)

                       (dc 'stroke-rect 130 190 40 60)  ; this doesn't ?!
                       (dc 'fill-rect   130 490 40 60)  ; this becomes blue
                       
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
  


(define (make-pict-drawer p)
  (let ([cmds (pict->command-list p)]
        [h    (pict-height p)])
    (js-log (format "cmds: ~a" cmds))
    (lambda (dc dx dy)
      (render dc (+ h dy)
              cmds
              dx 0))))

(define (draw-pict p dc dx dy)
  ((make-pict-drawer p) dc dx dy))

(js-log "bar")
; (js-log (format "~a" (blank 10)))
; (js-log (format "~a" (text "foo")))

; (js-log (format "~a" (hc-append (blank 10) (text "foo"))))


; (js-log (format "~a" (dash-frame (blank 10))))

; (js-log (format "~a" (make-font 'sans-serif)))


; (make-font 'sans-serif)

; (make-font 'sans-serif 12  "normal" "normal" "normal" 'normal)


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
      [(list 'direction dir)
       (js-set-canvas2d-direction! ctx (to-string dir))]
      [(list 'fill-style)
       (js-canvas2d-fill-style ctx)]
      [(list 'fill-style val)
       ; <color>, CanvasGradient, CanvasPattern
       (js-set-canvas2d-fill-style! ctx (to-string val))]
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
      

#;(define (make-command-drawer ctx)
  (define dc (canvas-context->dc ctx))
  (define (draw . args)
    (unless (equal? (length args) 1)
      (error 'draw "expected only one argument, got: ~a" args))
    (define command (car args))

    (js-log (format "draw: ~a" command))
    
    (match command 
      [(list 'line x0 y0 x1 y1)
       (dc 'begin-path)
       (dc 'move-to x0 y0)
       (dc 'line-to x1 y1)
       (dc 'stroke)]
      [(list 'with-thickness w cmds)
       ; #f transparent
       ; 0  as thin as possible   ; ignored by js linewidth
       (js-log w)
       #;(js-log (format "~a" command))
       (dc 'line-width w)
       #;(js-log cmds)
       (for-each draw cmds)]
      [(list 'with-color c cmds)
       #;(js-log (format "~a" command))
       (dc 'stroke-style (color->string c))
       (for-each draw cmds)]

      #;[(list 'offset x y cmds)
       (dc 'move-to x y)
       (for-each draw cmds)]
      [_
       (error 'command-drawer "unknown drawing command: ~a" command)]))    
  draw)
    

(let ()
  (define canvas (js-create-element "canvas"))
  (js-set-canvas-width!  canvas 1024)
  (js-set-canvas-height! canvas 800)
  (js-append-child! (js-document-body) canvas)

  (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))
  
  (define dc (canvas-context->dc ctx))

  (let (#;[dc (λ x (displayln x))]
        #;[dc (make-command-drawer ctx)])
    (displayln 
     (with-handlers ([(λ _ #t) (λ (e) e)])
       #;(draw-pict (hline 40 30) dc 200 200)
       #;(draw-pict (linewidth 10 (hline 40 30)) dc 200 200)
       #;(draw-pict (colorize (hline 40 30) "blue")
                  dc 200 200)
       
       #;(draw-pict (colorize (linewidth 10 (hline 40 30))
                            (make-color "blue"))
                    dc 200 200)

       (draw-pict (colorize (linewidth 10 (dash-vline 5 40 5) ) "blue")
                  dc 200 200)

       ; in utils.rkt - later
       #;(draw-pict (rectangle 100 50))

       ))
    (js-log (get-output-string (current-output-port))))
  
  #;(render dc h+top l dx dy)

  (dc 'fill-style "red")
  
  (dc 'font       "64px 'Arial'")
  (dc 'fill-text  "Hello World" 400 200)

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


(displayln 
 (with-handlers ([(λ _ #t) (λ (e) e)])
   ; (error 'font-family->string "expected a font-family, got: ~a" 'normal)
   #; (make-font 'sans-serif 12  "normal" "normal" "normal" 'normal)
   #;(hline 40 5)
   (js-log (color->string (make-color "blue")))
   
   ))

(js-log (get-output-string (current-output-port)))

(define (flush)
  (js-log (get-output-string (current-output-port))))

(js-log (color->string (make-color "blue")))
