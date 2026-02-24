#;(define show-pict
  (λ (p [w #f] 
        [h #f] 
        #:frame-style [frame-style '()]
        #:frame-x     [frame-x #f]
        #:frame-y     [frame-y #f])
    (define the-pict p)
    (define pict-drawer (make-pict-drawer the-pict))
    (define no-redraw? #f)
    (define pict-frame%
      (class (gui-dynamic-require 'frame%)
        (define/public (set-pict p)
          (set! the-pict p)
          (set! pict-drawer (make-pict-drawer the-pict))
          (set! no-redraw? #t)
          (let ([pw (inexact->exact (floor (pict-width the-pict)))]
                [ph (inexact->exact (floor (pict-height the-pict)))])
            (send c min-width (if w (max w pw) pw))
            (send c min-height (if h (max h ph) ph)))
          (set! no-redraw? #f)
          (send c on-paint))
        (super-instantiate ())))
    (define pict-canvas%
      (class (gui-dynamic-require 'canvas%)
        (inherit get-dc)
        (define/override (on-paint)
          (unless no-redraw?
            (let ([dc (get-dc)])
              (send dc clear)
              (let* ([pw (pict-width the-pict)]
                     [ph (pict-height the-pict)]
                     [xo (if (and w
                                  (pw . < . w))
                             (- (/ w 2) (/ pw 2))
                             0)]
                     [yo (if (and h
                                  (ph . < . h))
                             (- (/ h 2) (/ ph 2))
                             0)])
                (pict-drawer dc xo yo)))))
        (super-instantiate ())))
    (define f (new pict-frame% 
                   [label "MrPict"] 
                   [style frame-style] 
                   [x frame-x]
                   [y frame-y]))
    (define c (make-object pict-canvas% f))
    (send (send c get-dc) set-smoothing 'aligned)
    (send f set-pict p)
    (send f show #t)))

#;(define dc-for-text-size (make-parameter 
                          (make-object bitmap-dc% (make-bitmap 1 1))
                          (lambda (x)
                            (unless (or (not x)
                                        (is-a? x dc<%>))
                              (raise-argument-error 'dc-for-parameter "(or/c (is-a?/c dc<%>) #f)" x))
                            x)))

#;(define current-expected-text-scale (make-parameter (list 1 1)))
#;(define (with-text-scale dc thunk)
  (let ([x (current-expected-text-scale)])
    (if (equal? x '(1 1))
        (thunk)
        (let-values ([(xs ys) (send dc get-scale)])
          (send dc set-scale (* xs (car x)) (* ys (cadr x)))
          (let-values ([(w h d s) (thunk)])
            (send dc set-scale xs ys)
            (values w h d s))))))


#;(define (extend-font font size style weight hinting)
  (if (send font get-face)
      (send the-font-list find-or-create-font
            size 
            (send font get-face)
            (send font get-family)
            style
            weight
            #f
            'default
            #t
            hinting)
      (send the-font-list find-or-create-font
            size 
            (send font get-family)
            style
            weight
            #f
            'default
            #t
            hinting)))

#;(define (not-caps-text string orig-style size angle)
  (let ([font
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
        [combine? (let loop ([style orig-style])
                    (cond
                     [(eq? style 'modern) #f]
                     [(not (pair? style)) #t]
                     [(eq? (car style) 'combine) #t]
                     [(eq? (car style) 'no-combine) #f]
                     [else (loop (cdr style))]))]
        [sub? (memq* 'subscript orig-style)]
        [sup? (memq* 'superscript orig-style)]
        [large-script? (memq* 'large-script orig-style)]
        [outline? (memq* 'outline orig-style)]
        [color (let loop ([style orig-style])
                 (cond
                  [(not (pair? style)) #f]
                  [(is-a? (car style) color%) 
                   (resolve-color (car style))]
                  [else (loop (cdr style))]))])
    (let ([s-font (if (or sub? sup?)
                      (extend-font font
                                   (floor (* (if large-script?
                                                 85/100
                                                 6/10)
                                             (send font get-point-size)))
                                   (send font get-style)
                                   (send font get-weight)
                                   (send font get-hinting))
                      font)]
          [dc (dc-for-text-size)])
      (unless dc
        (error 'text "no dc<%> object installed for sizing"))
      (let-values ([(w h d s) (with-text-scale
                               dc
                               (lambda ()
                                 (send dc get-text-extent string s-font combine?)))])
        (define (make-draw adj-x adj-y angle)
          (define p 
            (and outline?
                 (let ([p (new dc-path%)])
                   (send p text-outline
                         s-font string 0 0 combine?)
                   (unless (zero? angle)
                     (send p rotate angle))
                   p)))
          (lambda (dc x y)
            (let ([f (send dc get-font)])
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
        (if (or sub? sup?)
            (let-values ([(ww wh wd ws) (with-text-scale
                                         dc
                                         (lambda ()
                                           (send dc get-text-extent "Wy" font)))])
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

#;(define caps-text
  (case-lambda
   [(string) (caps-text string '() 12)]
   [(string style) (caps-text string style 12)]
   [(string style size)
    (let ([strings
           (let loop ([l (string->list string)] [this null] [results null] [up? #f])
             (if (null? l)
                 (reverse (cons (reverse this) results))
                 (if (eq? up? (char-upper-case? (car l)))
                     (loop (cdr l) (cons (car l) this) results up?)
                     (loop (cdr l) (list (car l)) (cons (reverse this) results) (not up?)))))]
          [cap-style
           (let loop ([s style])
             (cond
              [(pair? s) (cons (car s) (loop (cdr s)))]
              [(is-a? s font%) (send the-font-list find-or-create-font
                                     (floor (* 8/10 (send s get-point-size)))
                                     (send s get-family)
                                     (send s get-style)
                                     (send s get-weight)
                                     (send s get-underlined?)
                                     (send s get-smoothing)
                                     (send s get-size-in-pixels?))]
              [else s]))]
          [cap-size (floor (* 8/10 size))])
      (let ([picts
             (let loop ([l strings] [up? #f])
               (if (null? l)
                   null
                   (let* ([first-string (list->string (map char-upcase (car l)))]
                          [first
                           (not-caps-text first-string
                                          (if up? style cap-style)
                                          (if up? size cap-size)
                                          0)]
                          [rest (loop (cdr l) (not up?))])
                     (if (and up? (pair? (cdr l)))
                         ;; kern capital followed by non-captial
                         (let ([plain-first (not-caps-text first-string
                                                           cap-style
                                                           cap-size
                                                           0)]
                               [together (not-caps-text (string-append
                                                         first-string
                                                         (list->string (map char-upcase (cadr l))))
                                                        cap-style
                                                        cap-size
                                                        0)])
                           (cons (hbl-append (- (pict-width together)
                                                (+ (pict-width plain-first)
                                                   (pict-width (car rest))))
                                             first
                                             (car rest))
                                 (cdr rest)))
                         ;; no kerning needed:
                         (cons first rest)))))])
        (apply hbl-append 0 picts)))]))

#;(define (resolve-color c)
  (let* ([requested-color (cond
                           [(is-a? c color%) c]
                           [(string? c)
                            (send the-color-database find-color c)]
                           [(list? c)
                            (apply make-object color% c)])]
         [color (or requested-color 
                    (send the-color-database find-color "BLACK"))])
    (unless requested-color
      (eprintf "WARNING: couldn't find color: ~s\n" c))
    color))



#;(define (convert-pict p format default [pad? #t])
  (cond
   [(member format '(pdf-bytes+bounds8 eps-bytes+bounds8
                                       png-bytes+bounds8 png@2x-bytes+bounds8
                                       svg-bytes+bounds8))
    (define xscale (box 1))
    (define yscale (box 1))
    (case format
      [(pdf-bytes+bounds8 eps-bytes+bounds8)
       (send (current-ps-setup) get-scaling xscale yscale)])
    (define-values (pad-l pad-t pad-r pad-b)
      (if pad?
          (apply values (convert-bounds-padding))
          (values 0 0 0 0)))
    (define pad-p (inset p pad-l pad-t pad-r pad-b))
    (list (convert-pict/bytes pad-p
                              (case format
                                [(pdf-bytes+bounds8) 'pdf-bytes]
                                [(eps-bytes+bounds8) 'eps-bytes]
                                [(png-bytes+bounds8) 'png-bytes]
                                [(png@2x-bytes+bounds8) 'png@2x-bytes]
                                [(svg-bytes+bounds8) 'svg-bytes]
                                [else (error "internal error" format)])
                              default)
          (* (unbox xscale) (pict-width pad-p))
          (* (unbox yscale) (pict-height pad-p))
          (* (unbox yscale) (pict-descent pad-p))
          0
          (* (unbox xscale) pad-l)
          (* (unbox yscale) pad-t)
          (* (unbox xscale) pad-r)
          (* (unbox yscale) pad-b))]
   [(member format '(pdf-bytes+bounds eps-bytes+bounds
                                      png-bytes+bounds
                                      png@2x-bytes+bounds
                                      svg-bytes+bounds))
    (take (convert-pict p
                        (case format
                          [(pdf-bytes+bounds) 'pdf-bytes+bounds8]
                          [(eps-bytes+bounds) 'eps-bytes+bounds8]
                          [(png-bytes+bounds) 'png-bytes+bounds8]
                          [(png@2x-bytes+bounds) 'png@2x-bytes+bounds8]
                          [(svg-bytes+bounds) 'svg-bytes+bounds8]
                          [else (error "internal error" format)])
                        default
                        #f)
          5)]
   [else
    (convert-pict/bytes p format default)]))

#;(define (convert-pict/bytes p format default)
  (case format
    [(png-bytes png@2x-bytes)
     (let* ([bm (make-bitmap
                 (max 1 (inexact->exact (ceiling (pict-width p))))
                 (max 1 (inexact->exact (ceiling (pict-height p))))
                 #:backing-scale (if (eq? format 'png@2x-bytes) 2 1))]
            [dc (make-object bitmap-dc% bm)])
       (send dc set-smoothing 'aligned)
       (draw-pict p dc 0 0)
       (send dc set-bitmap #f)
       (let ([s (open-output-bytes)])
         (send bm save-file s 'png #:unscaled? #t)
         (get-output-bytes s)))]
    [(eps-bytes pdf-bytes)
     (let ([s (open-output-bytes)]
           [xs (box 1)]
           [ys (box 1)])
       (send (current-ps-setup) get-scaling xs ys)
       (let ([dc (new (if (equal? format 'eps-bytes) post-script-dc% pdf-dc%)
                      [interactive #f]
                      [as-eps #t]
                      [width (* (pict-width p) (unbox xs))]
                      [height (* (pict-height p) (unbox ys))]
                      [output s])])
         (send dc set-smoothing 'smoothed)
         (send dc start-doc "pict")
         (send dc start-page)
         (draw-pict p dc 0 0)
         (send dc end-page)
         (send dc end-doc))
       (get-output-bytes s))]
    [(svg-bytes)
     (let ([s (open-output-bytes)])
       (define dc (new svg-dc% 
                       [width  (pict-width p)]
                       [height (pict-height p)]
                       [output s]))
       (send dc set-smoothing 'smoothed)
       (send dc start-doc "Generating svg")
       (send dc start-page)
       (draw-pict p dc 0 0)
       (send dc end-page)
       (send dc end-doc)
       (regexp-replace "width=\"([0-9.]*pt)\" height=\"([0-9.]*pt)\"" 
                       (get-output-bytes s)
                       (λ (all w h) 
                         (define (rem x) (bytes->string/utf-8 (regexp-replace "pt" x "")))
                         (string->bytes/utf-8
                          (string-append "width=\"" (rem w) "\" height=\"" (rem h) "\"")))))]
    [else default]))

#;(define (convert-pict-to-vector p)
  (define dc (new record-dc%
                  [width (pict-width p)]
                  [height (pict-height p)]))
  (draw-pict p dc 0 0)
  (vector (send dc get-recorded-datum)
          (pict-width p)
          (pict-height p)
          (pict-ascent p)
          (pict-descent p)))

#;(define (deserialize-pict datum w h d a)
  (define draw (recorded-datum->procedure datum))
  (make-pict `(prog ,(lambda (dc x y)
                       (define t (send dc get-transformation))
                       (send dc translate x y)
                       (draw dc)
                       (send dc set-transformation t))
                    ,h)
             w h d a
             null
             #f
             #f))

#;(define pict-deserialize-info
  (make-deserialize-info deserialize-pict
                         (lambda () (error "no cycles"))))

