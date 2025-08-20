(define sxml
  '(section
    (h1 (@ (style "color: blue")) "Hello #racket !!!!!")
    (p "Test: calling JavaScript from WebRacket")
    (small "Made with WebRacket")))

(define (add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))


(define (set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list name value)
       (js-set-attribute! elem (symbol->string name) value)])))
    
(define (sxml->dom exp)
  (match exp
    ; a string represents a text node
    [(? string? s)
     (js-create-text-node exp)]
    ; a tree 
    [(list tag (list '@ attrs ...) children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem children)
     elem]
    [(list tag children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (add-children elem children)
     elem]))
     
(js-append-child! (js-document-body) (sxml->dom sxml))

(js-log (vector "hello"))

(js-log (js-math-abs -3.))
(define pi (* 4. (js-math-atan 1.)))
(js-log (js-math-tan (/ pi 4.)))
(js-log (js-infinity))


