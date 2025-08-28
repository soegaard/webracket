(define sxml
  '(section
    (h1 "Callback Example:")
    (p (@ (id "message")) "You have pressed the button 0 times")
    (button (@ (id "press-button")) "Press me")))

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
    [(? string? s)
     (js-create-text-node exp)]
    [(list tag (list '@ attrs ...) children ...)
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem children)
     elem]
    [(list tag children ...)
     (define elem (js-create-element (symbol->string tag)))
     (add-children elem children)
     elem]))
(js-append-child! (js-document-body) (sxml->dom sxml))

(define count 0)

(define (update)
  (define msg (js-get-element-by-id "message"))
  (js-set-property! msg "textContent"
                    (string-append "You have pressed the button "
                                   (number->string count)
                                   " times")))

(define (on-click _event)
  (js-log count)
  (set! count (+ count 1))
  (update))

(define on-click-host (procedure->external on-click))
(js-add-event-listener! (js-get-element-by-id "press-button")
                        "click"
                        on-click-host)
