(define (add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list (? symbol? name) (? string? val))
       (js-set-attribute! elem (symbol->string name) val)]
      [(list (? symbol? name) (? procedure? proc))
       (js-add-event-listener! elem
                               (symbol->string name)
                               (procedure->external proc))]
      [_ (raise-user-error 'set-elem-attributes
                           "expected (symbol string) or (symbol procedure); got ~a"
                           attr)])))

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
     elem]
    [_ (raise-user-error 'sxml->dom
                         "expected a valid SXML expression; got ~a"
                         exp)]))

(define count 0)

(define (update)
  (js-set-property! (js-get-element-by-id "message")
                    "textContent"
                    (string-append "You have pressed the button "
                                   (number->string count)
                                   " times")))

(define (on-click _event)
  (js-log count)
  (set! count (+ count 1))
  (update))

(define sxml
  `(section
     (h1 "Callback Example:")
     (p (@ (id "message")) "You have pressed the button 0 times")
     (button (@ (id "press-button") (click ,on-click))
             "Press me")))

(js-append-child! (js-document-body) (sxml->dom sxml))

