(define (cb-add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (cb-sxml->dom child))))

(define (cb-set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list (? symbol? name) (? string? val))
       (js-set-attribute! elem (symbol->string name) val)]
      [(list (? symbol? name) (? procedure? proc))
       (js-add-event-listener! elem
                               (symbol->string name)
                               (procedure->external proc))]
       [_ (error 'cb-set-elem-attributes
                "expected (symbol string) or (symbol procedure); got ~a"
                attr)])))

(define (cb-sxml->dom exp)
  (match exp
    [(? string? s)
     (js-create-text-node exp)]
    [(list tag (list '@ attrs ...) children ...)
     (define elem (js-create-element (symbol->string tag)))
     (cb-set-elem-attributes elem attrs)
     (cb-add-children elem children)
     elem]
    [(list tag children ...)
     (define elem (js-create-element (symbol->string tag)))
     (cb-add-children elem children)
     elem]
    [_ (error 'cb-sxml->dom
                         "expected a valid SXML expression; got ~a"
                         exp)]))

(define press-count 0)

(define (update)
  (js-set! (js-get-element-by-id "message")
           "textContent"
           (string-append "You have pressed the button "
                          (number->string press-count)
                          " times")))

;; Intentional repro: the browser will pass an event argument here.
(define (on-click)
  (js-log press-count)
  (set! press-count (+ press-count 1))
  (update))

(define sxml
  `(section
     (h1 "Callback Example:")
     (p (@ (id "message")) "You have pressed the button 0 times")
     (button (@ (id "press-button") (click ,on-click))
             "Press me")))

(js-append-child! (js-document-body) (cb-sxml->dom sxml))
