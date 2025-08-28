(struct vtext (text) #:transparent)
(struct velem (tag attrs children) #:transparent)

(define (add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem child)))

(define (set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list (? symbol? name) (? string? val))
       (js-set-attribute! elem (symbol->string name) val)]
      [(list (? symbol? name) (? procedure? proc))
       (js-add-event-listener! elem (symbol->string name) (procedure->external proc))]
      [_ (raise-user-error 'set-elem-attributes
                           "expected (symbol string) or (symbol procedure); got ~a"
                           attr)])))

(define (vdom->dom node)
  (match node
    [(vtext s) (js-create-text-node s)]
    [(velem tag attrs children)
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem (map vdom->dom children))
     elem]
    [_ (raise-user-error 'vdom->dom "invalid virtual dom node: ~a" node)]))

(define (sxml->vdom exp)
  (match exp
    [(? string? s) (vtext s)]
    [(list tag (list '@ attrs ...) children ...)
     (velem tag attrs (map sxml->vdom children))]
    [(list tag children ...)
     (velem tag '() (map sxml->vdom children))]
    [_ (raise-user-error 'sxml->vdom
                         "expected a valid SXML expression; got ~a"
                         exp)]))

(struct todo (id text done?) #:mutable #:transparent)
(struct model (todos next-id) #:mutable #:transparent)

(define app (model '() 0))

(define (add-todo! text)
  (when (> (string-length text) 0)
    (set-model-todos! app (cons (todo (model-next-id app) text #f) (model-todos app)))
    (set-model-next-id! app (add1 (model-next-id app)))
    (render!)))

(define (toggle-todo! id)
  (for ([td (in-list (model-todos app))])
    (when (= (todo-id td) id)
      (set-todo-done?! td (not (todo-done? td)))))
  (render!))

(define (todo->sxml td)
  `(li
     (input (@ (type "checkbox")
               ,@(if (todo-done? td) '((checked "checked")) '())
               (change ,(lambda (_e) (toggle-todo! (todo-id td))))))
     (span ,(todo-text td))))

(define (view m)
  `(section
     (h1 "Todo List")
     (form (@ (submit ,on-submit))
       (input (@ (type "text") (id "new-todo")))
       (button "Add"))
     (ul ,@(map todo->sxml (reverse (model-todos m))))))

(define root (js-document-body))

(define (on-submit evt)
  (js-event-prevent-default evt)
  (define input (js-get-element-by-id "new-todo"))
  (define text (js-get-attribute input "value"))
  (js-set-attribute! input "value" "")
  (add-todo! text))

(define (render!)
  (define vdom (sxml->vdom (view app)))
  (js-replace-children! root (vdom->dom vdom)))

(render!)
