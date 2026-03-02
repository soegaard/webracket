#lang webracket

;;;
;;; web-easy Browser Backend
;;;

;; Browser DOM backend representation and primitive operations used by the renderer.
;;
;; Exports:
;;   dom-node                 Browser-backed node constructor.
;;   dom-node?                Predicate for browser-backed nodes.
;;   dom-node-tag             Access node tag symbol.
;;   dom-node-attrs           Access node attributes alist.
;;   dom-node-children        Access child node list.
;;   dom-node-text            Access node text content.
;;   dom-node-on-click        Access node click callback.
;;   dom-node-on-change       Access node change callback.
;;   set-dom-node-tag!        Mutate node tag.
;;   set-dom-node-attrs!      Mutate node attributes and sync browser node attributes.
;;   set-dom-node-children!   Mutate node children list.
;;   set-dom-node-text!       Mutate node text and sync browser node text.
;;   set-dom-node-on-click!   Mutate node click callback.
;;   set-dom-node-on-change!  Mutate node change callback.
;;   dom-node-native          Return host-native DOM node handle.
;;   backend-append-child!    Append child to parent node in model and browser DOM.
;;   backend-set-single-child! Replace node children with a single child in model and browser DOM.
;;   backend-replace-children! Replace node children with a child list in model and browser DOM.
;;   backend-mount-root!      Mount root node into browser container.

(define-values
  (dom-node
   dom-node?
   dom-node-tag
   dom-node-attrs
   dom-node-children
   dom-node-text
   dom-node-on-click
   dom-node-on-change
   set-dom-node-tag!
   set-dom-node-attrs!
   set-dom-node-children!
   set-dom-node-text!
   set-dom-node-on-click!
   set-dom-node-on-change!
   dom-node-native
   backend-append-child!
   backend-set-single-child!
   backend-replace-children!
   backend-mount-root!)
  (let ()
    (struct dom-node-record (tag attrs children text on-click on-change native)
      #:mutable
      #:transparent)

    ;; Constants used by browser backend DOM translation.
    (define attr/type "type") ; Attribute name for input element type.
    (define attr/value "value") ; Attribute name for value-bearing controls.
    (define attr/checked "checked") ; Property/attribute name for checked controls.

    ;; symbol->attr-name : symbol? -> string?
    ;;   Convert attribute symbol key to browser DOM attribute name.
    (define (symbol->attr-name key)
      (symbol->string key))

    ;; value->attr-string : any/c -> string?
    ;;   Convert attribute value to browser DOM attribute string.
    (define (value->attr-string v)
      (cond
        [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [(number? v) (number->string v)]
        [(boolean? v) (if v "true" "false")]
        [else "#<value>"]))

    ;; clear-attributes! : any/c list? -> void?
    ;;   Clear pass is currently a no-op for portability across host primitive sets.
    (define (clear-attributes! native attrs)
      (void))

    ;; alist-ref/default : list? symbol? any/c -> any/c
    ;;   Lookup key in attrs and return default when missing.
    (define (alist-ref/default attrs key default)
      (define p (assq key attrs))
      (if p (cdr p) default))

    ;; sync-select-options! : any/c list? -> void?
    ;;   Replace select children with option nodes for choices.
    (define (sync-select-options! native choices)
      (define fragment ((#%top . js-create-document-fragment)))
      (for-each (lambda (choice)
                  (define option ((#%top . js-create-element) "option"))
                  (define s (value->attr-string choice))
                  ((#%top . js-set-attribute!) option "value" s)
                  ((#%top . js-replace-children!) option ((#%top . js-create-text-node) s))
                  ((#%top . js-append-child!) fragment option))
                choices)
      ((#%top . js-replace-children!) native fragment))

    ;; apply-attributes! : dom-node-record? list? -> void?
    ;;   Apply tracked attributes to native node.
    (define (apply-attributes! n attrs)
      (define native (dom-node-record-native n))
      (define tag (dom-node-record-tag n))
      (when (eq? tag 'select)
        (define choices (alist-ref/default attrs 'choices '()))
        (sync-select-options! native choices))
      (for-each (lambda (a)
                  (define name (car a))
                  (define value (cdr a))
                  (case name
                    [(choices)
                     (void)]
                    [(selected)
                     ((#%top . js-set!) native "value" (value->attr-string value))]
                    [(value)
                     ((#%top . js-set!) native "value" (value->attr-string value))]
                    [(checked)
                     ((#%top . js-set!) native attr/checked (if value #t #f))]
                    [(layout)
                     (case value
                       [(row)
                        ((#%top . js-set-attribute!)
                         native
                         "style"
                         "display:flex;flex-direction:row;align-items:center;gap:4px;")]
                       [(column)
                        ((#%top . js-set-attribute!)
                         native
                         "style"
                         "display:flex;flex-direction:column;gap:4px;")]
                       [else
                        (void)])]
                    [(on-enter-action)
                     (void)]
                    [else
                     ((#%top . js-set-attribute!) native
                                                  (symbol->attr-name name)
                                                  (value->attr-string value))]))
                attrs))

    ;; apply-text! : any/c any/c -> void?
    ;;   Replace native node children with a single text child.
    (define (apply-text! native t)
      (if t
          ((#%top . js-replace-children!) native ((#%top . js-create-text-node) (value->attr-string t)))
          ((#%top . js-replace-children!) native ((#%top . js-create-text-node) ""))))

    ;; tag->element-name : symbol? -> string?
    ;;   Map backend tag symbol to HTML element name.
    (define (tag->element-name tag)
      (case tag
        [(window vpanel hpanel) "div"]
        [(style) "style"]
        [(group) "fieldset"]
        [(text menu-item) "span"]
        [(button) "button"]
        [(input) "input"]
        [(checkbox) "input"]
        [(choice radios select) "select"]
        [(slider) "input"]
        [(progress) "progress"]
        [(spacer) "div"]
        [(table) "table"]
        [(tr) "tr"]
        [(menu-bar menu) "nav"]
        [(image) "img"]
        [else "div"]))

    ;; install-default-node-shape! : dom-node-record? -> void?
    ;;   Apply default browser attributes based on node tag.
    (define (install-default-node-shape! n)
      (define native (dom-node-record-native n))
      (case (dom-node-record-tag n)
        [(checkbox) ((#%top . js-set-attribute!) native attr/type "checkbox")]
        [(slider) ((#%top . js-set-attribute!) native attr/type "range")]
        [else (void)]))

    ;; node-change-value : dom-node-record? -> any/c
    ;;   Read change payload for the current node from browser attributes.
    (define (node-change-value n)
      (define native (dom-node-record-native n))
      (case (dom-node-record-tag n)
        [(checkbox)
         (let ([checked-string
                ((#%top . js-value->string)
                 ((#%top . js-ref/extern) native attr/checked))])
           (if (string=? checked-string "true") #t #f))]
        [(slider)
         (define raw
           ((#%top . js-value->string)
            ((#%top . js-ref/extern) native "value")))
         (define parsed (string->number raw))
         (if parsed parsed 0)]
        [else
         ((#%top . js-value->string)
          ((#%top . js-ref/extern) native "value"))]))

    ;; event-key-value : any/c -> string?
    ;;   Read keyboard event key as a plain string.
    (define (event-key-value evt)
      ((#%top . js-value->string)
       ((#%top . js-ref/extern) evt "key")))

    ;; tab-key-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a tab header button that should receive key navigation.
    (define (tab-key-node? n)
      (and (eq? (dom-node-record-tag n) 'button)
           (let ([p (assq 'role (dom-node-record-attrs n))])
             (and p (eq? (cdr p) 'tab)))))

    ;; input-enter-action : dom-node-record? -> any/c
    ;;   Return input Enter callback when present.
    (define (input-enter-action n)
      (and (eq? (dom-node-record-tag n) 'input)
           (let ([p (assq 'on-enter-action (dom-node-record-attrs n))])
             (and p (cdr p)))))

    ;; nav-key? : string? -> boolean?
    ;;   Check whether key triggers tab keyboard navigation.
    (define (nav-key? key)
      (case (string->symbol key)
        [(ArrowRight ArrowLeft Home End) #t]
        [else #f]))

    ;; focus-selected-sibling-tab! : any/c -> void?
    ;;   Focus selected tab button in the same tablist as native.
    (define (focus-selected-sibling-tab! native)
      (define parent ((#%top . js-ref/extern) native "parentElement"))
      (when parent
        (define selected
          ((#%top . js-send/extern/nullish)
           parent
           "querySelector"
           (vector "button[aria-selected='true']")))
        (when selected
          ((#%top . js-send) selected "focus" (vector)))))

    ;; dom-node : symbol? list? list? any/c any/c any/c -> dom-node?
    ;;   Construct a browser-backed node and install event bridges.
    (define (dom-node tag attrs children text on-click on-change)
      (define native ((#%top . js-create-element) (tag->element-name tag)))
      (define n (dom-node-record tag attrs children text on-click on-change native))
      (install-default-node-shape! n)
      (apply-attributes! n attrs)
      (when text
        (apply-text! native text))
      ((#%top . js-add-event-listener!)
       native
       "click"
       ((#%top . procedure->external)
        (lambda (_evt)
          (define callback (dom-node-record-on-click n))
          (when callback
            (callback)))))
      ((#%top . js-add-event-listener!)
       native
       "change"
       ((#%top . procedure->external)
        (lambda (_evt)
          (define callback (dom-node-record-on-change n))
          (when callback
            (callback (node-change-value n))))))
      (when (eq? tag 'input)
        ((#%top . js-add-event-listener!)
         native
         "input"
         ((#%top . procedure->external)
          (lambda (_evt)
            (define callback (dom-node-record-on-change n))
            (when callback
              (callback (node-change-value n)))))))
      ((#%top . js-add-event-listener!)
       native
       "keydown"
       ((#%top . procedure->external)
        (lambda (evt)
          (define key (event-key-value evt))
          (define on-enter (input-enter-action n))
          (when (and on-enter (string=? key "Enter"))
            ((#%top . js-send) evt "preventDefault" (vector))
            (on-enter))
          (define callback (dom-node-record-on-change n))
          (when (and callback (tab-key-node? n))
            (when (nav-key? key)
              ((#%top . js-send) evt "preventDefault" (vector)))
            (callback key)
            (when (nav-key? key)
              (focus-selected-sibling-tab! native))))))
      n)

    ;; dom-node? : any/c -> boolean?
    ;;   Check whether v is a browser-backed node.
    (define (dom-node? v)
      (dom-node-record? v))

    ;; dom-node-tag : dom-node? -> symbol?
    ;;   Return node kind tag.
    (define (dom-node-tag n)
      (dom-node-record-tag n))

    ;; dom-node-attrs : dom-node? -> list?
    ;;   Return current attribute alist.
    (define (dom-node-attrs n)
      (dom-node-record-attrs n))

    ;; dom-node-children : dom-node? -> list?
    ;;   Return current child node list.
    (define (dom-node-children n)
      (dom-node-record-children n))

    ;; dom-node-text : dom-node? -> any/c
    ;;   Return current text payload.
    (define (dom-node-text n)
      (dom-node-record-text n))

    ;; dom-node-on-click : dom-node? -> any/c
    ;;   Return click callback value.
    (define (dom-node-on-click n)
      (dom-node-record-on-click n))

    ;; dom-node-on-change : dom-node? -> any/c
    ;;   Return change callback value.
    (define (dom-node-on-change n)
      (dom-node-record-on-change n))

    ;; set-dom-node-tag! : dom-node? symbol? -> void?
    ;;   Mutate tag in record; native element kind is unchanged.
    (define (set-dom-node-tag! n tag)
      (set-dom-node-record-tag! n tag))

    ;; set-dom-node-attrs! : dom-node? list? -> void?
    ;;   Replace tracked attributes and sync native DOM attributes.
    (define (set-dom-node-attrs! n attrs)
      (define native (dom-node-record-native n))
      (clear-attributes! native (dom-node-record-attrs n))
      (set-dom-node-record-attrs! n attrs)
      (apply-attributes! n attrs)
      (void))

    ;; set-dom-node-children! : dom-node? list? -> void?
    ;;   Replace tracked child list.
    (define (set-dom-node-children! n children)
      (set-dom-node-record-children! n children))

    ;; set-dom-node-text! : dom-node? any/c -> void?
    ;;   Update tracked text and sync native DOM text child.
    (define (set-dom-node-text! n text)
      (set-dom-node-record-text! n text)
      (apply-text! (dom-node-record-native n) text)
      (void))

    ;; set-dom-node-on-click! : dom-node? any/c -> void?
    ;;   Update click callback.
    (define (set-dom-node-on-click! n on-click)
      (set-dom-node-record-on-click! n on-click))

    ;; set-dom-node-on-change! : dom-node? any/c -> void?
    ;;   Update change callback.
    (define (set-dom-node-on-change! n on-change)
      (set-dom-node-record-on-change! n on-change))

    ;; dom-node-native : dom-node? -> any/c
    ;;   Return wrapped browser DOM node handle.
    (define (dom-node-native n)
      (dom-node-record-native n))

    ;; backend-append-child! : dom-node? dom-node? -> void?
    ;;   Append child in model and browser DOM.
    (define (backend-append-child! parent child)
      (set-dom-node-record-children!
       parent
       (append (dom-node-record-children parent) (list child)))
      ((#%top . js-append-child!) (dom-node-record-native parent)
                                  (dom-node-record-native child))
      (void))

    ;; backend-set-single-child! : dom-node? dom-node? -> void?
    ;;   Replace children with one node in model and browser DOM.
    (define (backend-set-single-child! parent child)
      (set-dom-node-record-children! parent (list child))
      ((#%top . js-replace-children!) (dom-node-record-native parent)
                                      (dom-node-record-native child))
      (void))

    ;; backend-replace-children! : dom-node? list? -> void?
    ;;   Replace children list in model and browser DOM.
    (define (backend-replace-children! parent children)
      (set-dom-node-record-children! parent children)
      (define fragment ((#%top . js-create-document-fragment)))
      (for-each (lambda (child)
                  ((#%top . js-append-child!) fragment (dom-node-record-native child)))
                children)
      ((#%top . js-replace-children!) (dom-node-record-native parent) fragment)
      (void))

    ;; backend-mount-root! : dom-node? [any/c] -> void?
    ;;   Mount root node into browser container.
    ;;   Optional parameter container defaults to document body.
    (define (backend-mount-root! root [container ((#%top . js-document-body))])
      ((#%top . js-replace-children!) container (dom-node-record-native root))
      (void))

    (values dom-node
            dom-node?
            dom-node-tag
            dom-node-attrs
            dom-node-children
            dom-node-text
            dom-node-on-click
            dom-node-on-change
            set-dom-node-tag!
            set-dom-node-attrs!
            set-dom-node-children!
            set-dom-node-text!
            set-dom-node-on-click!
            set-dom-node-on-change!
            dom-node-native
            backend-append-child!
            backend-set-single-child!
            backend-replace-children!
            backend-mount-root!)))
