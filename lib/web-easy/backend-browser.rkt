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
    (define dialog-focusable-selector
      "button,[href],input,select,textarea,[tabindex]:not([tabindex='-1'])") ; CSS selector for focusable dialog controls.
    (define dialog-focus-returns '()) ; Association list mapping dialog native nodes to return-focus targets.

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
      (define fragment (js-create-document-fragment))
      (for-each (lambda (choice)
                  (define option (js-create-element "option"))
                  (define s (value->attr-string choice))
                  (js-set-attribute! option "value" s)
                  (js-replace-children! option (js-create-text-node s))
                  (js-append-child! fragment option))
                choices)
      (js-replace-children! native fragment))

    ;; dialog-open-attr? : list? -> boolean?
    ;;   Read boolean dialog open state from attrs.
    (define (dialog-open-attr? attrs)
      (define p (assq 'open attrs))
      (and p (not (eq? (cdr p) #f))))

    ;; popover-panel-attr? : list? -> boolean?
    ;;   Check whether attrs describe a popover panel node.
    (define (popover-panel-attr? attrs)
      (define p (assq 'data-we-widget attrs))
      (and p
           (string=? (value->attr-string (cdr p)) "popover-panel")))

    ;; popover-hidden-attr? : list? -> boolean?
    ;;   Read popover visibility state from aria-hidden attrs.
    (define (popover-hidden-attr? attrs)
      (define p (assq 'aria-hidden attrs))
      (if p
          (string=? (value->attr-string (cdr p)) "true")
          #t))

    ;; dialog-focus-return-set! : any/c any/c -> void?
    ;;   Track focus return target for dialog-native.
    (define (dialog-focus-return-set! dialog-native target)
      (set! dialog-focus-returns
            (cons (cons dialog-native target)
                  (let loop ([pairs dialog-focus-returns])
                    (cond
                      [(null? pairs) '()]
                      [(eq? (caar pairs) dialog-native) (cdr pairs)]
                      [else (cons (car pairs) (loop (cdr pairs)))])))))

    ;; dialog-focus-return-ref : any/c -> any/c
    ;;   Return focus return target for dialog-native or #f.
    (define (dialog-focus-return-ref dialog-native)
      (define p (assq dialog-native dialog-focus-returns))
      (if p (cdr p) #f))

    ;; dialog-focus-return-remove! : any/c -> void?
    ;;   Remove focus return target mapping for dialog-native.
    (define (dialog-focus-return-remove! dialog-native)
      (set! dialog-focus-returns
            (let loop ([pairs dialog-focus-returns])
              (cond
                [(null? pairs) '()]
                [(eq? (caar pairs) dialog-native) (cdr pairs)]
                [else (cons (car pairs) (loop (cdr pairs)))]))))

    ;; focus-first-dialog-target! : any/c -> void?
    ;;   Focus the first focusable dialog descendant, falling back to the dialog node.
    (define (focus-first-dialog-target! dialog-native)
      (define first-target
        (js-send/extern/nullish
         dialog-native
         "querySelector"
         (vector dialog-focusable-selector)))
      (if (and first-target
               (not (extern-nullish? first-target)))
          (js-send first-target "focus" (vector))
          (js-send dialog-native "focus" (vector))))

    ;; focus-cycled-dialog-target! : any/c boolean? -> void?
    ;;   Cycle focus inside dialog-native forward/backward depending on shift?.
    (define (focus-cycled-dialog-target! dialog-native shift?)
      ;; same-dom-node? : any/c any/c -> boolean?
      ;;   Compare external DOM nodes by JS identity semantics.
      (define (same-dom-node? a b)
        (and a
             b
             (not (extern-nullish? a))
             (not (extern-nullish? b))
             (extern-bool-true?
              (js-send/extern/nullish
               a
               "isSameNode"
               (vector b)))))
      (define items
        (js-send/extern/nullish
         dialog-native
         "querySelectorAll"
         (vector dialog-focusable-selector)))
      (if (or (not items)
              (extern-nullish? items))
          (js-send dialog-native "focus" (vector))
          (let ([count (nodelist-length items)])
            (if (= count 0)
                (js-send dialog-native "focus" (vector))
                (let* ([doc (js-document)]
                       [active* (js-ref/extern doc "activeElement")]
                       [active (if (or (not active*) (extern-nullish? active*))
                                   #f
                                   active*)]
                       [current-index
                        (let loop ([i 0])
                          (if (>= i count)
                              -1
                              (let ([candidate (nodelist-item items i)])
                                (if (and candidate (same-dom-node? candidate active))
                                    i
                                    (loop (add1 i))))))]
                       [target-index
                        (if (= current-index -1)
                            (if shift? (- count 1) 0)
                            (if shift?
                                (modulo (+ current-index (- count 1)) count)
                                (modulo (+ current-index 1) count)))]
                       [target (nodelist-item items target-index)])
                  (if target
                      (js-send target "focus" (vector))
                      (js-send dialog-native "focus" (vector))))))))

    ;; handle-dialog-open-transition! : any/c -> void?
    ;;   Capture return focus target and move focus into dialog.
    (define (handle-dialog-open-transition! dialog-native)
      (define doc (js-document))
      (define active* (js-ref/extern doc "activeElement"))
      (define active
        (if (or (not active*) (extern-nullish? active*))
            #f
            active*))
      (dialog-focus-return-set! dialog-native active)
      (focus-first-dialog-target! dialog-native))

    ;; handle-dialog-close-transition! : any/c -> void?
    ;;   Restore focus to tracked return target when available.
    (define (handle-dialog-close-transition! dialog-native)
      (define target (dialog-focus-return-ref dialog-native))
      (when (and target (not (extern-nullish? target)))
        (js-send target "focus" (vector)))
      (dialog-focus-return-remove! dialog-native))

    ;; handle-popover-open-transition! : any/c -> void?
    ;;   Focus popover panel when it transitions to visible.
    (define (handle-popover-open-transition! panel-native)
      (js-send panel-native "focus" (vector)))

    ;; handle-popover-close-transition! : any/c -> void?
    ;;   Return focus to sibling popover trigger when panel closes.
    (define (handle-popover-close-transition! panel-native)
      (define parent* (js-ref/extern panel-native "parentElement"))
      (when (and parent* (not (extern-nullish? parent*)))
        (define trigger*
          (js-send/extern/nullish
           parent*
           "querySelector"
           (vector ".we-popover-trigger")))
        (when (and trigger* (not (extern-nullish? trigger*)))
          (js-send trigger* "focus" (vector)))))

    ;; apply-attributes! : dom-node-record? list? list? -> void?
    ;;   Apply tracked attributes to native node and handle dialog open/close transitions.
    (define (apply-attributes! n old-attrs attrs)
      (define native (dom-node-record-native n))
      (define tag (dom-node-record-tag n))
      (define old-open? (dialog-open-attr? old-attrs))
      (define new-open? (dialog-open-attr? attrs))
      (define popover-panel? (popover-panel-attr? attrs))
      (define old-popover-hidden? (popover-hidden-attr? old-attrs))
      (define new-popover-hidden? (popover-hidden-attr? attrs))
      (when (or (eq? tag 'select)
                (eq? tag 'radios)
                (eq? tag 'choice))
        (define choices (alist-ref/default attrs 'choices '()))
        (sync-select-options! native choices))
      (for-each (lambda (a)
                  (define name (car a))
                  (define value (cdr a))
                  (case name
                    [(choices columns density menu-trigger open)
                     (void)]
                    [(selected)
                     (js-set! native "value" (value->attr-string value))]
                    [(value)
                     (js-set! native "value" (value->attr-string value))]
                    [(checked)
                     (js-set! native attr/checked (if value #t #f))]
                    [(on-enter-action)
                     (void)]
                    [else
                     (js-set-attribute! native
                                                  (symbol->attr-name name)
                                                  (value->attr-string value))]))
                attrs)
      (when (and (eq? tag 'dialog) (not old-open?) new-open?)
        (handle-dialog-open-transition! native))
      (when (and (eq? tag 'dialog) old-open? (not new-open?))
        (handle-dialog-close-transition! native))
      (when (and popover-panel? old-popover-hidden? (not new-popover-hidden?))
        (handle-popover-open-transition! native))
      (when (and popover-panel? (not old-popover-hidden?) new-popover-hidden?)
        (handle-popover-close-transition! native)))

    ;; apply-text! : any/c any/c -> void?
    ;;   Replace native node children with a single text child.
    (define (apply-text! native t)
      (if t
          (js-replace-children! native (js-create-text-node (value->attr-string t)))
          (js-replace-children! native (js-create-text-node ""))))

    ;; tag->element-name : symbol? -> string?
    ;;   Map backend tag symbol to HTML element name.
    (define (tag->element-name tag)
      (case tag
        [(window vpanel hpanel) "div"]
        [(style) "style"]
        [(group) "fieldset"]
        [(legend) "legend"]
        [(span) "span"]
        [(text) "span"]
        [(menu-item) "button"]
        [(button) "button"]
        [(input) "input"]
        [(checkbox) "input"]
        [(choice radios select) "select"]
        [(slider) "input"]
        [(progress) "progress"]
        [(spacer) "div"]
        [(table) "table"]
        [(tr) "tr"]
        [(th) "th"]
        [(td) "td"]
        [(menu-bar) "nav"]
        [(menu) "div"]
        [(image) "img"]
        [else "div"]))

    ;; install-default-node-shape! : dom-node-record? -> void?
    ;;   Apply default browser attributes based on node tag.
    (define (install-default-node-shape! n)
      (define native (dom-node-record-native n))
      (case (dom-node-record-tag n)
        [(checkbox) (js-set-attribute! native attr/type "checkbox")]
        [(slider) (js-set-attribute! native attr/type "range")]
        [(menu-item) (js-set-attribute! native attr/type "button")]
        [else (void)]))

    ;; node-change-value : dom-node-record? -> any/c
    ;;   Read change payload for the current node from browser attributes.
    (define (node-change-value n)
      (define native (dom-node-record-native n))
      (case (dom-node-record-tag n)
        [(checkbox)
         (let ([checked-string
                (js-value->string
                 (js-ref/extern native attr/checked))])
           (if (string=? checked-string "true") #t #f))]
        [(slider)
         (define raw
           (js-value->string
            (js-ref/extern native "value")))
         (define parsed (string->number raw))
         (if parsed parsed 0)]
        [else
         (js-value->string
          (js-ref/extern native "value"))]))

    ;; event-key-value : any/c -> string?
    ;;   Read keyboard event key as a plain string.
    (define (event-key-value evt)
      (js-value->string
       (js-ref/extern evt "key")))

    ;; tab-key-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a tab header button that should receive key navigation.
    (define (tab-key-node? n)
      (and (eq? (dom-node-record-tag n) 'button)
           (let ([p (assq 'role (dom-node-record-attrs n))])
             (and p (eq? (cdr p) 'tab)))))

    ;; role-button-node? : dom-node-record? -> boolean?
    ;;   Check whether n is button-like via role=button.
    (define (role-button-node? n)
      (let ([p (assq 'role (dom-node-record-attrs n))])
        (and p (eq? (cdr p) 'button))))

    ;; role-dialog-node? : dom-node-record? -> boolean?
    ;;   Check whether n is dialog-like via role=dialog.
    (define (role-dialog-node? n)
      (let ([p (assq 'role (dom-node-record-attrs n))])
        (and p (eq? (cdr p) 'dialog))))

    ;; input-enter-action : dom-node-record? -> any/c
    ;;   Return input Enter callback when present.
    (define (input-enter-action n)
      (and (eq? (dom-node-record-tag n) 'input)
           (let ([p (assq 'on-enter-action (dom-node-record-attrs n))])
             (and p (cdr p)))))

    ;; menu-trigger-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a menu trigger button.
    (define (menu-trigger-node? n)
      (let ([p (assq 'menu-trigger (dom-node-record-attrs n))])
        (and p (cdr p))))

    ;; nav-key? : string? -> boolean?
    ;;   Check whether key triggers tab keyboard navigation.
    (define (nav-key? key)
      (case (string->symbol key)
        [(ArrowRight ArrowLeft Home End) #t]
        [else #f]))

    ;; activation-key? : string? -> boolean?
    ;;   Check whether key should activate a button-like control.
    (define (activation-key? key)
      (or (string=? key "Enter")
          (string=? key " ")))

    ;; class-value : dom-node-record? -> string?
    ;;   Read class attribute string for n, defaulting to empty.
    (define (class-value n)
      (define p (assq 'class (dom-node-record-attrs n)))
      (if p
          (value->attr-string (cdr p))
          ""))

    ;; menu-label-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a menu label button.
    (define (menu-label-node? n)
      (string=? (class-value n) "we-menu-label"))

    ;; menu-item-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a menu item button.
    (define (menu-item-node? n)
      (string=? (class-value n) "we-menu-item"))

    ;; string->int/default : string? integer? -> integer?
    ;;   Parse s as integer and return default-value on failure.
    (define (string->int/default s default-value)
      (define maybe-n (string->number s))
      (if maybe-n
          maybe-n
          default-value))

    ;; nodelist-length : any/c -> integer?
    ;;   Read NodeList length as integer.
    (define (nodelist-length node-list)
      (string->int/default
       (js-value->string (js-ref/extern node-list "length"))
       0))

    ;; nodelist-item : any/c integer? -> any/c
    ;;   Read NodeList item at index idx.
    (define (nodelist-item node-list idx)
      (js-send/extern/nullish node-list "item" (vector idx)))

    ;; extern-nullish? : any/c -> boolean?
    ;;   Check whether external value encodes null or undefined.
    (define (extern-nullish? v)
      (define s (js-value->string v))
      (or (string=? s "null")
          (string=? s "undefined")))

    ;; extern-bool-true? : any/c -> boolean?
    ;;   Interpret external value as boolean true/false by string conversion.
    (define (extern-bool-true? v)
      (string=? (js-value->string v) "true"))

    ;; menu-container-node? : any/c -> boolean?
    ;;   Check whether external node looks like a top-level .we-menu container.
    (define (menu-container-node? node)
      (and node
           (not (extern-nullish? node))
           (let ([label
                  (js-send/extern/nullish
                   node
                   "querySelector"
                   (vector ".we-menu-label[role='button']"))])
             (and label #t))))

    ;; focusout-menu-container : dom-node-record? any/c -> any/c
    ;;   Return enclosing menu container native node for focusout checks, or #f.
    (define (focusout-menu-container n native)
      (cond
        [(menu-label-node? n)
         (define parent* (js-ref/extern native "parentElement"))
         (if (or (not parent*)
                 (extern-nullish? parent*))
             #f
             parent*)]
        [(menu-item-node? n)
         (define popup* (js-ref/extern native "parentElement"))
         (if (or (not popup*)
                 (extern-nullish? popup*))
             #f
             (let ([menu* (js-ref/extern popup* "parentElement")])
               (if (or (not menu*)
                       (extern-nullish? menu*))
                   #f
                   menu*)))]
        [else
         #f]))

    ;; focus-first-menu-item! : any/c -> void?
    ;;   Focus first menu item in popup for label-native.
    (define (focus-first-menu-item! label-native)
      (define popup-native (js-ref/extern label-native "nextElementSibling"))
      (when popup-native
        (define first-item
          (js-send/extern/nullish
           popup-native
           "querySelector"
           (vector ".we-menu-item[role='menuitem']")))
        (when first-item
          (js-send first-item "focus" (vector)))))

    ;; focus-last-menu-item! : any/c -> void?
    ;;   Focus last menu item in popup for label-native.
    (define (focus-last-menu-item! label-native)
      (define popup-native (js-ref/extern label-native "nextElementSibling"))
      (when popup-native
        (define item-list
          (js-send/extern/nullish
           popup-native
           "querySelectorAll"
           (vector ".we-menu-item[role='menuitem']")))
        (when item-list
          (define len (nodelist-length item-list))
          (when (> len 0)
            (define last-item (nodelist-item item-list (- len 1)))
            (when last-item
              (js-send last-item "focus" (vector)))))))

    ;; sibling-menu-native : any/c symbol? -> any/c
    ;;   Return adjacent menu container from from-menu using direction dir with wrap.
    (define (sibling-menu-native from-menu dir)
      (define raw*
        (case dir
          [(next) (js-ref/extern from-menu "nextElementSibling")]
          [(prev) (js-ref/extern from-menu "previousElementSibling")]
          [else #f]))
      (define raw
        (if (or (not raw*)
                (extern-nullish? raw*))
            #f
            raw*))
      (if (and raw (menu-container-node? raw))
          raw
          (let* ([bar-native* (js-ref/extern from-menu "parentElement")]
                 [bar-native (if (or (not bar-native*)
                                     (extern-nullish? bar-native*))
                                 #f
                                 bar-native*)])
            (if bar-native
                (case dir
                  [(next)
                   (js-send/extern/nullish
                    bar-native
                    "querySelector"
                    (vector ".we-menu"))]
                  [(prev)
                   (let ([menu-list
                          (js-send/extern/nullish
                           bar-native
                           "querySelectorAll"
                           (vector ".we-menu"))])
                     (if menu-list
                         (let ([len (nodelist-length menu-list)])
                           (if (> len 0)
                               (nodelist-item menu-list (- len 1))
                               #f))
                         #f))]
                  [else
                   #f])
                #f))))

    ;; switch-menu-from-label! : any/c symbol? -> void?
    ;;   Open sibling menu from label-native in direction dir and focus sibling label.
    (define (switch-menu-from-label! label-native dir)
      (define from-menu (js-ref/extern label-native "parentElement"))
      (when from-menu
        (define to-menu (sibling-menu-native from-menu dir))
        (when to-menu
          (define to-label
            (js-send/extern/nullish
             to-menu
             "querySelector"
             (vector ".we-menu-label[role='button']")))
          (when to-label
            (js-send to-label "click" (vector))
            (js-send to-label "focus" (vector))))))

    ;; focus-edge-menu-label! : any/c symbol? -> void?
    ;;   Open and focus first/last top-level menu label for label-native.
    (define (focus-edge-menu-label! label-native edge)
      (define from-menu (js-ref/extern label-native "parentElement"))
      (when from-menu
        (define bar-native (js-ref/extern from-menu "parentElement"))
        (when bar-native
          (define target-label
            (case edge
              [(first)
               (js-send/extern/nullish
                bar-native
                "querySelector"
                (vector ".we-menu-label[role='button']"))]
              [(last)
               (let ([label-list
                      (js-send/extern/nullish
                       bar-native
                       "querySelectorAll"
                       (vector ".we-menu-label[role='button']"))])
                 (if label-list
                     (let ([len (nodelist-length label-list)])
                       (if (> len 0)
                           (nodelist-item label-list (- len 1))
                           #f))
                     #f))]
              [else
               #f]))
          (when target-label
            (js-send target-label "click" (vector))
            (js-send target-label "focus" (vector))))))

    ;; switch-menu-from-item! : any/c symbol? -> void?
    ;;   Open sibling menu from item-native in direction dir and focus sibling label.
    (define (switch-menu-from-item! item-native dir)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (when popup-native
        (define from-menu (js-ref/extern popup-native "parentElement"))
        (when from-menu
          (define to-menu (sibling-menu-native from-menu dir))
          (when to-menu
            (define to-label
              (js-send/extern/nullish
               to-menu
               "querySelector"
               (vector ".we-menu-label[role='button']")))
            (when to-label
              (js-send to-label "click" (vector))
              (js-send to-label "focus" (vector)))))))

    ;; focus-next-menu-item! : any/c -> void?
    ;;   Move focus to next item in same popup, clamping at last.
    (define (focus-next-menu-item! item-native)
      (define next-item (js-ref/extern item-native "nextElementSibling"))
      (if next-item
          (js-send next-item "focus" (vector))
          (js-send item-native "focus" (vector))))

    ;; focus-prev-menu-item! : any/c -> void?
    ;;   Move focus to previous item in same popup, clamping at first.
    (define (focus-prev-menu-item! item-native)
      (define prev-item (js-ref/extern item-native "previousElementSibling"))
      (if prev-item
          (js-send prev-item "focus" (vector))
          (js-send item-native "focus" (vector))))

    ;; focus-own-menu-label! : any/c -> void?
    ;;   Focus top-level menu label for item-native.
    (define (focus-own-menu-label! item-native)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (when popup-native
        (define menu-native (js-ref/extern popup-native "parentElement"))
        (when menu-native
          (define label-native
            (js-send/extern/nullish
             menu-native
             "querySelector"
             (vector ".we-menu-label[role='button']")))
          (when label-native
            (js-send label-native "focus" (vector))))))

    ;; menu-typeahead-key? : string? -> boolean?
    ;;   Check whether key should trigger menu type-ahead focus behavior.
    (define (menu-typeahead-key? key)
      (and (= (string-length key) 1)
           (char-alphabetic? (string-ref key 0))))

    ;; node-text-downcase : any/c -> string?
    ;;   Read node textContent as lowercase string.
    (define (node-text-downcase node)
      (string-downcase
       (js-value->string
        (js-ref/extern node "textContent"))))

    ;; node-first-non-space-char : any/c -> any/c
    ;;   Return first non-whitespace character from node text or #f.
    (define (node-first-non-space-char node)
      (define text (node-text-downcase node))
      (let loop ([idx 0])
        (cond
          [(>= idx (string-length text))
           #f]
          [(char-whitespace? (string-ref text idx))
           (loop (add1 idx))]
          [else
           (string-ref text idx)])))

    ;; menu-item-matches-key? : any/c string? -> boolean?
    ;;   Check whether item's visible label starts with key (ignoring leading spaces).
    (define (menu-item-matches-key? item-native key)
      (define key-ch (char-downcase (string-ref key 0)))
      (define first-ch (node-first-non-space-char item-native))
      (and first-ch
           (char=? first-ch key-ch)))

    ;; extern-node-same? : any/c any/c -> boolean?
    ;;   Check whether a and b refer to the same DOM node.
    (define (extern-node-same? a b)
      (and a
           b
           (extern-bool-true?
            (js-send/extern/nullish a "isSameNode" (vector b)))))

    ;; nodelist-index-of-node : any/c any/c -> integer?
    ;;   Return index of node in node-list, or -1 when not present.
    (define (nodelist-index-of-node node-list node)
      (define len (nodelist-length node-list))
      (let loop ([idx 0])
        (cond
          [(>= idx len)
           -1]
          [else
           (define candidate (nodelist-item node-list idx))
           (if (and candidate (extern-node-same? candidate node))
               idx
               (loop (add1 idx)))])))

    ;; focus-matching-menu-item-from-list! : any/c integer? string? -> boolean?
    ;;   Focus first item in node-list matching key, scanning from start with wrap.
    (define (focus-matching-menu-item-from-list! node-list start key)
      (define len (nodelist-length node-list))
      (let loop ([offset 0])
        (cond
          [(or (= len 0)
               (>= offset len))
           #f]
          [else
           (define idx (modulo (+ start offset) len))
           (define item (nodelist-item node-list idx))
           (if (and item (menu-item-matches-key? item key))
               (begin
                 (js-send item "focus" (vector))
                 #t)
               (loop (add1 offset)))])))

    ;; focus-matching-menu-item-from-label! : any/c string? -> boolean?
    ;;   Open label popup (if needed) and focus first matching item for key.
    (define (focus-matching-menu-item-from-label! label-native key)
      (define expanded
        (js-value->string
         (js-send/extern/nullish label-native "getAttribute" (vector "aria-expanded"))))
      (when (not (string=? expanded "true"))
        (js-send label-native "click" (vector)))
      (define popup-native (js-ref/extern label-native "nextElementSibling"))
      (if popup-native
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if item-list
                (focus-matching-menu-item-from-list! item-list 0 key)
                #f))
          #f))

    ;; focus-matching-menu-item-from-item! : any/c string? -> boolean?
    ;;   Focus next matching item in same popup for key, wrapping in menu order.
    (define (focus-matching-menu-item-from-item! item-native key)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (if popup-native
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if item-list
                (let* ([idx (nodelist-index-of-node item-list item-native)]
                       [len (nodelist-length item-list)]
                       [start (if (and (>= idx 0) (> len 0))
                                  (modulo (add1 idx) len)
                                  0)])
                  (focus-matching-menu-item-from-list! item-list start key))
                #f))
          #f))

    ;; focus-selected-sibling-tab! : any/c -> void?
    ;;   Focus selected tab button in the same tablist as native.
    (define (focus-selected-sibling-tab! native)
      (define parent (js-ref/extern native "parentElement"))
      (when parent
        (define selected
          (js-send/extern/nullish
           parent
           "querySelector"
           (vector "button[aria-selected='true']")))
        (when selected
          (js-send selected "focus" (vector)))))

    ;; dom-node : symbol? list? list? any/c any/c any/c -> dom-node?
    ;;   Construct a browser-backed node and install event bridges.
    (define (dom-node tag attrs children text on-click on-change)
      (define native (js-create-element (tag->element-name tag)))
      (define n (dom-node-record tag attrs children text on-click on-change native))
      (install-default-node-shape! n)
      (apply-attributes! n '() attrs)
      (when text
        (apply-text! native text))
      (js-add-event-listener!
       native
       "click"
       (procedure->external
        (lambda (_evt)
          (define callback (dom-node-record-on-click n))
          (when callback
            (callback)))))
      (js-add-event-listener!
       native
       "change"
       (procedure->external
        (lambda (_evt)
          (define callback (dom-node-record-on-change n))
          (when callback
            (callback (node-change-value n))))))
      (when (eq? tag 'input)
        (js-add-event-listener!
         native
         "input"
         (procedure->external
          (lambda (_evt)
            (define callback (dom-node-record-on-change n))
            (when callback
              (callback (node-change-value n)))))))
      (js-add-event-listener!
       native
       "keydown"
       (procedure->external
        (lambda (evt)
          (define key (event-key-value evt))
          (define on-enter (input-enter-action n))
          (when (and on-enter (string=? key "Enter"))
            (js-send evt "preventDefault" (vector))
            (on-enter))
          (define on-click (dom-node-record-on-click n))
          (define role-pair (assq 'role (dom-node-record-attrs n)))
          (when (and on-click
                     (or (eq? tag 'button)
                         (and role-pair
                              (or (eq? (cdr role-pair) 'button)
                                  (eq? (cdr role-pair) 'menuitem))))
                     (activation-key? key))
            (js-send evt "preventDefault" (vector))
            (on-click))
          (define callback (dom-node-record-on-change n))
          (when (and callback (tab-key-node? n))
            (when (nav-key? key)
              (js-send evt "preventDefault" (vector)))
            (callback key)
            (when (nav-key? key)
              (focus-selected-sibling-tab! native)))
          (when (menu-label-node? n)
            (case (string->symbol key)
              [(ArrowDown)
               (js-send evt "preventDefault" (vector))
               (focus-first-menu-item! native)]
              [(ArrowUp)
               (js-send evt "preventDefault" (vector))
               (focus-last-menu-item! native)]
              [(ArrowRight)
               (js-send evt "preventDefault" (vector))
               (switch-menu-from-label! native 'next)]
              [(ArrowLeft)
               (js-send evt "preventDefault" (vector))
               (switch-menu-from-label! native 'prev)]
              [(Home)
               (js-send evt "preventDefault" (vector))
               (focus-edge-menu-label! native 'first)]
              [(End)
               (js-send evt "preventDefault" (vector))
               (focus-edge-menu-label! native 'last)]
              [(Tab)
               (when callback
                 (callback "focusout"))]
              [else
               (when (menu-typeahead-key? key)
                 (when (focus-matching-menu-item-from-label! native key)
                   (js-send evt "preventDefault" (vector))))]))
          (when (menu-item-node? n)
            (case (string->symbol key)
              [(ArrowDown)
               (js-send evt "preventDefault" (vector))
               (focus-next-menu-item! native)]
              [(ArrowUp)
               (js-send evt "preventDefault" (vector))
               (focus-prev-menu-item! native)]
              [(ArrowRight)
               (js-send evt "preventDefault" (vector))
               (switch-menu-from-item! native 'next)]
              [(ArrowLeft)
               (js-send evt "preventDefault" (vector))
               (switch-menu-from-item! native 'prev)]
              [(Tab)
               (when callback
                 (callback "focusout"))]
              [(Escape)
               (when callback
                 (callback "Escape"))
               (focus-own-menu-label! native)]
              [else
               (when (menu-typeahead-key? key)
                 (when (focus-matching-menu-item-from-item! native key)
                   (js-send evt "preventDefault" (vector))))]))
          (when (and callback (role-button-node? n))
            (callback key))
          (when (and (role-dialog-node? n) (string=? key "Tab"))
            (js-send evt "preventDefault" (vector))
            (define shift? (extern-bool-true? (js-ref/extern evt "shiftKey")))
            (when (dialog-open-attr? (dom-node-record-attrs n))
              (focus-cycled-dialog-target! native shift?)))
          (when (and callback (role-dialog-node? n))
            (callback key)))))
      (js-add-event-listener!
       native
       "mouseenter"
       (procedure->external
        (lambda (_evt)
          (define callback (dom-node-record-on-change n))
          (when (and callback (menu-trigger-node? n))
            (callback "mouseenter")))))
      (js-add-event-listener!
       native
       "focusout"
       (procedure->external
        (lambda (evt)
          (define callback (dom-node-record-on-change n))
          (define menu-container (focusout-menu-container n native))
          (when (and callback menu-container)
            (define related* (js-ref/extern evt "relatedTarget"))
            (define related
              (if (or (not related*)
                      (extern-nullish? related*))
                  #f
                  related*))
            (define still-inside?
              (and related
                   (extern-bool-true?
                    (js-send/extern/nullish
                     menu-container
                     "contains"
                     (vector related)))))
            (unless still-inside?
              (callback "focusout"))))))
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
      (define old-attrs (dom-node-record-attrs n))
      (clear-attributes! native old-attrs)
      (set-dom-node-record-attrs! n attrs)
      (apply-attributes! n old-attrs attrs)
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
      (js-append-child! (dom-node-record-native parent)
                                  (dom-node-record-native child))
      (void))

    ;; backend-set-single-child! : dom-node? dom-node? -> void?
    ;;   Replace children with one node in model and browser DOM.
    (define (backend-set-single-child! parent child)
      (set-dom-node-record-children! parent (list child))
      (js-replace-children! (dom-node-record-native parent)
                                      (dom-node-record-native child))
      (void))

    ;; backend-replace-children! : dom-node? list? -> void?
    ;;   Replace children list in model and browser DOM.
    (define (backend-replace-children! parent children)
      (set-dom-node-record-children! parent children)
      (define fragment (js-create-document-fragment))
      (for-each (lambda (child)
                  (js-append-child! fragment (dom-node-record-native child)))
                children)
      (js-replace-children! (dom-node-record-native parent) fragment)
      (void))

    ;; backend-mount-root! : dom-node? [any/c] -> void?
    ;;   Mount root node into browser container.
    ;;   Optional parameter container defaults to document body.
    (define (backend-mount-root! root [container (js-document-body)])
      (js-replace-children! container (dom-node-record-native root))
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
