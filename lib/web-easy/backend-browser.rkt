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
;;   dom-node-event-handlers  Access generic event callback alist.
;;   set-dom-node-tag!        Mutate node tag.
;;   set-dom-node-attrs!      Mutate node attributes and sync browser node attributes.
;;   set-dom-node-children!   Mutate node children list.
;;   set-dom-node-text!       Mutate node text and sync browser node text.
;;   set-dom-node-on-click!   Mutate node click callback.
;;   set-dom-node-on-change!  Mutate node change callback.
;;   set-dom-node-event-handlers!  Mutate generic event callback alist.
;;   dom-node-native          Return host-native DOM node handle.
;;   backend-append-child!    Append child to parent node in model and browser DOM.
;;   backend-set-single-child! Replace node children with a single child in model and browser DOM.
;;   backend-replace-children! Replace node children with a child list in model and browser DOM.
;;   backend-mount-root!      Mount root node into browser container.
;;   backend-scrollspy-observe-scroll!  Register scroll observer callback for a container.
;;   backend-scrollspy-scroll-into-view! Scroll section node into view in the browser.
;;   backend-scrollspy-active-id  Compute active scrollspy id from section bindings.
;;   backend-set-timeout!     Register timeout callback in browser host.
;;   backend-clear-timeout!   Clear timeout callback handle in browser host.

(define-values
  (dom-node
   dom-node?
   dom-node-tag
   dom-node-attrs
   dom-node-children
   dom-node-text
   dom-node-on-click
   dom-node-on-change
   dom-node-event-handlers
   set-dom-node-tag!
   set-dom-node-attrs!
   set-dom-node-children!
   set-dom-node-text!
   set-dom-node-on-click!
   set-dom-node-on-change!
   set-dom-node-event-handlers!
   dom-node-native
   backend-append-child!
   backend-set-single-child!
   backend-replace-children!
   backend-mount-root!
   backend-scrollspy-observe-scroll!
   backend-scrollspy-scroll-into-view!
   backend-scrollspy-active-id
   backend-set-timeout!
   backend-clear-timeout!)
  (let ()
    (struct dom-node-record (tag attrs children text on-click on-change event-handlers native)
      #:mutable
      #:transparent)

    ;; primitive-dom-event-names : list?
    ;;   Supported generic primitive DOM event names bridged directly in the browser backend.
    (define primitive-dom-event-names
      '("click"
        "doubleclick"
        "contextmenu"
        "copy"
        "cut"
        "paste"
        "compositionstart"
        "compositionupdate"
        "compositionend"
        "keydown"
        "keyup"
        "focus"
        "blur"
        "focusin"
        "focusout"
        "input"
        "change"
        "beforeinput"
        "submit"
        "reset"
        "invalid"
        "wheel"
        "scroll"
        "drag"
        "dragstart"
        "dragend"
        "dragenter"
        "dragleave"
        "dragover"
        "drop"
        "touchstart"
        "touchmove"
        "touchend"
        "touchcancel"
        "load"
        "error"
        "abort"
        "animationstart"
        "animationend"
        "animationiteration"
        "transitionend"
        "mousedown"
        "mousemove"
        "mouseup"
        "mouseenter"
        "mouseleave"
        "mouseover"
        "mouseout"
        "pointerdown"
        "pointermove"
        "pointerup"
        "pointerenter"
        "pointerleave"
        "pointerover"
        "pointerout"
        "pointercancel"
        "gotpointercapture"
        "lostpointercapture"
        "loadeddata"
        "loadedmetadata"
        "canplay"
        "canplaythrough"
        "play"
        "playing"
        "pause"
        "ended"
        "timeupdate"
        "volumechange"))

    ;; Constants used by browser backend DOM translation.
    (define attr/type    "type")    ; Attribute name for input element type.
    (define attr/value   "value")   ; Attribute name for value-bearing controls.
    (define attr/checked "checked") ; Property/attribute name for checked controls.
    (define dialog-focusable-selector
      "button,[href],input,select,textarea,[tabindex]:not([tabindex='-1'])") ; CSS selector for focusable dialog controls.
    (define scrollspy-section-selector
      "[data-we-widget='scrollspy-section']") ; Selector used to observe scrollspy sections.
    (define scrollspy-item-selector
      ".we-scrollspy-item") ; Selector used for scrollspy keyboard roving.
    (define dialog-focus-returns         '()) ; Association list mapping dialog native nodes to return-focus targets.
    (define dialog-focus-timeouts        '()) ; Association list mapping dialog native nodes to pending focus timeout handles.
    (define scrollspy-observers          '()) ; Association list mapping scrollspy container native nodes to observer handles.
    (define scrollspy-listeners          '()) ; Association list mapping scrollspy container native nodes to listener handles.
    (define scrollspy-cleanup-registered '()) ; Native nodes with registered scrollspy cleanup callbacks.
    (define dom-node-listeners           '()) ; Association list mapping native DOM nodes to installed event-listener pairs.
    (define menu-typeahead-timeout-ms    700) ; Milliseconds before menu typeahead prefix resets.
    (define menu-typeahead-state         '()) ; Association list mapping popup native nodes to (cons timestamp-ms prefix).

    ;; symbol->attr-name : symbol? -> string?
    ;;   Convert attribute symbol key to browser DOM attribute name.
    (define (symbol->attr-name key)
      (symbol->string key))

    ;; value->attr-string : any/c -> string?
    ;;   Convert attribute value to browser DOM attribute string.
    (define (value->attr-string v)
      (cond
        [(string? v)  v]
        [(symbol? v)  (symbol->string v)]
        [(number? v)  (number->string v)]
        [(boolean? v) (if v "true" "false")]
        [(list? v)
         (let loop ([rest v] [acc ""])
           (cond
             [(null? rest) acc]
             [(string=? acc "")
              (loop (cdr rest) (value->attr-string (car rest)))]
             [else
              (loop (cdr rest)
                    (string-append acc "," (value->attr-string (car rest))))]))]
        [else "~unprintable~"]))

    ;; clear-attributes! : any/c list? -> void?
    ;;   Clear pass is currently a no-op for portability across host primitive sets.
    (define (clear-attributes! native attrs)
      (void))

    ;; alist-ref/default : list? symbol? any/c -> any/c
    ;;   Lookup key in attrs and return default when missing.
    (define (alist-ref/default attrs key default)
      (define p (assq key attrs))
      (if p (cdr p) default))

    ;; tag->display-name : symbol? -> string?
    ;;   Convert a primitive tag into a readable callback-name prefix.
    (define (tag->display-name tag)
      (define raw (symbol->string tag))
      (cond
        [(string=? raw "") "Node"]
        [else
         (string-append (string-upcase (substring raw 0 1))
                        (substring raw 1))]))

    ;; name-part->string : any/c -> (or/c string? #f)
    ;;   Convert name-like value to string, or #f when it has no useful string form.
    (define (name-part->string v)
      (cond
        [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [else #f]))

    ;; callback-context-name : (or/c symbol? #f) list? string? -> symbol?
    ;;   Derive a contextual callback name from tag/attrs and backend role.
    (define (callback-context-name tag attrs role)
      (define id-text
        (name-part->string (alist-ref/default attrs 'id #f)))
      (define widget-text
        (name-part->string (alist-ref/default attrs 'data-we-widget #f)))
      (define base-name
        (if tag
            (let ([tag-name (tag->display-name tag)])
              (cond
                [id-text     (string-append tag-name ":" id-text ":" role)]
                [widget-text (string-append tag-name ":" widget-text ":" role)]
                [else        (string-append tag-name ":" role)]))
            role))
      (string->symbol base-name))

    ;; callback-display-name : (or/c symbol? #f) list? string? any/c -> symbol?
    ;;   Preserve explicit callback names and derive contextual names for anonymous callbacks.
    (define (callback-display-name tag attrs role callback)
      (define explicit-name
        (and callback
             (name-part->string (object-name callback))))
      (if explicit-name
          (string->symbol explicit-name)
          (callback-context-name tag attrs role)))

    ;; contextual-procedure->external : (or/c symbol? #f) list? string? any/c procedure? -> any/c
    ;;   Rename wrapper for JS-side diagnostics before converting it to an external callback.
    (define (contextual-procedure->external tag attrs role callback wrapper)
      (procedure->external
       (procedure-rename wrapper
                         (callback-display-name tag attrs role callback))))

    ;; sync-select-options! : any/c list? list? -> void?
    ;;   Replace select children with option nodes for choices/option-pairs.
    (define (sync-select-options! native choices option-pairs)
      (define fragment (js-create-document-fragment))
      (define rows
        (if (null? option-pairs)
            (map (lambda (choice)
                   (cons (value->attr-string choice)
                         (value->attr-string choice)))
                 choices)
            option-pairs))
      (for-each (lambda (row)
                  (define option (js-create-element "option"))
                  (define value-text (car row))
                  (define label-text (cdr row))
                  (js-set-attribute! option "value" value-text)
                  (js-replace-children! option (js-create-text-node label-text))
                  (js-append-child! fragment option))
                rows)
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

    ;; autofocus-attr? : list? -> boolean?
    ;;   Read mount-time autofocus intent from attrs.
    (define (autofocus-attr? attrs)
      (define p (assq 'autofocus attrs))
      (and p (cdr p) #t))

    ;; focus-native-later! : any/c -> void?
    ;;   Schedule focus on native node after the current mount/update turn.
    (define (focus-native-later! native)
      (when (and native (not (extern-nullish? native)))
        (backend-set-timeout!
         0.0
         (lambda ()
           (js-send native "focus" (vector))))))

    ;; backend-set-timeout! : number? (-> void?) -> any/c
    ;;   Register callback to run after duration-ms; returns timeout handle.
    (define (backend-set-timeout! duration-ms callback)
      (js-send (js-window-window)
               "setTimeout"
               (vector (contextual-procedure->external
                        #f '() "timeout-callback" callback
                        callback)
                       duration-ms)))

    ;; backend-clear-timeout! : any/c -> void?
    ;;   Clear timeout callback handle.
    (define (backend-clear-timeout! handle)
      (when handle
        (js-send (js-window-window) "clearTimeout" (vector handle))))

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

    ;; dialog-focus-timeout-set! : any/c any/c -> void?
    ;;   Track pending focus timeout handle for dialog-native.
    (define (dialog-focus-timeout-set! dialog-native handle)
      (set! dialog-focus-timeouts
            (cons (cons dialog-native handle)
                  (let loop ([pairs dialog-focus-timeouts])
                    (cond
                      [(null? pairs) '()]
                      [(eq? (caar pairs) dialog-native) (cdr pairs)]
                      [else (cons (car pairs) (loop (cdr pairs)))])))))

    ;; dialog-focus-timeout-ref : any/c -> any/c
    ;;   Return pending focus timeout handle for dialog-native or #f.
    (define (dialog-focus-timeout-ref dialog-native)
      (define p (assq dialog-native dialog-focus-timeouts))
      (if p (cdr p) #f))

    ;; dialog-focus-timeout-remove! : any/c -> void?
    ;;   Remove pending focus timeout mapping for dialog-native.
    (define (dialog-focus-timeout-remove! dialog-native)
      (set! dialog-focus-timeouts
            (let loop ([pairs dialog-focus-timeouts])
              (cond
                [(null? pairs) '()]
                [(eq? (caar pairs) dialog-native) (cdr pairs)]
                [else (cons (car pairs) (loop (cdr pairs)))]))))

    ;; clear-pending-dialog-focus-timeout! : any/c -> void?
    ;;   Clear and forget pending deferred focus timer for dialog-native.
    (define (clear-pending-dialog-focus-timeout! dialog-native)
      (define handle (dialog-focus-timeout-ref dialog-native))
      (when handle
        (backend-clear-timeout! handle))
      (dialog-focus-timeout-remove! dialog-native))

    ;; native-mapping-ref : list? any/c -> any/c
    ;;   Lookup native key in mappings and return value or #f when missing.
    (define (native-mapping-ref mappings native)
      (define p (assq native mappings))
      (if p (cdr p) #f))

    ;; native-mapping-set : list? any/c any/c -> list?
    ;;   Return mappings updated with native => value (replacing prior entry).
    (define (native-mapping-set mappings native value)
      (cons (cons native value)
            (let loop ([pairs mappings])
              (cond
                [(null? pairs) '()]
                [(eq? (caar pairs) native) (cdr pairs)]
                [else (cons (car pairs) (loop (cdr pairs)))]))))

    ;; native-mapping-remove : list? any/c -> list?
    ;;   Return mappings without native entry.
    (define (native-mapping-remove mappings native)
      (let loop ([pairs mappings])
        (cond
          [(null? pairs) '()]
          [(eq? (caar pairs) native) (cdr pairs)]
          [else (cons (car pairs) (loop (cdr pairs)))])))

    ;; native-member? : list? any/c -> boolean?
    ;;   Return #t when native is tracked in natives list.
    (define (native-member? natives native)
      (let loop ([xs natives])
        (cond
          [(null? xs) #f]
          [(eq? (car xs) native) #t]
          [else (loop (cdr xs))])))

    ;; native-add-unique : list? any/c -> list?
    ;;   Add native to list only when not already present.
    (define (native-add-unique natives native)
      (if (native-member? natives native)
          natives
          (cons native natives)))

    ;; native-remove : list? any/c -> list?
    ;;   Remove native from list if present.
    (define (native-remove natives native)
      (let loop ([xs natives])
        (cond
          [(null? xs) '()]
          [(eq? (car xs) native) (cdr xs)]
          [else (cons (car xs) (loop (cdr xs)))])))

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
        (and (extern-present? a)
             (extern-present? b)
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
                                (if (and (extern-present? candidate)
                                         (same-dom-node? candidate active))
                                    i
                                    (loop (add1 i))))))]
                       [target-index
                        (if (= current-index -1)
                            (if shift? (- count 1) 0)
                            (if shift?
                                (modulo (+ current-index (- count 1)) count)
                                (modulo (+ current-index 1) count)))]
                       [target (nodelist-item items target-index)])
                  (if (extern-present? target)
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
      (clear-pending-dialog-focus-timeout! dialog-native)
      (dialog-focus-timeout-set!
       dialog-native
       (backend-set-timeout!
        0
        (lambda ()
          (dialog-focus-timeout-remove! dialog-native)
          (focus-first-dialog-target! dialog-native)))))

    ;; handle-dialog-close-transition! : any/c -> void?
    ;;   Restore focus to tracked return target when available.
    (define (handle-dialog-close-transition! dialog-native)
      (clear-pending-dialog-focus-timeout! dialog-native)
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

    (define boolean-attributes
      '(hidden
        inert
        itemscope
        async
        defer
        nomodule
        novalidate
        formnovalidate
        autofocus
        disabled
        readonly
        required
        checked
        multiple
        selected
        autoplay
        controls
        loop
        muted
        playsinline
        ismap
        reversed
        open))

    (define (boolean-attribute? x)
      (and (memq x boolean-attributes) #t))

    ;; apply-attributes! : dom-node-record? list? list? -> void?
    ;;   Apply tracked attributes to native node and handle dialog open/close transitions.
    (define (apply-attributes! n old-attrs attrs)
      (define native              (dom-node-record-native n))
      (define tag                 (dom-node-record-tag n))
      (define old-open?           (dialog-open-attr? old-attrs))
      (define new-open?           (dialog-open-attr? attrs))
      (define old-autofocus?      (autofocus-attr? old-attrs))
      (define new-autofocus?      (autofocus-attr? attrs))
      (define popover-panel?      (popover-panel-attr? attrs))
      (define old-popover-hidden? (popover-hidden-attr? old-attrs))
      (define new-popover-hidden? (popover-hidden-attr? attrs))

      (when (or (eq? tag 'select)
                (eq? tag 'choice))
        (define choices      (alist-ref/default attrs 'choices '()))
        (define option-pairs (alist-ref/default attrs 'option-pairs '()))
        (sync-select-options! native choices option-pairs))
      
      (for-each (lambda (a)
                  (define name  (car a))
                  (define value (cdr a))
                  (case name
                    [(choices option-pairs columns density menu-trigger)
                     (void)]
                    [(open)
                     (when (eq? tag 'dialog)
                       (js-set! native "open" (if value #t #f)))]
                    [(selected)
                     (when (or (eq? tag 'select)
                               (eq? tag 'choice))
                       (js-set! native "value" (value->attr-string value)))]
                    [(value)
                     (js-set! native "value" (value->attr-string value))]
                    [(checked)
                     (js-set! native attr/checked (if value #t #f))]
                    [(on-enter-action)
                     (void)]
                    [else
                     (define attr-name  (symbol->attr-name name))
                     (define attr-value (value->attr-string value))
                     (cond
                       [(boolean-attribute? name)
                        (if value
                            (js-set-attribute!    native attr-name attr-value)
                            (js-remove-attribute! native attr-name))]
                       [else
                        (js-set-attribute!    native attr-name attr-value)])]))
                attrs)
      (when (and (eq? tag 'dialog) (not old-open?) new-open?)
        (handle-dialog-open-transition! native))
      (when (and (eq? tag 'dialog) old-open? (not new-open?))
        (handle-dialog-close-transition! native))
      (when (and popover-panel? old-popover-hidden? (not new-popover-hidden?))
        (handle-popover-open-transition! native))
      (when (and popover-panel? (not old-popover-hidden?) new-popover-hidden?)
        (handle-popover-close-transition! native))
      (when (and (not old-autofocus?) new-autofocus?)
        (focus-native-later! native)))

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
        [(label) "label"]
        [(span) "span"]
        [(small) "small"]
        [(p) "p"]
        [(h1) "h1"]
        [(h2) "h2"]
        [(h3) "h3"]
        [(h4) "h4"]
        [(h5) "h5"]
        [(h6) "h6"]
        [(text) "span"]
        [(menu-item) "button"]
        [(button) "button"]
        [(a) "a"]
        [(hr) "hr"]
        [(input) "input"]
        [(textarea) "textarea"]
        [(checkbox) "input"]
        [(choice select) "select"]
        [(radios) "div"]
        [(slider) "input"]
        [(progress) "progress"]
        [(spacer) "div"]
        [(table) "table"]
        [(tr) "tr"]
        [(th) "th"]
        [(td) "td"]
        [(menu-bar) "nav"]
        [(menu) "div"]
        [(div) "div"]
        [(section) "section"]
        [(article) "article"]
        [(nav) "nav"]
        [(main) "main"]
        [(header) "header"]
        [(footer) "footer"]
        [(aside) "aside"]
        [(form) "form"]
        [(ul) "ul"]
        [(ol) "ol"]
        [(li) "li"]
        [(img) "img"]
        [(image) "img"]
        ;; Allow primitive html-element tags that are not explicitly listed above.
        [else (symbol->string tag)]))

    ;; install-default-node-shape! : dom-node-record? -> void?
    ;;   Apply default browser attributes based on node tag.
    (define (install-default-node-shape! n)
      (define native (dom-node-record-native n))
      (case (dom-node-record-tag n)
        [(checkbox)  (js-set-attribute! native attr/type "checkbox")]
        [(slider)    (js-set-attribute! native attr/type "range")]
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
        [(input)
         (define type-pair
           (assq 'type (dom-node-record-attrs n)))
         (if (and type-pair
                  (string? (cdr type-pair))
                  (string=? (cdr type-pair) "checkbox"))
             (let ([checked-string
                    (js-value->string
                     (js-ref/extern native attr/checked))])
               (if (string=? checked-string "true") #t #f))
             (js-value->string
              (js-ref/extern native "value")))]
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

    ;; widget-value : dom-node-record? -> string?
    ;;   Read data-we-widget attribute string for n, defaulting to empty.
    (define (widget-value n)
      (define p (assq 'data-we-widget (dom-node-record-attrs n)))
      (if p
          (value->attr-string (cdr p))
          ""))

    ;; menu-label-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a menu label button.
    (define (menu-label-node? n)
      (string=? (widget-value n) "menu-label"))

    ;; menu-item-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a menu item button.
    (define (menu-item-node? n)
      (string=? (widget-value n) "menu-item"))

    ;; menu-popup-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a standalone menu popup container.
    (define (menu-popup-node? n)
      (string=? (widget-value n) "menu-popup"))

    ;; scrollspy-item-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a scrollspy navigation item button.
    (define (scrollspy-item-node? n)
      (string=? (widget-value n) "scrollspy-item"))

    ;; carousel-node? : dom-node-record? -> boolean?
    ;;   Check whether n is a carousel root that handles arrow/home/end keys.
    (define (carousel-node? n)
      (string=? (widget-value n) "carousel"))

    ;; hover-change-node? : dom-node-record? -> boolean?
    ;;   Check whether n should receive synthetic mouseenter/mouseleave change payloads.
    (define (hover-change-node? n)
      (define widget (widget-value n))
      (or (string=? widget "menu-label")
          (string=? widget "menu-item")
          (string=? widget "toast")
          (string=? widget "tooltip")
          (string=? widget "tooltip-trigger")))

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

    ;; extern-present? : any/c -> boolean?
    ;;   Check whether external value is present and not nullish.
    (define (extern-present? v)
      (and v
           (not (extern-nullish? v))))

    ;; extern-bool-true? : any/c -> boolean?
    ;;   Interpret external value as boolean true/false by string conversion.
    (define (extern-bool-true? v)
      (string=? (js-value->string v) "true"))

    ;; extern-number/default : any/c number? -> number?
    ;;   Parse external number-like value, returning fallback when parsing fails.
    (define (extern-number/default v fallback)
      (define maybe-n (string->number (js-value->string v)))
      (if maybe-n
          maybe-n
          fallback))

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
      (when (extern-present? popup-native)
        (define first-item
          (js-send/extern/nullish
           popup-native
           "querySelector"
           (vector ".we-menu-item[role='menuitem']")))
        (when (extern-present? first-item)
          (js-send first-item "focus" (vector)))))

    ;; focus-last-menu-item! : any/c -> void?
    ;;   Focus last menu item in popup for label-native.
    (define (focus-last-menu-item! label-native)
      (define popup-native (js-ref/extern label-native "nextElementSibling"))
      (when (extern-present? popup-native)
        (define item-list
          (js-send/extern/nullish
           popup-native
           "querySelectorAll"
           (vector ".we-menu-item[role='menuitem']")))
        (when (extern-present? item-list)
          (define len (nodelist-length item-list))
          (when (> len 0)
            (define last-item (nodelist-item item-list (- len 1)))
            (when (extern-present? last-item)
              (js-send last-item "focus" (vector)))))))

    ;; focus-first-menu-item-in-popup! : any/c -> void?
    ;;   Focus first menu item inside popup-native.
    (define (focus-first-menu-item-in-popup! popup-native)
      (when (extern-present? popup-native)
        (define first-item
          (js-send/extern/nullish
           popup-native
           "querySelector"
           (vector ".we-menu-item[role='menuitem']")))
        (when (extern-present? first-item)
          (js-send first-item "focus" (vector)))))

    ;; focus-last-menu-item-in-popup! : any/c -> void?
    ;;   Focus last menu item inside popup-native.
    (define (focus-last-menu-item-in-popup! popup-native)
      (when (extern-present? popup-native)
        (define item-list
          (js-send/extern/nullish
           popup-native
           "querySelectorAll"
           (vector ".we-menu-item[role='menuitem']")))
        (when (extern-present? item-list)
          (define len (nodelist-length item-list))
          (when (> len 0)
            (define last-item (nodelist-item item-list (- len 1)))
            (when (extern-present? last-item)
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
      (when (extern-present? from-menu)
        (define to-menu (sibling-menu-native from-menu dir))
        (when (extern-present? to-menu)
          (define to-label
            (js-send/extern/nullish
             to-menu
             "querySelector"
             (vector ".we-menu-label[role='button']")))
          (when (extern-present? to-label)
            (js-send to-label "click" (vector))
            (js-send to-label "focus" (vector))))))

    ;; focus-edge-menu-label! : any/c symbol? -> void?
    ;;   Open and focus first/last top-level menu label for label-native.
    (define (focus-edge-menu-label! label-native edge)
      (define from-menu (js-ref/extern label-native "parentElement"))
      (when (extern-present? from-menu)
        (define bar-native (js-ref/extern from-menu "parentElement"))
        (when (extern-present? bar-native)
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
          (when (extern-present? target-label)
            (js-send target-label "click" (vector))
            (js-send target-label "focus" (vector))))))

    ;; switch-menu-from-item! : any/c symbol? -> void?
    ;;   Open sibling menu from item-native in direction dir and focus sibling label.
    (define (switch-menu-from-item! item-native dir)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (when (extern-present? popup-native)
        (define from-menu (js-ref/extern popup-native "parentElement"))
        (when (extern-present? from-menu)
          (define to-menu (sibling-menu-native from-menu dir))
          (when (extern-present? to-menu)
            (define to-label
              (js-send/extern/nullish
               to-menu
               "querySelector"
               (vector ".we-menu-label[role='button']")))
            (when (extern-present? to-label)
              (js-send to-label "click" (vector))
              (js-send to-label "focus" (vector)))))))

    ;; focus-next-menu-item! : any/c -> void?
    ;;   Move focus to next item in same popup, clamping at the last item.
    (define (focus-next-menu-item! item-native)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (if (extern-present? popup-native)
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if (extern-present? item-list)
                (let* ([idx (nodelist-index-of-node item-list item-native)]
                       [len (nodelist-length item-list)]
                       [next-idx (cond
                                   [(or (< idx 0) (= len 0))
                                    0]
                                   [else
                                    (min (add1 idx) (- len 1))])]
                       [next-item (nodelist-item item-list next-idx)])
                  (if (extern-present? next-item)
                      (js-send next-item "focus" (vector))
                      (js-send item-native "focus" (vector))))
                (js-send item-native "focus" (vector))))
          (js-send item-native "focus" (vector))))

    ;; focus-prev-menu-item! : any/c -> void?
    ;;   Move focus to previous item in same popup, clamping at the first item.
    (define (focus-prev-menu-item! item-native)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (if (extern-present? popup-native)
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if (extern-present? item-list)
                (let* ([idx (nodelist-index-of-node item-list item-native)]
                       [len (nodelist-length item-list)]
                       [prev-idx (cond
                                   [(= len 0)
                                    0]
                                   [(< idx 0)
                                    0]
                                   [else
                                    (max (sub1 idx) 0)])]
                       [prev-item (nodelist-item item-list prev-idx)])
                  (if (extern-present? prev-item)
                      (js-send prev-item "focus" (vector))
                      (js-send item-native "focus" (vector))))
                (js-send item-native "focus" (vector))))
          (js-send item-native "focus" (vector))))

    ;; focus-own-menu-label! : any/c -> void?
    ;;   Focus top-level menu label for item-native.
    (define (focus-own-menu-label! item-native)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (when (extern-present? popup-native)
        (define menu-native (js-ref/extern popup-native "parentElement"))
        (when (extern-present? menu-native)
          (define label-native
            (js-send/extern/nullish
             menu-native
             "querySelector"
             (vector ".we-menu-label[role='button']")))
          (when (extern-present? label-native)
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

    ;; node-text-trimmed-downcase : any/c -> string?
    ;;   Read node textContent as lowercase text with leading/trailing spaces removed.
    (define (node-text-trimmed-downcase node)
      (string-trim (node-text-downcase node)))

    ;; menu-item-matches-query? : any/c string? -> boolean?
    ;;   Check whether item's visible label starts with query.
    (define (menu-item-matches-query? item-native query)
      (define text (node-text-trimmed-downcase item-native))
      (define query-len (string-length query))
      (and (>= (string-length text) query-len)
           (string=? (substring text 0 query-len) query)))

    ;; extern-node-same? : any/c any/c -> boolean?
    ;;   Check whether a and b refer to the same DOM node.
    (define (extern-node-same? a b)
      (and (extern-present? a)
           (extern-present? b)
           (extern-bool-true?
            (js-send/extern/nullish a "isSameNode" (vector b)))))

    ;; extern-node-matches-selector? : any/c string? -> boolean?
    ;;   Check whether node matches CSS selector sel.
    (define (extern-node-matches-selector? node sel)
      (and (extern-present? node)
           (extern-bool-true?
            (js-send/extern/nullish node "matches" (vector sel)))))

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
    ;;   Focus first item in node-list matching query, scanning from start with wrap.
    (define (focus-matching-menu-item-from-list! node-list start query)
      (define len (nodelist-length node-list))
      (let loop ([offset 0])
        (cond
          [(or (= len 0)
               (>= offset len))
           #f]
          [else
           (define idx (modulo (+ start offset) len))
           (define item (nodelist-item node-list idx))
           (if (and item (menu-item-matches-query? item query))
               (begin
                 (js-send item "focus" (vector))
                 #t)
               (loop (add1 offset)))])))

    ;; event-timestamp-ms : any/c -> number?
    ;;   Return event timestamp in ms, or 0 when unavailable.
    (define (event-timestamp-ms evt)
      (define parsed (string->number (js-value->string (js-ref/extern evt "timeStamp"))))
      (if (number? parsed) parsed 0))

    ;; popup-typeahead-state-ref : any/c -> (or/c pair? false/c)
    ;;   Return saved (cons timestamp-ms prefix) for popup-native.
    (define (popup-typeahead-state-ref popup-native)
      (native-mapping-ref menu-typeahead-state popup-native))

    ;; popup-typeahead-state-set! : any/c number? string? -> void?
    ;;   Save typeahead state for popup-native.
    (define (popup-typeahead-state-set! popup-native timestamp-ms prefix)
      (set! menu-typeahead-state
            (native-mapping-set menu-typeahead-state popup-native (cons timestamp-ms prefix))))

    ;; popup-typeahead-query! : any/c string? any/c -> string?
    ;;   Build next prefix query from popup state, key, and timestamp.
    (define (popup-typeahead-query! popup-native key evt)
      (define stamp (event-timestamp-ms evt))
      (define state (popup-typeahead-state-ref popup-native))
      (define previous-stamp (if state (car state) 0))
      (define previous-query (if state (cdr state) ""))
      (define expired? (> (- stamp previous-stamp) menu-typeahead-timeout-ms))
      (define next-query
        (if (or expired?
                (string=? previous-query "")
                (and (= (string-length previous-query) 1)
                     (string=? previous-query key)))
            key
            (string-append previous-query key)))
      (popup-typeahead-state-set! popup-native stamp next-query)
      next-query)

    ;; focus-matching-menu-item-from-label! : any/c string? any/c -> boolean?
    ;;   Open label popup (if needed) and focus first matching item for key/query.
    (define (focus-matching-menu-item-from-label! label-native key evt)
      (define expanded
        (js-value->string
         (js-send/extern/nullish label-native "getAttribute" (vector "aria-expanded"))))
      (when (not (string=? expanded "true"))
        (js-send label-native "click" (vector)))
      (define popup-native (js-ref/extern label-native "nextElementSibling"))
      (if (extern-present? popup-native)
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if (extern-present? item-list)
                (focus-matching-menu-item-from-list!
                 item-list
                 0
                 (popup-typeahead-query! popup-native key evt))
                #f))
          #f))

    ;; focus-matching-menu-item-from-item! : any/c string? any/c -> boolean?
    ;;   Focus next matching item in same popup for key/query, wrapping in menu order.
    (define (focus-matching-menu-item-from-item! item-native key evt)
      (define popup-native (js-ref/extern item-native "parentElement"))
      (if (extern-present? popup-native)
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if (extern-present? item-list)
                (let* ([idx (nodelist-index-of-node item-list item-native)]
                       [len (nodelist-length item-list)]
                       [start (if (and (>= idx 0) (> len 0))
                                  (modulo (add1 idx) len)
                                  0)])
                  (focus-matching-menu-item-from-list!
                   item-list
                   start
                   (popup-typeahead-query! popup-native key evt)))
                #f))
          #f))

    ;; focus-matching-menu-item-from-popup! : any/c string? any/c -> boolean?
    ;;   Focus first matching item in popup-native for key/query.
    (define (focus-matching-menu-item-from-popup! popup-native key evt)
      (if (extern-present? popup-native)
          (let ([item-list
                 (js-send/extern/nullish
                  popup-native
                  "querySelectorAll"
                  (vector ".we-menu-item[role='menuitem']"))])
            (if (extern-present? item-list)
                (focus-matching-menu-item-from-list!
                 item-list
                 0
                 (popup-typeahead-query! popup-native key evt))
                #f))
          #f))

    ;; focus-selected-sibling-tab! : any/c -> void?
    ;;   Focus selected tab button in the same tablist as native.
    (define (focus-selected-sibling-tab! native)
      (define parent (js-ref/extern native "parentElement"))
      (when (extern-present? parent)
        (define selected
          (js-send/extern/nullish
           parent
           "querySelector"
           (vector "button[aria-selected='true']")))
        (when (extern-present? selected)
          (js-send selected "focus" (vector)))))

    ;; focus-scrollspy-item! : any/c integer? -> void?
    ;;   Focus item at idx in the current scrollspy nav and trigger selection by click.
    (define (focus-scrollspy-item! native idx)
      (define parent* (js-ref/extern native "parentElement"))
      (when (and parent* (not (extern-nullish? parent*)))
        (define items
          (js-send/extern/nullish parent* "querySelectorAll" (vector scrollspy-item-selector)))
        (when (and items (not (extern-nullish? items)))
          (define len (nodelist-length items))
          (when (and (> len 0) (>= idx 0) (< idx len))
            (define target (nodelist-item items idx))
            (when (and target (not (extern-nullish? target)))
              (js-send target "focus" (vector))
              (js-send target "click" (vector)))))))

    ;; focus-scrollspy-step! : any/c integer? -> void?
    ;;   Move to the next/previous scrollspy item with wrap-around.
    (define (focus-scrollspy-step! native step)
      (define parent* (js-ref/extern native "parentElement"))
      (when (and parent* (not (extern-nullish? parent*)))
        (define items
          (js-send/extern/nullish parent* "querySelectorAll" (vector scrollspy-item-selector)))
        (when (and items (not (extern-nullish? items)))
          (define len (nodelist-length items))
          (when (> len 0)
            (define idx (nodelist-index-of-node items native))
            (define start (if (>= idx 0) idx 0))
            (define next-idx (modulo (+ start step len) len))
            (focus-scrollspy-item! native next-idx)))))

    ;; event-handler-ref : list? string? -> any/c
    ;;   Return generic event callback for event-name, or #f when absent.
    (define (event-handler-ref handlers event-name)
      (define p (assoc event-name handlers))
      (if p
          (cdr p)
          #f))

    ;; invoke-click-callback! : any/c -> void?
    ;;   Invoke callback when present.
    (define (invoke-click-callback! callback)
      (when callback
        (callback)))

    ;; invoke-change-callback! : any/c any/c -> void?
    ;;   Invoke callback with payload when present.
    (define (invoke-change-callback! callback payload)
      (when callback
        (callback payload)))

    ;; invoke-generic-event-callback! : dom-node? string? any/c -> void?
    ;;   Invoke supported generic primitive event callback with raw event payload.
    (define (invoke-generic-event-callback! n event-name evt)
      (define callback
        (event-handler-ref (dom-node-record-event-handlers n) event-name))
      (when callback
        (callback evt)))

    ;; keydown-callback-source : dom-node? -> any/c
    ;;   Return the current callback whose explicit name should drive keydown bridge naming.
    (define (keydown-callback-source n)
      (or (alist-ref/default (dom-node-record-attrs n) 'on-enter-action #f)
          (dom-node-record-on-click n)
          (dom-node-record-on-change n)))

    ;; refresh-dom-node-listeners! : dom-node? -> void?
    ;;   Reinstall native browser listeners so callback names track current node callbacks.
    (define (refresh-dom-node-listeners! n)
      (define native* (dom-node-record-native n))
      (when (and native* (not (extern-nullish? native*)))
        (define old-listeners (native-mapping-ref dom-node-listeners native*))
        (when old-listeners
          (for-each (lambda (entry)
                      (js-send native* "removeEventListener" (vector (car entry) (cdr entry))))
                    old-listeners))
        (define listeners '())
        (define (register-listener! event-name listener)
          (js-add-event-listener! native* event-name listener)
          (set! listeners (cons (cons event-name listener) listeners)))
        (for-each
         (lambda (event-name)
           (define callback
             (event-handler-ref (dom-node-record-event-handlers n) event-name))
           (define listener
             (contextual-procedure->external
              (dom-node-record-tag n)
              (dom-node-record-attrs n)
              (string-append "on-" event-name)
              callback
              (lambda (evt)
                (invoke-generic-event-callback! n event-name evt))))
           (register-listener! event-name listener))
         primitive-dom-event-names)
        (register-listener!
         "click"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-click"
          (dom-node-record-on-click n)
          (lambda (evt)
            (define callback (dom-node-record-on-click n))
            (invoke-click-callback! callback)
            (void evt))))
        (register-listener!
         "change"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-change"
          (dom-node-record-on-change n)
          (lambda (_evt)
            (define callback (dom-node-record-on-change n))
            (invoke-change-callback! callback (node-change-value n)))))
        (register-listener!
         "keydown"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-keydown"
          (keydown-callback-source n)
          (lambda (evt)
            (define key (event-key-value evt))
            (define on-enter (input-enter-action n))
            (when (and on-enter (string=? key "Enter"))
              (js-send evt "preventDefault" (vector))
              (on-enter))
            (define on-click (dom-node-record-on-click n))
            (define role-pair (assq 'role (dom-node-record-attrs n)))
            (when (and on-click
                       (or (eq? (dom-node-record-tag n) 'button)
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
              (invoke-change-callback! callback key)
              (when (nav-key? key)
                (focus-selected-sibling-tab! native*)))
            (when (menu-label-node? n)
              (case (string->symbol key)
                [(ArrowDown)
                 (invoke-change-callback! callback "ArrowDown")
                 (js-send evt "preventDefault" (vector))
                 (focus-first-menu-item! native*)]
                [(ArrowUp)
                 (invoke-change-callback! callback "ArrowUp")
                 (js-send evt "preventDefault" (vector))
                 (focus-last-menu-item! native*)]
                [(ArrowRight)
                 (js-send evt "preventDefault" (vector))
                 (switch-menu-from-label! native* 'next)]
                [(ArrowLeft)
                 (js-send evt "preventDefault" (vector))
                 (switch-menu-from-label! native* 'prev)]
                [(Home)
                 (js-send evt "preventDefault" (vector))
                 (focus-edge-menu-label! native* 'first)]
                [(End)
                 (js-send evt "preventDefault" (vector))
                 (focus-edge-menu-label! native* 'last)]
                [(Tab)
                 (invoke-change-callback! callback "focusout")]
                [else
                 (when (menu-typeahead-key? key)
                   (when (focus-matching-menu-item-from-label! native* key evt)
                     (js-send evt "preventDefault" (vector))))]))
            (when (menu-item-node? n)
              (case (string->symbol key)
                [(ArrowDown)
                 (js-send evt "preventDefault" (vector))
                 (focus-next-menu-item! native*)]
                [(ArrowUp)
                 (js-send evt "preventDefault" (vector))
                 (focus-prev-menu-item! native*)]
                [(ArrowRight)
                 (js-send evt "preventDefault" (vector))
                 (switch-menu-from-item! native* 'next)]
                [(ArrowLeft)
                 (js-send evt "preventDefault" (vector))
                 (switch-menu-from-item! native* 'prev)]
                [(Tab)
                 (invoke-change-callback! callback "focusout")]
                [(Escape)
                 (invoke-change-callback! callback "Escape")
                 (focus-own-menu-label! native*)]
                [else
                 (when (menu-typeahead-key? key)
                   (when (focus-matching-menu-item-from-item! native* key evt)
                     (js-send evt "preventDefault" (vector))))]))
            (when (menu-popup-node? n)
              (define target* (js-ref/extern evt "target"))
              (when (extern-node-same? target* native*)
                (case (string->symbol key)
                  [(ArrowDown Home)
                   (js-send evt "preventDefault" (vector))
                   (focus-first-menu-item-in-popup! native*)]
                  [(ArrowUp End)
                   (js-send evt "preventDefault" (vector))
                   (focus-last-menu-item-in-popup! native*)]
                  [else
                   (when (menu-typeahead-key? key)
                     (when (focus-matching-menu-item-from-popup! native* key evt)
                       (js-send evt "preventDefault" (vector))))])))
            (when (scrollspy-item-node? n)
              (case (string->symbol key)
                [(ArrowRight ArrowDown)
                 (js-send evt "preventDefault" (vector))
                 (focus-scrollspy-step! native* 1)]
                [(ArrowLeft ArrowUp)
                 (js-send evt "preventDefault" (vector))
                 (focus-scrollspy-step! native* -1)]
                [(Home)
                 (js-send evt "preventDefault" (vector))
                 (focus-scrollspy-item! native* 0)]
                [(End)
                 (js-send evt "preventDefault" (vector))
                 (define parent* (js-ref/extern native* "parentElement"))
                 (when (and parent* (not (extern-nullish? parent*)))
                   (define items
                     (js-send/extern/nullish parent* "querySelectorAll" (vector scrollspy-item-selector)))
                   (when (and items (not (extern-nullish? items)))
                     (define len (nodelist-length items))
                     (when (> len 0)
                       (focus-scrollspy-item! native* (- len 1)))))]
                [else
                 (void)]))
            (when (carousel-node? n)
              (case (string->symbol key)
                [(ArrowLeft ArrowRight Home End)
                 (js-send evt "preventDefault" (vector))
                 (invoke-change-callback! callback key)]
                [else
                 (void)]))
            (when (and callback (role-button-node? n))
              (invoke-change-callback! callback key))
            (when (and (role-dialog-node? n) (string=? key "Tab"))
              (js-send evt "preventDefault" (vector))
              (define shift? (extern-bool-true? (js-ref/extern evt "shiftKey")))
              (when (dialog-open-attr? (dom-node-record-attrs n))
                (focus-cycled-dialog-target! native* shift?)))
            (when (and callback (role-dialog-node? n))
              (invoke-change-callback! callback key)))))
        (register-listener!
         "mouseenter"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-mouseenter"
          (dom-node-record-on-change n)
          (lambda (evt)
            (with-handlers ([(lambda (_e) #t)
                             (lambda (_e)
                               (void))])
              (define callback (dom-node-record-on-change n))
              (when (and callback (hover-change-node? n))
                (invoke-change-callback! callback "mouseenter"))
              (void evt)))))
        (register-listener!
         "mouseleave"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-mouseleave"
          (dom-node-record-on-change n)
          (lambda (evt)
            (with-handlers ([(lambda (_e) #t)
                             (lambda (_e)
                               (void))])
              (define callback (dom-node-record-on-change n))
              (when (and callback (hover-change-node? n))
                (invoke-change-callback! callback "mouseleave"))
              (void evt)))))
        (register-listener!
         "focusout"
         (contextual-procedure->external
          (dom-node-record-tag n)
          (dom-node-record-attrs n)
          "on-focusout"
          (dom-node-record-on-change n)
          (lambda (evt)
            (with-handlers ([(lambda (_e) #t)
                             (lambda (_e)
                               (void))])
              (define callback (dom-node-record-on-change n))
              (define menu-container (focusout-menu-container n native*))
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
                (define related-menu-control?
                  (and related
                       (or (extern-node-matches-selector? related ".we-menu-label[role='button']")
                           (extern-node-matches-selector? related ".we-menu-item[role='menuitem']"))))
                (unless (or still-inside?
                            related-menu-control?)
                  (invoke-change-callback! callback "focusout")))))))
        (when (or (eq? (dom-node-record-tag n) 'input)
                  (eq? (dom-node-record-tag n) 'textarea))
          (register-listener!
           "input"
           (contextual-procedure->external
            (dom-node-record-tag n)
            (dom-node-record-attrs n)
            "on-input"
            (dom-node-record-on-change n)
            (lambda (_evt)
              (define callback (dom-node-record-on-change n))
              (invoke-change-callback! callback (node-change-value n))))))
        (set! dom-node-listeners
              (native-mapping-set dom-node-listeners
                                  native*
                                  (reverse listeners))))
      (void))

    ;; dom-node : symbol? list? list? any/c any/c any/c list? -> dom-node?
    ;;   Construct a browser-backed node and install event bridges.
    (define (dom-node tag attrs children text on-click on-change [event-handlers '()])
      (define native
        (if (eq? tag 'text)
            (js-create-text-node (if text
                                     (value->attr-string text)
                                     ""))
            (js-create-element (tag->element-name tag))))
      (define n
        (dom-node-record tag attrs children text on-click on-change event-handlers native))
      (unless (eq? tag 'text)
        (install-default-node-shape! n)
        (apply-attributes! n '() attrs)
        (when text
          (apply-text! native text))
        (refresh-dom-node-listeners! n))
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

    ;; dom-node-event-handlers : dom-node? -> list?
    ;;   Return generic event callback alist.
    (define (dom-node-event-handlers n)
      (dom-node-record-event-handlers n))

    ;; set-dom-node-tag! : dom-node? symbol? -> void?
    ;;   Remount node with a new native element when tag changes.
    (define (set-dom-node-tag! n tag)
      (define old-tag (dom-node-record-tag n))
      (unless (eq? old-tag tag)
        (define old-native (dom-node-record-native n))
        (define old-parent (js-ref/extern old-native "parentElement"))
        (define replacement
          (dom-node tag
                    (dom-node-record-attrs n)
                    (dom-node-record-children n)
                    (dom-node-record-text n)
                    (dom-node-record-on-click n)
                    (dom-node-record-on-change n)
                    (dom-node-record-event-handlers n)))
        (define new-native (dom-node-record-native replacement))
        (define children (dom-node-record-children n))
        (when (pair? children)
          (define fragment (js-create-document-fragment))
          (for-each (lambda (child)
                      (js-append-child! fragment (dom-node-record-native child)))
                    children)
          (js-replace-children! new-native fragment))
        (when (and old-parent (not (extern-nullish? old-parent)))
          (js-send old-parent "replaceChild" (vector new-native old-native)))
        (set-dom-node-record-tag! n tag)
        (set-dom-node-record-attrs! n (dom-node-record-attrs replacement))
        (set-dom-node-record-children! n children)
        (set-dom-node-record-text! n (dom-node-record-text replacement))
        (set-dom-node-record-on-click! n (dom-node-record-on-click replacement))
        (set-dom-node-record-on-change! n (dom-node-record-on-change replacement))
        (set-dom-node-record-event-handlers! n
                                             (dom-node-record-event-handlers replacement))
        (set-dom-node-record-native! n new-native)
        (refresh-dom-node-listeners! n)))

    ;; set-dom-node-attrs! : dom-node? list? -> void?
    ;;   Replace tracked attributes and sync native DOM attributes.
    (define (set-dom-node-attrs! n attrs)
      (define native (dom-node-record-native n))
      (define old-attrs (dom-node-record-attrs n))
      (clear-attributes! native old-attrs)
      (set-dom-node-record-attrs! n attrs)
      (apply-attributes! n old-attrs attrs)
      (refresh-dom-node-listeners! n)
      (void))

    ;; set-dom-node-children! : dom-node? list? -> void?
    ;;   Replace tracked child list.
    (define (set-dom-node-children! n children)
      (set-dom-node-record-children! n children))

    ;; set-dom-node-text! : dom-node? any/c -> void?
    ;;   Update tracked text and sync native DOM text child.
    (define (set-dom-node-text! n text)
      (set-dom-node-record-text! n text)
      (if (eq? (dom-node-record-tag n) 'text)
          (let ()
            (define old-native (dom-node-record-native n))
            (define replacement
              (js-create-text-node (if text
                                       (value->attr-string text)
                                       "")))
            (define old-parent (js-ref/extern old-native "parentNode"))
            (when (and old-parent (not (extern-nullish? old-parent)))
              (js-send old-parent "replaceChild" (vector replacement old-native)))
            (set-dom-node-record-native! n replacement))
          (apply-text! (dom-node-record-native n) text))
      (void))

    ;; set-dom-node-on-click! : dom-node? any/c -> void?
    ;;   Update click callback.
    (define (set-dom-node-on-click! n on-click)
      (set-dom-node-record-on-click! n on-click)
      (refresh-dom-node-listeners! n))

    ;; set-dom-node-on-change! : dom-node? any/c -> void?
    ;;   Update change callback.
    (define (set-dom-node-on-change! n on-change)
      (set-dom-node-record-on-change! n on-change)
      (refresh-dom-node-listeners! n))

    ;; set-dom-node-event-handlers! : dom-node? list? -> void?
    ;;   Update generic event callback alist.
    (define (set-dom-node-event-handlers! n event-handlers)
      (set-dom-node-record-event-handlers! n event-handlers)
      (refresh-dom-node-listeners! n))

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

    ;; backend-scrollspy-observe-scroll! : dom-node? (-> void?) (-> (-> void?) void?) -> void?
    ;;   Register IntersectionObserver (fallback scroll listener) on container in browser backend.
    (define (backend-scrollspy-observe-scroll! container callback register-cleanup!)
      (define native (dom-node-record-native container))
      ;; invoke-scrollspy-callback! : string? -> void?
      ;;   Invoke callback.
      (define (invoke-scrollspy-callback! source)
        (callback))
      (when (and native (not (extern-nullish? native)))
        ;; Clear previously registered observer/listener before rebinding sections.
        (define old-observer* (native-mapping-ref scrollspy-observers native))
        (when (and old-observer* (not (extern-nullish? old-observer*)))
          (js-send old-observer* "disconnect" (vector)))
        (define old-listener* (native-mapping-ref scrollspy-listeners native))
        (when (and old-listener* (not (extern-nullish? old-listener*)))
          (js-send native "removeEventListener" (vector "scroll" old-listener*)))
        (set! scrollspy-observers (native-mapping-remove scrollspy-observers native))
        (set! scrollspy-listeners (native-mapping-remove scrollspy-listeners native))

        ;; Prefer IntersectionObserver when available.
        (define observer-ctor* (js-ref/extern (js-var "window") "IntersectionObserver"))
        (define reflect* (js-var "Reflect"))
        (define io-supported?
          (and observer-ctor*
               reflect*
               (not (extern-nullish? observer-ctor*))
               (not (extern-nullish? reflect*))))
        (if io-supported?
                (let* ([io-callback
                        (contextual-procedure->external
                     #f '() "Scrollspy:observer" #f
                     (lambda (_entries _observer)
                       (void)))]
                   [observer
                    (js-send/extern/nullish
                     reflect*
                     "construct"
                     (vector observer-ctor*
                             (vector io-callback)))]
                   [sections
                    (js-send/extern/nullish
                     native
                     "querySelectorAll"
                     (vector scrollspy-section-selector))])
              (if (or (not observer) (extern-nullish? observer))
                  (let ([listener
                         (contextual-procedure->external
                          #f '() "Scrollspy:scroll-fallback" #f
                          (lambda (_evt)
                            (invoke-scrollspy-callback! "scroll-fallback/no-observer")))])
                    (set! scrollspy-listeners
                          (native-mapping-set scrollspy-listeners native listener))
                    (js-send native "addEventListener" (vector "scroll" listener)))
                  (begin
                    (set! scrollspy-observers
                          (native-mapping-set scrollspy-observers native observer))
                    (let ([listener
                           (contextual-procedure->external
                            #f '() "Scrollspy:scroll-observer" #f
                            (lambda (_evt)
                              (invoke-scrollspy-callback! "scroll-with-observer")))])
                      (set! scrollspy-listeners
                            (native-mapping-set scrollspy-listeners native listener))
                      (js-send native "addEventListener" (vector "scroll" listener)))
                    (when (and sections (not (extern-nullish? sections)))
                      (define len (nodelist-length sections))
                      (let loop ([i 0])
                        (when (< i len)
                          (define section-native (nodelist-item sections i))
                          (when (and section-native (not (extern-nullish? section-native)))
                            (js-send observer "observe" (vector section-native)))
                          (loop (add1 i)))))
                    (void))))
            (let ([listener
                   (contextual-procedure->external
                    #f '() "Scrollspy:scroll-fallback" #f
                    (lambda (_evt)
                      (invoke-scrollspy-callback! "scroll-fallback/no-io")))])
              (set! scrollspy-listeners
                    (native-mapping-set scrollspy-listeners native listener))
              (js-send native "addEventListener" (vector "scroll" listener))))

        ;; Register one cleanup hook per native container.
        (when (not (native-member? scrollspy-cleanup-registered native))
          (set! scrollspy-cleanup-registered
                (native-add-unique scrollspy-cleanup-registered native))
          (register-cleanup!
           (lambda ()
             (define observer* (native-mapping-ref scrollspy-observers native))
             (when (and observer* (not (extern-nullish? observer*)))
               (js-send observer* "disconnect" (vector)))
             (define listener* (native-mapping-ref scrollspy-listeners native))
             (when (and listener* (not (extern-nullish? listener*)))
               (js-send native "removeEventListener" (vector "scroll" listener*)))
             (set! scrollspy-observers (native-mapping-remove scrollspy-observers native))
             (set! scrollspy-listeners (native-mapping-remove scrollspy-listeners native))
             (set! scrollspy-cleanup-registered
                   (native-remove scrollspy-cleanup-registered native))))))
      (void))

    ;; backend-scrollspy-scroll-into-view! : dom-node? -> void?
    ;;   Scroll section node into view in browser backend.
    (define (backend-scrollspy-scroll-into-view! section-node)
      (define native (dom-node-record-native section-node))
      (when (and native (not (extern-nullish? native)))
        ;; Pass `#t` so browser aligns selected section to top consistently.
        (js-send native "scrollIntoView" (vector #t)))
      (void))

    ;; backend-scrollspy-active-id : list? -> any/c
    ;;   Compute active section id from section bindings using container-relative geometry.
    (define (backend-scrollspy-active-id section-bindings)
      (if (null? section-bindings)
          #f
          (let* ([first-binding (car section-bindings)]
                 [first-id (car first-binding)]
                 [first-node (cdr first-binding)]
                 [first-native (dom-node-record-native first-node)]
                 [container-native* (if first-native
                                        (js-ref/extern first-native "parentElement")
                                        #f)])
            (if (or (not container-native*)
                    (extern-nullish? container-native*))
                first-id
                (let* ([container-rect
                        (js-send/extern/nullish
                         container-native*
                         "getBoundingClientRect"
                         (vector))]
                       [container-top
                        (if (extern-nullish? container-rect)
                            0
                            (extern-number/default (js-ref/extern container-rect "top") 0))])
                  (let loop ([pairs section-bindings]
                             [candidate #f])
                    (if (null? pairs)
                        (if candidate candidate first-id)
                        (let* ([binding (car pairs)]
                               [section-id (car binding)]
                               [section-node (cdr binding)]
                               [section-native (dom-node-record-native section-node)])
                          (if (or (not section-native)
                                  (extern-nullish? section-native))
                              (loop (cdr pairs) candidate)
                              (let ([section-rect
                                     (js-send/extern/nullish
                                      section-native
                                      "getBoundingClientRect"
                                      (vector))])
                                (if (extern-nullish? section-rect)
                                    (loop (cdr pairs) candidate)
                                    (let ([section-top
                                           (extern-number/default
                                            (js-ref/extern section-rect "top")
                                            0)])
                                      (if (<= section-top (+ container-top 8))
                                          (loop (cdr pairs) section-id)
                                          (loop (cdr pairs) candidate))))))))))))))

    (values dom-node
            dom-node?
            dom-node-tag
            dom-node-attrs
            dom-node-children
            dom-node-text
            dom-node-on-click
            dom-node-on-change
            dom-node-event-handlers
            set-dom-node-tag!
            set-dom-node-attrs!
            set-dom-node-children!
            set-dom-node-text!
            set-dom-node-on-click!
            set-dom-node-on-change!
            set-dom-node-event-handlers!
            dom-node-native
            backend-append-child!
            backend-set-single-child!
            backend-replace-children!
            backend-mount-root!
            backend-scrollspy-observe-scroll!
            backend-scrollspy-scroll-into-view!
            backend-scrollspy-active-id
            backend-set-timeout!
            backend-clear-timeout!)))
