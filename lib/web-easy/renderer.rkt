#lang webracket

;;;
;;; web-easy Renderer
;;;

;; Renderer runtime that builds and updates a DOM-like node tree from view values.
;;
;; Exports:
;;   renderer?               Predicate for renderer values.
;;   render                  Render a view into runtime nodes.
;;   renderer-root           Return renderer root node.
;;   renderer-destroy        Destroy renderer and run cleanups.
;;   dom-node-click!         Invoke node click callback when present.
;;   dom-node-change!        Invoke node change callback when present.
;;   dom-node-toggle!        Toggle checkbox state and invoke change callback.
;;   dom-node-select!        Set selected value and invoke change callback.
;;   dom-node-slide!         Set slider value and invoke change callback.
;;   dom-node-radio-select!  Set radio selection and invoke change callback.
;;   dom-node-keydown!       Invoke node keydown callback when present.
;;
;; Backend contract used by this renderer:
;;   dom-node
;;   dom-node?
;;   dom-node-tag
;;   dom-node-attrs
;;   dom-node-children
;;   dom-node-text
;;   dom-node-on-click
;;   dom-node-on-change
;;   set-dom-node-tag!
;;   set-dom-node-attrs!
;;   set-dom-node-children!
;;   set-dom-node-text!
;;   set-dom-node-on-click!
;;   set-dom-node-on-change!
;;   backend-append-child!
;;   backend-set-single-child!
;;   backend-replace-children!

(define-values
  (renderer?
   render
   renderer-root
   renderer-destroy
   dom-node-click!
   dom-node-change!
   dom-node-toggle!
   dom-node-select!
   dom-node-slide!
   dom-node-radio-select!
   dom-node-keydown!)
  (let ()
    (struct renderer-state (root cleanups destroyed?) #:mutable #:transparent)

    ;; Constants for node attributes and fallbacks.
    (define attr/role             'role)      ; Attribute key for semantic role.
    (define text/fallback         "#<value>") ; Fallback when value cannot be rendered as text.
    (define table/density-normal  'normal)    ; Default table spacing density.
    (define table/density-compact 'compact)   ; Compact table spacing density.
    (define tab-panel-counter     0)          ; Monotonic counter for tab-panel ids.
    (define menu-popup-counter    0)          ; Monotonic counter for menu popup ids.
    (define active-menu-close     #f)         ; Thunk closing currently open popup menu.

    ;; Style constants
    (define tab-panel-style-text ; CSS for class-based tab styles.
      ".we-tab-list{display:flex;gap:var(--we-gap-tab,6px);align-items:stretch;border-bottom:1px solid var(--we-border-muted,#999);padding-bottom:var(--we-space-sm,4px);margin-bottom:var(--we-space-sm,4px);}\
       .we-tab-btn{min-width:88px;padding:var(--we-space-sm,4px) var(--we-space-md,10px);border:1px solid var(--we-border-muted,#999);border-bottom-width:2px;background:var(--we-bg,#fff);font-weight:normal;}\
       .we-tab-btn.is-selected{border-color:var(--we-border-strong,#333);background:var(--we-bg-selected,#ececec);font-weight:bold;}\
       .we-tab-btn.is-disabled{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-disabled,#f3f3f3);color:var(--we-fg-muted,#777);opacity:.7;}\
       .we-tab-btn:focus-visible{outline:2px solid var(--we-focus,#0a66c2);outline-offset:1px;}") 
    (define dialog-style-text ; CSS for dialog overlay and panel.
      ".we-dialog{position:fixed;inset:0;display:none;align-items:center;justify-content:center;background:var(--we-overlay,rgba(0,0,0,0.45));z-index:2000;}\
       .we-dialog.is-open{display:flex;}\
       .we-dialog-panel{min-width:280px;max-width:520px;background:var(--we-bg,#fff);border:1px solid var(--we-border,#888);border-radius:8px;padding:14px;box-shadow:0 8px 22px var(--we-shadow,rgba(0,0,0,.28));}\
       .we-dialog-panel:focus-visible{outline:2px solid var(--we-focus,#0a66c2);outline-offset:2px;}") 
    (define menu-style-text      ; CSS for popup menu keyboard focus visibility and layout.
      ".we-menu-item:focus,.we-menu-item:focus-visible,.we-menu-label:focus,.we-menu-label:focus-visible{outline:2px solid var(--we-focus,#0a66c2);outline-offset:1px;}\
       .we-menu-bar{display:flex;flex-wrap:wrap;gap:8px;align-items:center;padding:var(--we-space-sm,4px) var(--we-space-lg,8px);border:1px solid var(--we-border-menu,#aaa);border-radius:4px;background:var(--we-bg-subtle,#f3f3f3);box-sizing:border-box;}\
       .we-menu{position:relative;display:inline-block;}\
       .we-menu-label{padding:var(--we-space-xs,2px) var(--we-space-lg,8px);border:1px solid transparent;border-radius:3px;background:transparent;cursor:pointer;user-select:none;}\
       .we-menu-label:hover{background:var(--we-bg-hover,#e8e8e8);border-color:var(--we-border-hover,#c0c0c0);}\
       .we-menu-label[aria-expanded='true']{background:var(--we-bg,#fff);border-color:var(--we-border,#888);border-bottom-color:var(--we-bg,#fff);position:relative;z-index:1001;}\
       .we-menu-popup{position:absolute;top:calc(100% + var(--we-space-xs,2px));left:0;min-width:120px;display:none;flex-direction:column;gap:var(--we-space-sm,4px);padding:var(--we-space-sm,4px);border:1px solid var(--we-border,#888);border-radius:4px;background:var(--we-bg,#fff);z-index:1000;}\
       .we-menu-popup.is-open{display:flex;}\
       .we-menu-item{display:block;width:100%;text-align:left;background:var(--we-bg,#fff);color:var(--we-fg,#111);border:1px solid var(--we-border-soft,#bbb);border-radius:3px;}\
       .we-menu-item:hover{background:var(--we-bg-hover,#e8e8e8);border-color:var(--we-border-hover,#c0c0c0);}") 
    (define control-style-text ; CSS defaults for controls and table density classes.
      ":root{--we-focus:#0a66c2;--we-fg:#111;--we-bg:#fff;--we-bg-subtle:#f3f3f3;--we-bg-selected:#ececec;--we-bg-disabled:#f3f3f3;--we-bg-hover:#e8e8e8;--we-border:#888;--we-border-menu:#aaa;--we-border-muted:#999;--we-border-soft:#bbb;--we-border-hover:#c0c0c0;--we-border-strong:#333;--we-fg-muted:#777;--we-overlay:rgba(0,0,0,0.45);--we-shadow:rgba(0,0,0,.28);--we-space-xs:2px;--we-space-sm:4px;--we-space-md:8px;--we-space-lg:10px;--we-gap:4px;--we-gap-tab:6px;}\
       .we-vpanel,.we-group,.we-if-view,.we-cond-view,.we-case-view,.we-observable-view,.we-list-view{display:flex;flex-direction:column;gap:var(--we-gap,4px);}\
       .we-hpanel{display:flex;flex-direction:row;align-items:center;gap:var(--we-gap,4px);}\
       .we-button{align-self:flex-start;width:auto;}\
       .we-input{align-self:stretch;width:100%;box-sizing:border-box;}\
       .we-checkbox,.we-choice,.we-slider,.we-progress,.we-radios,.we-image{align-self:flex-start;}\
       .we-table{border-collapse:separate;border:1px solid var(--we-border-muted,#999);margin-bottom:6px;align-self:flex-start;}\
       .we-table.we-density-normal{border-spacing:2px 0;}\
       .we-table.we-density-compact{border-spacing:0 0;}\
       .we-table-header-cell.we-density-normal{padding:2px 8px;text-align:left;border-bottom:1px solid var(--we-border-soft,#bbb);}\
       .we-table-header-cell.we-density-compact{padding:1px 4px;text-align:left;border-bottom:1px solid var(--we-border-soft,#bbb);}\
       .we-table-data-cell.we-density-normal{padding:2px 8px;}\
       .we-table-data-cell.we-density-compact{padding:1px 4px;}")
    (define shared-style-text ; Shared stylesheet injected once per window root.
      (string-append control-style-text tab-panel-style-text dialog-style-text menu-style-text))

    ;; renderer? : any/c -> boolean?
    ;;   Check whether v is a renderer state value.
    (define (renderer? v)
      (renderer-state? v))

    ;; renderer-root : renderer? -> dom-node?
    ;;   Return the root node managed by renderer r.
    (define (renderer-root r)
      (renderer-state-root r))

    ;; renderer-destroy : renderer? -> void?
    ;;   Run all cleanup hooks and mark renderer as destroyed.
    (define (renderer-destroy r)
      (unless (renderer-state-destroyed? r)
        (for-each (lambda (cleanup) (cleanup))
                  (renderer-state-cleanups r))
        (set-renderer-state-cleanups! r '())
        (set-renderer-state-destroyed?! r #t))
      (void))

    ;; dom-node-click! : dom-node? -> void?
    ;;   Invoke the node click callback when present.
    (define (dom-node-click! n)
      (define on-click (dom-node-on-click n))
      (when on-click
        (on-click)))

    ;; dom-node-change! : dom-node? any/c -> void?
    ;;   Update node text and invoke the change callback when present.
    (define (dom-node-change! n value)
      (define text-value (value->text value))
      (set-dom-node-text! n text-value)
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-toggle! : dom-node? boolean? -> void?
    ;;   Update checkbox checked attribute and invoke the change callback.
    (define (dom-node-toggle! n checked?)
      (set-dom-node-attrs!
       n
       (list (cons 'checked (not (not checked?)))))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change (not (not checked?)))))

    ;; dom-node-select! : dom-node? any/c -> void?
    ;;   Update selected attribute and invoke the change callback.
    (define (dom-node-select! n selected)
      (set-dom-node-attrs!
       n
       (list (cons 'choices (cdr (assq 'choices (dom-node-attrs n))))
             (cons 'selected selected)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-slide! : dom-node? number? -> void?
    ;;   Update slider value attribute and invoke the change callback.
    (define (dom-node-slide! n value)
      (define attrs     (dom-node-attrs n))
      (define min-pair  (assq 'min attrs))
      (define max-pair  (assq 'max attrs))
      (define min-value (if min-pair (cdr min-pair) 0))
      (define max-value (if max-pair (cdr max-pair) 100))
      (set-dom-node-attrs!
       n
       (list (cons 'min min-value)
             (cons 'max max-value)
             (cons 'value value)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-radio-select! : dom-node? any/c -> void?
    ;;   Update radio selected attribute and invoke the change callback.
    (define (dom-node-radio-select! n selected)
      (define attrs         (dom-node-attrs n))
      (define choices-pair  (assq 'choices attrs))
      (define choices-value (if choices-pair (cdr choices-pair) '()))
      (set-dom-node-attrs!
       n
       (list (cons 'choices choices-value)
             (cons 'selected selected)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-keydown! : dom-node? string? -> void?
    ;;   Dispatch keydown payload for tabs, input Enter actions, and menu-item key activation.
    (define (dom-node-keydown! n key)
      (define on-click      (dom-node-on-click n))
      (define on-change     (dom-node-on-change n))
      (define on-enter-pair (assq 'on-enter-action (dom-node-attrs n)))
      (define role-pair     (assq 'role (dom-node-attrs n)))
      (when (and on-enter-pair
                 (procedure? (cdr on-enter-pair))
                 (string=? key "Enter"))
        ((cdr on-enter-pair)))
      (when (and on-click
                 role-pair
                 (or (eq? (cdr role-pair) 'button)
                     (eq? (cdr role-pair) 'menuitem))
                 (or (string=? key "Enter")
                     (string=? key " ")))
        (on-click))
      (when (and on-change
                 role-pair
                 (or (eq? (cdr role-pair) 'tab)
                     (eq? (cdr role-pair) 'button)
                     (eq? (cdr role-pair) 'menuitem)
                     (eq? (cdr role-pair) 'dialog)))
        (on-change key)))

    ;; alist-ref : (listof pair?) symbol? symbol? -> any/c
    ;;   Look up key in props, raising an argument error when missing.
    (define (alist-ref props key who)
      (define p (assq key props))
      (if p
          (cdr p)
          (raise-arguments-error who
                                 "missing property"
                                 "key"
                                 key
                                 "props"
                                 props)))

    ;; ensure-list : any/c symbol? symbol? -> list?
    ;;   Validate that v is a list for argument-name in who.
    (define (ensure-list v who argument-name)
      (unless (list? v)
        (raise-arguments-error who
                               "expected list?"
                               argument-name
                               v))
      v)

    ;; value->text : any/c -> string?
    ;;   Convert supported primitive values to display text.
    (define (value->text v)
      (cond
        [(string? v) v]
        [(number? v) (number->string v)]
        [(symbol? v) (symbol->string v)]
        [else        text/fallback]))

    ;; next-tab-panel-id : -> string?
    ;;   Allocate a unique id string for tab-panel content region.
    (define (next-tab-panel-id)
      (set! tab-panel-counter (add1 tab-panel-counter))
      (string-append "tab-panel-" (number->string tab-panel-counter)))

    ;; next-menu-popup-id : -> string?
    ;;   Allocate a unique id string for menu popup region.
    (define (next-menu-popup-id)
      (set! menu-popup-counter (add1 menu-popup-counter))
      (string-append "menu-popup-" (number->string menu-popup-counter)))

    ;; normalize-tab-entry : any/c -> list?
    ;;   Normalize tab entry to (list id view disabled?) supporting pair or list forms.
    (define (normalize-tab-entry tab)
      (cond
        [(list? tab)
         (define n (length tab))
         (cond
           [(= n 2)
            (list (list-ref tab 0) (list-ref tab 1) #f)]
           [(= n 3)
            (list (list-ref tab 0) (list-ref tab 1) (not (not (list-ref tab 2))))]
           [else
            (raise-arguments-error 'tab-panel
                                   "expected tab entry of arity 2 or 3"
                                   "tab"
                                   tab)])]
        [(pair? tab)
         (list (car tab) (cdr tab) #f)]
        [else
         (raise-arguments-error 'tab-panel
                                "expected pair? or list?"
                                "tab"
                                tab)]))

    ;; render-list-items : dom-node? list? list? procedure? procedure? procedure? -> list?
    ;;   Render entries into parent using keyed node reuse and return new item state.
    (define (render-list-items parent entries old-items key-proc make-view-proc register-cleanup!)
      (define new-items
        (map (lambda (entry)
               (define key      (key-proc entry))
               (define old-item (assoc key old-items))
               (cond
                 [(and old-item (equal? (cadr old-item) entry))
                  (list key entry (caddr old-item))]
                 [else
                  (define child-view (make-view-proc key entry))
                  (define child-node (build-node child-view register-cleanup!))
                  (list key entry child-node)]))
             entries))
      (backend-replace-children! parent (map caddr new-items))
      new-items)

    ;; replace-with-single-child! : dom-node? view? procedure? -> void?
    ;;   Replace parent children with a single child rendered from child-view.
    (define (replace-with-single-child! parent child-view register-cleanup!)
      (backend-set-single-child! parent (build-node child-view register-cleanup!)))

    ;; cond-clause-active? : any/c -> boolean?
    ;;   Check whether a cond clause test value counts as true.
    (define (cond-clause-active? v)
      (not (eq? v #f)))

    ;; maybe-observable-value : any/c -> any/c
    ;;   Read observable content when v is observable, otherwise return v.
    (define (maybe-observable-value v)
      (if (obs? v) (obs-peek v) v))

    ;; normalize-table-density : any/c -> symbol?
    ;;   Normalize density to 'normal or 'compact.
    (define (normalize-table-density density)
      (if (symbol? density)
          (case density
            [(normal compact) density]
            [else             table/density-normal])
          table/density-normal))

    ;; density-class : symbol? -> string?
    ;;   Return CSS class for table density variants.
    (define (density-class density)
      (case density
        [(compact) "we-density-compact"]
        [else      "we-density-normal"]))

    ;; render-table-rows! : dom-node? list? list? symbol? -> void?
    ;;   Replace table rows with a header row and data rows rendered as table cells.
    (define (render-table-rows! table-node columns rows density)
      (define normalized-rows (ensure-list rows 'table "rows"))
      (define (header-cell column)
        (define density-css (density-class density))
        (dom-node 'th
                  (list (cons 'data-we-widget "table-header-cell")
                        (cons 'class (string-append "we-table-header-cell " density-css)))
                  '()
                  (value->text column)
                  #f
                  #f))
      (define (data-cell cell-value)
        (define density-css (density-class density))
        (dom-node 'td
                  (list (cons 'data-we-widget "table-data-cell")
                        (cons 'class (string-append "we-table-data-cell " density-css)))
                  '()
                  (value->text cell-value)
                  #f
                  #f))
      (define (row-values row)
        (if (list? row)
            row
            (list row)))
      (define (build-row cell-values build-cell)
        (define row-node (dom-node 'tr (list (cons 'data-we-widget "table-row")) '() #f #f #f))
        (backend-replace-children! row-node (map build-cell cell-values))
        row-node)
      (define header-row
        (if (null? columns)
            '()
            (list (build-row columns header-cell))))
      (define data-rows
        (map (lambda (row)
               (build-row (row-values row) data-cell))
             normalized-rows))
      (backend-replace-children! table-node (append header-row data-rows)))

    ;; build-node : view? (-> (-> void?) void?) -> dom-node?
    ;;   Build a dom-node tree from v and register lifecycle cleanups.
    (define (build-node v register-cleanup!)
      (define kind (view-kind v))
      (case kind
        [(window)
         (define node (dom-node 'div (list (cons attr/role 'window)
                                           (cons 'data-we-widget "window")) '() #f #f #f))
         (define style-node (dom-node 'style '() '() shared-style-text #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         (backend-append-child! node style-node)
         node]
        [(vpanel)
         (define node (dom-node 'div (list (cons 'data-we-widget "vpanel")
                                           (cons 'class "we-vpanel")) '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(hpanel)
         (define node (dom-node 'div (list (cons 'data-we-widget "hpanel")
                                           (cons 'class "we-hpanel")) '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(group)
         (define raw-label (alist-ref (view-props v) 'label 'render))
         (define node (dom-node 'group
                                (list (cons 'data-we-widget "group")
                                      (cons 'class "we-group"))
                                '()
                                #f
                                #f
                                #f))
         (define legend-node (dom-node 'legend '() '() "" #f #f))
         (define (set-label! label-value)
           (set-dom-node-text! legend-node (value->text label-value)))
         (cond
           [(obs? raw-label)
            (set-label! (obs-peek raw-label))
            (define (listener updated)
              (set-label! updated))
            (obs-observe! raw-label listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
           [else
            (set-label! raw-label)])
         (backend-append-child! node legend-node)
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(text)
         (define raw  (alist-ref (view-props v) 'value 'render))
         (define node (dom-node 'span (list (cons 'data-we-widget "text")) '() "" #f #f))
         (cond
           [(obs? raw)
            (set-dom-node-text! node (value->text (obs-peek raw)))
            (define (listener updated)
              (set-dom-node-text! node (value->text updated)))
            (obs-observe! raw listener)
            (register-cleanup! (lambda () (obs-unobserve! raw listener)))]
           [else
           (set-dom-node-text! node (value->text raw))])
         node]
        [(button)
         (define label  (alist-ref (view-props v) 'label  'render))
         (define action (alist-ref (view-props v) 'action 'render))
         (dom-node 'button
                   (list (cons 'data-we-widget "button")
                         (cons 'class "we-button"))
                   '()
                   (value->text label)
                   action
                   #f)]
        [(input)
         (define raw-value (alist-ref (view-props v) 'value    'render))
         (define action    (alist-ref (view-props v) 'action   'render))
         (define on-enter  (alist-ref (view-props v) 'on-enter 'render))
         (define node (dom-node 'input
                                (list (cons 'value "")
                                      (cons 'data-we-widget "input")
                                      (cons 'class "we-input")
                                      (cons 'on-enter-action on-enter))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-value) (action new-value)))
         (define (set-input-value! value)
           (set-dom-node-attrs! node (list (cons 'value (value->text value))
                                           (cons 'data-we-widget "input")
                                           (cons 'class "we-input")
                                           (cons 'on-enter-action on-enter))))
         (cond
           [(obs? raw-value)
            (set-input-value! (obs-peek raw-value))
            (define (listener updated)
              (set-input-value! updated))
            (obs-observe! raw-value listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
           [else
            (set-input-value! raw-value)])
         node]
        [(checkbox)
         (define raw-value (alist-ref (view-props v) 'value  'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'checkbox
                                (list (cons 'checked #f)
                                      (cons 'data-we-widget "checkbox")
                                      (cons 'class "we-checkbox"))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-checked) (action (not (not new-checked)))))
         (define (set-checked! v)
           (set-dom-node-attrs! node (list (cons 'checked (not (not v)))
                                           (cons 'data-we-widget "checkbox")
                                           (cons 'class "we-checkbox"))))
         (cond
           [(obs? raw-value)
            (set-checked! (obs-peek raw-value))
            (define (listener updated)
              (set-checked! updated))
            (obs-observe! raw-value listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
           [else
            (set-checked! raw-value)])
         node]
        [(choice)
         (define choices      (ensure-list (alist-ref (view-props v) 'choices 'render)
                                           'choice
                                           "choices"))
         (define raw-selected (alist-ref (view-props v) 'selected 'render))
         (define action       (alist-ref (view-props v) 'action   'render))
         (define node (dom-node 'select
                                (list (cons 'choices choices)
                                      (cons 'data-we-widget "choice")
                                      (cons 'class "we-choice")
                                      (cons 'selected #f))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-selected) (action new-selected)))
         (define (set-selected! v)
           (set-dom-node-attrs!
            node
            (list (cons 'choices  choices)
                  (cons 'data-we-widget "choice")
                  (cons 'class    "we-choice")
                  (cons 'selected v))))
         (cond
           [(obs? raw-selected)
            (set-selected! (obs-peek raw-selected))
            (define (listener updated)
              (set-selected! updated))
            (obs-observe! raw-selected listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-selected listener)))]
           [else
            (set-selected! raw-selected)])
         node]
        [(slider)
         (define raw-value (alist-ref (view-props v) 'value  'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define min-value (alist-ref (view-props v) 'min    'render))
         (define max-value (alist-ref (view-props v) 'max    'render))
         (define node (dom-node 'slider
                                (list (cons 'min   min-value)
                                      (cons 'max   max-value)
                                      (cons 'data-we-widget "slider")
                                      (cons 'class "we-slider")
                                      (cons 'value 0))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-value) (action new-value)))
         (define (set-slider-value! v)
           (set-dom-node-attrs!
            node
            (list (cons 'min min-value)
                  (cons 'max max-value)
                  (cons 'data-we-widget "slider")
                  (cons 'class "we-slider")
                  (cons 'value v))))
         (cond
           [(obs? raw-value)
            (set-slider-value! (obs-peek raw-value))
            (define (listener updated)
              (set-slider-value! updated))
            (obs-observe! raw-value listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
           [else
            (set-slider-value! raw-value)])
         node]
        [(progress)
         (define raw-value (alist-ref (view-props v) 'value 'render))
         (define min-value (alist-ref (view-props v) 'min   'render))
         (define max-value (alist-ref (view-props v) 'max   'render))
         (define node (dom-node 'progress
                                (list (cons 'min   min-value)
                                      (cons 'max   max-value)
                                      (cons 'data-we-widget "progress")
                                      (cons 'class "we-progress")
                                      (cons 'value 0))
                                '()
                                #f
                                #f
                                #f))
         (define (set-progress-value! v)
           (set-dom-node-attrs!
            node
            (list (cons 'min   min-value)
                  (cons 'max   max-value)
                  (cons 'data-we-widget "progress")
                  (cons 'class "we-progress")
                  (cons 'value v))))
         (cond
           [(obs? raw-value)
            (set-progress-value! (obs-peek raw-value))
            (define (listener updated)
              (set-progress-value! updated))
            (obs-observe! raw-value listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
           [else
            (set-progress-value! raw-value)])
         node]
        [(if-view)
         (define raw-cond  (alist-ref (view-props v) 'cond 'render))
         (define then-view (alist-ref (view-props v) 'then 'render))
         (define else-view (alist-ref (view-props v) 'else 'render))
         (define node (dom-node 'div (list (cons 'data-we-widget "if-view")
                                           (cons 'class "we-if-view")) '() #f #f #f))
         (define (render-branch! cond-value)
           (replace-with-single-child! node
                                       (if (cond-clause-active? cond-value) then-view else-view)
                                       register-cleanup!))
         (cond
           [(obs? raw-cond)
            (render-branch! (obs-peek raw-cond))
            (define (listener updated)
              (render-branch! updated))
            (obs-observe! raw-cond listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-cond listener)))]
           [else
            (render-branch! raw-cond)])
         node]
        [(cond-view)
         (define clauses   (ensure-list (alist-ref (view-props v) 'clauses 'render)
                                        'cond-view
                                        "clauses"))
         (define else-view (alist-ref (view-props v) 'else 'render))
         (define node (dom-node 'div (list (cons 'data-we-widget "cond-view")
                                           (cons 'class "we-cond-view")) '() #f #f #f))
         (define (choose-view)
           (define selected
             (let loop ([cs clauses])
               (cond
                 [(null? cs) #f]
                 [else
                  (define clause (car cs))
                  (define test-value (maybe-observable-value (car clause)))
                  (if (cond-clause-active? test-value)
                      (cdr clause)
                      (loop (cdr cs)))])))
           (if selected selected else-view))
         (define (render-branch!)
           (replace-with-single-child! node (choose-view) register-cleanup!))
         (for-each (lambda (clause)
                     (define raw-test (car clause))
                     (when (obs? raw-test)
                       (define (listener _updated)
                         (render-branch!))
                       (obs-observe! raw-test listener)
                       (register-cleanup! (lambda () (obs-unobserve! raw-test listener)))))
                   clauses)
         (render-branch!)
         node]
        [(case-view)
         (define raw-value (alist-ref (view-props v) 'value 'render))
         (define clauses   (ensure-list (alist-ref (view-props v) 'clauses 'render)
                                        'case-view
                                        "clauses"))
         (define else-view (alist-ref (view-props v) 'else 'render))
         (define node (dom-node 'div (list (cons 'data-we-widget "case-view")
                                           (cons 'class "we-case-view")) '() #f #f #f))
         (define (choose-view v*)
           (define selected
             (let loop ([cs clauses])
               (cond
                 [(null? cs) #f]
                 [else
                  (define clause (car cs))
                  (define lits (ensure-list (car clause) 'case-view "clause literals"))
                  (if (member v* lits)
                      (cdr clause)
                      (loop (cdr cs)))])))
           (if selected selected else-view))
         (define (render-branch! v*)
           (replace-with-single-child! node (choose-view v*) register-cleanup!))
         (cond
           [(obs? raw-value)
            (render-branch! (obs-peek raw-value))
            (define (listener updated)
              (render-branch! updated))
            (obs-observe! raw-value listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
           [else
            (render-branch! raw-value)])
         node]
        [(tab-panel)
         (define raw-selected (alist-ref (view-props v) 'selected 'render))
         (define tabs/raw     (ensure-list (alist-ref (view-props v) 'tabs 'render)
                                           'tab-panel
                                           "tabs"))
         (define tabs         (map normalize-tab-entry tabs/raw))
         (define panel-id     (next-tab-panel-id))
         (define node (dom-node 'tab-panel (list (cons 'selected #f)
                                                 (cons 'data-we-widget "tab-panel")
                                                 (cons 'class    "we-tab-panel"))
                               '()
                               #f
                               #f
                               #f))
         (define tabs-node  (dom-node 'div (list (cons attr/role   'tablist)
                                                 (cons 'data-we-widget "tab-list")
                                                 (cons 'class      "we-tab-list"))
                                      '()
                                      #f
                                      #f
                                      #f))
         (define content-node (dom-node 'div (list (cons attr/role 'tabpanel)
                                                   (cons 'id       panel-id)
                                                   (cons 'data-we-widget "tab-content")
                                                   (cons 'aria-labelledby "")
                                                   (cons 'class    "we-tab-content"))
                                        '()
                                        #f
                                        #f
                                        #f))
         (backend-append-child! node tabs-node)
         (backend-append-child! node content-node)
         (define tab-buttons    '())
         (define selected-value #f)
         (define enabled-tab-ids
           (map car (filter (lambda (tab) (not (list-ref tab 2))) tabs)))
         (define (choose-view selected)
           (define selected-view
             (let loop ([ts tabs])
               (cond
                 [(null? ts) #f]
                 [else
                  (define tab (car ts))
                  (if (and (equal? (car tab) selected)
                           (not (list-ref tab 2)))
                      (list-ref tab 1)
                      (loop (cdr ts)))])))
           (cond
             [selected-view selected-view]
             [(null? enabled-tab-ids)
              (if (null? tabs)
                  (spacer)
                  (list-ref (car tabs) 1))]
             [else
              (let loop ([ts tabs])
                (define tab (car ts))
                (if (equal? (car tab) (car enabled-tab-ids))
                    (list-ref tab 1)
                    (loop (cdr ts))))]))
         (define (tab-disabled? tab-id)
           (let loop ([ts tabs])
             (cond
               [(null? ts) #t]
               [else
                (define tab (car ts))
                (if (equal? (car tab) tab-id)
                    (list-ref tab 2)
                    (loop (cdr ts)))])))
         (define (set-selected! selected)
           (define selected-button-id
             (let loop ([entries tab-buttons])
               (cond
                 [(null? entries) ""]
                 [else
                  (define entry (car entries))
                  (if (equal? (list-ref entry 0) selected)
                      (list-ref entry 1)
                      (loop (cdr entries)))])))
           (set-dom-node-attrs!
            content-node
           (list (cons attr/role 'tabpanel)
                  (cons 'id panel-id)
                  (cons 'data-we-widget "tab-content")
                  (cons 'aria-labelledby selected-button-id)
                  (cons 'class "we-tab-content")))
           (set-dom-node-attrs! node (list (cons 'selected selected)
                                           (cons 'data-we-widget "tab-panel")
                                           (cons 'class "we-tab-panel")))
           (set! selected-value selected)
           (for-each (lambda (entry)
                       (define tab-id (list-ref entry 0))
                       (define button-id (list-ref entry 1))
                       (define button-node (list-ref entry 2))
                       (define disabled? (tab-disabled? tab-id))
                       (set-dom-node-attrs!
                        button-node
                        (list (cons 'tab-id tab-id)
                              (cons 'id button-id)
                              (cons 'role 'tab)
                              (cons 'data-we-widget "tab-button")
                              (cons 'aria-controls panel-id)
                              (cons 'aria-disabled disabled?)
                              (cons 'aria-selected (and (equal? tab-id selected) (not disabled?)))
                              (cons 'tabindex (if (and (equal? tab-id selected) (not disabled?)) 0 -1))
                              (cons 'class (cond
                                             [disabled? "we-tab-btn is-disabled"]
                                             [(equal? tab-id selected) "we-tab-btn is-selected"]
                                             [else "we-tab-btn"])))))
                     tab-buttons))
         (define (index-of-tab selected)
           (let loop ([i 0] [ids enabled-tab-ids])
             (cond
               [(null? ids) 0]
               [(equal? (car ids) selected) i]
               [else (loop (add1 i) (cdr ids))])))
         (define (tab-at index)
           (list-ref enabled-tab-ids index))
         (define (next-tab-id)
           (if (null? enabled-tab-ids)
               #f
               (let* ([count (length enabled-tab-ids)]
                      [i (index-of-tab selected-value)]
                      [j (modulo (+ i 1) count)])
                 (tab-at j))))
         (define (prev-tab-id)
           (if (null? enabled-tab-ids)
               #f
               (let* ([count (length enabled-tab-ids)]
                      [i (index-of-tab selected-value)]
                      [j (modulo (+ i (- count 1)) count)])
                 (tab-at j))))
         (define (first-tab-id)
           (if (null? enabled-tab-ids) #f (car enabled-tab-ids)))
         (define (last-tab-id)
           (if (null? enabled-tab-ids) #f (list-ref enabled-tab-ids (- (length enabled-tab-ids) 1))))
         (define (select-if-possible tab-id)
           (when (and tab-id (obs? raw-selected) (not (tab-disabled? tab-id)))
             (obs-set! raw-selected tab-id)))
         (define (handle-tab-key key)
           (case (string->symbol key)
             [(ArrowRight) (select-if-possible (next-tab-id))]
             [(ArrowLeft)  (select-if-possible (prev-tab-id))]
             [(Home)       (select-if-possible (first-tab-id))]
             [(End)        (select-if-possible (last-tab-id))]
             [else (void)]))
         (define (init-tabs!)
           (set! tab-buttons
                 (let loop ([remaining tabs] [idx 0])
                   (cond
                     [(null? remaining) '()]
                     [else
                      (define tab (car remaining))
                        (define tab-id (car tab))
                        (define button-id
                          (string-append panel-id "-tab-" (number->string idx)))
                        (define disabled? (list-ref tab 2))
                        (define button-node
                          (dom-node 'button
                                    (list (cons 'tab-id   tab-id)
                                          (cons 'id button-id)
                                          (cons 'selected #f))
                                    '()
                                    (value->text tab-id)
                                    #f
                                    #f))
                        (when (obs? raw-selected)
                          (set-dom-node-on-click!
                           button-node
                           (lambda ()
                             (unless disabled?
                               (obs-set! raw-selected tab-id))))
                          (set-dom-node-on-change!
                           button-node
                           (lambda (key)
                             (unless disabled?
                               (handle-tab-key key)))))
                        (cons (list tab-id button-id button-node)
                              (loop (cdr remaining) (add1 idx)))])))
           (backend-replace-children! tabs-node (map (lambda (entry) (list-ref entry 2)) tab-buttons)))
         (define (render-tab! selected)
           (set-selected! selected)
           (backend-set-single-child! content-node (build-node (choose-view selected) register-cleanup!)))
         (init-tabs!)
         (cond
           [(obs? raw-selected)
            (render-tab! (obs-peek raw-selected))
            (define (listener updated)
              (render-tab! updated))
            (obs-observe! raw-selected listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-selected listener)))]
           [else
            (render-tab! raw-selected)])
         node]
        [(dialog)
         (define raw-open  (alist-ref (view-props v) 'open 'render))
         (define on-close  (alist-ref (view-props v) 'on-close 'render))
         (define node (dom-node 'dialog
                                (list (cons attr/role 'dialog)
                                      (cons 'data-we-widget "dialog")
                                      (cons 'class "we-dialog")
                                      (cons 'tabindex -1)
                                      (cons 'aria-modal "true")
                                      (cons 'aria-hidden "true"))
                                '()
                                #f
                                #f
                                #f))
         (define panel-node (dom-node 'div
                                      (list (cons 'class "we-dialog-panel")
                                            (cons 'data-we-widget "dialog-panel")
                                            (cons 'tabindex -1))
                                      '()
                                      #f
                                      #f
                                      #f))
         (define (set-open! open?)
           (define open-value (not (eq? open? #f)))
           (set-dom-node-attrs!
            node
            (list (cons attr/role 'dialog)
                  (cons 'data-we-widget "dialog")
                  (cons 'open open-value)
                  (cons 'class (if open-value "we-dialog is-open" "we-dialog"))
                  (cons 'tabindex -1)
                  (cons 'aria-modal "true")
                  (cons 'aria-hidden (if open-value "false" "true")))))
         (when (procedure? on-close)
           (set-dom-node-on-change!
            node
            (lambda (key)
              (when (string=? key "Escape")
                (on-close)))))
         (backend-append-child! node panel-node)
         (for-each (lambda (child)
                     (backend-append-child! panel-node (build-node child register-cleanup!)))
                   (view-children v))
         (cond
           [(obs? raw-open)
            (set-open! (obs-peek raw-open))
            (define (listener updated)
              (set-open! updated))
            (obs-observe! raw-open listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-open listener)))]
           [else
            (set-open! raw-open)])
         node]
        [(observable-view)
         (define raw-data    (alist-ref (view-props v) 'data       'render))
         (define make-view   (alist-ref (view-props v) 'make-view  'render))
         (define equal-proc  (alist-ref (view-props v) 'equal-proc 'render))
         (define node (dom-node 'div (list (cons 'data-we-widget "observable-view")
                                           (cons 'class "we-observable-view")) '() #f #f #f))
         (define last-value #f)
         (define have-last? #f)
         (define (render-from-value! value)
           (set! have-last? #t)
           (set! last-value value)
           (replace-with-single-child! node (make-view value) register-cleanup!))
         (cond
           [(obs? raw-data)
            (render-from-value! (obs-peek raw-data))
            (define (listener updated)
              (unless (and have-last? (equal-proc updated last-value))
                (render-from-value! updated)))
            (obs-observe! raw-data listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-data listener)))]
           [else
            (render-from-value! raw-data)])
         node]
        [(spacer)
         (dom-node 'spacer (list (cons 'data-we-widget "spacer")) '() #f #f #f)]
        [(table)
         (define columns (ensure-list (alist-ref (view-props v) 'columns 'render)
                                      'table
                                      "columns"))
         (define raw-rows    (alist-ref (view-props v) 'rows    'render))
         (define raw-density (alist-ref (view-props v) 'density 'render))
         (define density     (normalize-table-density (maybe-observable-value raw-density)))
         (define density-css (density-class density))
         (define node (dom-node 'table
                                (list (cons 'columns columns)
                                      (cons 'data-we-widget "table")
                                      (cons 'density density)
                                      (cons 'class (string-append "we-table " density-css)))
                                '()
                                #f
                                #f
                                #f))
         (cond
           [(obs? raw-rows)
            (render-table-rows! node columns (obs-peek raw-rows) density)
            (define (listener updated)
              (render-table-rows! node columns updated density))
            (obs-observe! raw-rows listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-rows listener)))]
           [else
            (render-table-rows! node columns raw-rows density)])
         node]
        [(radios)
         (define choices      (ensure-list (alist-ref (view-props v) 'choices 'render)
                                           'radios
                                           "choices"))
         (define raw-selected (alist-ref (view-props v) 'selected 'render))
         (define action       (alist-ref (view-props v) 'action   'render))
         (define node (dom-node 'radios
                                (list (cons 'choices choices)
                                      (cons 'data-we-widget "radios")
                                      (cons 'class "we-radios")
                                      (cons 'selected #f))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-selected) (action new-selected)))
         (define (set-selected! selected)
           (set-dom-node-attrs!
            node
            (list (cons 'choices  choices)
                  (cons 'data-we-widget "radios")
                  (cons 'class    "we-radios")
                  (cons 'selected selected))))
         (cond
           [(obs? raw-selected)
            (set-selected! (obs-peek raw-selected))
            (define (listener updated)
              (set-selected! updated))
            (obs-observe! raw-selected listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-selected listener)))]
           [else
            (set-selected! raw-selected)])
         node]
        [(image)
         (define raw-src    (alist-ref (view-props v) 'src    'render))
         (define raw-width  (alist-ref (view-props v) 'width  'render))
         (define raw-height (alist-ref (view-props v) 'height 'render))
         (define node (dom-node 'image
                                (list (cons 'src "")
                                      (cons 'data-we-widget "image")
                                      (cons 'class "we-image"))
                                '()
                                #f
                                #f
                                #f))
         (define (with-optional-attr attrs key value)
           (if (eq? value #f)
               attrs
               (append attrs (list (cons key value)))))
         (define (current-value v)
           (if (obs? v) (obs-peek v) v))
         (define (set-image-attrs! src width height)
           (define attrs/base (list (cons 'src   (value->text src))
                                    (cons 'data-we-widget "image")
                                    (cons 'class "we-image")))
           (define attrs/width (with-optional-attr attrs/base  'width  width))
           (define attrs/final (with-optional-attr attrs/width 'height height))
           (set-dom-node-attrs! node attrs/final))
         (define (refresh-image!)
           (set-image-attrs! (current-value raw-src)
                             (current-value raw-width)
                             (current-value raw-height)))
         (refresh-image!)
         (when (obs? raw-src)
           (define (src-listener _updated)
             (refresh-image!))
           (obs-observe! raw-src src-listener)
           (register-cleanup! (lambda () (obs-unobserve! raw-src src-listener))))
         (when (obs? raw-width)
           (define (width-listener _updated)
             (refresh-image!))
           (obs-observe! raw-width width-listener)
           (register-cleanup! (lambda () (obs-unobserve! raw-width width-listener))))
         (when (obs? raw-height)
           (define (height-listener _updated)
             (refresh-image!))
           (obs-observe! raw-height height-listener)
           (register-cleanup! (lambda () (obs-unobserve! raw-height height-listener))))
         node]
        [(menu-bar)
         (define node (dom-node 'menu-bar
                                (list (cons 'class "we-menu-bar")
                                      (cons 'data-we-widget "menu-bar")
                                      (cons attr/role 'menubar)
                                      (cons 'aria-orientation "horizontal"))
                                '()
                                #f
                                #f
                                #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(menu)
         (define raw-label (alist-ref (view-props v) 'label 'render))
         (define popup-id (next-menu-popup-id))
         (define open? #f)
         (define node (dom-node 'menu
                                (list (cons 'class "we-menu")
                                      (cons 'data-we-widget "menu"))
                                '()
                                #f
                                #f
                                #f))
         (define label-node (dom-node 'button
                                      (list (cons attr/role 'button)
                                            (cons 'class "we-menu-label")
                                            (cons 'data-we-widget "menu-label")
                                            (cons 'menu-trigger #t)
                                            (cons 'tabindex 0)
                                            (cons 'aria-haspopup "true")
                                            (cons 'aria-controls popup-id)
                                            (cons 'aria-expanded "false"))
                                      '()
                                      ""
                                      (lambda ()
                                        (set-open! (not open?)))
                                      (lambda (key)
                                        (case (string->symbol key)
                                          [(ArrowDown)
                                           (set-open! #t)]
                                          [(mouseenter)
                                           (when (and active-menu-close
                                                      (not open?))
                                             (set-open! #t))]
                                          [(focusout)
                                           (set-open! #f)]
                                          [(Escape)
                                           (set-open! #f)]
                                          [else
                                           (void)]))))
         (define popup-node (dom-node 'vpanel
                                      (list (cons attr/role 'menu)
                                            (cons 'id popup-id)
                                            (cons 'data-we-widget "menu-popup")
                                            (cons 'class "we-menu-popup"))
                                      '()
                                      #f
                                      #f
                                      #f))
         (define close-self!
           (lambda ()
             (set-open! #f)))
         ;; set-open! : boolean? -> void?
         ;;   Toggle popup visibility and update menu trigger aria state.
         (define (set-open! next-open?)
           (when (and next-open?
                      active-menu-close
                      (not (eq? active-menu-close close-self!)))
             (active-menu-close))
           (set! open? (not (not next-open?)))
           (when open?
             (set! active-menu-close close-self!))
           (when (and (not open?)
                      active-menu-close
                      (eq? active-menu-close close-self!))
             (set! active-menu-close #f))
           (set-dom-node-attrs!
           label-node
            (list (cons attr/role 'button)
                  (cons 'class "we-menu-label")
                  (cons 'data-we-widget "menu-label")
                  (cons 'menu-trigger #t)
                  (cons 'tabindex 0)
                  (cons 'aria-haspopup "true")
                  (cons 'aria-controls popup-id)
                  (cons 'aria-expanded (if open? "true" "false"))))
          (set-dom-node-attrs!
            popup-node
            (list (cons attr/role 'menu)
                  (cons 'id popup-id)
                  (cons 'data-we-widget "menu-popup")
                  (cons 'class (if open? "we-menu-popup is-open" "we-menu-popup")))))
         (define (set-label! label-value)
           (set-dom-node-text! label-node (value->text label-value)))
         (cond
           [(obs? raw-label)
            (set-label! (obs-peek raw-label))
            (define (listener updated)
              (set-label! updated))
            (obs-observe! raw-label listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
           [else
            (set-label! raw-label)])
         (backend-append-child! node label-node)
         (backend-append-child! node popup-node)
         (for-each (lambda (child)
                     (define child-node (build-node child register-cleanup!))
                     (when (eq? (dom-node-tag child-node) 'menu-item)
                       (define on-click (dom-node-on-click child-node))
                       (when on-click
                         (set-dom-node-on-click!
                          child-node
                          (lambda ()
                            (on-click)
                            (set-open! #f))))
                       (set-dom-node-on-change!
                        child-node
                        (lambda (key)
                          (case (string->symbol key)
                            [(focusout)
                             (set-open! #f)]
                            [(Escape)
                             (set-open! #f)]
                            [else
                             (void)]))))
                     (backend-append-child! popup-node child-node))
                   (view-children v))
         node]
        [(menu-item)
         (define raw-label (alist-ref (view-props v) 'label  'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'menu-item
                                (list (cons attr/role 'menuitem)
                                      (cons 'class    "we-menu-item")
                                      (cons 'data-we-widget "menu-item")
                                      (cons 'tabindex 0))
                                '()
                                ""
                                action
                                #f))
         (cond
           [(obs? raw-label)
            (set-dom-node-text! node (value->text (obs-peek raw-label)))
            (define (listener updated)
              (set-dom-node-text! node (value->text updated)))
            (obs-observe! raw-label listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
           [else
            (set-dom-node-text! node (value->text raw-label))])
         node]
        [(list-view)
         (define raw-entries (alist-ref (view-props v) 'entries   'render))
         (define key-proc    (alist-ref (view-props v) 'key       'render))
         (define make-view   (alist-ref (view-props v) 'make-view 'render))
         (define node (dom-node 'div (list (cons 'data-we-widget "list-view")
                                           (cons 'class "we-list-view")) '() #f #f #f))
         (define items '())
         (define (render-from-entries entries)
           (set! items
             (render-list-items node
                                (ensure-list entries 'list-view "entries")
                                items
                                key-proc
                                make-view
                                register-cleanup!)))
         (cond
           [(obs? raw-entries)
            (render-from-entries (obs-peek raw-entries))
            (define (listener updated-entries)
              (render-from-entries updated-entries))
            (obs-observe! raw-entries listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-entries listener)))]
           [else
            (render-from-entries raw-entries)])
         node]
        [else
         (raise-arguments-error 'render
                                "unknown view kind"
                                "kind"
                                kind)]))

    ;; render : view? [renderer?] -> renderer?
    ;;   Create a renderer for v, optionally as a child of parent.
    ;;   Optional parameter parent defaults to #f.
    (define (render v [parent #f])
      (define cleanups '())
      (define (register-cleanup! thunk)
        (set! cleanups (cons thunk cleanups)))
      (define node (build-node v register-cleanup!))
      (when parent
        (unless (renderer? parent)
          (raise-arguments-error 'render
                                 "expected #f or renderer?"
                                 "parent"
                                 parent))
        (backend-append-child! (renderer-root parent) node))
      (renderer-state node cleanups #f))

    (values renderer?
            render
            renderer-root
            renderer-destroy
            dom-node-click!
            dom-node-change!
            dom-node-toggle!
            dom-node-select!
            dom-node-slide!
            dom-node-radio-select!
            dom-node-keydown!)))
