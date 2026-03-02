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
    (define attr/role   'role)   ; Attribute key for semantic role.
    (define attr/layout 'layout) ; Attribute key for layout direction.
    (define text/fallback "#<value>") ; Fallback when value cannot be rendered as text.
    (define tab-panel-counter 0) ; Monotonic counter for tab-panel ids.
    (define tab-panel-style-text
      ".we-tab-list{display:flex;gap:4px;align-items:stretch;}\
       .we-tab-btn{min-width:88px;padding:4px 8px;border:2px solid #999;background:#fff;font-weight:normal;}\
       .we-tab-btn.is-selected{border-color:#333;background:#ddd;font-weight:bold;}\
       .we-tab-btn.is-disabled{border-color:#bbb;background:#f3f3f3;color:#777;opacity:.7;}\
       .we-tab-btn:focus-visible{outline:2px solid #0a66c2;outline-offset:1px;}") ; CSS for class-based tab styles.

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
      (define attrs (dom-node-attrs n))
      (define min-pair (assq 'min attrs))
      (define max-pair (assq 'max attrs))
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
      (define attrs (dom-node-attrs n))
      (define choices-pair (assq 'choices attrs))
      (define choices-value (if choices-pair (cdr choices-pair) '()))
      (set-dom-node-attrs!
       n
       (list (cons 'choices choices-value)
             (cons 'selected selected)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-keydown! : dom-node? string? -> void?
    ;;   Dispatch keydown payload for tabs and Enter callbacks for inputs.
    (define (dom-node-keydown! n key)
      (define on-change (dom-node-on-change n))
      (define on-enter-pair (assq 'on-enter-action (dom-node-attrs n)))
      (define role-pair (assq 'role (dom-node-attrs n)))
      (when (and on-enter-pair
                 (procedure? (cdr on-enter-pair))
                 (string=? key "Enter"))
        ((cdr on-enter-pair)))
      (when (and on-change role-pair (eq? (cdr role-pair) 'tab))
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
        [else text/fallback]))

    ;; next-tab-panel-id : -> string?
    ;;   Allocate a unique id string for tab-panel content region.
    (define (next-tab-panel-id)
      (set! tab-panel-counter (add1 tab-panel-counter))
      (string-append "tab-panel-" (number->string tab-panel-counter)))

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
               (define key (key-proc entry))
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

    ;; render-table-rows! : dom-node? list? -> void?
    ;;   Replace table rows with row nodes rendered from rows data.
    (define (render-table-rows! table-node rows)
      (define row-nodes
        (map (lambda (row)
               (dom-node 'tr '() '() (value->text row) #f #f))
             (ensure-list rows 'table "rows")))
      (backend-replace-children! table-node row-nodes))

    ;; build-node : view? (-> (-> void?) void?) -> dom-node?
    ;;   Build a dom-node tree from v and register lifecycle cleanups.
    (define (build-node v register-cleanup!)
      (define kind (view-kind v))
      (case kind
        [(window)
         (define node (dom-node 'div (list (cons attr/role 'window)) '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(vpanel)
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(hpanel)
         (define node (dom-node 'div (list (cons attr/layout 'row)) '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(group)
         (define raw-label (alist-ref (view-props v) 'label 'render))
         (define node (dom-node 'group
                                (list (cons attr/layout 'column)
                                      (cons 'label ""))
                                '()
                                #f
                                #f
                                #f))
         (define (set-label! label-value)
           (set-dom-node-attrs!
            node
            (list (cons attr/layout 'column)
                  (cons 'label (value->text label-value)))))
         (cond
           [(obs? raw-label)
            (set-label! (obs-peek raw-label))
            (define (listener updated)
              (set-label! updated))
            (obs-observe! raw-label listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
           [else
            (set-label! raw-label)])
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(text)
         (define raw (alist-ref (view-props v) 'value 'render))
         (define node (dom-node 'span '() '() "" #f #f))
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
         (define label (alist-ref (view-props v) 'label 'render))
         (define action (alist-ref (view-props v) 'action 'render))
         (dom-node 'button '() '() (value->text label) action #f)]
        [(input)
         (define raw-value (alist-ref (view-props v) 'value 'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define on-enter  (alist-ref (view-props v) 'on-enter 'render))
         (define node (dom-node 'input
                                (list (cons 'value "")
                                      (cons 'on-enter-action on-enter))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-value) (action new-value)))
         (define (set-input-value! value)
           (set-dom-node-attrs! node (list (cons 'value (value->text value))
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
         (define raw-value (alist-ref (view-props v) 'value 'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'checkbox (list (cons 'checked #f)) '() #f #f #f))
         (set-dom-node-on-change! node (lambda (new-checked) (action (not (not new-checked)))))
         (define (set-checked! v)
           (set-dom-node-attrs! node (list (cons 'checked (not (not v))))))
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
         (define choices     (ensure-list (alist-ref (view-props v) 'choices 'render)
                                          'choice
                                          "choices"))
         (define raw-selected (alist-ref (view-props v) 'selected 'render))
         (define action       (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'select
                                (list (cons 'choices choices)
                                      (cons 'selected #f))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-selected) (action new-selected)))
         (define (set-selected! v)
           (set-dom-node-attrs!
            node
            (list (cons 'choices choices)
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
         (define raw-value (alist-ref (view-props v) 'value 'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define min-value (alist-ref (view-props v) 'min 'render))
         (define max-value (alist-ref (view-props v) 'max 'render))
         (define node (dom-node 'slider
                                (list (cons 'min min-value)
                                      (cons 'max max-value)
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
         (define min-value (alist-ref (view-props v) 'min 'render))
         (define max-value (alist-ref (view-props v) 'max 'render))
         (define node (dom-node 'progress
                                (list (cons 'min min-value)
                                      (cons 'max max-value)
                                      (cons 'value 0))
                                '()
                                #f
                                #f
                                #f))
         (define (set-progress-value! v)
           (set-dom-node-attrs!
            node
            (list (cons 'min min-value)
                  (cons 'max max-value)
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
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
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
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
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
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
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
                                                 (cons 'class "we-tab-panel"))
                               '()
                               #f
                               #f
                               #f))
         (define style-node (dom-node 'style '() '() tab-panel-style-text #f #f))
         (define tabs-node (dom-node 'div (list (cons attr/layout 'row)
                                                (cons attr/role 'tablist)
                                                (cons 'class "we-tab-list"))
                                     '()
                                     #f
                                     #f
                                     #f))
         (define content-node (dom-node 'div (list (cons attr/role 'tabpanel)
                                                   (cons 'id panel-id)
                                                   (cons 'class "we-tab-content"))
                                        '()
                                        #f
                                        #f
                                        #f))
         (backend-append-child! node style-node)
         (backend-append-child! node tabs-node)
         (backend-append-child! node content-node)
         (define tab-buttons '())
         (define selected-value #f)
         (define tab-ids (map car tabs))
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
           (set-dom-node-attrs! node (list (cons 'selected selected)))
           (set! selected-value selected)
           (for-each (lambda (entry)
                       (define tab-id (car entry))
                       (define button-node (cdr entry))
                       (define disabled? (tab-disabled? tab-id))
                       (set-dom-node-attrs!
                        button-node
                        (list (cons 'tab-id tab-id)
                              (cons 'role 'tab)
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
                 (map (lambda (tab)
                        (define tab-id (car tab))
                        (define disabled? (list-ref tab 2))
                        (define button-node
                          (dom-node 'button
                                    (list (cons 'tab-id tab-id)
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
                        (cons tab-id button-node))
                      tabs))
           (backend-replace-children! tabs-node (map cdr tab-buttons)))
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
        [(observable-view)
         (define raw-data    (alist-ref (view-props v) 'data 'render))
         (define make-view   (alist-ref (view-props v) 'make-view 'render))
         (define equal-proc  (alist-ref (view-props v) 'equal-proc 'render))
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
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
         (dom-node 'spacer '() '() #f #f #f)]
        [(table)
         (define columns (ensure-list (alist-ref (view-props v) 'columns 'render)
                                      'table
                                      "columns"))
         (define raw-rows (alist-ref (view-props v) 'rows 'render))
         (define node (dom-node 'table
                                (list (cons 'columns columns))
                                '()
                                #f
                                #f
                                #f))
         (cond
           [(obs? raw-rows)
            (render-table-rows! node (obs-peek raw-rows))
            (define (listener updated)
              (render-table-rows! node updated))
            (obs-observe! raw-rows listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-rows listener)))]
           [else
            (render-table-rows! node raw-rows)])
         node]
        [(radios)
         (define choices      (ensure-list (alist-ref (view-props v) 'choices 'render)
                                           'radios
                                           "choices"))
         (define raw-selected (alist-ref (view-props v) 'selected 'render))
         (define action       (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'radios
                                (list (cons 'choices choices)
                                      (cons 'selected #f))
                                '()
                                #f
                                #f
                                #f))
         (set-dom-node-on-change! node (lambda (new-selected) (action new-selected)))
         (define (set-selected! selected)
           (set-dom-node-attrs!
            node
            (list (cons 'choices choices)
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
         (define raw-src (alist-ref (view-props v) 'src 'render))
         (define node (dom-node 'image (list (cons 'src "")) '() #f #f #f))
         (define (set-src! src)
           (set-dom-node-attrs! node (list (cons 'src (value->text src)))))
         (cond
           [(obs? raw-src)
            (set-src! (obs-peek raw-src))
            (define (listener updated)
              (set-src! updated))
            (obs-observe! raw-src listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-src listener)))]
           [else
            (set-src! raw-src)])
         node]
        [(menu-bar)
         (define node (dom-node 'menu-bar '() '() #f #f #f))
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(menu)
         (define raw-label (alist-ref (view-props v) 'label 'render))
         (define node (dom-node 'menu (list (cons 'label "")) '() #f #f #f))
         (define (set-label! label-value)
           (set-dom-node-attrs! node (list (cons 'label (value->text label-value)))))
         (cond
           [(obs? raw-label)
            (set-label! (obs-peek raw-label))
            (define (listener updated)
              (set-label! updated))
            (obs-observe! raw-label listener)
            (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
           [else
            (set-label! raw-label)])
         (for-each (lambda (child)
                     (backend-append-child! node (build-node child register-cleanup!)))
                   (view-children v))
         node]
        [(menu-item)
         (define raw-label (alist-ref (view-props v) 'label 'render))
         (define action    (alist-ref (view-props v) 'action 'render))
         (define node (dom-node 'menu-item '() '() "" action #f))
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
         (define raw-entries (alist-ref (view-props v) 'entries 'render))
         (define key-proc    (alist-ref (view-props v) 'key 'render))
         (define make-view   (alist-ref (view-props v) 'make-view 'render))
         (define node (dom-node 'div (list (cons attr/layout 'column)) '() #f #f #f))
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
