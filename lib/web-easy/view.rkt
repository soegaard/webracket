#lang webracket

;;;
;;; web-easy Views
;;;

;; Class-free declarative view constructors and shared view data representation.
;;
;; Exports:
;;   view           View structure constructor.
;;   view?          Predicate for view values.
;;   view-kind      Access the view kind tag.
;;   view-props     Access the view property alist.
;;   view-children  Access the view child list.
;;   window         Build a root window view.
;;   vpanel         Build a vertical panel view.
;;   hpanel         Build a horizontal panel view.
;;   group          Build a labeled container view.
;;   text           Build a text label view.
;;   button         Build a button view with click action.
;;   input          Build an input view with change action and optional Enter action.
;;   checkbox       Build a checkbox view with toggle action.
;;   choice         Build a single-choice select view.
;;   slider         Build a numeric slider view.
;;   progress       Build a one-way progress display view.
;;   if-view        Build a conditional branch view.
;;   cond-view      Build a multi-branch conditional view.
;;   case-view      Build an equality-based branch view.
;;   tab-panel      Build a selected-tab branch view.
;;   observable-view  Build a dynamic view from observable value.
;;   spacer         Build an empty layout spacer view.
;;   table          Build a minimal table view.
;;   list-view      Build a keyed dynamic list view.
;;   radios         Build a radio-choice view.
;;   image          Build an image view from source.
;;   menu-bar       Build a menu-bar container view.
;;   menu           Build a menu container view.
;;   menu-item      Build a clickable menu-item view.

(define-values
  (view
   view?
   view-kind
   view-props
   view-children
   window
   vpanel
   hpanel
   group
   text
   button
   input
   checkbox
   choice
   slider
   progress
   if-view
   cond-view
   case-view
   tab-panel
   observable-view
   spacer
   table
   list-view
   radios
   image
   menu-bar
   menu
   menu-item)
  (let ()
    (struct view (kind props children) #:transparent)

    ;; Constants for view kind tags.
    (define kind/window    'window)    ; Root container view.
    (define kind/vpanel    'vpanel)    ; Vertical container view.
    (define kind/hpanel    'hpanel)    ; Horizontal container view.
    (define kind/group     'group)     ; Labeled container view.
    (define kind/text      'text)      ; Text label view.
    (define kind/button    'button)    ; Clickable action view.
    (define kind/input     'input)     ; Text input view.
    (define kind/checkbox  'checkbox)  ; Checkbox input view.
    (define kind/choice    'choice)    ; Single select input view.
    (define kind/slider    'slider)    ; Numeric slider input view.
    (define kind/progress  'progress)  ; Numeric progress display view.
    (define kind/if-view   'if-view)   ; Conditional branch view.
    (define kind/cond-view 'cond-view) ; Multi-branch conditional view.
    (define kind/case-view 'case-view) ; Equality-based conditional view.
    (define kind/tab-panel 'tab-panel) ; Selected-tab conditional view.
    (define kind/observable-view 'observable-view) ; Dynamic single-child view.
    (define kind/spacer    'spacer)    ; Empty layout spacer view.
    (define kind/table     'table)     ; Minimal tabular data view.
    (define kind/list-view 'list-view) ; Dynamic keyed list container.
    (define kind/radios    'radios)    ; Radio-choice control view.
    (define kind/image     'image)     ; Image display view.
    (define kind/menu-bar  'menu-bar)  ; Menu bar container view.
    (define kind/menu      'menu)      ; Menu container view.
    (define kind/menu-item 'menu-item) ; Menu item action view.

    ;; window : view? ... -> view?
    ;;   Construct a root window view with children.
    (define (window . children)
      (view kind/window '() children))

    ;; vpanel : view? ... -> view?
    ;;   Construct a vertically stacked container view.
    (define (vpanel . children)
      (view kind/vpanel '() children))

    ;; hpanel : view? ... -> view?
    ;;   Construct a horizontally stacked container view.
    (define (hpanel . children)
      (view kind/hpanel '() children))

    ;; group : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled container view with children.
    (define (group label . children)
      (view kind/group (list (cons 'label label)) children))

    ;; text : (or/c string? observable?) -> view?
    ;;   Construct a text view from static or observable value.
    (define (text s)
      (view kind/text (list (cons 'value s)) '()))

    ;; button : string? (-> any/c) -> view?
    ;;   Construct a button view with label and click action.
    (define (button label action)
      (view kind/button (list (cons 'label label)
                              (cons 'action action))
            '()))

    ;; input : (or/c string? observable?) (-> any/c any/c) [(-> any/c)] -> view?
    ;;   Construct an input view with current value, change action, and optional Enter action.
    ;;   Optional parameter on-enter defaults to #f.
    (define (input value action [on-enter #f])
      (view kind/input (list (cons 'value value)
                             (cons 'action action)
                             (cons 'on-enter on-enter))
            '()))

    ;; checkbox : (or/c boolean? observable?) (-> any/c any/c) -> view?
    ;;   Construct a checkbox view with current state and toggle action.
    (define (checkbox value action)
      (view kind/checkbox (list (cons 'value value)
                                (cons 'action action))
            '()))

    ;; choice : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a choice view with options, selected value, and action.
    (define (choice choices selected action)
      (view kind/choice (list (cons 'choices choices)
                              (cons 'selected selected)
                              (cons 'action action))
            '()))

    ;; slider : (or/c number? observable?) (-> any/c any/c) [number?] [number?] -> view?
    ;;   Construct a slider with value, action, and optional min/max bounds.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    (define (slider value action [min 0] [max 100])
      (view kind/slider (list (cons 'value value)
                              (cons 'action action)
                              (cons 'min min)
                              (cons 'max max))
            '()))

    ;; progress : (or/c number? observable?) [number?] [number?] -> view?
    ;;   Construct a progress display with optional min/max bounds.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    (define (progress value [min 0] [max 100])
      (view kind/progress (list (cons 'value value)
                                (cons 'min min)
                                (cons 'max max))
            '()))

    ;; if-view : (or/c any/c observable?) view? view? -> view?
    ;;   Construct a conditional view that selects then-view or else-view.
    (define (if-view cond-value then-view else-view)
      (view kind/if-view (list (cons 'cond cond-value)
                               (cons 'then then-view)
                               (cons 'else else-view))
            '()))

    ;; cond-view : (listof (cons (or/c any/c observable?) view?)) view? -> view?
    ;;   Construct a multi-branch conditional view with explicit else-view.
    (define (cond-view clauses else-view)
      (view kind/cond-view (list (cons 'clauses clauses)
                                 (cons 'else else-view))
            '()))

    ;; case-view : (or/c any/c observable?) (listof (cons list? view?)) view? -> view?
    ;;   Construct an equality-based branch view with explicit else-view.
    (define (case-view value clauses else-view)
      (view kind/case-view (list (cons 'value value)
                                 (cons 'clauses clauses)
                                 (cons 'else else-view))
            '()))

    ;; tab-panel : (or/c any/c observable?) (listof (cons any/c view?)) -> view?
    ;;   Construct a selected-tab branch view keyed by tab id.
    (define (tab-panel selected tabs)
      (view kind/tab-panel (list (cons 'selected selected)
                                 (cons 'tabs tabs))
            '()))

    ;; observable-view : (or/c any/c observable?) (-> any/c view?) [(-> any/c any/c boolean?)] -> view?
    ;;   Construct a dynamic single-child view from value using make-view.
    ;;   Optional parameter equal-proc defaults to equal?.
    (define (observable-view data make-view [equal-proc equal?])
      (view kind/observable-view (list (cons 'data data)
                                       (cons 'make-view make-view)
                                       (cons 'equal-proc equal-proc))
            '()))

    ;; spacer : -> view?
    ;;   Construct an empty spacer view.
    (define (spacer)
      (view kind/spacer '() '()))

    ;; table : list? (or/c list? observable?) -> view?
    ;;   Construct a minimal table view with columns and row data.
    (define (table columns rows)
      (view kind/table (list (cons 'columns columns)
                             (cons 'rows rows))
            '()))

    ;; radios : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a radio-choice control with choices and selected value.
    (define (radios choices selected action)
      (view kind/radios (list (cons 'choices choices)
                              (cons 'selected selected)
                              (cons 'action action))
            '()))

    ;; image : (or/c string? observable?) -> view?
    ;;   Construct an image view from a source path/string.
    (define (image src)
      (view kind/image (list (cons 'src src)) '()))

    ;; menu-bar : view? ... -> view?
    ;;   Construct a menu bar containing menu children.
    (define (menu-bar . children)
      (view kind/menu-bar '() children))

    ;; menu : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled menu containing menu-item children.
    (define (menu label . children)
      (view kind/menu (list (cons 'label label)) children))

    ;; menu-item : (or/c string? observable?) (-> any/c) -> view?
    ;;   Construct a menu item with label and action callback.
    (define (menu-item label action)
      (view kind/menu-item (list (cons 'label label)
                                 (cons 'action action))
            '()))

    ;; list-view : (or/c list? observable?) (-> any/c any/c view?) [(-> any/c any/c)] -> view?
    ;;   Construct a keyed dynamic list container.
    ;;   Optional parameter key defaults to values.
    (define (list-view entries make-view [key values])
      (view kind/list-view (list (cons 'entries entries)
                                 (cons 'make-view make-view)
                                 (cons 'key key))
            '()))

    (values view
            view?
            view-kind
            view-props
            view-children
            window
            vpanel
            hpanel
            group
            text
            button
            input
            checkbox
            choice
            slider
            progress
            if-view
            cond-view
            case-view
            tab-panel
            observable-view
            spacer
            table
            list-view
            radios
            image
            menu-bar
            menu
            menu-item)))
