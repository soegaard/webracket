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
;;   with-attrs     Attach extra DOM attrs to a specific view instance.
;;   with-class     Attach extra CSS class(es) to a specific view instance.
;;   with-id        Attach a specific DOM id to a view instance.
;;   window         Build a root window view.
;;   vpanel         Build a vertical panel view.
;;   hpanel         Build a horizontal panel view.
;;   container      Build a centered layout container view.
;;   grid           Build a grid layout container view.
;;   stack          Build a vertical stack layout container view.
;;   inline         Build a horizontal inline layout container view.
;;   group          Build a labeled container view.
;;   alert          Build an inline status alert view.
;;   toast          Build a non-modal toast notification view.
;;   close-button   Build a standardized close button view.
;;   badge          Build a compact inline badge view.
;;   spinner        Build a loading spinner view.
;;   placeholder    Build a placeholder/skeleton view.
;;   text           Build a text label view.
;;   button         Build a button view with click action.
;;   link           Build a link view with href.
;;   button-group   Build a grouped button container view.
;;   button-toolbar Build a horizontal toolbar of button groups.
;;   toolbar        Build a generic horizontal toolbar container.
;;   toolbar-group  Build a grouped toolbar section container.
;;   input          Build an input view with change action and optional Enter action.
;;   checkbox       Build a checkbox view with toggle action.
;;   choice         Build a single-choice select view.
;;   slider         Build a numeric slider view.
;;   progress       Build a one-way progress display view.
;;   pagination     Build a page navigation control view.
;;   breadcrumb     Build a breadcrumb navigation view.
;;   list-group     Build a selectable list-group navigation view.
;;   if-view        Build a conditional branch view.
;;   cond-view      Build a multi-branch conditional view.
;;   case-view      Build an equality-based branch view.
;;   tab-panel      Build a selected-tab branch view.
;;   collapse       Build a conditional visibility container view.
;;   accordion      Build a single-open section accordion view.
;;   offcanvas      Build an offcanvas side panel view.
;;   dialog         Build a modal dialog container view.
;;   modal          Build a modal container view.
;;   observable-view  Build a dynamic view from observable value.
;;   spacer         Build an empty layout spacer view.
;;   divider        Build a horizontal or vertical divider view.
;;   table          Build a minimal table view.
;;   list-view      Build a keyed dynamic list view.
;;   radios         Build a radio-choice view.
;;   image          Build an image view from source.
;;   dropdown       Build a dropdown menu view.
;;   carousel       Build a carousel with indicators and navigation actions.
;;   scrollspy      Build a section navigation view with active tracking.
;;   tooltip        Build a tooltip wrapper view.
;;   popover        Build a popover wrapper view.
;;   card           Build a card container with optional title/footer.
;;   navigation-bar Build a navigation bar container view.
;;   menu-bar       Build a menu-bar container view.
;;   menu           Build a menu container view.
;;   menu-item      Build a clickable menu-item view.

(define-values
  (view
   view?
   view-kind
   view-props
   view-children
   with-attrs
   with-class
   with-id
   window
   vpanel
   hpanel
   container
   grid
   stack
   inline
   group
   alert
   toast
   close-button
   badge
   spinner
   placeholder
   text
   button
   link
   button-group
   button-toolbar
   toolbar
   toolbar-group
   input
   checkbox
   choice
   slider
   progress
   pagination
   breadcrumb
   list-group
   if-view
   cond-view
   case-view
   tab-panel
   collapse
   accordion
   offcanvas
   dialog
   modal
   observable-view
   spacer
   divider
   table
   list-view
   radios
   image
   dropdown
   carousel
   scrollspy
   tooltip
   popover
   card
   navigation-bar
   menu-bar
   menu
   menu-item)
  (let ()
    (struct view (kind props children) #:transparent)

    ;; Constants for view kind tags.
    (define kind/window    'window)    ; Root container view.
    (define kind/vpanel    'vpanel)    ; Vertical container view.
    (define kind/hpanel    'hpanel)    ; Horizontal container view.
    (define kind/container 'container) ; Centered width-constrained container view.
    (define kind/grid      'grid)      ; Grid layout container view.
    (define kind/stack     'stack)     ; Vertical stack layout container view.
    (define kind/inline    'inline)    ; Horizontal inline layout container view.
    (define kind/group     'group)     ; Labeled container view.
    (define kind/alert     'alert)     ; Inline status alert view.
    (define kind/toast     'toast)     ; Non-modal toast notification view.
    (define kind/close-button 'close-button) ; Standardized close button view.
    (define kind/badge     'badge)     ; Compact inline badge view.
    (define kind/spinner   'spinner)   ; Loading spinner view.
    (define kind/placeholder 'placeholder) ; Placeholder/skeleton view.
    (define kind/text      'text)      ; Text label view.
    (define kind/button    'button)    ; Clickable action view.
    (define kind/link      'link)      ; Link view.
    (define kind/button-group 'button-group) ; Grouped button container view.
    (define kind/button-toolbar 'button-toolbar) ; Horizontal toolbar for button groups.
    (define kind/toolbar   'toolbar)   ; Generic horizontal toolbar container.
    (define kind/toolbar-group 'toolbar-group) ; Grouped toolbar section container.
    (define kind/input     'input)     ; Text input view.
    (define kind/checkbox  'checkbox)  ; Checkbox input view.
    (define kind/choice    'choice)    ; Single select input view.
    (define kind/slider    'slider)    ; Numeric slider input view.
    (define kind/progress  'progress)  ; Numeric progress display view.
    (define kind/pagination 'pagination) ; Page navigation control view.
    (define kind/breadcrumb 'breadcrumb) ; Breadcrumb navigation view.
    (define kind/list-group 'list-group) ; Selectable list-group navigation view.
    (define kind/if-view   'if-view)   ; Conditional branch view.
    (define kind/cond-view 'cond-view) ; Multi-branch conditional view.
    (define kind/case-view 'case-view) ; Equality-based conditional view.
    (define kind/tab-panel 'tab-panel) ; Selected-tab conditional view.
    (define kind/collapse 'collapse) ; Conditional visibility view.
    (define kind/accordion 'accordion) ; Single-open section accordion view.
    (define kind/offcanvas 'offcanvas) ; Side-sheet/offcanvas panel view.
    (define kind/dialog 'dialog) ; Modal dialog container view.
    (define kind/modal 'modal) ; Modal container view.
    (define kind/observable-view 'observable-view) ; Dynamic single-child view.
    (define kind/spacer    'spacer)    ; Empty layout spacer view.
    (define kind/divider   'divider)   ; Horizontal/vertical divider view.
    (define kind/table     'table)     ; Minimal tabular data view.
    (define kind/list-view 'list-view) ; Dynamic keyed list container.
    (define kind/radios    'radios)    ; Radio-choice control view.
    (define kind/image     'image)     ; Image display view.
    (define kind/dropdown  'dropdown)  ; Dropdown menu view.
    (define kind/carousel  'carousel)  ; Carousel control view.
    (define kind/scrollspy 'scrollspy) ; Active section navigation view.
    (define kind/tooltip   'tooltip)   ; Tooltip wrapper view.
    (define kind/popover   'popover)   ; Popover wrapper view.
    (define kind/card      'card)      ; Card container view.
    (define kind/navigation-bar 'navigation-bar) ; Navigation bar container view.
    (define kind/menu-bar  'menu-bar)  ; Menu bar container view.
    (define kind/menu      'menu)      ; Menu container view.
    (define kind/menu-item 'menu-item) ; Menu item action view.

    ;; view-props-ref/default : list? symbol? any/c -> any/c
    ;;   Return property value for key in props, else default-value.
    (define (view-props-ref/default props key default-value)
      (define p (assq key props))
      (if p
          (cdr p)
          default-value))

    ;; view-props-remove-key : list? symbol? -> list?
    ;;   Remove key from props association list.
    (define (view-props-remove-key props key)
      (cond
        [(null? props) '()]
        [(eq? (caar props) key)
         (view-props-remove-key (cdr props) key)]
        [else
         (cons (car props)
               (view-props-remove-key (cdr props) key))]))

    ;; view-props-set : list? symbol? any/c -> list?
    ;;   Set key to value in props, replacing any prior key entry.
    (define (view-props-set props key value)
      (cons (cons key value)
            (view-props-remove-key props key)))

    ;; normalize-class-entry : any/c symbol? -> string?
    ;;   Normalize a class entry to string or raise argument error.
    (define (normalize-class-entry value who)
      (cond
        [(string? value) value]
        [(symbol? value) (symbol->string value)]
        [else
         (raise-arguments-error who
                                "expected class entry as string? or symbol?"
                                "class"
                                value)]))

    ;; normalize-class-list : any/c symbol? -> list?
    ;;   Normalize class value to list of class name strings.
    (define (normalize-class-list classes who)
      (cond
        [(string? classes)
         (list classes)]
        [(symbol? classes)
         (list (symbol->string classes))]
        [(list? classes)
         (map (lambda (entry)
                (normalize-class-entry entry who))
              classes)]
        [else
         (raise-arguments-error who
                                "expected class as string?, symbol?, or list?"
                                "class"
                                classes)]))

    ;; normalize-attrs-entry : any/c symbol? -> pair?
    ;;   Normalize attr entry as (cons key value) from pair or 2-element list.
    (define (normalize-attrs-entry entry who)
      (cond
        [(and (list? entry) (= (length entry) 2))
         (cons (car entry) (cadr entry))]
        [(pair? entry)
         (cons (car entry) (cdr entry))]
        [else
         (raise-arguments-error who
                                "expected attr entry as pair? or 2-element list?"
                                "entry"
                                entry)]))

    ;; normalize-attrs-list : any/c symbol? -> list?
    ;;   Normalize attrs to list of (cons symbol? any/c).
    (define (normalize-attrs-list attrs who)
      (unless (list? attrs)
        (raise-arguments-error who
                               "expected attrs list?"
                               "attrs"
                               attrs))
      (map (lambda (entry)
             (define normalized (normalize-attrs-entry entry who))
             (unless (symbol? (car normalized))
               (raise-arguments-error who
                                      "expected attribute key as symbol?"
                                      "key"
                                      (car normalized)))
             normalized)
           attrs))

    ;; view-with-props : view? list? -> view?
    ;;   Rebuild original view with replacement props.
    (define (view-with-props v props)
      (view (view-kind v)
            props
            (view-children v)))

    ;; with-class/internal : view? (or/c string? symbol? list?) -> view?
    ;;   Return v with extra class(es) added to the rendered root node.
    (define (with-class/internal v classes)
      (define props      (view-props v))
      (define old-classes (view-props-ref/default props 'extra-class '()))
      (define new-classes (append old-classes
                                  (normalize-class-list classes 'with-class)))
      (view-with-props v
                       (view-props-set props 'extra-class new-classes)))

    ;; with-attrs/internal : view? list? -> view?
    ;;   Return v with extra attrs added to the rendered root node.
    (define (with-attrs/internal v attrs)
      (define normalized (normalize-attrs-list attrs 'with-attrs))
      (define attrs-without-class
        (let loop ([remaining normalized])
          (cond
            [(null? remaining) '()]
            [(eq? (caar remaining) 'class)
             (loop (cdr remaining))]
            [else
             (cons (car remaining)
                   (loop (cdr remaining)))])))
      (define class-values
        (let loop ([remaining normalized])
          (cond
            [(null? remaining) '()]
            [(eq? (caar remaining) 'class)
             (append (normalize-class-list (cdar remaining) 'with-attrs)
                     (loop (cdr remaining)))]
            [else
             (loop (cdr remaining))])))
      (define base-props (view-props v))
      (define old-attrs (view-props-ref/default base-props 'extra-attrs '()))
      (define v-with-attrs
        (view-with-props v
                         (view-props-set base-props
                                         'extra-attrs
                                         (append old-attrs attrs-without-class))))
      (if (null? class-values)
          v-with-attrs
          (with-class/internal v-with-attrs class-values)))

    ;; with-class : (or/c string? symbol? list?) view? -> view?
    ;;   Add class(es) to a view root.
    (define (with-class classes v)
      (unless (view? v)
        (raise-arguments-error 'with-class
                               "expected view?"
                               "view"
                               v))
      (with-class/internal v classes))

    ;; with-attrs : list? view? -> view?
    ;;   Add attrs to a view root.
    (define (with-attrs attrs v)
      (unless (view? v)
        (raise-arguments-error 'with-attrs
                               "expected view?"
                               "view"
                               v))
      (with-attrs/internal v attrs))

    ;; with-id : (or/c string? symbol?) view? -> view?
    ;;   Add id attr to a view root.
    (define (with-id id-value v)
      (unless (view? v)
        (raise-arguments-error 'with-id
                               "expected view?"
                               "view"
                               v))
      (cond
        [(string? id-value)
         (with-attrs/internal v (list (list 'id id-value)))]
        [(symbol? id-value)
         (with-attrs/internal v (list (list 'id (symbol->string id-value))))]
        [else
         (raise-arguments-error 'with-id
                                "expected id as string? or symbol?"
                                "id"
                                id-value)]))

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

    ;; container : view? ... -> view?
    ;;   Construct a centered width-constrained layout container view.
    (define (container . children)
      (view kind/container '() children))

    ;; grid : any/c view? ... -> view?
    ;;   Construct a grid layout container with columns specification and children.
    (define (grid columns . children)
      (view kind/grid (list (cons 'columns columns)) children))

    ;; stack : view? ... -> view?
    ;;   Construct a vertical stack layout container view.
    (define (stack . children)
      (view kind/stack '() children))

    ;; inline : view? ... -> view?
    ;;   Construct a horizontal inline layout container view.
    (define (inline . children)
      (view kind/inline '() children))

    ;; group : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled container view with children.
    (define (group label . children)
      (view kind/group (list (cons 'label label)) children))

    ;; alert : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct an inline alert/status view with optional severity level.
    ;;   Optional parameter level defaults to 'info.
    (define (alert value [level 'info])
      (view kind/alert (list (cons 'value value)
                             (cons 'level level))
            '()))

    ;; toast : (or/c boolean? observable?) (-> any/c) (or/c string? observable?) [(or/c symbol? observable?)] [(or/c string? observable? false/c)] [(or/c boolean? observable?)] [number?] [boolean?] -> view?
    ;;   Construct a non-modal toast with open flag, close action, message, optional title/dismiss control, optional auto-hide duration, and pause-on-hover.
    ;;   Optional parameter level defaults to 'info.
    ;;   Optional parameter title defaults to #f.
    ;;   Optional parameter dismissible? defaults to #t.
    ;;   Optional parameter duration-ms defaults to 0.
    ;;   Optional parameter pause-on-hover? defaults to #t.
    (define (toast open on-close value [level 'info] [title #f] [dismissible? #t] [duration-ms 0] [pause-on-hover? #t])
      (view kind/toast (list (cons 'open open)
                             (cons 'on-close on-close)
                             (cons 'value value)
                             (cons 'level level)
                             (cons 'title title)
                             (cons 'dismissible? dismissible?)
                             (cons 'duration-ms duration-ms)
                             (cons 'pause-on-hover? pause-on-hover?))
            '()))

    ;; close-button : (-> any/c) [(or/c string? observable?)] -> view?
    ;;   Construct a standardized close button with action and optional aria-label.
    ;;   Optional parameter aria-label defaults to "Close".
    (define (close-button action [aria-label "Close"])
      (view kind/close-button (list (cons 'action action)
                                    (cons 'aria-label aria-label))
            '()))

    ;; badge : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct a compact inline badge with optional severity level.
    ;;   Optional parameter level defaults to 'info.
    (define (badge value [level 'info])
      (view kind/badge (list (cons 'value value)
                             (cons 'level level))
            '()))

    ;; spinner : [(or/c string? observable? false/c)] -> view?
    ;;   Construct a loading spinner with optional label text.
    ;;   Optional parameter label defaults to "Loading...".
    (define (spinner [label "Loading..."])
      (view kind/spinner (list (cons 'label label))
            '()))

    ;; placeholder : [(or/c symbol? observable?)] [(or/c any/c observable?)] -> view?
    ;;   Construct a placeholder/skeleton block with optional shape and width.
    ;;   Optional parameter shape defaults to 'text.
    ;;   Optional parameter width defaults to #f.
    (define (placeholder [shape 'text] [width #f])
      (view kind/placeholder (list (cons 'shape shape)
                                   (cons 'width width))
            '()))

    ;; text : (or/c string? observable?) -> view?
    ;;   Construct a text view from static or observable value.
    (define (text s)
      (view kind/text (list (cons 'value s)) '()))

    ;; button : (or/c string? observable?) (-> any/c) [any/c] [any/c] -> view?
    ;;   Construct a button view with optional leading/trailing icon labels.
    ;;   Optional parameter leading-icon defaults to #f.
    ;;   Optional parameter trailing-icon defaults to #f.
    (define (button label action [leading-icon #f] [trailing-icon #f])
      (view kind/button (list (cons 'label label)
                              (cons 'action action)
                              (cons 'leading-icon leading-icon)
                              (cons 'trailing-icon trailing-icon))
            '()))

    ;; link : (or/c string? observable?) (or/c string? observable?) [boolean?] [any/c] -> view?
    ;;   Construct a link view with href and optional download/target attributes.
    ;;   Optional parameter download? defaults to #f.
    ;;   Optional parameter target defaults to #f.
    (define (link label href [download? #f] [target #f])
      (view kind/link (list (cons 'label label)
                            (cons 'href href)
                            (cons 'download download?)
                            (cons 'target target))
            '()))

    ;; button-group : view? ... -> view?
    ;;   Construct a grouped button container view.
    (define (button-group . children)
      (view kind/button-group '() children))

    ;; button-toolbar : view? ... -> view?
    ;;   Construct a horizontal toolbar of grouped button controls.
    (define (button-toolbar . children)
      (view kind/button-toolbar '() children))

    ;; toolbar : view? ... -> view?
    ;;   Construct a generic horizontal toolbar container.
    (define (toolbar . children)
      (view kind/toolbar '() children))

    ;; toolbar-group : view? ... -> view?
    ;;   Construct a grouped toolbar section container.
    (define (toolbar-group . children)
      (view kind/toolbar-group '() children))

    ;; input : (or/c string? observable?) (-> any/c any/c) [(or/c (-> any/c) false/c)] [list?] -> view?
    ;;   Construct an input view with current value, change action, optional Enter action, and attrs.
    ;;   Optional parameter on-enter defaults to #f.
    ;;   Optional parameter attrs defaults to '().
    (define (input value action [on-enter #f] [attrs '()])
      (view kind/input (list (cons 'value value)
                             (cons 'action action)
                             (cons 'on-enter on-enter)
                             (cons 'attrs attrs))
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

    ;; progress : (or/c number? observable?) [number?] [number?] [(or/c symbol? observable?)] -> view?
    ;;   Construct a progress display with optional min/max bounds and variant.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    ;;   Optional parameter variant defaults to 'info.
    (define (progress value [min 0] [max 100] [variant 'info])
      (view kind/progress (list (cons 'value value)
                                (cons 'min min)
                                (cons 'max max)
                                (cons 'variant variant))
            '()))

    ;; pagination : (or/c number? observable?) (or/c number? observable?) (-> any/c any/c) -> view?
    ;;   Construct a pagination control for page-count, current page, and page-change action.
    (define (pagination page-count current-page action)
      (view kind/pagination (list (cons 'page-count page-count)
                                  (cons 'current-page current-page)
                                  (cons 'action action))
            '()))

    ;; breadcrumb : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a breadcrumb control for entries, current id, and navigation action.
    (define (breadcrumb entries current action)
      (view kind/breadcrumb (list (cons 'entries entries)
                                  (cons 'current current)
                                  (cons 'action action))
            '()))

    ;; list-group : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a selectable list-group from entries, current id, and selection action.
    (define (list-group entries current action)
      (view kind/list-group (list (cons 'entries entries)
                                  (cons 'current current)
                                  (cons 'action action))
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

    ;; collapse : (or/c boolean? observable?) view? -> view?
    ;;   Construct a container view that shows child only when open is true.
    (define (collapse open child)
      (view kind/collapse (list (cons 'open open))
            (list child)))

    ;; accordion : (or/c any/c observable?) list? -> view?
    ;;   Construct a single-open accordion from section rows: (list id label view).
    (define (accordion selected sections)
      (view kind/accordion (list (cons 'selected selected)
                                 (cons 'sections sections))
            '()))

    ;; offcanvas : (or/c boolean? observable?) (-> any/c) [(or/c symbol? observable?)] view? ... -> view?
    ;;   Construct an offcanvas side panel with open flag, close action, and optional side.
    ;;   Optional parameter side defaults to 'end.
    (define (offcanvas open on-close [side 'end] . children)
      (view kind/offcanvas (list (cons 'open open)
                                 (cons 'on-close on-close)
                                 (cons 'side side))
            children))

    ;; dialog : (or/c boolean? observable?) (-> any/c) view? ... -> view?
    ;;   Construct a modal dialog that is visible when open is true and closes via on-close.
    (define (dialog open on-close . children)
      (view kind/dialog (list (cons 'open open)
                              (cons 'on-close on-close))
            children))

    ;; modal : (or/c boolean? observable?) (-> any/c) view? ... -> view?
    ;;   Construct a modal container that mirrors dialog behavior.
    (define (modal open on-close . children)
      (view kind/modal (list (cons 'open open)
                             (cons 'on-close on-close))
            children))

    ;; observable-view : (or/c any/c observable?) (-> any/c view?) [(-> any/c any/c boolean?)] -> view?
    ;;   Construct a dynamic single-child view from value using make-view.
    ;;   Optional parameter equal-proc defaults to equal?.
    (define (observable-view data make-view [equal-proc equal?])
      (view kind/observable-view (list (cons 'data data)
                                       (cons 'make-view make-view)
                                       (cons 'equal-proc equal-proc))
            '()))

    ;; spacer : [number?] -> view?
    ;;   Construct an empty spacer view with optional grow factor.
    ;;   Optional parameter grow defaults to 1.
    (define (spacer [grow 1])
      (view kind/spacer (list (cons 'grow grow)) '()))

    ;; divider : [symbol?] -> view?
    ;;   Construct a divider with orientation 'horizontal or 'vertical.
    ;;   Optional parameter orientation defaults to 'horizontal.
    (define (divider [orientation 'horizontal])
      (view kind/divider (list (cons 'orientation orientation)) '()))

    ;; table : list? (or/c list? observable?) [symbol?] -> view?
    ;;   Construct a minimal table view with columns/rows and optional spacing density.
    ;;   Column entries can be plain labels or (list label align) where align is left/center/right.
    ;;   Optional parameter density defaults to 'normal.
    (define (table columns rows [density 'normal])
      (view kind/table (list (cons 'columns columns)
                             (cons 'rows rows)
                             (cons 'density density))
            '()))

    ;; radios : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a radio-choice control with choices and selected value.
    (define (radios choices selected action)
      (view kind/radios (list (cons 'choices choices)
                              (cons 'selected selected)
                              (cons 'action action))
            '()))

    ;; image : (or/c string? observable?) [any/c] [any/c] -> view?
    ;;   Construct an image view from a source path/string with optional width/height attrs.
    ;;   Optional parameter width defaults to #f.
    ;;   Optional parameter height defaults to #f.
    (define (image src [width #f] [height #f])
      (view kind/image (list (cons 'src src)
                             (cons 'width width)
                             (cons 'height height))
            '()))

    ;; dropdown : (or/c string? observable?) list? (-> any/c any/c) -> view?
    ;;   Construct a dropdown menu from a label and entry rows: (list id label).
    (define (dropdown label entries action)
      (view kind/dropdown (list (cons 'label label)
                                (cons 'entries entries)
                                (cons 'action action))
            '()))

    ;; carousel : list? (or/c number? observable?) (-> any/c any/c) [boolean?] [boolean?] -> view?
    ;;   Construct a carousel from item rows, current index, and index-change action with optional wrap and autoplay flags.
    ;;   Optional parameter wrap? defaults to #t.
    ;;   Optional parameter autoplay? defaults to #f.
    (define (carousel items current-index action [wrap? #t] [autoplay? #f])
      (view kind/carousel (list (cons 'items items)
                                (cons 'current-index current-index)
                                (cons 'action action)
                                (cons 'wrap? wrap?)
                                (cons 'autoplay? autoplay?))
            '()))

    ;; scrollspy : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct scroll-tracking section navigation from rows: (list id label [content-view]).
    (define (scrollspy sections current action)
      (view kind/scrollspy (list (cons 'sections sections)
                                 (cons 'current current)
                                 (cons 'action action))
            '()))

    ;; tooltip : (or/c string? observable?) view? -> view?
    ;;   Construct a tooltip wrapper with message and trigger child view.
    (define (tooltip message child)
      (view kind/tooltip (list (cons 'message message))
            (list child)))

    ;; popover : (or/c string? observable?) view? ... -> view?
    ;;   Construct a click-toggle popover with trigger label and body children.
    (define (popover label . children)
      (view kind/popover (list (cons 'label label))
            children))

    ;; card : [(or/c string? observable? false/c)] [(or/c string? observable? false/c)] any/c ... -> view?
    ;;   Construct a card with optional title/footer, optional variant(s), and body children.
    ;;   Optional parameter title defaults to #f.
    ;;   Optional parameter footer defaults to #f.
    (define (card [title #f] [footer #f] . args)
      (define (all-symbols? xs)
        (cond
          [(null? xs) #t]
          [(symbol? (car xs)) (all-symbols? (cdr xs))]
          [else #f]))
      (define has-variant?
        (and (pair? args)
             (or (symbol? (car args))
                 (and (list? (car args))
                      (not (null? (car args)))
                      (all-symbols? (car args))))))
      (define variants (if has-variant?
                           (car args)
                           'default))
      (define children (if has-variant? (cdr args) args))
      (view kind/card (list (cons 'title title)
                            (cons 'footer footer)
                            (cons 'variants variants))
            children))

    ;; navigation-bar : [(or/c symbol? observable?)] [(or/c boolean? observable?)] [symbol?] view? ... -> view?
    ;;   Construct a navigation bar with optional orientation/collapsed/expand props and children.
    ;;   Optional parameter orientation defaults to 'horizontal.
    ;;   Optional parameter collapsed? defaults to #f.
    ;;   Optional parameter expand defaults to 'always.
    (define (navigation-bar . args)
      (define orientation 'horizontal)
      (define collapsed? #f)
      (define expand 'always)
      (define children args)
      (when (and (pair? children)
                 (or (symbol? (car children))
                     (obs? (car children))))
        (set! orientation (car children))
        (set! children (cdr children)))
      (when (and (pair? children)
                 (or (boolean? (car children))
                     (obs? (car children))))
        (set! collapsed? (car children))
        (set! children (cdr children)))
      (when (and (pair? children)
                 (symbol? (car children)))
        (set! expand (car children))
        (set! children (cdr children)))
      (view kind/navigation-bar
            (list (cons 'orientation orientation)
                  (cons 'collapsed? collapsed?)
                  (cons 'expand expand))
            children))

    ;; menu-bar : view? ... -> view?
    ;;   Construct a menu bar containing menu children.
    (define (menu-bar . children)
      (view kind/menu-bar '() children))

    ;; menu : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled menu containing menu-item children.
    (define (menu label . children)
      (view kind/menu (list (cons 'label label)) children))

    ;; menu-item : (or/c string? observable?) (-> any/c) [any/c] [any/c] -> view?
    ;;   Construct a menu item with optional leading/trailing icon labels.
    ;;   Optional parameter leading-icon defaults to #f.
    ;;   Optional parameter trailing-icon defaults to #f.
    (define (menu-item label action [leading-icon #f] [trailing-icon #f])
      (view kind/menu-item (list (cons 'label label)
                                 (cons 'action action)
                                 (cons 'leading-icon leading-icon)
                                 (cons 'trailing-icon trailing-icon))
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
            with-attrs
            with-class
            with-id
            window
            vpanel
            hpanel
            container
            grid
            stack
            inline
            group
            alert
            toast
            close-button
            badge
            spinner
            placeholder
            text
            button
            link
            button-group
            button-toolbar
            toolbar
            toolbar-group
            input
            checkbox
            choice
            slider
            progress
            pagination
            breadcrumb
            list-group
            if-view
            cond-view
            case-view
            tab-panel
            collapse
            accordion
            offcanvas
            dialog
            modal
            observable-view
            spacer
            divider
            table
            list-view
            radios
            image
            dropdown
            carousel
            scrollspy
            tooltip
            popover
            card
            navigation-bar
            menu-bar
            menu
            menu-item)))
