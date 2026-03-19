#lang webracket

;;;
;;; web-easy Views
;;;

;; Class-free declarative view constructors and shared view data representation.
;;
;; Constructor keyword contract:
;;   Constructors that materialize a concrete root node support
;;   `#:id`, `#:class`, and `#:attrs` for root decoration.
;;   (Core composition forms like `window`/`vpanel`/`hpanel` and
;;    branch combinators keep positional contracts.)
;;
;; Exports:
;;   view           View structure constructor.
;;   view?          Predicate for view values.
;;   view-kind      Access the view kind tag.
;;   view-props     Access the view property alist.
;;   view-children  Access the view child list.
;;   text-content/c Predicate for text-like content (including observable text-like values).
;;   window         Build a root window view.
;;   vpanel         Build a vertical panel view.
;;   hpanel         Build a horizontal panel view.
;;   container      Build a centered layout container view.
;;   grid           Build a grid layout container view.
;;   stack          Build a vertical stack layout container view.
;;   inline         Build a horizontal inline layout container view.
;;   group          Build a labeled container view.
;;   alert          Build an inline status alert view.
;;   alert-rich     Build a rich alert view with title/body/link parts.
;;   toast          Build a non-modal toast notification view.
;;   close-button   Build a standardized close button view.
;;   badge          Build a compact inline badge view.
;;   spinner        Build a loading spinner view.
;;   placeholder    Build a placeholder/skeleton view.
;;   text           Build a text label view.
;;   heading        Build a semantic heading view (h1..h6).
;;   h1             Build a semantic level-1 heading view.
;;   H1             Build a primitive HTML h1 element view with generic keyword attrs.
;;   H2             Build a primitive HTML h2 element view with generic keyword attrs.
;;   H3             Build a primitive HTML h3 element view with generic keyword attrs.
;;   H4             Build a primitive HTML h4 element view with generic keyword attrs.
;;   H5             Build a primitive HTML h5 element view with generic keyword attrs.
;;   H6             Build a primitive HTML h6 element view with generic keyword attrs.
;;   P              Build a primitive HTML p element view with generic keyword attrs.
;;   Span           Build a primitive HTML span element view with generic keyword attrs.
;;   Strong         Build a primitive HTML strong element view with generic keyword attrs.
;;   Em             Build a primitive HTML em element view with generic keyword attrs.
;;   Code           Build a primitive HTML code element view with generic keyword attrs.
;;   Pre            Build a primitive HTML pre element view with generic keyword attrs.
;;   Small          Build a primitive HTML small element view with generic keyword attrs.
;;   Br             Build a primitive HTML br element view with generic keyword attrs.
;;   Hr             Build a primitive HTML hr element view with generic keyword attrs.
;;   Img            Build a primitive HTML img element view with required #:src and generic keyword attrs.
;;   A              Build a primitive HTML a element view with generic keyword attrs.
;;   Button         Build a primitive HTML button element view with generic keyword attrs.
;;   Div            Build a primitive HTML div element view with children and generic keyword attrs.
;;   Section        Build a primitive HTML section element view with children and generic keyword attrs.
;;   Article        Build a primitive HTML article element view with children and generic keyword attrs.
;;   Nav            Build a primitive HTML nav element view with children and generic keyword attrs.
;;   Main           Build a primitive HTML main element view with children and generic keyword attrs.
;;   Header         Build a primitive HTML header element view with children and generic keyword attrs.
;;   Footer         Build a primitive HTML footer element view with children and generic keyword attrs.
;;   Aside          Build a primitive HTML aside element view with children and generic keyword attrs.
;;   Form           Build a primitive HTML form element view with children and generic keyword attrs.
;;   Label          Build a primitive HTML label element view with generic keyword attrs.
;;   Ul             Build a primitive HTML ul element view with children and generic keyword attrs.
;;   Ol             Build a primitive HTML ol element view with children and generic keyword attrs.
;;   Li             Build a primitive HTML li element view with children and generic keyword attrs.
;;   h2             Build a semantic level-2 heading view.
;;   h3             Build a semantic level-3 heading view.
;;   h4             Build a semantic level-4 heading view.
;;   h5             Build a semantic level-5 heading view.
;;   h6             Build a semantic level-6 heading view.
;;   display-heading  Build a semantic heading view with display style variant.
;;   display-1      Build a semantic display level-1 heading view.
;;   display-2      Build a semantic display level-2 heading view.
;;   display-3      Build a semantic display level-3 heading view.
;;   display-4      Build a semantic display level-4 heading view.
;;   display-5      Build a semantic display level-5 heading view.
;;   display-6      Build a semantic display level-6 heading view.
;;   heading-with-subtitle  Build a semantic heading view with muted subtitle text.
;;   display-heading-with-subtitle  Build a semantic display heading view with muted subtitle text.
;;   lead           Build a lead paragraph view.
;;   blockquote     Build a semantic blockquote with optional attribution and alignment.
;;   button         Build a button view with click action.
;;   link           Build a link view with href.
;;   button-group   Build a grouped button container view.
;;   toggle-button-group Build an exclusive/non-exclusive toggle button group view.
;;   button-toolbar Build a horizontal toolbar of button groups.
;;   toolbar        Build a generic horizontal toolbar container.
;;   toolbar-group  Build a grouped toolbar section container.
;;   input          Build an input view with change action and optional Enter action.
;;   textarea       Build a textarea view with change action and optional attrs.
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
;;   table          Build a table view with optional caption and variants.
;;   list-view      Build a keyed dynamic list view.
;;   radios         Build a radio-choice view.
;;   image          Build an image view from source.
;;   dropdown       Build a dropdown menu view.
;;   carousel       Build a carousel with indicators and navigation actions.
;;   scrollspy      Build a section navigation view with active tracking.
;;   tooltip        Build a tooltip container view.
;;   popover        Build a popover container view.
;;   card           Build a card container with optional title/footer.
;;   top-bar        Build a top bar container view.
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
   text-content/c
   window
   vpanel
   hpanel
   container
   grid
   stack
   inline
   group
   alert
   alert-rich
   toast
   close-button
   badge
   spinner
   placeholder
   text
   heading
   h1
   H1
   H2
   H3
   H4
   H5
   H6
   P
   Span
   Strong
   Em
   Code
   Pre
   Small
   Br
   Hr
   Img
   A
   Button
   Div
   Section
   Article
   Nav
   Main
   Header
   Footer
   Aside
   Form
   Label
   Ul
   Ol
   Li
   h2
   h3
   h4
   h5
   h6
   display-heading
   display-1
   display-2
   display-3
   display-4
   display-5
   display-6
   heading-with-subtitle
   display-heading-with-subtitle
   lead
   blockquote
   button
   link
   button-group
   toggle-button-group
   button-toolbar
   toolbar
   toolbar-group
   input
   textarea
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
   top-bar
   navigation-bar
   menu-bar
   menu
   menu-item)
  (let ()
    (struct view (kind props children) #:transparent)

    ;; text-content/c : any/c -> boolean?
    ;;   Predicate for text-like values accepted by text-bearing constructors.
    (define (text-content/c v)
      (or (string? v)
          (symbol? v)
          (number? v)
          (boolean? v)
          (char? v)
          (obs? v)))

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
    (define kind/alert-rich 'alert-rich) ; Rich alert view with title/body/link parts.
    (define kind/toast     'toast)     ; Non-modal toast notification view.
    (define kind/close-button 'close-button) ; Standardized close button view.
    (define kind/badge     'badge)     ; Compact inline badge view.
    (define kind/spinner   'spinner)   ; Loading spinner view.
    (define kind/placeholder 'placeholder) ; Placeholder/skeleton view.
    (define kind/text      'text)      ; Text label view.
    (define kind/html-element 'html-element) ; Primitive HTML element leaf view.
    (define kind/html-element-children 'html-element-children) ; Primitive HTML element container view.
    (define kind/heading   'heading)   ; Semantic heading text view.
    (define kind/display-heading 'display-heading) ; Semantic heading with display style.
    (define kind/heading-with-subtitle 'heading-with-subtitle) ; Semantic heading with muted subtitle.
    (define kind/display-heading-with-subtitle 'display-heading-with-subtitle) ; Display heading with muted subtitle.
    (define kind/lead      'lead)      ; Lead paragraph text view.
    (define kind/blockquote 'blockquote) ; Semantic blockquote view with optional attribution.
    (define kind/button    'button)    ; Clickable action view.
    (define kind/link      'link)      ; Link view.
    (define kind/button-group 'button-group) ; Grouped button container view.
    (define kind/toggle-button-group 'toggle-button-group) ; Toggle button group view.
    (define kind/button-toolbar 'button-toolbar) ; Horizontal toolbar for button groups.
    (define kind/toolbar   'toolbar)   ; Generic horizontal toolbar container.
    (define kind/toolbar-group 'toolbar-group) ; Grouped toolbar section container.
    (define kind/input     'input)     ; Text input view.
    (define kind/textarea  'textarea)  ; Multi-line text input view.
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
    (define kind/tooltip   'tooltip)   ; Tooltip container view.
    (define kind/popover   'popover)   ; Popover container view.
    (define kind/card      'card)      ; Card container view.
    (define kind/top-bar   'top-bar)   ; Top bar container view.
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

    ;; normalize-blockquote-align : any/c symbol? -> symbol?
    ;;   Normalize blockquote alignment to one of 'left, 'center, or 'right.
    (define (normalize-blockquote-align align who)
      (unless (symbol? align)
        (raise-arguments-error who
                               "expected #:align as symbol? ('left, 'center, or 'right)"
                               "align"
                               align))
      (case align
        [(left center right) align]
        [else
         (raise-arguments-error who
                                "expected #:align as one of 'left, 'center, or 'right"
                                "align"
                                align)]))

    ;; grid-gap-value? : any/c -> boolean?
    ;;   Return #t when v can be interpreted as a grid gap value.
    (define (grid-gap-value? v)
      (or (number? v) (string? v)))

    ;; view-with-props : view? list? -> view?
    ;;   Rebuild original view with replacement props.
    (define (view-with-props v props)
      (view (view-kind v)
            props
            (view-children v)))

    ;; apply-extra-class/internal : view? (or/c string? symbol? list?) -> view?
    ;;   Return v with extra class(es) added to the rendered root node.
    (define (apply-extra-class/internal v classes)
      (define props      (view-props v))
      (define old-classes (view-props-ref/default props 'extra-class '()))
      (define new-classes (append old-classes
                                  (normalize-class-list classes 'apply-root-decorators)))
      (view-with-props v
                       (view-props-set props 'extra-class new-classes)))

    ;; apply-extra-attrs/internal : view? list? -> view?
    ;;   Return v with extra attrs added to the rendered root node.
    (define (apply-extra-attrs/internal v attrs)
      (define normalized (normalize-attrs-list attrs 'apply-root-decorators))
      (define attrs-without-class
        (let loop ([remaining normalized])
          (cond
            [(null? remaining) '()]
            [(and (eq? (caar remaining) 'class)
                  (not (obs? (cdar remaining))))
             (loop (cdr remaining))]
            [else
             (cons (car remaining)
                   (loop (cdr remaining)))])))
      (define class-values
        (let loop ([remaining normalized])
          (cond
            [(null? remaining) '()]
            [(and (eq? (caar remaining) 'class)
                  (not (obs? (cdar remaining))))
             (append (normalize-class-list (cdar remaining) 'apply-root-decorators)
                     (loop (cdr remaining)))]
            [else
             (loop (cdr remaining))])))
      (define base-props (view-props v))
      (define old-attrs (view-props-ref/default base-props 'extra-attrs '()))
      (define v-with-extra-attrs
        (view-with-props v
                         (view-props-set base-props
                                         'extra-attrs
                                         (append old-attrs attrs-without-class))))
      (if (null? class-values)
          v-with-extra-attrs
          (apply-extra-class/internal v-with-extra-attrs class-values)))

    ;; apply-root-decorators : view? any/c any/c any/c symbol? -> view?
    ;;   Apply #:id #:class #:attrs to a view root in keyword-capable constructors.
    (define keyword-not-given (list 'keyword-not-given))

    ;; keyword-given? : any/c -> boolean?
    ;;   Return #t when v is a supplied keyword argument value.
    (define (keyword-given? v)
      (not (eq? v keyword-not-given)))

    (define (apply-root-decorators v id-value class-value attrs-value who)
      (define decorated-with-attrs
        (if (or (eq? attrs-value #f)
                (and (list? attrs-value) (null? attrs-value)))
            v
            (apply-extra-attrs/internal v attrs-value)))
      (define decorated-with-id
        (cond
          [(eq? id-value #f)
           decorated-with-attrs]
          [(string? id-value)
           (apply-extra-attrs/internal decorated-with-attrs
                                       (list (list 'id id-value)))]
          [(symbol? id-value)
           (apply-extra-attrs/internal decorated-with-attrs
                                       (list (list 'id (symbol->string id-value))))]
          [else
           (raise-arguments-error who
                                  "expected #:id as string?, symbol?, or #f"
                                  "id"
                                  id-value)]))
      (if (eq? class-value #f)
          decorated-with-id
          (apply-extra-class/internal decorated-with-id class-value)))

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
    (define/key (container
                 #:id    [id    #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/container '() children)
       id
       class
       attrs
       'container))

    ;; grid : any/c view? ... -> view?
    ;;   Construct a grid layout container with columns specification and children.
    ;;   Columns can be count (e.g. 2), responsive symbol ('auto/'responsive),
    ;;   CSS template string, or weighted list (e.g. '(70 30)).
    ;;   Optional first child can be a gap value (number px or CSS length string).
    (define/key (grid columns
                      #:id [id #f]
                      #:class [class #f]
                      #:attrs [attrs '()]
                      . children0)
      (define gap #f)
      (define children children0)
      (when (and (pair? children)
                 (grid-gap-value? (car children)))
        (set! gap (car children))
        (set! children (cdr children)))
      (apply-root-decorators
       (view kind/grid (list (cons 'columns columns)
                             (cons 'gap gap))
             children)
       id
       class
       attrs
       'grid))

    ;; stack : view? ... -> view?
    ;;   Construct a vertical stack layout container view.
    (define/key (stack
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/stack '() children)
       id
       class
       attrs
       'stack))

    ;; inline : view? ... -> view?
    ;;   Construct a horizontal inline layout container view.
    (define/key (inline
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/inline '() children)
       id
       class
       attrs
       'inline))

    ;; group : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled container view with children.
    (define/key (group label
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()]
                       . children)
      (apply-root-decorators
       (view kind/group (list (cons 'label label)) children)
       id
       class
       attrs
       'group))

    ;; alert : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct an inline alert/status view with optional severity level and root decorators.
    ;;   Optional parameter level defaults to 'info.
    (define/key (alert value
                       [level 'info]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/alert (list (cons 'value value)
                              (cons 'level level))
             '())
       id
       class
       attrs
       'alert))

    ;; options-alist? : any/c -> boolean?
    ;;   Check whether value is an options alist with symbol keys.
    (define (options-alist? value)
      (and (list? value)
           (let loop ([rest value])
             (cond
               [(null? rest) #t]
               [(and (pair? (car rest))
                     (symbol? (caar rest)))
                (loop (cdr rest))]
               [else #f]))))

    ;; alert-rich : (or/c string? observable?) (or/c string? observable? false/c) (or/c string? observable? false/c) (or/c string? observable? false/c) [(or/c symbol? observable?)] [list?] -> view?
    ;;   Construct a rich alert with required body and optional title/link text/link href.
    ;;   Optional parameter level defaults to 'info.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     dismiss-action -> procedure to dismiss the alert.
    ;;     dismiss-label  -> string/observable label for dismiss affordance.
    ;;     layout         -> 'stack (default) or 'inline body arrangement.
    ;;     scale          -> 'normal (default) or 'major title emphasis.
    ;;     tone           -> symbol/observable tone override (primary/secondary/success/info/warning/danger/light/dark).
    (define/key (alert-rich body
                            title
                            link-text
                            link-href
                            [level 'info]
                            [options '()]
                            #:level [level-kw #f]
                            #:dismiss-action [dismiss-action keyword-not-given]
                            #:dismiss-label [dismiss-label keyword-not-given]
                            #:layout [layout keyword-not-given]
                            #:inline-segments [inline-segments keyword-not-given]
                            #:scale [scale keyword-not-given]
                            #:tone [tone keyword-not-given]
                            #:id [id #f]
                            #:class [class #f]
                            #:attrs [attrs '()])
      (define final-level
        (if (eq? level-kw #f) level level-kw))
      (define final-options
        (if (list? options) options '()))
      (define (option-given-pair key value)
        (if (keyword-given? value)
            (list (cons key value))
            '()))
      (define options-with-keywords
        (append final-options
                (option-given-pair 'dismiss-action dismiss-action)
                (option-given-pair 'dismiss-label dismiss-label)
                (option-given-pair 'layout layout)
                (option-given-pair 'inline-segments inline-segments)
                (option-given-pair 'scale scale)
                (option-given-pair 'tone tone)))
      (apply-root-decorators
       (view kind/alert-rich
             (list (cons 'body body)
                   (cons 'title title)
                   (cons 'link-text link-text)
                   (cons 'link-href link-href)
                   (cons 'level final-level)
                   (cons 'options options-with-keywords))
             '())
       id
       class
       attrs
       'alert-rich))

    ;; toast : (or/c boolean? observable?) (-> any/c) (or/c string? observable?) [(or/c symbol? observable?)] [(or/c string? observable? false/c)] [(or/c boolean? observable?)] [number?] [boolean?] -> view?
    ;;   Construct a non-modal toast with open flag, close action, message, optional title/dismiss control, optional auto-hide duration, pause-on-hover, and root decorators.
    ;;   Optional parameter level defaults to 'info.
    ;;   Optional parameter title defaults to #f.
    ;;   Optional parameter dismissible? defaults to #t.
    ;;   Optional parameter duration-ms defaults to 0.
    ;;   Optional parameter pause-on-hover? defaults to #t.
    (define/key (toast open
                       on-close
                       value
                       [level 'info]
                       [title #f]
                       [dismissible? #t]
                       [duration-ms 0]
                       [pause-on-hover? #t]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/toast (list (cons 'open open)
                              (cons 'on-close on-close)
                              (cons 'value value)
                              (cons 'level level)
                              (cons 'title title)
                              (cons 'dismissible? dismissible?)
                              (cons 'duration-ms duration-ms)
                              (cons 'pause-on-hover? pause-on-hover?))
             '())
       id
       class
       attrs
       'toast))

    ;; close-button : (-> any/c) [(or/c string? observable?)] -> view?
    ;;   Construct a standardized close button with action, optional aria-label, and root decorators.
    ;;   Optional parameter aria-label defaults to "Close".
    (define/key (close-button action
                              [aria-label "Close"]
                              #:id [id #f]
                              #:class [class #f]
                              #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/close-button (list (cons 'action action)
                                     (cons 'aria-label aria-label))
             '())
       id
       class
       attrs
       'close-button))

    ;; badge : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct a compact inline badge with optional severity level.
    ;;   Optional parameter level defaults to 'info.
    (define/key (badge value
                       [level 'info]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/badge (list (cons 'value value)
                              (cons 'level level))
             '())
       id
       class
       attrs
       'badge))

    ;; spinner : [(or/c string? observable? false/c)] -> view?
    ;;   Construct a loading spinner with optional label text and root decorators.
    ;;   Optional parameter label defaults to "Loading...".
    (define/key (spinner [label "Loading..."]
                         #:id [id #f]
                         #:class [class #f]
                         #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/spinner (list (cons 'label label))
             '())
       id
       class
       attrs
       'spinner))

    ;; placeholder : [(or/c symbol? observable?)] [(or/c any/c observable?)] -> view?
    ;;   Construct a placeholder/skeleton block with optional shape and width.
    ;;   Optional parameter shape defaults to 'text.
    ;;   Optional parameter width defaults to #f.
    (define/key (placeholder [shape 'text]
                             [width #f]
                             #:shape [shape-kw #f]
                             #:width [width-kw #f]
                             #:id [id #f]
                             #:class [class #f]
                             #:attrs [attrs '()])
      (define final-shape
        (if (eq? shape-kw #f) shape shape-kw))
      (define final-width
        (if (eq? width-kw #f) width width-kw))
      (apply-root-decorators
       (view kind/placeholder (list (cons 'shape final-shape)
                                    (cons 'width final-width))
             '())
       id
       class
       attrs
       'placeholder))

    ;; text : (or/c string? observable?) -> view?
    ;;   Construct a text view from static or observable value.
    (define/key (text s
                      #:id    [id    #f]
                      #:class [class #f]
                      #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/text (list (cons 'value s)) '())
       id
       class
       attrs
       'text))

    ;; html-element : symbol? (or/c string? observable?) -> view?
    ;;   Construct a primitive HTML leaf element view from tag and content.
    (define/key (html-element tag
                              content
                              #:id    [id    #f]
                              #:class [class #f]
                              #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/html-element
             (list (cons 'tag   tag)
                   (cons 'value content))
             '())
       id
       class
       attrs
       'html-element))

    ;; html-element-children : symbol? view? ... -> view?
    ;;   Construct a primitive HTML element view with children.
    (define/key (html-element-children tag
                                       #:attrs [attrs '()]
                                       . children)
      (apply-root-decorators
       (view kind/html-element-children
             (list (cons 'tag tag))
             children)
       #f
       #f
       attrs
       'html-element-children))

    ;; normalize-heading-level/internal : any/c -> number?
    ;;   Normalize heading level to integer in the closed interval 1..6.
    (define (normalize-heading-level/internal level)
      (cond
        [(and (number? level)
              (integer? level)
              (>= level 1)
              (<= level 6))
         level]
        [else
         1]))

    ;; normalize-heading-align/internal : any/c -> symbol?
    ;;   Normalize heading alignment to one of 'left, 'center, or 'right.
    (define (normalize-heading-align/internal align)
      (if (symbol? align)
          (case align
            [(left center right) align]
            [else                'left])
          'left))

    ;; normalize-heading-spacing/internal : any/c -> symbol?
    ;;   Normalize heading spacing to one of 'compact, 'normal, or 'loose.
    (define (normalize-heading-spacing/internal spacing)
      (if (symbol? spacing)
          (case spacing
            [(compact normal loose) spacing]
            [else                   'normal])
          'normal))

    ;; heading-tag-for-level/internal : any/c -> symbol?
    ;;   Convert level to normalized heading tag symbol ('h1..'h6).
    (define (heading-tag-for-level/internal level)
      (string->symbol
       (string-append "h"
                      (number->string
                       (normalize-heading-level/internal level)))))

    ;; heading-class-for/internal : any/c any/c any/c -> string?
    ;;   Build semantic heading class string from level/align/spacing.
    (define (heading-class-for/internal level align spacing)
      (define normalized-level   (normalize-heading-level/internal level))
      (define normalized-align   (normalize-heading-align/internal align))
      (define normalized-spacing (normalize-heading-spacing/internal spacing))
      (string-append "we-heading we-heading-"
                     (number->string normalized-level)
                     " we-heading-align-"
                     (symbol->string normalized-align)
                     " we-heading-space-"
                     (symbol->string normalized-spacing)))

    ;; observable-or-const : any/c -> observable?
    ;;   Return v when observable, else wrap v in a constant observable.
    (define (observable-or-const v)
      (if (obs? v)
          v
          (obs v)))

    ;; heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with level normalized to 1..6 and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define/key (heading level
                         content
                         [align   'left]
                         [spacing 'normal]
                         #:align   [align-kw   #f]
                         #:spacing [spacing-kw #f]
                         #:id      [id         #f]
                         #:class   [class      #f]
                         #:attrs   [attrs      '()])
      (define final-align
        (if (eq? align-kw #f) align align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing spacing-kw))
      (apply-root-decorators
       (view kind/heading
             (list (cons 'level level)
                   (cons 'value content)
                   (cons 'align final-align)
                   (cons 'spacing final-spacing))
             '())
       id
       class
       attrs
       'heading))

    ;; h1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-1 heading view with optional root decorators.
    (define/key (h1 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 1 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; H1 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-1 heading element with generic keyword attributes.
    (define/element H1 html-element 'h1)

    ;; H2 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-2 heading element with generic keyword attributes.
    (define/element H2 html-element 'h2)

    ;; H3 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-3 heading element with generic keyword attributes.
    (define/element H3 html-element 'h3)

    ;; H4 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-4 heading element with generic keyword attributes.
    (define/element H4 html-element 'h4)

    ;; H5 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-5 heading element with generic keyword attributes.
    (define/element H5 html-element 'h5)

    ;; H6 : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-6 heading element with generic keyword attributes.
    (define/element H6 html-element 'h6)

    ;; P : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML paragraph element with generic keyword attributes.
    (define/element P html-element 'p)

    ;; Span : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML span element with generic keyword attributes.
    (define/element Span html-element 'span)

    ;; Strong : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML strong element with generic keyword attributes.
    (define/element Strong html-element 'strong)

    ;; Em : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML em element with generic keyword attributes.
    (define/element Em html-element 'em)

    ;; Code : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML code element with generic keyword attributes.
    (define/element Code html-element 'code)

    ;; Pre : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML pre element with generic keyword attributes.
    (define/element Pre html-element 'pre)

    ;; Small : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML small element with generic keyword attributes.
    (define/element Small html-element 'small)

    ;; Br : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML br element with generic keyword attributes.
    (define/element Br html-element 'br
      #:required-keywords ()
      #:positional-count 0)

    ;; Hr : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML hr element with generic keyword attributes.
    (define/element Hr html-element 'hr
      #:required-keywords ()
      #:positional-count 0)

    ;; Img : [#:src any/c] [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML img element with required #:src and generic keyword attrs.
    (define/element Img html-element 'img
      #:required-keywords (#:src)
      #:positional-count 0)

    ;; A : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML anchor element with generic keyword attributes.
    (define/element A html-element 'a)

    ;; Button : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML button element with generic keyword attributes.
    (define/element Button html-element 'button)

    ;; Div : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML div element with children and generic keyword attrs.
    (define/element Div html-element-children 'div
      #:required-keywords ()
      #:positional-count any)

    ;; Section : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML section element with children and generic keyword attrs.
    (define/element Section html-element-children 'section
      #:required-keywords ()
      #:positional-count any)

    ;; Article : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML article element with children and generic keyword attrs.
    (define/element Article html-element-children 'article
      #:required-keywords ()
      #:positional-count any)

    ;; Nav : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML nav element with children and generic keyword attrs.
    (define/element Nav html-element-children 'nav
      #:required-keywords ()
      #:positional-count any)

    ;; Main : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML main element with children and generic keyword attrs.
    (define/element Main html-element-children 'main
      #:required-keywords ()
      #:positional-count any)

    ;; Header : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML header element with children and generic keyword attrs.
    (define/element Header html-element-children 'header
      #:required-keywords ()
      #:positional-count any)

    ;; Footer : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML footer element with children and generic keyword attrs.
    (define/element Footer html-element-children 'footer
      #:required-keywords ()
      #:positional-count any)

    ;; Aside : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML aside element with children and generic keyword attrs.
    (define/element Aside html-element-children 'aside
      #:required-keywords ()
      #:positional-count any)

    ;; Form : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML form element with children and generic keyword attrs.
    (define/element Form html-element-children 'form
      #:required-keywords ()
      #:positional-count any)

    ;; Label : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML label element with generic keyword attrs.
    (define/element Label html-element 'label)

    ;; Ul : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML ul element with children and generic keyword attrs.
    (define/element Ul html-element-children 'ul
      #:required-keywords ()
      #:positional-count any)

    ;; Ol : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML ol element with children and generic keyword attrs.
    (define/element Ol html-element-children 'ol
      #:required-keywords ()
      #:positional-count any)

    ;; Li : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML li element with children and generic keyword attrs.
    (define/element Li html-element-children 'li
      #:required-keywords ()
      #:positional-count any)

    ;; h2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-2 heading view with optional root decorators.
    (define/key (h2 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 2 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; h3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-3 heading view with optional root decorators.
    (define/key (h3 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 3 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; h4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-4 heading view with optional root decorators.
    (define/key (h4 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 4 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; h5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-5 heading view with optional root decorators.
    (define/key (h5 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 5 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; h6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-6 heading view with optional root decorators.
    (define/key (h6 content
                    #:id [id #f]
                    #:class [class #f]
                    #:attrs [attrs '()])
      (heading 6 content
               #:id id
               #:class class
               #:attrs attrs))

    ;; display-heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with display style, level normalized to 1..6, and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define/key (display-heading level
                                 content
                                 [align 'left]
                                 [spacing 'normal]
                                 #:align [align-kw #f]
                                 #:spacing [spacing-kw #f]
                                 #:id [id #f]
                                 #:class [class #f]
                                 #:attrs [attrs '()])
      (define final-align
        (if (eq? align-kw #f) align align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing spacing-kw))
      (apply-root-decorators
       (view kind/display-heading (list (cons 'level level)
                                        (cons 'value content)
                                        (cons 'align final-align)
                                        (cons 'spacing final-spacing))
             '())
       id
       class
       attrs
       'display-heading))

    ;; display-1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-1 heading view with optional root decorators.
    (define/key (display-1 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 1 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; display-2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-2 heading view with optional root decorators.
    (define/key (display-2 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 2 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; display-3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-3 heading view with optional root decorators.
    (define/key (display-3 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 3 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; display-4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-4 heading view with optional root decorators.
    (define/key (display-4 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 4 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; display-5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-5 heading view with optional root decorators.
    (define/key (display-5 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 5 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; display-6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-6 heading view with optional root decorators.
    (define/key (display-6 content
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (display-heading 6 content
                       #:id id
                       #:class class
                       #:attrs attrs))

    ;; heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define/key (heading-with-subtitle level
                                       content
                                       subtitle
                                       [align 'left]
                                       [spacing 'normal]
                                       #:align [align-kw #f]
                                       #:spacing [spacing-kw #f]
                                       #:id [id #f]
                                       #:class [class #f]
                                       #:attrs [attrs '()])
      (define final-align
        (if (eq? align-kw #f) align align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing spacing-kw))
      (apply-root-decorators
       (view kind/heading-with-subtitle
             (list (cons 'level level)
                   (cons 'value content)
                   (cons 'subtitle subtitle)
                   (cons 'align final-align)
                   (cons 'spacing final-spacing))
             '())
       id
       class
       attrs
       'heading-with-subtitle))

    ;; display-heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic display heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define/key (display-heading-with-subtitle level
                                               content
                                               subtitle
                                               [align 'left]
                                               [spacing 'normal]
                                               #:align [align-kw #f]
                                               #:spacing [spacing-kw #f]
                                               #:id [id #f]
                                               #:class [class #f]
                                               #:attrs [attrs '()])
      (define final-align
        (if (eq? align-kw #f) align align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing spacing-kw))
      (apply-root-decorators
       (view kind/display-heading-with-subtitle
             (list (cons 'level level)
                   (cons 'value content)
                   (cons 'subtitle subtitle)
                   (cons 'align final-align)
                   (cons 'spacing final-spacing))
             '())
       id
       class
       attrs
       'display-heading-with-subtitle))

    ;; lead : (or/c string? observable?) -> view?
    ;;   Construct a lead paragraph view from static or observable value.
    (define/key (lead content
                      #:id [id #f]
                      #:class [class #f]
                      #:attrs [attrs '()])
      (define attrs/final
        (append attrs
                (list (cons 'data-we-widget "lead")
                      (cons 'class "we-lead"))))
      (apply P
             (append (list content)
                     (if (eq? id #f)
                         '()
                         (list '#:id id))
                     (if (eq? class #f)
                         '()
                         (list '#:class class))
                     (list '#:attrs attrs/final))))

    ;; blockquote : (or/c string? observable?) [(or/c string? observable? false/c)] -> view?
    ;;   Construct a semantic blockquote with optional attribution footer and alignment.
    ;;   Optional parameter attribution defaults to #f.
    (define/key (blockquote content
                            [attribution #f]
                            #:align [align 'left]
                            #:id [id #f]
                            #:class [class #f]
                            #:attrs [attrs '()])
      (define final-align
        (normalize-blockquote-align align 'blockquote))
      (apply-root-decorators
       (view kind/blockquote (list (cons 'value content)
                                   (cons 'attribution attribution)
                                   (cons 'align final-align))
             '())
       id
       class
       attrs
       'blockquote))

    ;; button : (or/c string? observable?) (-> any/c) [any/c] [any/c] -> view?
    ;;   Construct a button view with optional leading/trailing icon labels.
    ;;   Optional parameter leading-icon defaults to #f.
    ;;   Optional parameter trailing-icon defaults to #f.
    (define/key (button label
                        action
                        [leading-icon #f]
                        [trailing-icon #f]
                        #:id [id #f]
                        #:class [class #f]
                        #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/button (list (cons 'label label)
                               (cons 'action action)
                               (cons 'leading-icon leading-icon)
                               (cons 'trailing-icon trailing-icon))
             '())
       id
       class
       attrs
       'button))

    ;; link : (or/c string? observable?) (or/c string? observable?) [boolean?] [any/c] -> view?
    ;;   Construct a link view with href and optional download/target attributes.
    ;;   Optional parameter download? defaults to #f.
    ;;   Optional parameter target defaults to #f.
    (define/key (link label
                      href
                      [download? #f]
                      [target #f]
                      #:download? [download?-kw #f]
                      #:target [target-kw #f]
                      #:id [id #f]
                      #:class [class #f]
                      #:attrs [attrs '()])
      (define final-download?
        (if (eq? download?-kw #f) download? download?-kw))
      (define final-target
        (if (eq? target-kw #f) target target-kw))
      (apply-root-decorators
       (view kind/link (list (cons 'label label)
                             (cons 'href href)
                             (cons 'download final-download?)
                             (cons 'target final-target))
             '())
       id
       class
       attrs
       'link))

    ;; button-group : view? ... -> view?
    ;;   Construct a grouped button container view.
    (define/key (button-group
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/button-group '() children)
       id
       class
       attrs
       'button-group))

    ;; toggle-button-group : symbol? list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct an exclusive/non-exclusive toggle button group.
    (define/key (toggle-button-group mode
                                     choices
                                     selected
                                     action
                                     #:id [id #f]
                                     #:class [class #f]
                                     #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/toggle-button-group (list (cons 'mode mode)
                                            (cons 'choices choices)
                                            (cons 'selected selected)
                                            (cons 'action action))
             '())
       id
       class
       attrs
       'toggle-button-group))

    ;; button-toolbar : view? ... -> view?
    ;;   Construct a horizontal toolbar of grouped button controls.
    (define/key (button-toolbar
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/button-toolbar '() children)
       id
       class
       attrs
       'button-toolbar))

    ;; toolbar : view? ... -> view?
    ;;   Construct a generic horizontal toolbar container.
    (define/key (toolbar
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/toolbar '() children)
       id
       class
       attrs
       'toolbar))

    ;; toolbar-group : view? ... -> view?
    ;;   Construct a grouped toolbar section container.
    (define/key (toolbar-group
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/toolbar-group '() children)
       id
       class
       attrs
       'toolbar-group))

    ;; input : (or/c string? observable?) (-> any/c any/c) [(or/c (-> any/c) false/c)] [list?] -> view?
    ;;   Construct an input view with current value, change action, optional Enter action, and attrs.
    ;;   Optional parameter on-enter defaults to #f.
    ;;   Optional parameter attrs defaults to '().
    (define/key (input value
                       action
                       [on-enter #f]
                       [input-attrs '()]
                       #:on-enter [on-enter-kw #f]
                       #:input-attrs [input-attrs-kw #f]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (define final-on-enter
        (if (eq? on-enter-kw #f) on-enter on-enter-kw))
      (define final-input-attrs
        (if (eq? input-attrs-kw #f) input-attrs input-attrs-kw))
      (apply-root-decorators
       (view kind/input (list (cons 'value value)
                              (cons 'action action)
                              (cons 'on-enter final-on-enter)
                              (cons 'attrs final-input-attrs))
             '())
       id
       class
       attrs
       'input))

    ;; textarea : (or/c string? observable?) (-> any/c any/c) [number?] [list?] -> view?
    ;;   Construct a textarea view with current value, change action, optional rows, and attrs.
    ;;   Optional parameter rows defaults to 3.
    ;;   Optional parameter attrs defaults to '().
    (define/key (textarea value
                          action
                          [rows 3]
                          [textarea-attrs '()]
                          #:rows [rows-kw #f]
                          #:textarea-attrs [textarea-attrs-kw #f]
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (define final-rows
        (if (eq? rows-kw #f) rows rows-kw))
      (define final-textarea-attrs
        (if (eq? textarea-attrs-kw #f) textarea-attrs textarea-attrs-kw))
      (apply-root-decorators
       (view kind/textarea (list (cons 'value value)
                                 (cons 'action action)
                                 (cons 'rows final-rows)
                                 (cons 'attrs final-textarea-attrs))
             '())
       id
       class
       attrs
       'textarea))

    ;; checkbox : (or/c boolean? observable?) (-> any/c any/c) -> view?
    ;;   Construct a checkbox view with current state and toggle action.
    (define/key (checkbox value
                          action
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/checkbox (list (cons 'value value)
                                 (cons 'action action))
             '())
       id
       class
       attrs
       'checkbox))

    ;; choice : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a choice view with options, selected value, and action.
    (define/key (choice choices
                        selected
                        action
                        #:id [id #f]
                        #:class [class #f]
                        #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/choice (list (cons 'choices choices)
                               (cons 'selected selected)
                               (cons 'action action))
             '())
       id
       class
       attrs
       'choice))

    ;; slider : (or/c number? observable?) (-> any/c any/c) [number?] [number?] -> view?
    ;;   Construct a slider with value, action, and optional min/max bounds.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    (define/key (slider value
                        action
                        [min 0]
                        [max 100]
                        #:min [min-kw #f]
                        #:max [max-kw #f]
                        #:id [id #f]
                        #:class [class #f]
                        #:attrs [attrs '()])
      (define final-min
        (if (eq? min-kw #f) min min-kw))
      (define final-max
        (if (eq? max-kw #f) max max-kw))
      (apply-root-decorators
       (view kind/slider (list (cons 'value value)
                               (cons 'action action)
                               (cons 'min final-min)
                               (cons 'max final-max))
             '())
       id
       class
       attrs
       'slider))

    ;; progress : (or/c number? observable?) [number?] [number?] [(or/c symbol? observable?)] -> view?
    ;;   Construct a progress display with optional min/max bounds and variant.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    ;;   Optional parameter variant defaults to 'info.
    (define/key (progress value
                          [min 0]
                          [max 100]
                          [variant 'info]
                          #:min [min-kw #f]
                          #:max [max-kw #f]
                          #:variant [variant-kw #f]
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (define final-min
        (if (eq? min-kw #f) min min-kw))
      (define final-max
        (if (eq? max-kw #f) max max-kw))
      (define final-variant
        (if (eq? variant-kw #f) variant variant-kw))
      (apply-root-decorators
       (view kind/progress (list (cons 'value value)
                                 (cons 'min final-min)
                                 (cons 'max final-max)
                                 (cons 'variant final-variant))
             '())
       id
       class
       attrs
       'progress))

    ;; pagination : (or/c number? observable?) (or/c number? observable?) (-> any/c any/c) -> view?
    ;;   Construct a pagination control for page-count, current page, and page-change action.
    (define/key (pagination page-count
                            current-page
                            action
                            #:id [id #f]
                            #:class [class #f]
                            #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/pagination (list (cons 'page-count page-count)
                                   (cons 'current-page current-page)
                                   (cons 'action action))
             '())
       id
       class
       attrs
       'pagination))

    ;; breadcrumb : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a breadcrumb control for entries, current id, and navigation action.
    (define/key (breadcrumb entries
                            current
                            action
                            #:id [id #f]
                            #:class [class #f]
                            #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/breadcrumb (list (cons 'entries entries)
                                   (cons 'current current)
                                   (cons 'action action))
             '())
       id
       class
       attrs
       'breadcrumb))

    ;; list-group : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a selectable list-group from entries, current id, and selection action.
    (define/key (list-group entries
                            current
                            action
                            #:id [id #f]
                            #:class [class #f]
                            #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/list-group (list (cons 'entries entries)
                                   (cons 'current current)
                                   (cons 'action action))
             '())
       id
       class
       attrs
       'list-group))

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

    ;; tab-panel : (or/c any/c observable?) (listof (cons any/c view?)) [any/c] -> view?
    ;;   Construct a selected-tab branch view keyed by tab id, with optional style variants.
    ;;   Optional parameter variants defaults to 'default.
    (define/key (tab-panel selected
                           tabs
                           [variants 'default]
                           #:variants [variants-kw #f]
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (define final-variants
        (if (eq? variants-kw #f) variants variants-kw))
      (apply-root-decorators
       (view kind/tab-panel (list (cons 'selected selected)
                                  (cons 'tabs tabs)
                                  (cons 'variants final-variants))
             '())
       id
       class
       attrs
       'tab-panel))

    ;; collapse : (or/c boolean? observable?) view? -> view?
    ;;   Construct a container view that shows child only when open is true.
    (define/key (collapse open
                          child
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/collapse (list (cons 'open open))
             (list child))
       id
       class
       attrs
       'collapse))

    ;; accordion : (or/c any/c observable?) list? -> view?
    ;;   Construct a single-open accordion from section rows: (list id label view).
    (define/key (accordion selected
                           sections
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/accordion (list (cons 'selected selected)
                                  (cons 'sections sections))
             '())
       id
       class
       attrs
       'accordion))

    ;; offcanvas : (or/c boolean? observable?) (-> any/c) [(or/c symbol? observable?)] view? ... -> view?
    ;;   Construct an offcanvas side panel with open flag, close action, and optional side.
    ;;   Optional parameter side defaults to 'end.
    (define/key (offcanvas open
                           on-close
                           #:side [side-kw #f]
                           #:id [id #f]
                           #:class [class #f]
                           #:attrs [attrs '()]
                           . args)
      (define side 'end)
      (define children args)
      (when (and (pair? children)
                 (or (symbol? (car children))
                     (obs? (car children))))
        (set! side (car children))
        (set! children (cdr children)))
      (define final-side
        (if (eq? side-kw #f) side side-kw))
      (apply-root-decorators
       (view kind/offcanvas (list (cons 'open open)
                                  (cons 'on-close on-close)
                                  (cons 'side final-side))
             children)
       id
       class
       attrs
       'offcanvas))

    ;; dialog : (or/c boolean? observable?) (-> any/c) [symbol?] [list?] view? ... -> view?
    ;;   Construct a modal dialog that is visible when open is true and closes via on-close.
    ;;   Optional parameter size defaults to 'md.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title       -> string/observable title text.
    ;;     description -> string/observable description text.
    ;;     footer      -> string/observable or view content.
    ;;     show-close? -> boolean toggle for top-right close button (default #f).
    ;;     close-label -> string/observable aria-label for close button.
    ;;     tone        -> symbol tone: primary/secondary/success/danger/warning/info/light/dark.
    ;;     tone-style  -> symbol tone style: fill/outline.
    (define/key (dialog open
                        on-close
                        #:size        [size-kw     #f]
                        #:title       [title       #f]
                        #:description [description #f]
                        #:footer      [footer      #f]
                        #:show-close? [show-close? #f]
                        #:close-label [close-label "Close dialog"]
                        #:tone        [tone        #f]
                        #:tone-style  [tone-style  #f]
                        #:id          [id          #f]
                        #:class       [class       #f]
                        #:attrs       [attrs      '()]
                        . args)
      
      (define size        'md)
      (define rest/args   args)
      (define old-options '())
      (when (and (pair? rest/args)
                 (symbol? (car rest/args))
                 (memq (car rest/args) '(sm md lg xl)))
        (set! size (car rest/args))
        (set! rest/args (cdr rest/args)))
      (when (and (pair? rest/args)
                 (options-alist? (car rest/args)))
        (set! old-options (car rest/args))
        (set! rest/args (cdr rest/args)))
      
      (define final-size
        (if (eq? size-kw #f) size size-kw))
      (define options
        (append old-options
                (list (cons 'title       title)
                      (cons 'description description)
                      (cons 'footer      footer)
                      (cons 'show-close? show-close?)
                      (cons 'close-label close-label)
                      (cons 'tone        tone)
                      (cons 'tone-style  tone-style))))

      (apply-root-decorators
       (view kind/dialog (list (cons 'open     open)
                               (cons 'on-close on-close)
                               (cons 'size     final-size)
                               (cons 'options  options))
             rest/args)
       id
       class
       attrs
       'dialog))

    ;; modal : (or/c boolean? observable?) (-> any/c) [symbol?] [list?] view? ... -> view?
    ;;   Construct a modal container that mirrors dialog behavior.
    ;;   Optional parameter size defaults to 'md.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title       -> string/observable title text.
    ;;     description -> string/observable description text.
    ;;     footer      -> string/observable or view content.
    ;;     show-close? -> boolean toggle for top-right close button (default #f).
    ;;     close-label -> string/observable aria-label for close button.
    ;;     tone        -> symbol tone: primary/secondary/success/danger/warning/info/light/dark.
    ;;     tone-style  -> symbol tone style: fill/outline.
    (define/key (modal open
                       on-close
                       #:size        [size-kw     #f]
                       #:title       [title       #f]
                       #:description [description #f]
                       #:footer      [footer      #f]
                       #:show-close? [show-close? #f]
                       #:close-label [close-label "Close modal"]
                       #:tone        [tone        #f]
                       #:tone-style  [tone-style  #f]
                       #:id          [id          #f]
                       #:class       [class       #f]
                       #:attrs       [attrs      '()]
                       . args)
      
      (define size        'md)
      (define rest/args   args)
      (define old-options '())
      
      (when (and (pair? rest/args)
                 (symbol? (car rest/args))
                 (memq (car rest/args) '(sm md lg xl)))
        (set! size (car rest/args))
        (set! rest/args (cdr rest/args)))
      (when (and (pair? rest/args)
                 (options-alist? (car rest/args)))
        (set! old-options (car rest/args))
        (set! rest/args (cdr rest/args)))
      (define final-size
        (if (eq? size-kw #f) size size-kw))
      (define options
        (append old-options
                (list (cons 'title title)
                      (cons 'description description)
                      (cons 'footer footer)
                      (cons 'show-close? show-close?)
                      (cons 'close-label close-label)
                      (cons 'tone tone)
                      (cons 'tone-style tone-style))))
      
      (apply-root-decorators
       (view kind/modal (list (cons 'open open)
                              (cons 'on-close on-close)
                              (cons 'size final-size)
                              (cons 'options options))
             rest/args)
       id
       class
       attrs
       'modal))

    ;; observable-view : (or/c any/c observable?) (-> any/c view?) [(-> any/c any/c boolean?)] -> view?
    ;;   Construct a dynamic single-child view from value using make-view.
    ;;   Optional parameter equal-proc defaults to equal?.
    (define (observable-view data make-view [equal-proc equal?])
      (view kind/observable-view (list (cons 'data       data)
                                       (cons 'make-view  make-view)
                                       (cons 'equal-proc equal-proc))
            '()))

    ;; spacer : [number?] -> view?
    ;;   Construct an empty spacer view with optional grow factor.
    ;;   Optional parameter grow defaults to 1.
    (define/key (spacer [grow 1]
                        #:grow [grow-kw #f]
                        #:id [id #f]
                        #:class [class #f]
                        #:attrs [attrs '()])
      (define final-grow
        (if (eq? grow-kw #f) grow grow-kw))
      (apply-root-decorators
       (view kind/spacer (list (cons 'grow final-grow)) '())
       id
       class
       attrs
       'spacer))

    ;; divider : [symbol?] -> view?
    ;;   Construct a divider with orientation 'horizontal or 'vertical.
    ;;   Optional parameter orientation defaults to 'horizontal.
    (define/key (divider [orientation 'horizontal]
                         #:orientation [orientation-kw #f]
                         #:id [id #f]
                         #:class [class #f]
                         #:attrs [attrs '()])
      (define final-orientation
        (if (eq? orientation-kw #f) orientation orientation-kw))
      (apply-root-decorators
       (view kind/divider (list (cons 'orientation final-orientation)) '())
       id
       class
       attrs
       'divider))

    ;; table : list? (or/c list? observable?) [symbol?] [list?] -> view?
    ;;   Construct a table view with columns/rows, optional spacing density, and optional options alist.
    ;;   Column entries can be plain labels or (list label align) where align is left/center/right.
    ;;   Optional parameter density defaults to 'normal.
    ;;   Optional parameter options defaults to '() with supported keys:
    ;;     caption  -> string/observable caption text shown above header.
    ;;     variants -> symbol or list of symbols: striped/hover/borderless/sm.
    ;;     row-variants -> list of symbols per data row:
    ;;                     active/primary/secondary/success/danger/warning/info/light/dark.
    ;;     row-header-column -> non-negative column index rendered as <th scope=\"row\"> in data rows.
    (define/key (table columns
                       rows
                       [density 'normal]
                       [options-pos '()]
                       #:density [density-kw #f]
                       #:caption [caption #f]
                       #:variants [variants #f]
                       #:row-variants [row-variants #f]
                       #:row-header-column [row-header-column #f]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (define final-density
        (if (eq? density-kw #f) density density-kw))
      (define old-caption
        (if (list? options-pos)
            (let ([p (assq 'caption options-pos)])
              (if p (cdr p) #f))
            #f))
      (define old-variants
        (if (list? options-pos)
            (let ([p (assq 'variants options-pos)])
              (if p (cdr p) #f))
            #f))
      (define old-row-variants
        (if (list? options-pos)
            (let ([p (assq 'row-variants options-pos)])
              (if p (cdr p) #f))
            #f))
      (define old-row-header-column
        (if (list? options-pos)
            (let ([p (assq 'row-header-column options-pos)])
              (if p (cdr p) #f))
            #f))
      (define options
        (list (cons 'caption (if (eq? caption #f) old-caption caption))
              (cons 'variants (if (eq? variants #f) old-variants variants))
              (cons 'row-variants (if (eq? row-variants #f) old-row-variants row-variants))
              (cons 'row-header-column (if (eq? row-header-column #f)
                                           old-row-header-column
                                           row-header-column))))
      (apply-root-decorators
       (view kind/table (list (cons 'columns columns)
                              (cons 'rows rows)
                              (cons 'density final-density)
                              (cons 'options options))
             '())
       id
       class
       attrs
       'table))

    ;; radios : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a radio-choice control with choices and selected value.
    (define/key (radios choices
                        selected
                        action
                        #:id [id #f]
                        #:class [class #f]
                        #:attrs [attrs '()])
      (apply-root-decorators
       (view kind/radios (list (cons 'choices choices)
                               (cons 'selected selected)
                               (cons 'action action))
             '())
       id
       class
       attrs
       'radios))

    ;; image : (or/c string? observable?) [any/c] [any/c] -> view?
    ;;   Construct an image view from a source path/string with optional width/height attrs.
    ;;   Optional parameter width defaults to #f.
    ;;   Optional parameter height defaults to #f.
    (define/key (image src
                       [width #f]
                       [height #f]
                       #:width [width-kw #f]
                       #:height [height-kw #f]
                       #:id [id #f]
                       #:class [class #f]
                       #:attrs [attrs '()])
      (define final-width
        (if (eq? width-kw #f) width width-kw))
      (define final-height
        (if (eq? height-kw #f) height height-kw))
      (apply-root-decorators
       (view kind/image (list (cons 'src src)
                              (cons 'width final-width)
                              (cons 'height final-height))
             '())
       id
       class
       attrs
       'image))

    ;; dropdown : (or/c string? observable?) list? (-> any/c any/c) [symbol?] -> view?
    ;;   Construct a dropdown menu from a label and entry rows: (list id label).
    ;;   Optional parameter placement defaults to 'down.
    (define/key (dropdown label
                          entries
                          action
                          [placement 'down]
                          #:placement [placement-kw #f]
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (define final-placement
        (if (eq? placement-kw #f) placement placement-kw))
      (apply-root-decorators
       (view kind/dropdown (list (cons 'label label)
                                 (cons 'entries entries)
                                 (cons 'action action)
                                 (cons 'placement final-placement))
             '())
       id
       class
       attrs
       'dropdown))

    ;; carousel : list? (or/c number? observable?) (-> any/c any/c) [boolean?] [boolean?] -> view?
    ;;   Construct a carousel from item rows, current index, and index-change action with optional wrap and autoplay flags.
    ;;   Optional parameter wrap? defaults to #t.
    ;;   Optional parameter autoplay? defaults to #f.
    (define/key (carousel items
                          current-index
                          action
                          [wrap? #t]
                          [autoplay? #f]
                          #:wrap? [wrap-kw keyword-not-given]
                          #:autoplay? [autoplay-kw keyword-not-given]
                          #:id [id #f]
                          #:class [class #f]
                          #:attrs [attrs '()])
      (define final-wrap?
        (if (keyword-given? wrap-kw) wrap-kw wrap?))
      (define final-autoplay?
        (if (keyword-given? autoplay-kw) autoplay-kw autoplay?))
      (apply-root-decorators
       (view kind/carousel (list (cons 'items items)
                                 (cons 'current-index current-index)
                                 (cons 'action action)
                                 (cons 'wrap? final-wrap?)
                                 (cons 'autoplay? final-autoplay?))
             '())
       id
       class
       attrs
       'carousel))

    ;; scrollspy : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct scroll-tracking section navigation from rows: (list id label [content-view]).
    (define (scrollspy sections current action)
      (view kind/scrollspy (list (cons 'sections sections)
                                 (cons 'current current)
                                 (cons 'action action))
            '()))

    ;; tooltip : (or/c string? observable?) view? [symbol?] [list?] -> view?
    ;;   Construct a tooltip container with message, trigger child view, optional placement, and optional options.
    ;;   Optional parameter placement defaults to 'top.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title  -> string/observable heading shown above message.
    ;;     footer -> string/observable text shown below message.
    (define/key (tooltip message
                         child
                         #:placement [placement-kw #f]
                         #:title [title #f]
                         #:footer [footer #f]
                         #:id [id #f]
                         #:class [class #f]
                         #:attrs [attrs '()]
                         . args)
      (define placement 'top)
      (define old-options '())
      (when (and (pair? args)
                 (symbol? (car args))
                 (memq (car args) '(top right bottom left)))
        (set! placement (car args))
        (set! args (cdr args)))
      (when (and (pair? args)
                 (options-alist? (car args)))
        (set! old-options (car args))
        (set! args (cdr args)))
      (define final-placement
        (if (eq? placement-kw #f) placement placement-kw))
      (define options
        (append old-options
                (list (cons 'title title)
                      (cons 'footer footer))))
      (apply-root-decorators
       (view kind/tooltip (list (cons 'message message)
                                (cons 'placement final-placement)
                                (cons 'options options))
             (list child))
       id
       class
       attrs
       'tooltip))

    ;; popover : (or/c string? observable?) [symbol?] [list?] view? ... -> view?
    ;;   Construct a click-toggle popover with trigger label, optional placement, optional options, and body children.
    ;;   Optional parameter placement defaults to 'bottom.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title  -> string/observable heading shown above body.
    ;;     footer -> string/observable text shown below body.
    (define/key (popover label
                         #:placement [placement-kw #f]
                         #:title [title #f]
                         #:footer [footer #f]
                         #:id [id #f]
                         #:class [class #f]
                         #:attrs [attrs '()]
                         . args)
      (define placement 'bottom)
      (define old-options '())
      (define children args)
      (when (and (pair? children)
                 (symbol? (car children))
                 (memq (car children) '(top right bottom left)))
        (set! placement (car children))
        (set! children (cdr children)))
      (when (and (pair? children)
                 (options-alist? (car children)))
        (set! old-options (car children))
        (set! children (cdr children)))
      (define final-placement
        (if (eq? placement-kw #f) placement placement-kw))
      (define options
        (append old-options
                (list (cons 'title title)
                      (cons 'footer footer))))
      (apply-root-decorators
       (view kind/popover (list (cons 'label label)
                                (cons 'placement final-placement)
                                (cons 'options options))
             children)
       id
       class
       attrs
       'popover))

    ;; card : [(or/c string? observable? false/c)] [(or/c string? observable? false/c)] [any/c] [list?] any/c ... -> view?
    ;;   Construct a card with optional title/footer, optional variant(s), and body children.
    ;;   Optional parameter title defaults to #f.
    ;;   Optional parameter footer defaults to #f.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     subtitle -> string/observable subtitle rendered under title.
    ;;     media    -> view content rendered before body.
    ;;     actions  -> list of view values rendered in a card action row.
    ;;     tone     -> symbol color tone: primary/secondary/success/danger/warning/info/light/dark.
    ;;     tone-style -> symbol tone style: fill/outline.
    (define/key (card [title #f]
                      [footer #f]
                      #:variants [variants-kw #f]
                      #:subtitle [subtitle #f]
                      #:media [media #f]
                      #:actions [actions #f]
                      #:tone [tone #f]
                      #:tone-style [tone-style #f]
                      #:id [id #f]
                      #:class [class #f]
                      #:attrs [attrs '()]
                      . args)
      (define (all-symbols? xs)
        (cond
          [(null? xs) #t]
          [(symbol? (car xs)) (all-symbols? (cdr xs))]
          [else #f]))
      (define variants 'default)
      (define rest/args args)
      (define old-options '())
      (define has-variant?
        (and (pair? rest/args)
             (or (symbol? (car rest/args))
                 (and (list? (car rest/args))
                      (not (null? (car rest/args)))
                      (all-symbols? (car rest/args))))))
      (when has-variant?
        (set! variants (car rest/args))
        (set! rest/args (cdr rest/args)))
      (when (and (pair? rest/args)
                 (options-alist? (car rest/args)))
        (set! old-options (car rest/args))
        (set! rest/args (cdr rest/args)))
      (define final-variants
        (if (eq? variants-kw #f) variants variants-kw))
      (define old-subtitle
        (let ([p (assq 'subtitle old-options)])
          (if p (cdr p) #f)))
      (define old-media
        (let ([p (assq 'media old-options)])
          (if p (cdr p) #f)))
      (define old-actions
        (let ([p (assq 'actions old-options)])
          (if p (cdr p) #f)))
      (define old-tone
        (let ([p (assq 'tone old-options)])
          (if p (cdr p) #f)))
      (define old-tone-style
        (let ([p (assq 'tone-style old-options)])
          (if p (cdr p) #f)))
      (define options
        (append old-options
                (list (cons 'subtitle (if (eq? subtitle #f) old-subtitle subtitle))
                      (cons 'media (if (eq? media #f) old-media media))
                      (cons 'actions (if (eq? actions #f) old-actions actions))
                      (cons 'tone (if (eq? tone #f) old-tone tone))
                      (cons 'tone-style (if (eq? tone-style #f) old-tone-style tone-style)))))
      (apply-root-decorators
       (view kind/card (list (cons 'title title)
                             (cons 'footer footer)
                             (cons 'variants final-variants)
                             (cons 'options options))
             rest/args)
       id
       class
       attrs
       'card))

    ;; top-bar : view? ... -> view?
    ;;   Construct a top bar container for page-level header content.
    (define/key (top-bar
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . children)
      (apply-root-decorators
       (view kind/top-bar '() children)
       id
       class
       attrs
       'top-bar))

    ;; navigation-bar : [(or/c symbol? observable?)] [(or/c boolean? observable?)] [symbol?] view? ... -> view?
    ;;   Construct a navigation bar with optional orientation/collapsed/expand props and children.
    ;;   Optional parameter orientation defaults to 'horizontal.
    ;;   Optional parameter collapsed? defaults to #f.
    ;;   Optional parameter expand defaults to 'never.
    (define/key (navigation-bar
                 #:orientation [orientation-kw #f]
                 #:collapsed? [collapsed?-kw #f]
                 #:expand [expand-kw #f]
                 #:id [id #f]
                 #:class [class #f]
                 #:attrs [attrs '()]
                 . args)
      (define orientation 'horizontal)
      (define collapsed? #f)
      (define expand 'never)
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
      (define final-orientation (if (eq? orientation-kw #f) orientation orientation-kw))
      (define final-collapsed?  (if (eq? collapsed?-kw #f) collapsed? collapsed?-kw))
      (define final-expand      (if (eq? expand-kw #f) expand expand-kw))
      (apply-root-decorators
       (view kind/navigation-bar
             (list (cons 'orientation final-orientation)
                   (cons 'collapsed? final-collapsed?)
                   (cons 'expand final-expand))
             children)
       id
       class
       attrs
       'navigation-bar))

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
      (view kind/menu-item (list (cons 'label         label)
                                 (cons 'action        action)
                                 (cons 'leading-icon  leading-icon)
                                 (cons 'trailing-icon trailing-icon))
            '()))

    ;; list-view : (or/c list? observable?) (-> any/c any/c view?) [(-> any/c any/c)] -> view?
    ;;   Construct a keyed dynamic list container.
    ;;   Optional parameter key defaults to values.
    (define (list-view entries make-view [key values])
      (view kind/list-view (list (cons 'entries   entries)
                                 (cons 'make-view make-view)
                                 (cons 'key       key))
            '()))

    (values view
            view?
            view-kind
            view-props
            view-children
            text-content/c
            window
            vpanel
            hpanel
            container
            grid
            stack
            inline
            group
            alert
            alert-rich
            toast
            close-button
            badge
            spinner
            placeholder
            text
            heading
            h1
            H1
            H2
            H3
            H4
            H5
            H6
            P
            Span
            Strong
            Em
            Code
            Pre
            Small
            Br
            Hr
            Img
            A
            Button
            Div
            Section
            Article
            Nav
            Main
            Header
            Footer
            Aside
            Form
            Label
            Ul
            Ol
            Li
            h2
            h3
            h4
            h5
            h6
            display-heading
            display-1
            display-2
            display-3
            display-4
            display-5
            display-6
            heading-with-subtitle
            display-heading-with-subtitle
            lead
            blockquote
            button
            link
            button-group
            toggle-button-group
            button-toolbar
            toolbar
            toolbar-group
            input
            textarea
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
            top-bar
            navigation-bar
            menu-bar
            menu
            menu-item)))
