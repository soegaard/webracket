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
;;   alert-rich     Build a rich alert view with title/body/link parts.
;;   toast          Build a non-modal toast notification view.
;;   close-button   Build a standardized close button view.
;;   badge          Build a compact inline badge view.
;;   spinner        Build a loading spinner view.
;;   placeholder    Build a placeholder/skeleton view.
;;   text           Build a text label view.
;;   heading        Build a semantic heading view (h1..h6).
;;   h1             Build a semantic level-1 heading view.
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
;;   blockquote     Build a semantic blockquote with optional attribution.
;;   button         Build a button view with click action.
;;   link           Build a link view with href.
;;   button-group   Build a grouped button container view.
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
   alert-rich
   toast
   close-button
   badge
   spinner
   placeholder
   text
   heading
   h1
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
    (define kind/alert-rich 'alert-rich) ; Rich alert view with title/body/link parts.
    (define kind/toast     'toast)     ; Non-modal toast notification view.
    (define kind/close-button 'close-button) ; Standardized close button view.
    (define kind/badge     'badge)     ; Compact inline badge view.
    (define kind/spinner   'spinner)   ; Loading spinner view.
    (define kind/placeholder 'placeholder) ; Placeholder/skeleton view.
    (define kind/text      'text)      ; Text label view.
    (define kind/heading   'heading)   ; Semantic heading text view.
    (define kind/display-heading 'display-heading) ; Semantic heading with display style.
    (define kind/heading-with-subtitle 'heading-with-subtitle) ; Semantic heading with muted subtitle.
    (define kind/display-heading-with-subtitle 'display-heading-with-subtitle) ; Display heading with muted subtitle.
    (define kind/lead      'lead)      ; Lead paragraph text view.
    (define kind/blockquote 'blockquote) ; Semantic blockquote view with optional attribution.
    (define kind/button    'button)    ; Clickable action view.
    (define kind/link      'link)      ; Link view.
    (define kind/button-group 'button-group) ; Grouped button container view.
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
    (define (alert-rich body title link-text link-href [level 'info] [options '()])
      (view kind/alert-rich
            (list (cons 'body body)
                  (cons 'title title)
                  (cons 'link-text link-text)
                  (cons 'link-href link-href)
                  (cons 'level level)
                  (cons 'options options))
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

    ;; heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with level normalized to 1..6 and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define (heading level content [align 'left] [spacing 'normal])
      (view kind/heading (list (cons 'level level)
                               (cons 'value content)
                               (cons 'align align)
                               (cons 'spacing spacing))
            '()))

    ;; h1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-1 heading view.
    (define (h1 content)
      (heading 1 content))

    ;; h2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-2 heading view.
    (define (h2 content)
      (heading 2 content))

    ;; h3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-3 heading view.
    (define (h3 content)
      (heading 3 content))

    ;; h4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-4 heading view.
    (define (h4 content)
      (heading 4 content))

    ;; h5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-5 heading view.
    (define (h5 content)
      (heading 5 content))

    ;; h6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-6 heading view.
    (define (h6 content)
      (heading 6 content))

    ;; display-heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with display style, level normalized to 1..6, and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define (display-heading level content [align 'left] [spacing 'normal])
      (view kind/display-heading (list (cons 'level level)
                                       (cons 'value content)
                                       (cons 'align align)
                                       (cons 'spacing spacing))
            '()))

    ;; display-1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-1 heading view.
    (define (display-1 content)
      (display-heading 1 content))

    ;; display-2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-2 heading view.
    (define (display-2 content)
      (display-heading 2 content))

    ;; display-3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-3 heading view.
    (define (display-3 content)
      (display-heading 3 content))

    ;; display-4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-4 heading view.
    (define (display-4 content)
      (display-heading 4 content))

    ;; display-5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-5 heading view.
    (define (display-5 content)
      (display-heading 5 content))

    ;; display-6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-6 heading view.
    (define (display-6 content)
      (display-heading 6 content))

    ;; heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define (heading-with-subtitle level content subtitle [align 'left] [spacing 'normal])
      (view kind/heading-with-subtitle
            (list (cons 'level level)
                  (cons 'value content)
                  (cons 'subtitle subtitle)
                  (cons 'align align)
                  (cons 'spacing spacing))
            '()))

    ;; display-heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic display heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define (display-heading-with-subtitle level content subtitle [align 'left] [spacing 'normal])
      (view kind/display-heading-with-subtitle
            (list (cons 'level level)
                  (cons 'value content)
                  (cons 'subtitle subtitle)
                  (cons 'align align)
                  (cons 'spacing spacing))
            '()))

    ;; lead : (or/c string? observable?) -> view?
    ;;   Construct a lead paragraph view from static or observable value.
    (define (lead content)
      (view kind/lead (list (cons 'value content)) '()))

    ;; blockquote : (or/c string? observable?) [(or/c string? observable? false/c)] -> view?
    ;;   Construct a semantic blockquote with optional attribution footer.
    ;;   Optional parameter attribution defaults to #f.
    (define (blockquote content [attribution #f])
      (view kind/blockquote (list (cons 'value content)
                                  (cons 'attribution attribution))
            '()))

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

    ;; textarea : (or/c string? observable?) (-> any/c any/c) [number?] [list?] -> view?
    ;;   Construct a textarea view with current value, change action, optional rows, and attrs.
    ;;   Optional parameter rows defaults to 3.
    ;;   Optional parameter attrs defaults to '().
    (define (textarea value action [rows 3] [attrs '()])
      (view kind/textarea (list (cons 'value value)
                                (cons 'action action)
                                (cons 'rows rows)
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

    ;; tab-panel : (or/c any/c observable?) (listof (cons any/c view?)) [any/c] -> view?
    ;;   Construct a selected-tab branch view keyed by tab id, with optional style variants.
    ;;   Optional parameter variants defaults to 'default.
    (define (tab-panel selected tabs [variants 'default])
      (view kind/tab-panel (list (cons 'selected selected)
                                 (cons 'tabs tabs)
                                 (cons 'variants variants))
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
    (define (dialog open on-close . args)
      (define has-size?
        (and (pair? args)
             (symbol? (car args))
             (memq (car args) '(sm md lg xl))))
      (define size      (if has-size? (car args) 'md))
      (define rest/args (if has-size? (cdr args) args))
      (define has-options?
        (and (pair? rest/args)
             (options-alist? (car rest/args))))
      (define options  (if has-options? (car rest/args) '()))
      (define children (if has-options? (cdr rest/args) rest/args))
      (view kind/dialog (list (cons 'open open)
                              (cons 'on-close on-close)
                              (cons 'size size)
                              (cons 'options options))
            children))

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
    (define (modal open on-close . args)
      (define has-size?
        (and (pair? args)
             (symbol? (car args))
             (memq (car args) '(sm md lg xl))))
      (define size      (if has-size? (car args) 'md))
      (define rest/args (if has-size? (cdr args) args))
      (define has-options?
        (and (pair? rest/args)
             (options-alist? (car rest/args))))
      (define options  (if has-options? (car rest/args) '()))
      (define children (if has-options? (cdr rest/args) rest/args))
      (view kind/modal (list (cons 'open open)
                             (cons 'on-close on-close)
                             (cons 'size size)
                             (cons 'options options))
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
    (define (table columns rows [density 'normal] [options '()])
      (view kind/table (list (cons 'columns columns)
                             (cons 'rows rows)
                             (cons 'density density)
                             (cons 'options options))
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

    ;; dropdown : (or/c string? observable?) list? (-> any/c any/c) [symbol?] -> view?
    ;;   Construct a dropdown menu from a label and entry rows: (list id label).
    ;;   Optional parameter placement defaults to 'down.
    (define (dropdown label entries action [placement 'down])
      (view kind/dropdown (list (cons 'label label)
                                (cons 'entries entries)
                                (cons 'action action)
                                (cons 'placement placement))
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

    ;; tooltip : (or/c string? observable?) view? [symbol?] [list?] -> view?
    ;;   Construct a tooltip wrapper with message, trigger child view, optional placement, and optional options.
    ;;   Optional parameter placement defaults to 'top.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title  -> string/observable heading shown above message.
    ;;     footer -> string/observable text shown below message.
    (define (tooltip message child . args)
      (define has-placement?
        (and (pair? args)
             (symbol? (car args))
             (memq (car args) '(top right bottom left))))
      (define placement (if has-placement? (car args) 'top))
      (define rest/args (if has-placement? (cdr args) args))
      (define has-options?
        (and (pair? rest/args)
             (options-alist? (car rest/args))))
      (define options (if has-options? (car rest/args) '()))
      (view kind/tooltip (list (cons 'message message)
                               (cons 'placement placement)
                               (cons 'options options))
            (list child)))

    ;; popover : (or/c string? observable?) [symbol?] [list?] view? ... -> view?
    ;;   Construct a click-toggle popover with trigger label, optional placement, optional options, and body children.
    ;;   Optional parameter placement defaults to 'bottom.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     title  -> string/observable heading shown above body.
    ;;     footer -> string/observable text shown below body.
    (define (popover label . args)
      (define has-placement?
        (and (pair? args)
             (symbol? (car args))
             (memq (car args) '(top right bottom left))))
      (define placement (if has-placement? (car args) 'bottom))
      (define rest/args (if has-placement? (cdr args) args))
      (define has-options?
        (and (pair? rest/args)
             (options-alist? (car rest/args))))
      (define options  (if has-options? (car rest/args) '()))
      (define children (if has-options? (cdr rest/args) rest/args))
      (view kind/popover (list (cons 'label label)
                               (cons 'placement placement)
                               (cons 'options options))
            children))

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
      (define rest/args (if has-variant? (cdr args) args))
      (define has-options?
        (and (pair? rest/args)
             (options-alist? (car rest/args))))
      (define options  (if has-options? (car rest/args) '()))
      (define children (if has-options? (cdr rest/args) rest/args))
      (view kind/card (list (cons 'title title)
                            (cons 'footer footer)
                            (cons 'variants variants)
                            (cons 'options options))
            children))

    ;; navigation-bar : [(or/c symbol? observable?)] [(or/c boolean? observable?)] [symbol?] view? ... -> view?
    ;;   Construct a navigation bar with optional orientation/collapsed/expand props and children.
    ;;   Optional parameter orientation defaults to 'horizontal.
    ;;   Optional parameter collapsed? defaults to #f.
    ;;   Optional parameter expand defaults to 'never.
    (define (navigation-bar . args)
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
            alert-rich
            toast
            close-button
            badge
            spinner
            placeholder
            text
            heading
            h1
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
            navigation-bar
            menu-bar
            menu
            menu-item)))
