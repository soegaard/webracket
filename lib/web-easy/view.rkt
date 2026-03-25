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
;;   Fragment       Build a zero-wrapper composition view that contributes only children.
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
;;   B              Build a primitive HTML b element view with generic keyword attrs.
;;   I              Build a primitive HTML i element view with generic keyword attrs.
;;   U              Build a primitive HTML u element view with generic keyword attrs.
;;   S              Build a primitive HTML s element view with generic keyword attrs.
;;   Mark           Build a primitive HTML mark element view with generic keyword attrs.
;;   Sub            Build a primitive HTML sub element view with generic keyword attrs.
;;   Sup            Build a primitive HTML sup element view with generic keyword attrs.
;;   Kbd            Build a primitive HTML kbd element view with generic keyword attrs.
;;   Samp           Build a primitive HTML samp element view with generic keyword attrs.
;;   Var            Build a primitive HTML var element view with generic keyword attrs.
;;   Q              Build a primitive HTML q element view with generic keyword attrs.
;;   Cite           Build a primitive HTML cite element view with generic keyword attrs.
;;   Dfn            Build a primitive HTML dfn element view with generic keyword attrs.
;;   Abbr           Build a primitive HTML abbr element view with generic keyword attrs.
;;   Time           Build a primitive HTML time element view with generic keyword attrs.
;;   Data           Build a primitive HTML data element view with generic keyword attrs.
;;   Del            Build a primitive HTML del element view with generic keyword attrs.
;;   Ins            Build a primitive HTML ins element view with generic keyword attrs.
;;   Br             Build a primitive HTML br element view with generic keyword attrs.
;;   Wbr            Build a primitive HTML wbr element view with generic keyword attrs.
;;   Hr             Build a primitive HTML hr element view with generic keyword attrs.
;;   Img            Build a primitive HTML img element view with required #:src and generic keyword attrs.
;;   A              Build a primitive HTML a element view with generic keyword attrs.
;;   Button         Build a primitive HTML button element view with generic keyword attrs and child content.
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
;;   Menu           Build a primitive HTML menu element view with children and generic keyword attrs.
;;   Ol             Build a primitive HTML ol element view with children and generic keyword attrs.
;;   Li             Build a primitive HTML li element view with children and generic keyword attrs.
;;   Dl             Build a primitive HTML dl element view with children and generic keyword attrs.
;;   Dt             Build a primitive HTML dt element view with children and generic keyword attrs.
;;   Dd             Build a primitive HTML dd element view with children and generic keyword attrs.
;;   Table          Build a primitive HTML table element view with children and generic keyword attrs.
;;   Caption        Build a primitive HTML caption element view with children and generic keyword attrs.
;;   Thead          Build a primitive HTML thead element view with children and generic keyword attrs.
;;   Tbody          Build a primitive HTML tbody element view with children and generic keyword attrs.
;;   Tfoot          Build a primitive HTML tfoot element view with children and generic keyword attrs.
;;   Tr             Build a primitive HTML tr element view with children and generic keyword attrs.
;;   Th             Build a primitive HTML th element view with children and generic keyword attrs.
;;   Td             Build a primitive HTML td element view with children and generic keyword attrs.
;;   Audio          Build a primitive HTML audio element view with children and generic keyword attrs.
;;   Video          Build a primitive HTML video element view with children and generic keyword attrs.
;;   Source         Build a primitive HTML source element view with generic keyword attrs.
;;   Track          Build a primitive HTML track element view with generic keyword attrs.
;;   Canvas         Build a primitive HTML canvas element view with children and generic keyword attrs.
;;   Iframe         Build a primitive HTML iframe element view with children and generic keyword attrs.
;;   Embed          Build a primitive HTML embed element view with generic keyword attrs.
;;   Object         Build a primitive HTML object element view with children and generic keyword attrs.
;;   Input          Build a primitive HTML input element view with generic keyword attrs.
;;   Select         Build a primitive HTML select element view with children and generic keyword attrs.
;;   Option         Build a primitive HTML option element view with children and generic keyword attrs.
;;   Textarea       Build a primitive HTML textarea element view with generic keyword attrs.
;;   Details        Build a primitive HTML details element view with children and generic keyword attrs.
;;   Dialog         Build a primitive HTML dialog element view with children and generic keyword attrs.
;;   Summary        Build a primitive HTML summary element view with children and generic keyword attrs.
;;   Figure         Build a primitive HTML figure element view with children and generic keyword attrs.
;;   Figcaption     Build a primitive HTML figcaption element view with children and generic keyword attrs.
;;   Hgroup         Build a primitive HTML hgroup element view with children and generic keyword attrs.
;;   Address        Build a primitive HTML address element view with children and generic keyword attrs.
;;   Blockquote     Build a primitive HTML blockquote element view with children and generic keyword attrs.
;;   Ruby           Build a primitive HTML ruby element view with children and generic keyword attrs.
;;   Rt             Build a primitive HTML rt element view with children and generic keyword attrs.
;;   Rp             Build a primitive HTML rp element view with children and generic keyword attrs.
;;   Bdi            Build a primitive HTML bdi element view with children and generic keyword attrs.
;;   Bdo            Build a primitive HTML bdo element view with children and generic keyword attrs.
;;   Progress       Build a primitive HTML progress element view with children and generic keyword attrs.
;;   Meter          Build a primitive HTML meter element view with children and generic keyword attrs.
;;   Output         Build a primitive HTML output element view with children and generic keyword attrs.
;;   Fieldset       Build a primitive HTML fieldset element view with children and generic keyword attrs.
;;   Legend         Build a primitive HTML legend element view with children and generic keyword attrs.
;;   Datalist       Build a primitive HTML datalist element view with children and generic keyword attrs.
;;   Optgroup       Build a primitive HTML optgroup element view with children and generic keyword attrs.
;;   Colgroup       Build a primitive HTML colgroup element view with children and generic keyword attrs.
;;   Col            Build a primitive HTML col element view with generic keyword attrs.
;;   Map            Build a primitive HTML map element view with children and generic keyword attrs.
;;   Area           Build a primitive HTML area element view with generic keyword attrs.
;;   Script         Build a primitive HTML script element view with generic keyword attrs.
;;   Link           Build a primitive HTML link element view with generic keyword attrs.
;;   Meta           Build a primitive HTML meta element view with generic keyword attrs.
;;   Title          Build a primitive HTML title element view with generic keyword attrs.
;;   Base           Build a primitive HTML base element view with generic keyword attrs.
;;   Style          Build a primitive HTML style element view with generic keyword attrs.
;;   Slot           Build a primitive HTML slot element view with children and generic keyword attrs.
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
   Fragment
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
   B
   I
   U
   S
   Mark
   Sub
   Sup
   Kbd
   Samp
   Var
   Q
   Cite
   Dfn
   Abbr
   Time
   Data
   Del
   Ins
   Br
   Wbr
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
   Menu
   Ol
   Li
   Dl
   Dt
   Dd
   Table
   Caption
   Thead
   Tbody
   Tfoot
   Tr
   Th
   Td
   Audio
   Video
   Source
   Track
   Canvas
   Iframe
   Embed
   Object
   Input
   Select
   Option
   Textarea
   Details
   Dialog
   Summary
   Figure
   Figcaption
   Hgroup
   Address
   Blockquote
   Ruby
   Rt
   Rp
   Bdi
   Bdo
   Progress
   Meter
   Output
   Fieldset
   Legend
   Datalist
   Optgroup
   Colgroup
   Col
   Map
   Area
   Script
   Link
   Meta
   Title
   Base
   Style
   Slot
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
   menu-item
   rich-list-group)
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
    (define kind/fragment  'fragment)  ; Zero-wrapper composition view.
    (define kind/raw-text  'raw-text)  ; Internal raw text child view for primitive mixed content.
    (define kind/html-element 'html-element) ; Primitive HTML element leaf view.
    (define kind/html-element-children 'html-element-children) ; Primitive HTML element container view.
    (define kind/observable-element-children 'observable-element-children) ; Dynamic multi-child primitive element view.

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

    ;; normalize-alert-level/internal : any/c -> symbol?
    ;;   Normalize alert level to supported semantic/tone variants.
    (define (normalize-alert-level/internal level)
      (if (symbol? level)
          (case level
            [(warning)                                'warning]
            [(danger)                                 'danger]
            [(info success warning danger
                   primary secondary light dark)      level]
            [else                                     'info])
          'info))

    ;; alert-level-class/internal : symbol? -> string?
    ;;   Return CSS class suffix for alert level.
    (define (alert-level-class/internal level)
      (case level
        [(primary)   "we-alert-primary"]
        [(secondary) "we-alert-secondary"]
        [(success)   "we-alert-success"]
        [(warning)   "we-alert-warning"]
        [(danger)    "we-alert-danger"]
        [(light)     "we-alert-light"]
        [(dark)      "we-alert-dark"]
        [else        "we-alert-info"]))

    ;; alert-level-role/internal : symbol? -> symbol?
    ;;   Return semantic role for alert level severity.
    (define (alert-level-role/internal level)
      (case level
        [(warning danger) 'alert]
        [else             'status]))

    ;; normalize-badge-level/internal : any/c -> symbol?
    ;;   Normalize badge level to supported variants.
    (define (normalize-badge-level/internal level)
      (if (symbol? level)
          (case level
            [(primary secondary success info warning danger light dark) level]
            [else                                                      'info])
          'info))

    ;; badge-level-class/internal : symbol? -> string?
    ;;   Return CSS class suffix for badge level.
    (define (badge-level-class/internal level)
      (case level
        [(primary)   "we-badge-primary"]
        [(secondary) "we-badge-secondary"]
        [(success)   "we-badge-success"]
        [(warning)   "we-badge-warning"]
        [(danger)    "we-badge-danger"]
        [(light)     "we-badge-light"]
        [(dark)      "we-badge-dark"]
        [else        "we-badge-info"]))

    ;; progress-level-class/internal : symbol? -> string?
    ;;   Return CSS class suffix for progress variant level.
    (define (progress-level-class/internal level)
      (case level
        [(primary)   "we-progress-primary"]
        [(secondary) "we-progress-secondary"]
        [(success)   "we-progress-success"]
        [(warning)   "we-progress-warning"]
        [(danger)    "we-progress-danger"]
        [(light)     "we-progress-light"]
        [(dark)      "we-progress-dark"]
        [else        "we-progress-info"]))

    ;; grid-gap-value? : any/c -> boolean?
    ;;   Return #t when v can be interpreted as a grid gap value.
    (define (grid-gap-value? v)
      (or (number? v) (string? v)))

    ;; positive-number-list?/internal : any/c -> boolean?
    ;;   Check whether v is a non-empty list of positive numbers.
    (define (positive-number-list?/internal v)
      (cond
        [(not (list? v)) #f]
        [(null? v)       #f]
        [else
         (let loop ([xs v])
           (cond
             [(null? xs) #t]
             [(and (number? (car xs)) (> (car xs) 0))
              (loop (cdr xs))]
             [else #f]))]))

    ;; grid-columns-template/internal : any/c -> string?
    ;;   Normalize grid columns value to CSS template expression string.
    (define (grid-columns-template/internal columns)
      (cond
        [(or (eq? columns 'auto) (eq? columns 'responsive))
         "repeat(auto-fit,minmax(320px,1fr))"]
        [(number? columns)
         (if (> columns 0)
             (string-append "repeat(" (number->string columns) ",minmax(0,1fr))")
             "repeat(auto-fit,minmax(320px,1fr))")]
        [(positive-number-list?/internal columns)
         (let loop ([xs columns] [acc ""])
           (if (null? xs)
               acc
               (let ([part (string-append "minmax(0," (number->string (car xs)) "fr)")])
                 (loop (cdr xs)
                       (if (string=? acc "")
                           part
                           (string-append acc " " part))))))]
        [(string? columns) columns]
        [else
         "repeat(auto-fit,minmax(320px,1fr))"]))

    ;; grid-gap-template/internal : any/c -> string?
    ;;   Normalize grid gap value to CSS length expression string.
    (define (grid-gap-template/internal gap)
      (cond
        [(number? gap)
         (if (>= gap 0)
             (string-append (number->string gap) "px")
             "12px")]
        [(string? gap) gap]
        [else "12px"]))

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
    (define/component container
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "container")
              (cons 'class "we-container")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; grid : any/c view? ... -> view?
    ;;   Construct a grid layout container with columns specification and children.
    ;;   Columns can be count (e.g. 2), responsive symbol ('auto/'responsive),
    ;;   CSS template string, or weighted list (e.g. '(70 30)).
    ;;   Optional first child can be a gap value (number px or CSS length string).
    (define/component grid
      #:root-tag 'div
      #:rest args
      #:root-attrs attrs/final
      (when (null? args)
        (error 'grid "wrong number of positional arguments (expected at least 1, got 0)"))
      (define columns
        (car args))
      (define children0
        (cdr args))
      (define gap #f)
      (define children children0)
      (when (and (pair? children)
                 (grid-gap-value? (car children)))
        (set! gap (car children))
        (set! children (cdr children)))
      (define @columns
        (observable-or-const columns))
      (define @gap
        (observable-or-const gap))
      (define @style
        (obs-combine
         (lambda (columns0 gap0)
           (string-append "--we-grid-columns:"
                          (grid-columns-template/internal columns0)
                          ";--we-grid-gap:"
                          (grid-gap-template/internal gap0)
                          ";"))
         @columns
         @gap))
      (define attrs/final
        (list (cons 'data-we-widget "grid")
              (cons 'class "we-grid")
              (cons 'style @style)))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; stack : view? ... -> view?
    ;;   Construct a vertical stack layout container view.
    (define/component stack
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "stack")
              (cons 'class "we-stack")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; inline : view? ... -> view?
    ;;   Construct a horizontal inline layout container view.
    (define/component inline
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "inline")
              (cons 'class "we-inline")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; Fragment : view? ... -> view?
    ;;   Construct a composition view that contributes children without a wrapper node.
    (define (Fragment . children)
      (view kind/fragment '() children))

    ;; group : (or/c string? observable?) view? ... -> view?
    ;;   Construct a labeled container view with children.
    (define/component group
      #:root-tag 'fieldset
      #:rest args
      #:root-attrs attrs/final
      (when (null? args)
        (error 'group "wrong number of positional arguments (expected at least 1, got 0)"))
      (define label
        (car args))
      (define children
        (cdr args))
      (define attrs/final
        (list (cons 'data-we-widget "group")
              (cons 'class "we-group")))
      (apply Fieldset
             (append
              (list (html-element 'legend
                                  label
                                  #:attrs (list (cons 'data-we-widget "group-legend")
                                                (cons 'class "we-group-legend"))))
              children
              (list #:attrs attrs/final))))

    ;; alert : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct an inline alert/status view with optional severity level.
    ;;   Optional parameter level defaults to 'info.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component alert
      #:root-tag 'div
      #:positional ([value]
                    [level 'info])
      #:root-attrs attrs/final
      (define @level
        (observable-or-const level))
      (define @normalized-level
        (~> @level normalize-alert-level/internal))
      (define @role
        (~> @normalized-level alert-level-role/internal))
      (define @aria-live
        (~> @role (lambda (role0)
                    (if (eq? role0 'alert)
                        "assertive"
                        "polite"))))
      (define @class
        (~> @normalized-level
            (lambda (level0)
              (string-append "we-alert "
                             (alert-level-class/internal level0)))))
      (define attrs/final
        (list (cons 'role @role)
              (cons 'data-we-widget "alert")
              (cons 'class @class)
              (cons 'aria-live @aria-live)))
      (apply html-element
             (list 'div value '#:attrs attrs/final)))

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

    ;; options-ref/internal : list? symbol? any/c -> any/c
    ;;   Read option key from alist options with default fallback.
    (define (options-ref/internal options key default)
      (define p
        (and (list? options)
             (assq key options)))
      (if p
          (cdr p)
          default))

    ;; normalize-dialog-size/internal : any/c -> symbol?
    ;;   Normalize dialog/modal size to one of sm/md/lg/xl.
    (define (normalize-dialog-size/internal raw-size)
      (if (memq raw-size '(sm md lg xl))
          raw-size
          'md))

    ;; normalize-card-tone/internal : any/c -> any/c
    ;;   Normalize tone option to accepted symbols or #f.
    (define (normalize-card-tone/internal raw)
      (if (symbol? raw)
          (case raw
            [(primary secondary success danger warning info light dark) raw]
            [else #f])
          #f))

    ;; normalize-card-tone-style/internal : any/c -> any/c
    ;;   Normalize tone-style option to fill/outline or #f.
    (define (normalize-card-tone-style/internal raw)
      (if (symbol? raw)
          (case raw
            [(fill outline) raw]
            [else #f])
          #f))

    ;; normalize-tab-variants/internal : any/c -> list?
    ;;   Normalize tab variants to a list of symbols.
    (define (normalize-tab-variants/internal raw-variants)
      (cond
        [(symbol? raw-variants)
         (list raw-variants)]
        [(list? raw-variants)
         (let loop ([xs raw-variants] [acc '()])
           (cond
             [(null? xs) (reverse acc)]
             [else
              (define x (car xs))
              (loop (cdr xs)
                    (if (symbol? x) (cons x acc) acc))]))]
        [else '(default)]))

    ;; tab-variant-class/internal : list? -> string?
    ;;   Build tab-panel class suffix from normalized variants.
    (define (tab-variant-class/internal tab-variants)
      (let loop ([rest tab-variants] [acc ""])
        (cond
          [(null? rest) acc]
          [else
           (define variant-class
             (if (eq? (car rest) 'default)
                 "we-tab-style-default"
                 (string-append "we-tab-style-" (symbol->string (car rest)))))
           (loop (cdr rest)
                 (if (string=? acc "")
                     variant-class
                     (string-append acc " " variant-class)))])))

    ;; normalize-tab-entry/internal : any/c -> list?
    ;;   Normalize tab entry to (list id view disabled?) supporting pair or list forms.
    (define (normalize-tab-entry/internal tab)
      (cond
        [(list? tab)
         (define n (length tab))
         (cond
           [(= n 2)
            (list (list-ref tab 0) (list-ref tab 1) #f)]
           [(= n 3)
            (list (list-ref tab 0) (list-ref tab 1) (not (eq? (list-ref tab 2) #f)))]
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

    ;; tab-panel-counter/internal : number?
    ;;   Monotonic counter for generated tab-panel id values.
    (define tab-panel-counter/internal 0)

    ;; next-tab-panel-id/internal : -> string?
    ;;   Allocate a unique id string for tab-panel content region.
    (define (next-tab-panel-id/internal)
      (set! tab-panel-counter/internal (add1 tab-panel-counter/internal))
      (string-append "tab-panel-" (number->string tab-panel-counter/internal)))

    ;; accordion-panel-counter/internal : number?
    ;;   Monotonic counter for generated accordion panel ids.
    (define accordion-panel-counter/internal 0)

    ;; next-accordion-panel-id/internal : -> string?
    ;;   Allocate a unique id string for accordion content panel region.
    (define (next-accordion-panel-id/internal)
      (set! accordion-panel-counter/internal
            (add1 accordion-panel-counter/internal))
      (string-append "accordion-panel-" (number->string accordion-panel-counter/internal)))

    ;; normalize-card-variants/internal : any/c -> list?
    ;;   Normalize variant value to accepted card variant symbols.
    (define (normalize-card-variants/internal raw)
      (define (allowed-variant? v)
        (and (symbol? v)
             (case v
               [(default compact flat headerless) #t]
               [else #f])))
      (define (loop xs)
        (cond
          [(null? xs) '()]
          [(allowed-variant? (car xs))
           (cons (car xs) (loop (cdr xs)))]
          [else
           (loop (cdr xs))]))
      (cond
        [(allowed-variant? raw)
         (list raw)]
        [(list? raw)
         (loop raw)]
        [else
         (list 'default)]))

    ;; card-variant-class/internal : list? -> string?
    ;;   Build card class string from variant symbols.
    (define (card-variant-class/internal variants)
      (define compact? (contains-equal/internal variants 'compact))
      (define flat?    (contains-equal/internal variants 'flat))
      (string-append
       "we-card"
       (if compact? " we-card-compact" "")
       (if flat? " we-card-flat" "")))

    ;; dialog-body-counter/internal : number?
    ;;   Monotonic counter for generated dialog/modal description ids.
    (define dialog-body-counter/internal 0)

    ;; next-dialog-body-id/internal : -> string?
    ;;   Allocate a unique id for dialog/modal descriptive body content.
    (define (next-dialog-body-id/internal)
      (set! dialog-body-counter/internal
            (add1 dialog-body-counter/internal))
      (string-append "dialog-body-"
                     (number->string dialog-body-counter/internal)))

    ;; invoke-close-callback/internal : any/c symbol? -> void?
    ;;   Call close callback with reason when arity allows, else call without args.
    (define (invoke-close-callback/internal on-close reason)
      (when (procedure? on-close)
        (if (procedure-arity-includes? on-close 1)
            (on-close reason)
            (on-close))))

    ;; text-view?/internal : any/c -> boolean?
    ;;   Check whether v is a text constructor view suitable for aria-describedby id forwarding.
    (define (text-view?/internal v)
      (and (view? v)
           (eq? (view-kind v) kind/html-element)
           (let* ([props (view-props v)]
                  [extra (view-props-ref/default props 'extra-attrs #f)]
                  [widget-pair (and (list? extra) (assq 'data-we-widget extra))])
             (and widget-pair
                  (equal? (cdr widget-pair) "text")))))

    ;; parse-offcanvas-args/internal : list? any/c -> list?
    ;;   Parse optional legacy side positional argument and merge with #:side when present.
    (define (normalize-offcanvas-side/internal side)
      (if (symbol? side)
          (case side
            [(start end) side]
            [else        'end])
          'end))

    (define (parse-offcanvas-args/internal args side-kw)
      (define side 'end)
      (define children args)
      (when (and (pair? children)
                 (or (symbol? (car children))
                     (obs? (car children))))
        (set! side (car children))
        (set! children (cdr children)))
      (list (if (eq? side-kw #f) side side-kw)
            children))

    ;; parse-dialog-options/internal : list? any/c any/c any/c any/c any/c any/c any/c any/c any/c string? -> list?
    ;;   Parse legacy dialog/modal optional args and merged options into normalized fields.
    (define (parse-dialog-options/internal args
                                           size-kw
                                           title
                                           description
                                           footer
                                           show-close?
                                           close-label
                                           tone
                                           tone-style
                                           default-close-label)
      (define size 'md)
      (define rest/args args)
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
        (normalize-dialog-size/internal
         (if (eq? size-kw #f) size size-kw)))
      (define options
        (append old-options
                (list (cons 'title title)
                      (cons 'description description)
                      (cons 'footer footer)
                      (cons 'show-close? show-close?)
                      (cons 'close-label close-label)
                      (cons 'tone tone)
                      (cons 'tone-style tone-style))))
      (list final-size
            rest/args
            (options-ref/internal options 'title #f)
            (options-ref/internal options 'description #f)
            (options-ref/internal options 'footer #f)
            (options-ref/internal options 'show-close? #f)
            (options-ref/internal options 'close-label default-close-label)
            (options-ref/internal options 'tone #f)
            (options-ref/internal options 'tone-style #f)))

    ;; normalize-alert-layout/internal : any/c -> symbol?
    ;;   Normalize alert layout mode to 'stack or 'inline.
    (define (normalize-alert-layout/internal value)
      (if (symbol? value)
          (case value
            [(inline) 'inline]
            [else 'stack])
          'stack))

    ;; normalize-alert-scale/internal : any/c -> symbol?
    ;;   Normalize alert scale mode to 'normal or 'major.
    (define (normalize-alert-scale/internal value)
      (if (symbol? value)
          (case value
            [(major) 'major]
            [else 'normal])
          'normal))

    ;; toast-level-class/internal : symbol? -> string?
    ;;   Return CSS class suffix for toast severity.
    (define (toast-level-class/internal level)
      (case level
        [(success) "we-toast-success"]
        [(warning) "we-toast-warning"]
        [(danger)  "we-toast-danger"]
        [else      "we-toast-info"]))

    ;; normalize-toast-duration/internal : any/c -> number?
    ;;   Normalize toast auto-hide duration to non-negative integer milliseconds.
    (define (normalize-toast-duration/internal duration-ms)
      (cond
        [(and (number? duration-ms)
              (integer? duration-ms)
              (> duration-ms 0))
         duration-ms]
        [else
         0]))

    ;; alert-rich : (or/c string? observable?) (or/c string? observable? false/c) (or/c string? observable? false/c) (or/c string? observable? false/c) [(or/c symbol? observable?)] [list?] -> view?
    ;;   Construct a rich alert with required body and optional title/link text/link href.
    ;;   Optional parameter level defaults to 'info.
    ;;   Optional parameter options defaults to '() and accepts:
    ;;     dismiss-action -> procedure to dismiss the alert.
    ;;     dismiss-label  -> string/observable label for dismiss affordance.
    ;;     layout         -> 'stack (default) or 'inline body arrangement.
    ;;     scale          -> 'normal (default) or 'major title emphasis.
    ;;     tone           -> symbol/observable tone override (primary/secondary/success/info/warning/danger/light/dark).
    (define/component alert-rich
      #:root-tag 'div
      #:component-keywords ([#:level level-kw #f]
                            [#:dismiss-action dismiss-action keyword-not-given]
                            [#:dismiss-label dismiss-label keyword-not-given]
                            [#:layout layout keyword-not-given]
                            [#:inline-segments inline-segments keyword-not-given]
                            [#:scale scale keyword-not-given]
                            [#:tone tone keyword-not-given])
      #:rest all-positional
      #:root-attrs attrs/final
      (define positional-count
        (length all-positional))
      (when (or (< positional-count 4) (> positional-count 6))
        (error 'alert-rich
               "wrong number of positional arguments (expected 4 to 6, got ~a)"
               positional-count))
      (define body
        (list-ref all-positional 0))
      (define title
        (list-ref all-positional 1))
      (define link-text
        (list-ref all-positional 2))
      (define link-href
        (list-ref all-positional 3))
      (define level
        (if (>= positional-count 5)
            (list-ref all-positional 4)
            'info))
      (define options
        (if (>= positional-count 6)
            (list-ref all-positional 5)
            '()))
      (define (non-empty-text? value)
        (and (not (eq? value #f))
             (not (string=? (format "~a" value) ""))))
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
      (define raw-dismiss-action
        (options-ref/internal options-with-keywords 'dismiss-action #f))
      (define raw-dismiss-label
        (options-ref/internal options-with-keywords 'dismiss-label "Dismiss"))
      (define raw-layout
        (options-ref/internal options-with-keywords 'layout 'stack))
      (define raw-inline-segments
        (options-ref/internal options-with-keywords 'inline-segments #f))
      (define raw-scale
        (options-ref/internal options-with-keywords 'scale 'normal))
      (define raw-tone
        (options-ref/internal options-with-keywords 'tone #f))
      (define @body
        (observable-or-const body))
      (define @title
        (observable-or-const title))
      (define @link-text
        (observable-or-const link-text))
      (define @link-href
        (observable-or-const link-href))
      (define @level
        (observable-or-const final-level))
      (define @dismiss-action
        (observable-or-const raw-dismiss-action))
      (define @dismiss-label
        (observable-or-const raw-dismiss-label))
      (define @layout
        (observable-or-const raw-layout))
      (define @inline-segments
        (observable-or-const raw-inline-segments))
      (define @scale
        (observable-or-const raw-scale))
      (define @tone
        (observable-or-const raw-tone))
      (define @state
        (obs-combine list
                     @body
                     @title
                     @link-text
                     @link-href
                     @level
                     @dismiss-action
                     @dismiss-label
                     @layout
                     @inline-segments
                     @scale
                     @tone))
      (define (inline-segment-view segment)
        (cond
          [(and (list? segment)
                (= (length segment) 2)
                (eq? (car segment) 'text))
           (Span (list-ref segment 1)
                 #:data-we-widget "alert-body"
                 #:class "we-alert-body")]
          [(and (list? segment)
                (= (length segment) 3)
                (eq? (car segment) 'link))
           (A (list-ref segment 1)
              #:href (list-ref segment 2)
              #:data-we-widget "alert-link"
              #:class "we-alert-link")]
          [else
           #f]))
      (define (alert-rich-root-role state)
        (define level0
          (normalize-alert-level/internal (list-ref state 4)))
        (alert-level-role/internal level0))
      (define @root-role
        (~> @state alert-rich-root-role))
      (define @root-aria-live
        (~> @root-role
            (lambda (role0)
              (if (eq? role0 'alert)
                  "assertive"
                  "polite"))))
      (define @root-class
        (~> @state
            (lambda (state)
              (define level0
                (normalize-alert-level/internal (list-ref state 4)))
              (define layout0
                (normalize-alert-layout/internal (list-ref state 7)))
              (define scale0
                (normalize-alert-scale/internal (list-ref state 9)))
              (define tone0
                (normalize-alert-level/internal
                 (if (eq? (list-ref state 10) #f)
                     level0
                     (list-ref state 10))))
              (string-append
               "we-alert "
               (alert-level-class/internal tone0)
               (if (eq? layout0 'inline)
                   " we-alert-layout-inline"
                   "")
               (if (eq? scale0 'major)
                   " we-alert-scale-major"
                   "")))))
      (define (make-alert-rich-children state)
        (define title0
          (list-ref state 1))
        (define link-text0
          (list-ref state 2))
        (define link-href0
          (list-ref state 3))
        (define body0
          (list-ref state 0))
        (define dismiss-action0
          (list-ref state 5))
        (define dismiss-label0
          (list-ref state 6))
        (define inline-segments0
          (list-ref state 8))
        (define inline-segment-views
          (if (and (list? inline-segments0)
                   (pair? inline-segments0))
              (let loop ([segments inline-segments0])
                (cond
                  [(null? segments)
                   '()]
                  [else
                   (define maybe-view
                     (inline-segment-view (car segments)))
                   (if maybe-view
                       (cons maybe-view (loop (cdr segments)))
                       (loop (cdr segments)))]))
              '()))
        (append
         (if (non-empty-text? title0)
             (list (Strong title0
                           #:data-we-widget "alert-title"
                           #:class "we-alert-title"))
             '())
         (if (pair? inline-segment-views)
             inline-segment-views
             (append (list (Span body0
                                 #:data-we-widget "alert-body"
                                 #:class "we-alert-body"))
                     (if (and (non-empty-text? link-text0)
                              (non-empty-text? link-href0))
                         (list (A link-text0
                                  #:href link-href0
                                  #:data-we-widget "alert-link"
                                  #:class "we-alert-link"))
                         '())))
         (if (procedure? dismiss-action0)
             (list (Button "×"
                           #:attrs (list (cons 'aria-label dismiss-label0)
                                         (cons 'data-we-widget "alert-dismiss")
                                         (cons 'class "we-alert-dismiss")
                                         (cons 'on-click-action dismiss-action0))))
             '())))
      (define attrs/final
        (list (cons 'role @root-role)
              (cons 'data-we-widget "alert")
              (cons 'class @root-class)
              (cons 'aria-live @root-aria-live)))
      (observable-element-children
       'div
       @state
       make-alert-rich-children
       #:attrs attrs/final))

    ;; toast : (or/c boolean? observable?) (-> any/c) (or/c string? observable?) [(or/c symbol? observable?)] [(or/c string? observable? false/c)] [(or/c boolean? observable?)] [number?] [boolean?] -> view?
    ;;   Construct a non-modal toast with open flag, close action, message, optional title/dismiss control, optional auto-hide duration, pause-on-hover, and root decorators.
    ;;   Optional parameter level defaults to 'info.
    ;;   Optional parameter title defaults to #f.
    ;;   Optional parameter dismissible? defaults to #t.
    ;;   Optional parameter duration-ms defaults to 0.
    ;;   Optional parameter pause-on-hover? defaults to #t.
    (define/component toast
      #:root-tag 'div
      #:positional ([open]
                    [on-close]
                    [value]
                    [level 'info]
                    [title #f]
                    [dismissible? #t]
                    [duration-ms 0]
                    [pause-on-hover? #t])
      #:root-attrs attrs/final
      (define @open
        (observable-or-const open))
      (define @value
        (observable-or-const value))
      (define @level
        (observable-or-const level))
      (define @title
        (observable-or-const title))
      (define @dismissible?
        (observable-or-const dismissible?))
      (define @duration-ms
        (observable-or-const duration-ms))
      (define @pause-on-hover?
        (observable-or-const pause-on-hover?))
      (define @on-close
        (observable-or-const on-close))
      (define @hovered
        (@ #f))
      (define @state
        (obs-combine list
                     @open
                     @value
                     @level
                     @title
                     @dismissible?
                     @duration-ms
                     @pause-on-hover?
                     @hovered
                     @on-close))
      (define toast-timeout-handle #f)
      (define toast-cleanup-registered? #f)
      (define (clear-toast-timeout! backend-clear-timeout!)
        (when toast-timeout-handle
          (backend-clear-timeout! toast-timeout-handle)
          (set! toast-timeout-handle #f)))
      (define (toast-after-render _root-node state register-cleanup! api)
        (define (api-proc key)
          (define p (assq key api))
          (if (and p (procedure? (cdr p)))
              (cdr p)
              (raise-arguments-error 'toast-after-render
                                     "missing callback API procedure"
                                     "key"
                                     key)))
        (define backend-set-timeout!
          (api-proc 'backend-set-timeout!))
        (define backend-clear-timeout!
          (api-proc 'backend-clear-timeout!))
        (unless toast-cleanup-registered?
          (set! toast-cleanup-registered? #t)
          (register-cleanup!
           (lambda ()
             (clear-toast-timeout! backend-clear-timeout!))))
        (clear-toast-timeout! backend-clear-timeout!)
        (define open?
          (not (eq? (car state) #f)))
        (define duration0
          (normalize-toast-duration/internal (list-ref state 5)))
        (define pause-on-hover0?
          (not (eq? (list-ref state 6) #f)))
        (define hovered0?
          (not (eq? (list-ref state 7) #f)))
        (define on-close0
          (list-ref state 8))
        (when (and open?
                   (> duration0 0)
                   (procedure? on-close0)
                   (or (not pause-on-hover0?)
                       (not hovered0?)))
          (set! toast-timeout-handle
                (backend-set-timeout!
                 duration0
                 (lambda ()
                   (set! toast-timeout-handle #f)
                   (on-close0))))))
      (define (toast-root-role state)
        (define level0
          (normalize-alert-level/internal (list-ref state 2)))
        (alert-level-role/internal level0))
      (define @root-role
        (~> @state toast-root-role))
      (define @root-aria-live
        (~> @root-role
            (lambda (role0)
              (if (eq? role0 'alert)
                  "assertive"
                  "polite"))))
      (define @root-aria-hidden
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "true"
                  "false"))))
      (define @root-class
        (~> @state
            (lambda (state)
              (define level0
                (normalize-alert-level/internal (list-ref state 2)))
              (define open0
                (list-ref state 0))
              (string-append
               "we-toast "
               (toast-level-class/internal level0)
               (if (eq? open0 #f)
                   ""
                   " is-open")))))
      (define (toast-change-action event-key)
        (case (string->symbol event-key)
          [(mouseenter)
           (:= @hovered #t)]
          [(mouseleave)
           (:= @hovered #f)]
          [else
           (void)]))
      (define (make-toast-children state)
        (define title0
          (list-ref state 3))
        (define message0
          (list-ref state 1))
        (define dismissible0?
          (not (eq? (list-ref state 4) #f)))
        (define on-close0
          (list-ref state 8))
        (append
         (if (eq? title0 #f)
             '()
             (list (Span title0
                         #:data-we-widget "toast-title"
                         #:class "we-toast-title")))
         (list (Span message0
                     #:data-we-widget "toast-message"
                     #:class "we-toast-message"))
         (if dismissible0?
             (list (Button "×"
                           #:attrs (list (cons 'role 'button)
                                         (cons 'data-we-widget "toast-close")
                                         (cons 'class "we-close-button we-toast-close")
                                         (cons 'aria-label "Close toast")
                                         (cons 'on-click-action on-close0))))
             '())))
      (define attrs/final
        (list (cons 'role @root-role)
              (cons 'data-we-widget "toast")
              (cons 'class @root-class)
              (cons 'aria-live @root-aria-live)
              (cons 'aria-hidden @root-aria-hidden)
              (cons 'on-change-action toast-change-action)))
      (observable-element-children
       'div
       @state
       make-toast-children
       #:after-render toast-after-render
       #:attrs attrs/final))

    ;; close-button : (-> any/c) [text-content/c] -> view?
    ;;   Construct a standardized close button with action, optional aria-label, and root decorators.
    ;;   Optional parameter aria-label defaults to "Close".
    (define/component close-button
      #:root-tag 'button
      #:positional ([action]
                    [aria-label "Close"])
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'role 'button)
              (cons 'data-we-widget "close-button")
              (cons 'class "we-close-button")
              (cons 'aria-label aria-label)
              (cons 'on-click-action action)))
      (html-element-children
       'button
       (Span ""
             #:data-we-widget "close-button-icon"
             #:class "we-close-button-icon"
             #:aria-hidden "true")
       #:attrs attrs/final))

    ;; badge : (or/c string? observable?) [(or/c symbol? observable?)] -> view?
    ;;   Construct a compact inline badge with optional severity level.
    ;;   Optional parameter level defaults to 'info.
    ;;   Accepts global HTML attributes for the root <span> via keyword arguments.
    (define/component badge
      #:root-tag 'span
      #:positional ([value]
                    [level/positional 'info])
      #:component-keywords ([#:level level-kw #f])
      #:root-attrs attrs/final
      (define final-level
        (if (eq? level-kw #f)
            level/positional
            level-kw))
      (define @level
        (observable-or-const final-level))
      (define @normalized-level
        (~> @level normalize-badge-level/internal))
      (define @class
        (~> @normalized-level
            (lambda (level0)
              (string-append "we-badge "
                             (badge-level-class/internal level0)))))
      (define attrs/final
        (list (cons 'data-we-widget "badge")
              (cons 'class @class)))
      (Span value
            #:attrs attrs/final))

    ;; spinner : [(or/c string? observable? false/c)] -> view?
    ;;   Construct a loading spinner with optional label text.
    ;;   Optional parameter label defaults to "Loading...".
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component spinner
      #:root-tag 'div
      #:positional ([label "Loading..."])
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'role 'status)
              (cons 'data-we-widget "spinner")
              (cons 'class "we-spinner")
              (cons 'aria-live "polite")))
      (Div (Span ""
                 #:data-we-widget "spinner-icon"
                 #:class "we-spinner-icon"
                 #:aria-hidden "true")
           (Span label
                 #:data-we-widget "spinner-label"
                 #:class "we-spinner-label")
           #:attrs attrs/final))

    ;; placeholder : [(or/c symbol? observable?)] [(or/c any/c observable?)] -> view?
    ;;   Construct a placeholder/skeleton block with optional shape and width.
    ;;   Optional parameter shape defaults to 'text.
    ;;   Optional parameter width defaults to #f.
    ;;   Accepts global HTML attributes for the root <span> via keyword arguments.
    (define/component placeholder
      #:root-tag 'span
      #:positional ([shape/positional 'text]
                    [width/positional #f])
      #:component-keywords ([#:shape shape-kw #f]
                            [#:width width-kw #f])
      #:root-attrs attrs/final
      (define final-shape
        (if (eq? shape-kw #f) shape/positional shape-kw))
      (define final-width
        (if (eq? width-kw #f) width/positional width-kw))
      (define @shape
        (observable-or-const final-shape))
      (define @width
        (observable-or-const final-width))
      (define @shape-class
        (~> @shape
            (lambda (shape0)
              (case shape0
                [(rect)   "we-placeholder-rect"]
                [(circle) "we-placeholder-circle"]
                [else     "we-placeholder-text"]))))
      (define @class
        (~> @shape-class
            (lambda (shape-class)
              (string-append "we-placeholder " shape-class))))
      (define attrs/final
        (list (cons 'data-we-widget "placeholder")
              (cons 'class @class)
              (cons 'aria-hidden "true")
              (cons 'width @width)))
      (Span ""
            #:attrs attrs/final))

    ;; text : (or/c string? observable?) -> view?
    ;;   Construct a text view from static or observable value.
    ;;   Accepts global HTML attributes for the root <span> via keyword arguments.
    (define/component text
      #:root-tag 'span
      #:positional ([s])
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "text")))
      (Span s
            #:attrs attrs/final))

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

    ;; raw-text-view/internal : text-content/c -> view?
    ;;   Construct an internal raw text child view for primitive mixed content.
    (define (raw-text-view/internal value)
      (view kind/raw-text
            (list (cons 'value value))
            '()))

    ;; content-item->child-view/internal : symbol? any/c -> view?
    ;;   Normalize primitive mixed content item as child view, preserving raw text.
    (define (content-item->child-view/internal who item)
      (cond
        [(view? item) item]
        [(text-content/c item)
         (raw-text-view/internal item)]
        [else
         (error who
                "expected text-like content or child view, got ~e"
                item)]))

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

    ;; cond-clause-active/internal : any/c -> boolean?
    ;;   Treat #f as false and all other values as true for conditional views.
    (define (cond-clause-active/internal v)
      (if (boolean? v)
          v
          (not (eq? v #f))))

    ;; heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with level normalized to 1..6 and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    ;;   Accepts global HTML attributes for the root <h1..h6> via keyword arguments.
    (define/component heading
      #:root-tag 'h1
      #:positional ([level]
                    [content]
                    [align/positional 'left]
                    [spacing/positional 'normal])
      #:component-keywords ([#:align align-kw #f]
                            [#:spacing spacing-kw #f])
      #:root-attrs attrs/final
      (define final-align
        (if (eq? align-kw #f) align/positional align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing/positional spacing-kw))
      (define @level   (observable-or-const level))
      (define @align   (observable-or-const final-align))
      (define @spacing (observable-or-const final-spacing))
      (define @normalized-level
        (~> @level normalize-heading-level/internal))
      (define @normalized-align
        (~> @align normalize-heading-align/internal))
      (define @normalized-spacing
        (~> @spacing normalize-heading-spacing/internal))
      (define @tag
        (~> @normalized-level heading-tag-for-level/internal))
      (define @class
        (obs-combine
         (lambda (level0 align0 spacing0)
           (heading-class-for/internal level0 align0 spacing0))
         @normalized-level
         @normalized-align
         @normalized-spacing))
      (define attrs/final
        (list (cons 'data-we-widget "heading")
              (cons 'class @class)))
      (html-element @tag
                    content
                    #:attrs attrs/final))

    ;; h1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-1 heading view.
    ;;   Accepts global HTML attributes for the root <h1> via keyword arguments.
    (define/component h1
      #:root-tag 'h1
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 1 content)
                     forwarded-args)))

    ;; H1 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-1 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H1 html-element 'h1
      #:content-mode text-or-children)

    ;; H2 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-2 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H2 html-element 'h2
      #:content-mode text-or-children)

    ;; H3 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-3 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H3 html-element 'h3
      #:content-mode text-or-children)

    ;; H4 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-4 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H4 html-element 'h4
      #:content-mode text-or-children)

    ;; H5 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-5 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H5 html-element 'h5
      #:content-mode text-or-children)

    ;; H6 : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML level-6 heading element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element H6 html-element 'h6
      #:content-mode text-or-children)

    ;; P : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML paragraph element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element P html-element 'p
      #:content-mode text-or-children)

    ;; Span : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML span element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Span html-element 'span
      #:content-mode text-or-children)

    ;; Strong : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML strong element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Strong html-element 'strong
      #:content-mode text-or-children)

    ;; Em : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML em element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Em html-element 'em
      #:content-mode text-or-children)

    ;; Code : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML code element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Code html-element 'code
      #:content-mode text-or-children)

    ;; Pre : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML pre element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Pre html-element 'pre
      #:content-mode text-or-children)

    ;; Small : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML small element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Small html-element 'small
      #:content-mode text-or-children)

    ;; B : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML b element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element B html-element 'b
      #:content-mode text-or-children)

    ;; I : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML i element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element I html-element 'i
      #:content-mode text-or-children)

    ;; U : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML u element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element U html-element 'u
      #:content-mode text-or-children)

    ;; S : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML s element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element S html-element 's
      #:content-mode text-or-children)

    ;; Mark : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML mark element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Mark html-element 'mark
      #:content-mode text-or-children)

    ;; Sub : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML sub element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Sub html-element 'sub
      #:content-mode text-or-children)

    ;; Sup : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML sup element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Sup html-element 'sup
      #:content-mode text-or-children)

    ;; Kbd : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML kbd element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Kbd html-element 'kbd
      #:content-mode text-or-children)

    ;; Samp : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML samp element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Samp html-element 'samp
      #:content-mode text-or-children)

    ;; Var : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML var element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Var html-element 'var
      #:content-mode text-or-children)

    ;; Q : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML q element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Q html-element 'q
      #:content-mode text-or-children)

    ;; Cite : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML cite element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Cite html-element 'cite
      #:content-mode text-or-children)

    ;; Dfn : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML dfn element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Dfn html-element 'dfn
      #:content-mode text-or-children)

    ;; Abbr : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML abbr element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Abbr html-element 'abbr
      #:content-mode text-or-children)

    ;; Time : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML time element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Time html-element 'time
      #:content-mode text-or-children)

    ;; Data : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML data element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Data html-element 'data
      #:content-mode text-or-children)

    ;; Del : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML del element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Del html-element 'del
      #:content-mode text-or-children)

    ;; Ins : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML ins element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Ins html-element 'ins
      #:content-mode text-or-children)

    ;; Br : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML br element with generic keyword attributes.
    (define/element Br html-element 'br
      #:required-keywords ()
      #:positional-count 0)

    ;; Wbr : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML wbr element with generic keyword attributes.
    (define/element Wbr html-element 'wbr
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

    ;; A : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML anchor element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element A html-element 'a
      #:content-mode text-or-children)

    ;; Button : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML button element with generic keyword attributes.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Button html-element 'button
      #:content-mode text-or-children)

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

    ;; Label : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML label element with generic keyword attrs.
    ;;   A single text-like positional value preserves the historical text-bearing form.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Label html-element 'label
      #:content-mode text-or-children)

    ;; Ul : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML ul element with children and generic keyword attrs.
    (define/element Ul html-element-children 'ul
      #:required-keywords ()
      #:positional-count any)

    ;; Menu : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML menu element with children and generic keyword attrs.
    (define/element Menu html-element-children 'menu
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

    ;; Dl : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML dl element with children and generic keyword attrs.
    (define/element Dl html-element-children 'dl
      #:required-keywords ()
      #:positional-count any)

    ;; Dt : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML dt element with children and generic keyword attrs.
    (define/element Dt html-element-children 'dt
      #:required-keywords ()
      #:positional-count any)

    ;; Dd : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML dd element with children and generic keyword attrs.
    (define/element Dd html-element-children 'dd
      #:required-keywords ()
      #:positional-count any)

    ;; Table : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML table element with children and generic keyword attrs.
    (define/element Table html-element-children 'table
      #:required-keywords ()
      #:positional-count any)

    ;; Caption : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML caption element with children and generic keyword attrs.
    (define/element Caption html-element-children 'caption
      #:required-keywords ()
      #:positional-count any)

    ;; Thead : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML thead element with children and generic keyword attrs.
    (define/element Thead html-element-children 'thead
      #:required-keywords ()
      #:positional-count any)

    ;; Tbody : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML tbody element with children and generic keyword attrs.
    (define/element Tbody html-element-children 'tbody
      #:required-keywords ()
      #:positional-count any)

    ;; Tfoot : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML tfoot element with children and generic keyword attrs.
    (define/element Tfoot html-element-children 'tfoot
      #:required-keywords ()
      #:positional-count any)

    ;; Tr : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML tr element with children and generic keyword attrs.
    (define/element Tr html-element-children 'tr
      #:required-keywords ()
      #:positional-count any)

    ;; Th : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML th element with children and generic keyword attrs.
    (define/element Th html-element-children 'th
      #:required-keywords ()
      #:positional-count any)

    ;; Td : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML td element with children and generic keyword attrs.
    (define/element Td html-element-children 'td
      #:required-keywords ()
      #:positional-count any)

    ;; Audio : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML audio element with children and generic keyword attrs.
    (define/element Audio html-element-children 'audio
      #:required-keywords ()
      #:positional-count any)

    ;; Video : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML video element with children and generic keyword attrs.
    (define/element Video html-element-children 'video
      #:required-keywords ()
      #:positional-count any)

    ;; Source : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML source element with generic keyword attrs.
    (define/element Source html-element 'source
      #:required-keywords ()
      #:positional-count 0)

    ;; Track : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML track element with generic keyword attrs.
    (define/element Track html-element 'track
      #:required-keywords ()
      #:positional-count 0)

    ;; Canvas : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML canvas element with children and generic keyword attrs.
    (define/element Canvas html-element-children 'canvas
      #:required-keywords ()
      #:positional-count any)

    ;; Iframe : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML iframe element with children and generic keyword attrs.
    (define/element Iframe html-element-children 'iframe
      #:required-keywords ()
      #:positional-count any)

    ;; Embed : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML embed element with generic keyword attrs.
    (define/element Embed html-element 'embed
      #:required-keywords ()
      #:positional-count 0)

    ;; Object : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML object element with children and generic keyword attrs.
    (define/element Object html-element-children 'object
      #:required-keywords ()
      #:positional-count any)

    ;; Input : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML input element with generic keyword attrs.
    (define/element Input html-element 'input
      #:required-keywords ()
      #:positional-count 0)

    ;; Select : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML select element with children and generic keyword attrs.
    (define/element Select html-element-children 'select
      #:required-keywords ()
      #:positional-count any)

    ;; Option : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML option element with generic keyword attrs.
    ;;   A single text-like positional value is accepted directly.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Option html-element 'option
      #:content-mode text-or-children)

    ;; Textarea : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML textarea element with generic keyword attrs.
    (define/element Textarea html-element 'textarea)

    ;; Details : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML details element with children and generic keyword attrs.
    (define/element Details html-element-children 'details
      #:required-keywords ()
      #:positional-count any)

    ;; Dialog : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML dialog element with children and generic keyword attrs.
    (define/element Dialog html-element-children 'dialog
      #:required-keywords ()
      #:positional-count any)

    ;; Summary : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML summary element with children and generic keyword attrs.
    (define/element Summary html-element-children 'summary
      #:required-keywords ()
      #:positional-count any)

    ;; Figure : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML figure element with children and generic keyword attrs.
    (define/element Figure html-element-children 'figure
      #:required-keywords ()
      #:positional-count any)

    ;; Figcaption : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML figcaption element with children and generic keyword attrs.
    (define/element Figcaption html-element-children 'figcaption
      #:required-keywords ()
      #:positional-count any)

    ;; Hgroup : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML hgroup element with children and generic keyword attrs.
    (define/element Hgroup html-element-children 'hgroup
      #:required-keywords ()
      #:positional-count any)

    ;; Address : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML address element with children and generic keyword attrs.
    (define/element Address html-element-children 'address
      #:required-keywords ()
      #:positional-count any)

    ;; Blockquote : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML blockquote element with children and generic keyword attrs.
    (define/element Blockquote html-element-children 'blockquote
      #:required-keywords ()
      #:positional-count any)

    ;; Ruby : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML ruby element with children and generic keyword attrs.
    (define/element Ruby html-element-children 'ruby
      #:required-keywords ()
      #:positional-count any)

    ;; Rt : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML rt element with children and generic keyword attrs.
    (define/element Rt html-element-children 'rt
      #:required-keywords ()
      #:positional-count any)

    ;; Rp : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML rp element with children and generic keyword attrs.
    (define/element Rp html-element-children 'rp
      #:required-keywords ()
      #:positional-count any)

    ;; Bdi : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML bdi element with children and generic keyword attrs.
    (define/element Bdi html-element-children 'bdi
      #:required-keywords ()
      #:positional-count any)

    ;; Bdo : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML bdo element with children and generic keyword attrs.
    (define/element Bdo html-element-children 'bdo
      #:required-keywords ()
      #:positional-count any)

    ;; Progress : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML progress element with children and generic keyword attrs.
    (define/element Progress html-element-children 'progress
      #:required-keywords ()
      #:positional-count any)

    ;; Meter : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML meter element with children and generic keyword attrs.
    (define/element Meter html-element-children 'meter
      #:required-keywords ()
      #:positional-count any)

    ;; Output : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML output element with children and generic keyword attrs.
    (define/element Output html-element-children 'output
      #:required-keywords ()
      #:positional-count any)

    ;; Fieldset : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML fieldset element with children and generic keyword attrs.
    (define/element Fieldset html-element-children 'fieldset
      #:required-keywords ()
      #:positional-count any)

    ;; Legend : any/c ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML legend element with generic keyword attrs.
    ;;   A single text-like positional value is accepted directly.
    ;;   One or more view positional values are rendered as primitive child content.
    (define/element Legend html-element 'legend
      #:content-mode text-or-children)

    ;; Datalist : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML datalist element with children and generic keyword attrs.
    (define/element Datalist html-element-children 'datalist
      #:required-keywords ()
      #:positional-count any)

    ;; Optgroup : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML optgroup element with children and generic keyword attrs.
    (define/element Optgroup html-element-children 'optgroup
      #:required-keywords ()
      #:positional-count any)

    ;; Colgroup : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML colgroup element with children and generic keyword attrs.
    (define/element Colgroup html-element-children 'colgroup
      #:required-keywords ()
      #:positional-count any)

    ;; Col : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML col element with generic keyword attrs.
    (define/element Col html-element 'col
      #:required-keywords ()
      #:positional-count 0)

    ;; Map : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML map element with children and generic keyword attrs.
    (define/element Map html-element-children 'map
      #:required-keywords ()
      #:positional-count any)

    ;; Area : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML area element with generic keyword attrs.
    (define/element Area html-element 'area
      #:required-keywords ()
      #:positional-count 0)

    ;; Script : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML script element with generic keyword attrs.
    (define/element Script html-element 'script
      #:required-keywords ()
      #:positional-count 0)

    ;; Link : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML link element with generic keyword attrs.
    (define/element Link html-element 'link
      #:required-keywords ()
      #:positional-count 0)

    ;; Meta : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML meta element with generic keyword attrs.
    (define/element Meta html-element 'meta
      #:required-keywords ()
      #:positional-count 0)

    ;; Title : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML title element with generic keyword attrs.
    (define/element Title html-element 'title)

    ;; Base : [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML base element with generic keyword attrs.
    ;;   Requires at least one of #:href or #:target.
    (define/element Base html-element 'base
      #:required-keywords ()
      #:required-any-keywords (#:href #:target)
      #:positional-count 0)

    ;; Style : (or/c string? observable?) [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML style element with generic keyword attrs.
    (define/element Style html-element 'style)

    ;; Slot : view? ... [#:attrs any/c] [#:* any/c] -> view?
    ;;   Construct a primitive HTML slot element with children and generic keyword attrs.
    (define/element Slot html-element-children 'slot
      #:required-keywords ()
      #:positional-count any)

    ;; h2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-2 heading view.
    ;;   Accepts global HTML attributes for the root <h2> via keyword arguments.
    (define/component h2
      #:root-tag 'h2
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 2 content)
                     forwarded-args)))

    ;; h3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-3 heading view.
    ;;   Accepts global HTML attributes for the root <h3> via keyword arguments.
    (define/component h3
      #:root-tag 'h3
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 3 content)
                     forwarded-args)))

    ;; h4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-4 heading view.
    ;;   Accepts global HTML attributes for the root <h4> via keyword arguments.
    (define/component h4
      #:root-tag 'h4
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 4 content)
                     forwarded-args)))

    ;; h5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-5 heading view.
    ;;   Accepts global HTML attributes for the root <h5> via keyword arguments.
    (define/component h5
      #:root-tag 'h5
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 5 content)
                     forwarded-args)))

    ;; h6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic level-6 heading view.
    ;;   Accepts global HTML attributes for the root <h6> via keyword arguments.
    (define/component h6
      #:root-tag 'h6
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply heading
             (append (list 6 content)
                     forwarded-args)))

    ;; display-heading : (or/c number? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with display style, level normalized to 1..6, and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    (define/component display-heading
      #:root-tag 'h1
      #:positional ([level]
                    [content]
                    [align/positional 'left]
                    [spacing/positional 'normal])
      #:component-keywords ([#:align align-kw #f]
                            [#:spacing spacing-kw #f])
      #:root-attrs attrs/final
      (define final-align
        (if (eq? align-kw #f) align/positional align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing/positional spacing-kw))
      (define @level   (observable-or-const level))
      (define @align   (observable-or-const final-align))
      (define @spacing (observable-or-const final-spacing))
      (define @normalized-level
        (~> @level normalize-heading-level/internal))
      (define @normalized-align
        (~> @align normalize-heading-align/internal))
      (define @normalized-spacing
        (~> @spacing normalize-heading-spacing/internal))
      (define @tag
        (~> @normalized-level heading-tag-for-level/internal))
      (define @class
        (obs-combine
         (lambda (level0 align0 spacing0)
           (string-append "we-display-heading we-display-heading-"
                          (number->string level0)
                          " we-display-heading-align-"
                          (symbol->string align0)
                          " we-display-heading-space-"
                          (symbol->string spacing0)))
         @normalized-level
         @normalized-align
         @normalized-spacing))
      (define attrs/final
        (list (cons 'data-we-widget "display-heading")
              (cons 'class @class)))
      (html-element @tag
                    content
                    #:attrs attrs/final))

    ;; display-1 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-1 heading view.
    ;;   Accepts global HTML attributes for the root <h1> via keyword arguments.
    (define/component display-1
      #:root-tag 'h1
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 1 content)
                     forwarded-args)))

    ;; display-2 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-2 heading view.
    ;;   Accepts global HTML attributes for the root <h2> via keyword arguments.
    (define/component display-2
      #:root-tag 'h2
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 2 content)
                     forwarded-args)))

    ;; display-3 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-3 heading view.
    ;;   Accepts global HTML attributes for the root <h3> via keyword arguments.
    (define/component display-3
      #:root-tag 'h3
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 3 content)
                     forwarded-args)))

    ;; display-4 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-4 heading view.
    ;;   Accepts global HTML attributes for the root <h4> via keyword arguments.
    (define/component display-4
      #:root-tag 'h4
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 4 content)
                     forwarded-args)))

    ;; display-5 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-5 heading view.
    ;;   Accepts global HTML attributes for the root <h5> via keyword arguments.
    (define/component display-5
      #:root-tag 'h5
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 5 content)
                     forwarded-args)))

    ;; display-6 : (or/c string? observable?) -> view?
    ;;   Construct a semantic display level-6 heading view.
    ;;   Accepts global HTML attributes for the root <h6> via keyword arguments.
    (define/component display-6
      #:root-tag 'h6
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final '())
      (apply display-heading
             (append (list 6 content)
                     forwarded-args)))

    ;; heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    ;;   Accepts global HTML attributes for the root <h1..h6> via keyword arguments.
    (define/component heading-with-subtitle
      #:root-tag 'h1
      #:positional ([level]
                    [content]
                    [subtitle]
                    [align/positional 'left]
                    [spacing/positional 'normal])
      #:component-keywords ([#:align align-kw #f]
                            [#:spacing spacing-kw #f])
      #:root-attrs attrs/final
      (define final-align
        (if (eq? align-kw #f) align/positional align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing/positional spacing-kw))
      (define @level   (observable-or-const level))
      (define @align   (observable-or-const final-align))
      (define @spacing (observable-or-const final-spacing))
      (define @normalized-level
        (~> @level normalize-heading-level/internal))
      (define @normalized-align
        (~> @align normalize-heading-align/internal))
      (define @normalized-spacing
        (~> @spacing normalize-heading-spacing/internal))
      (define @tag
        (~> @normalized-level heading-tag-for-level/internal))
      (define @class
        (obs-combine
         (lambda (level0 align0 spacing0)
           (string-append "we-heading-with-subtitle we-heading-with-subtitle-"
                          (number->string level0)
                          " we-heading-with-subtitle-align-"
                          (symbol->string align0)
                          " we-heading-with-subtitle-space-"
                          (symbol->string spacing0)))
         @normalized-level
         @normalized-align
         @normalized-spacing))
      (define attrs/final
        (list (cons 'data-we-widget "heading-with-subtitle")
              (cons 'class @class)))
      (html-element-children
       @tag
       (Span content
             #:data-we-widget "heading-title"
             #:class "we-heading-title")
       (Small subtitle
              #:data-we-widget "heading-subtitle"
              #:class "we-heading-subtitle")
       #:attrs attrs/final))

    ;; display-heading-with-subtitle : (or/c number? observable?) (or/c string? observable?) (or/c string? observable?) [symbol?] [symbol?] -> view?
    ;;   Construct a semantic display heading view with muted subtitle text and optional align/spacing style variants.
    ;;   Optional parameter align defaults to 'left.
    ;;   Optional parameter spacing defaults to 'normal.
    ;;   Accepts global HTML attributes for the root <h1..h6> via keyword arguments.
    (define/component display-heading-with-subtitle
      #:root-tag 'h1
      #:positional ([level]
                    [content]
                    [subtitle]
                    [align/positional 'left]
                    [spacing/positional 'normal])
      #:component-keywords ([#:align align-kw #f]
                            [#:spacing spacing-kw #f])
      #:root-attrs attrs/final
      (define final-align
        (if (eq? align-kw #f) align/positional align-kw))
      (define final-spacing
        (if (eq? spacing-kw #f) spacing/positional spacing-kw))
      (define @level   (observable-or-const level))
      (define @align   (observable-or-const final-align))
      (define @spacing (observable-or-const final-spacing))
      (define @normalized-level
        (~> @level normalize-heading-level/internal))
      (define @normalized-align
        (~> @align normalize-heading-align/internal))
      (define @normalized-spacing
        (~> @spacing normalize-heading-spacing/internal))
      (define @tag
        (~> @normalized-level heading-tag-for-level/internal))
      (define @class
        (obs-combine
         (lambda (level0 align0 spacing0)
           (string-append "we-display-heading-with-subtitle we-display-heading-with-subtitle-"
                          (number->string level0)
                          " we-display-heading-with-subtitle-align-"
                          (symbol->string align0)
                          " we-display-heading-with-subtitle-space-"
                          (symbol->string spacing0)))
         @normalized-level
         @normalized-align
         @normalized-spacing))
      (define attrs/final
        (list (cons 'data-we-widget "display-heading-with-subtitle")
              (cons 'class @class)))
      (html-element-children
       @tag
       (Span content
             #:data-we-widget "heading-title"
             #:class "we-heading-title")
       (Small subtitle
              #:data-we-widget "heading-subtitle"
              #:class "we-heading-subtitle")
       #:attrs attrs/final))

    ;; lead : (or/c string? observable?) -> view?
    ;;   Construct a lead paragraph view from static or observable value.
    ;;   Accepts global HTML attributes for the root <p> via keyword arguments.
    (define/component lead
      #:root-tag 'p
      #:positional ([content])
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "lead")
              (cons 'class "we-lead")))
      (P content
         #:attrs attrs/final))

    ;; blockquote : text-content/c [(or/c #f text-content/c)] -> view?
    ;;   Construct a semantic blockquote with optional attribution footer and alignment.
    ;;   Optional parameter attribution defaults to #f.
    ;;   Accepts global HTML attributes for the root <figure> via keyword arguments.
    (define/component blockquote
      #:root-tag 'figure
      #:positional ([content]
                    [attribution #f])
      #:component-keywords ([#:align align 'left])
      #:root-attrs root-attrs
      (define final-align
        (normalize-blockquote-align align 'blockquote))
      (define align-class
        (case final-align
          [(left)   #f]
          [(center) "we-blockquote-align-center"]
          [(right)  "we-blockquote-align-right"]
          [else     #f]))
      (define root-class
        (if align-class
            (string-append "we-blockquote " align-class)
            "we-blockquote"))
      (define quote-text-node
        (P content
           #:data-we-widget "blockquote-text"
           #:class "we-blockquote-text"))
      (define quote-node
        (Blockquote
         #:data-we-widget "blockquote-quote"
         #:class "we-blockquote-quote"
         quote-text-node))
      (define has-attribution-static?
        (and (not (obs? attribution))
             (not (eq? attribution #f))))
      (define attribution-hidden
        (if (obs? attribution)
            (~> attribution (lambda (v) (eq? v #f)))
            #f))
      (define attribution-value
        (if (obs? attribution)
            (~> attribution (lambda (v) (if (eq? v #f) "" v)))
            attribution))
      (define children
        (append
         (list quote-node)
         (if (or (obs? attribution) has-attribution-static?)
             (list
              (html-element
               'figcaption
               attribution-value
               #:attrs (list (cons 'data-we-widget "blockquote-attrib")
                             (cons 'class "we-blockquote-attrib")
                             (cons 'hidden attribution-hidden))))
             '())))
      (define root-attrs
        (list (cons 'data-we-widget "blockquote")
              (cons 'class root-class)))
      (apply Figure
             (append children
                     (list '#:attrs root-attrs))))


    ;; button : content/c (-> any/c) [(or/c #f content/c)] [(or/c #f content/c)] -> view?
    ;;   Construct a button view with optional leading/trailing icon labels.
    ;;   Label and icons may be plain text or child views.
    ;;   The rendered root is a primitive HTML <button>.
    ;;   For static slot content this composes the primitive Button directly.
    (define/component button
      #:root-tag 'button
      #:positional ([label]
                    [action]
                    [leading-icon #f]
                    [trailing-icon #f])
      #:root-attrs attrs/final
      (define (button-inline-label x data-we-widget class)
        (if (view? x)
            (html-element-children
             'span
             x
             #:attrs
             (list (cons 'data-we-widget data-we-widget)
                   (cons 'class class)))
            (Span x
                  #:data-we-widget data-we-widget
                  #:class class)))
      (define (button-children/static label0 leading-icon0 trailing-icon0)
        (append
         (if (eq? leading-icon0 #f)
             '()
             (list
              (button-inline-label
               leading-icon0
               "button-icon"
               "we-button-icon we-button-icon-leading")))
         (list
          (button-inline-label
           label0
           "button-label"
           "we-button-label"))
         (if (eq? trailing-icon0 #f)
             '()
             (list
              (button-inline-label
               trailing-icon0
               "button-icon"
               "we-button-icon we-button-icon-trailing")))))

      (define @label
        (observable-or-const label))
      (define @leading-icon
        (observable-or-const leading-icon))
      (define @trailing-icon
        (observable-or-const trailing-icon))
      (define @state
        (obs-combine list @label @leading-icon @trailing-icon))

      (define (make-button-children state)
        (define label0
          (car state))
        (define leading-icon0
          (cadr state))
        (define trailing-icon0
          (caddr state))
        (append
         (if (eq? leading-icon0 #f)
             '()
             (list
              (button-inline-label
               leading-icon0
               "button-icon"
               "we-button-icon we-button-icon-leading")))
         (list
          (button-inline-label
           label0
           "button-label"
           "we-button-label"))
         (if (eq? trailing-icon0 #f)
             '()
             (list
              (button-inline-label
               trailing-icon0
               "button-icon"
               "we-button-icon we-button-icon-trailing")))))

      (define attrs/final
        (list (cons 'data-we-widget "button")
              (cons 'class "we-button")
              (cons 'on-click-action action)))

      (if (and (not (obs? label))
               (not (obs? leading-icon))
               (not (obs? trailing-icon)))
          (apply Button
                 (append (button-children/static label leading-icon trailing-icon)
                         (list #:attrs attrs/final)))
          (observable-element-children
           'button
           @state
           make-button-children
           #:attrs attrs/final)))


    ;; ;; button : (or/c string? observable?) (-> any/c) [any/c] [any/c] -> view?
    ;; ;;   Construct a button view with optional leading/trailing icon labels.
    ;; ;;   Optional parameter leading-icon defaults to #f.
    ;; ;;   Optional parameter trailing-icon defaults to #f.
    ;; (define/component button
    ;;   #:root-tag 'button
    ;;   #:positional ([label]
    ;;                 [action]
    ;;                 [leading-icon #f]
    ;;                 [trailing-icon #f])
    ;;   #:root-attrs attrs/final
    ;;   (define @label
    ;;     (observable-or-const label))
    ;;   (define @leading-icon
    ;;     (observable-or-const leading-icon))
    ;;   (define @trailing-icon
    ;;     (observable-or-const trailing-icon))
    ;;   (define @state
    ;;     (obs-combine list @label @leading-icon @trailing-icon))
    ;;   (define (make-button-children state)
    ;;     (define label0
    ;;       (car state))
    ;;     (define leading-icon0
    ;;       (cadr state))
    ;;     (define trailing-icon0
    ;;       (caddr state))
    ;;     (append
    ;;      (if (eq? leading-icon0 #f)
    ;;          '()
    ;;          (list (Span leading-icon0
    ;;                      #:data-we-widget "button-icon"
    ;;                      #:class "we-button-icon we-button-icon-leading")))
    ;;      (list (Span label0
    ;;                  #:data-we-widget "button-label"
    ;;                  #:class "we-button-label"))
    ;;      (if (eq? trailing-icon0 #f)
    ;;          '()
    ;;          (list (Span trailing-icon0
    ;;                      #:data-we-widget "button-icon"
    ;;                      #:class "we-button-icon we-button-icon-trailing")))))
    ;;   (define attrs/final
    ;;     (list (cons 'data-we-widget "button")
    ;;           (cons 'class "we-button")
    ;;           (cons 'on-click-action action)))
    ;;   (observable-element-children
    ;;    'button
    ;;    @state
    ;;    make-button-children
    ;;    #:attrs attrs/final))

    ;; link : (or/c string? observable?) (or/c string? observable?) [boolean?] [any/c] -> view?
    ;;   Construct a link view with href and optional download/target attributes.
    ;;   Optional parameter download? defaults to #f.
    ;;   Optional parameter target defaults to #f.
    ;;   Accepts global HTML attributes for the root <a> via keyword arguments.
    (define/component link
      #:root-tag 'a
      #:positional ([label]
                    [href]
                    [download?/positional #f]
                    [target/positional #f])
      #:component-keywords ([#:download? download?-kw #f]
                            [#:target target-kw #f])
      #:root-attrs attrs/final
      (define final-download?
        (if (eq? download?-kw #f) download?/positional download?-kw))
      (define final-target
        (if (eq? target-kw #f) target/positional target-kw))
      (define @download?
        (observable-or-const final-download?))
      (define @target
        (observable-or-const final-target))
      (define attrs/final
        (list (cons 'role 'link)
              (cons 'data-we-widget "link")
              (cons 'class "we-link")
              (cons 'href href)
              (cons 'download (~> @download?
                                  (lambda (download?)
                                    (if download? "download" #f))))
              (cons 'target @target)))
      (A label
         #:attrs attrs/final))

    ;; button-group : view? ... -> view?
    ;;   Construct a grouped button container view.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component button-group
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'role 'group)
              (cons 'data-we-widget "button-group")
              (cons 'class "we-button-group")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; toggle-button-group : symbol? list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct an exclusive/non-exclusive toggle button group.
    (define/component toggle-button-group
      #:root-tag 'div
      #:positional ([mode]
                    [choices]
                    [selected]
                    [action])
      #:root-attrs attrs/final
      (define mode/radio
        'radio)
      (define mode/checkbox
        'checkbox)
      (define @mode
        (observable-or-const mode))
      (define @choices
        (observable-or-const choices))
      (define @selected
        (observable-or-const selected))
      (define @structure
        (obs-combine list @mode @choices))
      (define (selected?/selection mode0 selection item-id)
        (case mode0
          [(checkbox)
           (and (list? selection)
                (member item-id selection))]
          [else
           (equal? selection item-id)]))
      (define (toggle-class mode0 selection item-id)
        (if (selected?/selection mode0 selection item-id)
            "we-button is-active"
            "we-button"))
      (define (next-selection mode0 selection item-id)
        (case mode0
          [(checkbox)
           (cond
             [(not (list? selection))
              (list item-id)]
             [(member item-id selection)
              (let loop ([xs selection])
                (cond
                  [(null? xs)
                   '()]
                  [(equal? (car xs) item-id)
                   (loop (cdr xs))]
                  [else
                   (cons (car xs)
                         (loop (cdr xs)))]))]
             [else
              (append selection (list item-id))])]
          [else
           item-id]))
      (define (make-toggle-children state)
        (define mode0
          (car state))
        (define choices0
          (cadr state))
        (map (lambda (choice)
               (define choice-id
                 (list-ref choice 0))
               (define choice-label
                 (list-ref choice 1))
               (define @choice-class
                 (~> @selected
                     (lambda (selected0)
                       (toggle-class mode0 selected0 choice-id))))
               (Button choice-label
                       #:attrs (list (cons 'role 'button)
                                     (cons 'data-we-widget "button")
                                     (cons 'class @choice-class)
                                     (cons 'on-click-action
                                           (lambda ()
                                             (action (next-selection (obs-peek @mode)
                                                                     (obs-peek @selected)
                                                                     choice-id)))))))
             choices0))
      (define attrs/final
        (list (cons 'role 'group)
              (cons 'data-we-widget "toggle-button-group")
              (cons 'class "we-toggle-button-group")
              (cons 'mode (~> @mode
                              (lambda (mode0)
                                (if (eq? mode0 mode/checkbox)
                                    mode/checkbox
                                    mode/radio))))))
      (observable-element-children
       'div
       @structure
       make-toggle-children
       #:attrs attrs/final))

    ;; button-toolbar : view? ... -> view?
    ;;   Construct a horizontal toolbar of grouped button controls.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component button-toolbar
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'role 'toolbar)
              (cons 'data-we-widget "button-toolbar")
              (cons 'class "we-button-toolbar")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; toolbar : view? ... -> view?
    ;;   Construct a generic horizontal toolbar container.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component toolbar
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "toolbar")
              (cons 'class "we-toolbar")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; toolbar-group : view? ... -> view?
    ;;   Construct a grouped toolbar section container.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component toolbar-group
      #:root-tag 'div
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'data-we-widget "toolbar-group")
              (cons 'class "we-toolbar-group")))
      (apply Div
             (append children
                     (list #:attrs attrs/final))))

    ;; input : (or/c string? observable?) (-> any/c any/c) [(or/c (-> any/c) false/c)] [list?] -> view?
    ;;   Construct an input view with current value, change action, optional Enter action, and attrs.
    ;;   Optional parameter on-enter defaults to #f.
    ;;   Optional parameter attrs defaults to '().
    ;;   Accepts global HTML attributes for the root <input> via keyword arguments.
    (define/component input
      #:root-tag 'input
      #:positional ([value]
                    [action]
                    [on-enter/positional #f]
                    [input-attrs/positional '()])
      #:component-keywords ([#:on-enter on-enter-kw #f]
                            [#:input-attrs input-attrs-kw #f])
      #:root-attrs attrs/final
      (define final-on-enter
        (if (eq? on-enter-kw #f) on-enter/positional on-enter-kw))
      (define final-input-attrs
        (if (eq? input-attrs-kw #f) input-attrs/positional input-attrs-kw))
      (define attrs/final
        (append (if (list? final-input-attrs) final-input-attrs '())
                (list (cons 'value value)
                      (cons 'data-we-widget "input")
                      (cons 'class "we-input")
                      (cons 'on-enter-action final-on-enter)
                      (cons 'on-change-action
                            (lambda (new-value)
                              (action new-value))))))
      (Input #:attrs attrs/final))

    ;; textarea : (or/c string? observable?) (-> any/c any/c) [number?] [list?] -> view?
    ;;   Construct a textarea view with current value, change action, optional rows, and attrs.
    ;;   Optional parameter rows defaults to 3.
    ;;   Optional parameter attrs defaults to '().
    ;;   Accepts global HTML attributes for the root <textarea> via keyword arguments.
    (define/component textarea
      #:root-tag 'textarea
      #:positional ([value]
                    [action]
                    [rows/positional 3]
                    [textarea-attrs/positional '()])
      #:component-keywords ([#:rows rows-kw #f]
                            [#:textarea-attrs textarea-attrs-kw #f])
      #:root-attrs attrs/final
      (define final-rows
        (if (eq? rows-kw #f) rows/positional rows-kw))
      (define final-textarea-attrs
        (if (eq? textarea-attrs-kw #f) textarea-attrs/positional textarea-attrs-kw))
      (define attrs/final
        (append (if (list? final-textarea-attrs) final-textarea-attrs '())
                (list (cons 'value value)
                      (cons 'rows final-rows)
                      (cons 'data-we-widget "textarea")
                      (cons 'class "we-textarea")
                      (cons 'on-change-action
                            (lambda (new-value)
                              (action new-value))))))
      (Textarea value
                #:attrs attrs/final))

    ;; checkbox : (or/c boolean? observable?) (-> any/c any/c) -> view?
    ;;   Construct a checkbox view with current state and toggle action.
    ;;   Accepts global HTML attributes for the root <input> via keyword arguments.
    (define/component checkbox
      #:root-tag 'input
      #:positional ([value]
                    [action])
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'type "checkbox")
              (cons 'checked value)
              (cons 'data-we-widget "checkbox")
              (cons 'class "we-checkbox")
              (cons 'on-change-action
                    (lambda (new-checked)
                      (action (not (not new-checked)))))))
      (Input #:attrs attrs/final))

    ;; choice : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a choice view with options, selected value, and action.
    ;;   Accepts global HTML attributes for the root <select> via keyword arguments.
    (define/component choice
      #:root-tag 'select
      #:positional ([choices]
                    [selected]
                    [action])
      #:root-attrs attrs/final
      (define (choice-entry-id/internal entry)
        (cond
          [(and (list? entry)
                (pair? entry)
                (pair? (cdr entry)))
           (car entry)]
          [(pair? entry)
           (car entry)]
          [else
           entry]))
      (define (choice-entry-label/internal entry)
        (cond
          [(and (list? entry)
                (pair? entry)
                (pair? (cdr entry)))
           (cadr entry)]
          [(pair? entry)
           (cdr entry)]
          [else
           entry]))
      (define option-pairs
        (map (lambda (row)
               (cons (choice-entry-id/internal row)
                     (choice-entry-label/internal row)))
             choices))
      (define choices/encoded
        (map (lambda (entry)
               (format "~a" (car entry)))
             option-pairs))
      (define option-pairs/encoded
        (map (lambda (entry)
               (cons (format "~a" (car entry))
                     (format "~a" (cdr entry))))
             option-pairs))
      (define (decode-selection selected0)
        (define selected-text
          (if (string? selected0)
              selected0
              (format "~a" selected0)))
        (let loop ([remaining option-pairs])
          (cond
            [(null? remaining)
             selected0]
            [(string=? (format "~a" (caar remaining))
                       selected-text)
             (caar remaining)]
            [else
             (loop (cdr remaining))])))
      (define attrs/final
        (list (cons 'choices choices/encoded)
              (cons 'option-pairs option-pairs/encoded)
              (cons 'data-we-widget "choice")
              (cons 'class "we-choice")
              (cons 'selected selected)
              (cons 'on-change-action
                    (lambda (new-selected)
                      (action (decode-selection new-selected))))))
      (Select #:attrs attrs/final))

    ;; slider : (or/c number? observable?) (-> any/c any/c) [number?] [number?] -> view?
    ;;   Construct a slider with value, action, and optional min/max bounds.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    ;;   Accepts global HTML attributes for the root <input> via keyword arguments.
    (define/component slider
      #:root-tag 'input
      #:positional ([value]
                    [action]
                    [min/positional 0]
                    [max/positional 100])
      #:component-keywords ([#:min min-kw #f]
                            [#:max max-kw #f])
      #:root-attrs attrs/final
      (define final-min
        (if (eq? min-kw #f) min/positional min-kw))
      (define final-max
        (if (eq? max-kw #f) max/positional max-kw))
      (define (decode-slider-value v0)
        (cond
          [(number? v0) v0]
          [(string? v0)
           (define n (string->number v0))
           (if n n v0)]
          [else v0]))
      (define attrs/final
        (list (cons 'min final-min)
              (cons 'max final-max)
              (cons 'data-we-widget "slider")
              (cons 'class "we-slider")
              (cons 'value value)
              (cons 'on-change-action
                    (lambda (new-value)
                      (action (decode-slider-value new-value))))))
      (Input #:type "range"
             #:attrs attrs/final))

    ;; progress : (or/c number? observable?) [number?] [number?] [(or/c symbol? observable?)] -> view?
    ;;   Construct a progress display with optional min/max bounds and variant.
    ;;   Optional parameter min defaults to 0.
    ;;   Optional parameter max defaults to 100.
    ;;   Optional parameter variant defaults to 'info.
    ;;   Accepts global HTML attributes for the root <progress> via keyword arguments.
    (define/component progress
      #:root-tag 'progress
      #:positional ([value]
                    [min/positional 0]
                    [max/positional 100]
                    [variant/positional 'info])
      #:component-keywords ([#:min min-kw #f]
                            [#:max max-kw #f]
                            [#:variant variant-kw #f])
      #:root-attrs attrs/final
      (define final-min
        (if (eq? min-kw #f) min/positional min-kw))
      (define final-max
        (if (eq? max-kw #f) max/positional max-kw))
      (define final-variant
        (if (eq? variant-kw #f) variant/positional variant-kw))
      (define @variant
        (observable-or-const final-variant))
      (define @normalized-variant
        (~> @variant normalize-badge-level/internal))
      (define @class
        (~> @normalized-variant
            (lambda (variant0)
              (string-append "we-progress "
                             (progress-level-class/internal variant0)))))
      (define @max (observable-or-const final-max))
      (define @min (observable-or-const final-min))      
      (define attrs/final
        (list (cons 'data-we-widget "progress")
              (cons 'class @class)
              (cons 'min @min)
              (cons 'max @max)
              (cons 'value value)))
      (Progress #:attrs attrs/final))

    ;; pagination : (or/c number? observable?) (or/c number? observable?) (-> any/c any/c) -> view?
    ;;   Construct a pagination control for page-count, current page, and page-change action.
    ;;   Accepts global HTML attributes for the root <nav> via keyword arguments.
    (define/component pagination
      #:root-tag 'nav
      #:positional ([page-count]
                    [current-page]
                    [action])
      #:root-attrs attrs/final
      (define @page-count
        (observable-or-const page-count))
      (define @current-page
        (observable-or-const current-page))
      (define @state
        (obs-combine list @page-count @current-page))
      (define (make-page-button label target-page disabled? current?)
        (define button-attrs
          (list (cons 'role 'button)
                (cons 'data-we-widget "page-button")
                (cons 'aria-current (if current? "page" "false"))
                (cons 'aria-disabled (if disabled? "true" "false"))
                (cons 'class (cond
                               [disabled? "we-page-btn is-disabled"]
                               [current?  "we-page-btn is-current"]
                               [else      "we-page-btn"]))))
        (define button-attrs/final
          (if disabled?
              button-attrs
              (append button-attrs
                      (list (cons 'on-click-action
                                  (lambda ()
                                    (action target-page)))))))
        (Button label
                #:attrs button-attrs/final))
      (define (make-page-children state)
        (define page-count0
          (normalize-page-count/internal (car state)))
        (define current-page0
          (clamp-current-page/internal (cadr state) page-count0))
        (define first-disabled? (<= current-page0 1))
        (define prev-disabled? (<= current-page0 1))
        (define next-disabled? (>= current-page0 page-count0))
        (define last-disabled? (>= current-page0 page-count0))
        (define page-items
          (pagination-visible-pages/internal page-count0 current-page0))
        (define page-buttons
          (map (lambda (item)
                 (if (eq? item 'ellipsis)
                     (Span "..."
                           #:data-we-widget "page-ellipsis"
                           #:class "we-page-ellipsis"
                           #:aria-hidden "true")
                     (make-page-button (number->string item)
                                       item
                                       #f
                                       (= item current-page0))))
               page-items))
        (append (list (make-page-button "First" 1 first-disabled? #f)
                      (make-page-button "Prev" (max 1 (- current-page0 1)) prev-disabled? #f))
                page-buttons
                (list (make-page-button "Next" (min page-count0 (+ current-page0 1)) next-disabled? #f)
                      (make-page-button "Last" page-count0 last-disabled? #f))))
      (define attrs/final
        (list (cons 'role 'navigation)
              (cons 'data-we-widget "pagination")
              (cons 'class "we-pagination")))
      (observable-element-children 'nav
                                   @state
                                   make-page-children
                                   #:attrs attrs/final))

    ;; breadcrumb : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a breadcrumb control for entries, current id, and navigation action.
    ;;   Accepts global HTML attributes for the root <nav> via keyword arguments.
    (define/component breadcrumb
      #:root-tag 'nav
      #:positional ([entries]
                    [current]
                    [action])
      #:root-attrs attrs/final
      (define @entries
        (observable-or-const entries))
      (define @current
        (observable-or-const current))
      (define @state
        (obs-combine list @entries @current))
      (define (make-item-view item-id item-label current?)
        (if current?
            (Span item-label
                  #:data-we-widget "breadcrumb-item"
                  #:class "we-breadcrumb-item is-current"
                  #:aria-current "page")
            (Button item-label
                    #:attrs
                    (list (cons 'role 'button)
                          (cons 'data-we-widget "breadcrumb-item")
                          (cons 'class "we-breadcrumb-item")
                          (cons 'on-click-action
                                (lambda ()
                                  (action item-id)))))))
      (define (make-breadcrumb-children state)
        (define entries0
          (ensure-list/internal (car state) 'breadcrumb "entries"))
        (define current0
          (cadr state))
        (let loop ([remaining entries0]
                   [acc '()])
          (cond
            [(null? remaining)
             (reverse acc)]
            [else
             (define entry (car remaining))
             (define item-id (breadcrumb-id/internal entry))
             (define item-label (breadcrumb-label/internal entry))
             (define current? (equal? item-id current0))
             (define next-acc
               (cons (make-item-view item-id item-label current?) acc))
             (if (null? (cdr remaining))
                 (loop (cdr remaining) next-acc)
                 (loop (cdr remaining)
                       (cons (Span "/"
                                   #:data-we-widget "breadcrumb-sep"
                                   #:class "we-breadcrumb-sep"
                                   #:aria-hidden "true")
                             next-acc)))])))
      (define attrs/final
        (list (cons 'role 'navigation)
              (cons 'data-we-widget "breadcrumb")
              (cons 'class "we-breadcrumb")))
      (observable-element-children 'nav
                                   @state
                                   make-breadcrumb-children
                                   #:attrs attrs/final))

    ;; list-group : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a selectable list-group from entries, current id, and selection action.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component list-group
      #:root-tag 'div
      #:positional ([entries]
                    [current]
                    [action])
      #:root-attrs attrs/final
      (define @entries
        (observable-or-const entries))
      (define @current
        (observable-or-const current))
      (define @state
        (obs-combine list @entries @current))
      (define (make-list-group-children state)
        (define entries0
          (ensure-list/internal (car state) 'list-group "entries"))
        (define current0
          (cadr state))
        (map (lambda (entry)
               (define item-id
                 (list-group-id/internal entry))
               (define item-label
                 (list-group-label/internal entry))
               (define current?
                 (equal? item-id current0))
               (define attrs0
                 (list (cons 'role 'listitem)
                       (cons 'data-we-widget "list-group-item")
                       (cons 'class (if current?
                                        "we-list-group-item is-current"
                                        "we-list-group-item"))
                       (cons 'aria-current (if current? "true" "false"))))
               (define attrs/final0
                 ; Also enable `on-click-action` on the selected element.
                 ; This allows users to make selections toggable.
                 (append attrs0
                         (list (cons 'on-click-action
                                     (lambda ()
                                       (action item-id))))))
               (Button item-label
                       #:attrs attrs/final0))
             entries0))
      (define attrs/final
        (list (cons 'role 'list)
              (cons 'data-we-widget "list-group")
              (cons 'class "we-list-group")))
      (observable-element-children 'div
                                   @state
                                   make-list-group-children
                                   #:attrs attrs/final))

    ;; if-view : (or/c any/c observable?) view? view? -> view?
    ;;   Construct a conditional view that selects then-view or else-view.
    (define (if-view cond-value then-view else-view)
      (define @cond
        (observable-or-const cond-value))
      (observable-element-children
       'div
       @cond
       (lambda (cond0)
         (list (if (cond-clause-active/internal cond0)
                   then-view
                   else-view)))
       #:attrs (list (cons 'data-we-widget "if-view")
                     (cons 'class "we-if-view"))))

    ;; cond-view : (listof (cons (or/c any/c observable?) view?)) view? -> view?
    ;;   Construct a multi-branch conditional view with explicit else-view.
    (define (cond-view clauses else-view)
      (define clause-tests
        (map car clauses))
      (define clause-views
        (map cdr clauses))
      (define test-observables
        (map observable-or-const clause-tests))
      (define @test-values
        (if (null? test-observables)
            (obs '())
            (apply obs-combine list test-observables)))
      (define (choose-view test-values)
        (let loop ([tests test-values]
                   [views clause-views])
          (cond
            [(or (null? tests) (null? views))
             else-view]
            [(cond-clause-active/internal (car tests))
             (car views)]
            [else
             (loop (cdr tests) (cdr views))])))
      (observable-element-children
       'div
       @test-values
       (lambda (test-values)
         (list (choose-view test-values)))
       #:attrs (list (cons 'data-we-widget "cond-view")
                     (cons 'class "we-cond-view"))))

    ;; case-view : (or/c any/c observable?) (listof (cons list? view?)) view? -> view?
    ;;   Construct an equality-based branch view with explicit else-view.
    (define (case-view value clauses else-view)
      (define @value
        (observable-or-const value))
      (define (choose-view value0)
        (let loop ([remaining clauses])
          (cond
            [(null? remaining)
             else-view]
            [else
             (define clause (car remaining))
             (define literals
               (ensure-list/internal (car clause) 'case-view "clause literals"))
             (if (member value0 literals)
                 (cdr clause)
                 (loop (cdr remaining)))])))
      (observable-element-children
       'div
       @value
       (lambda (value0)
         (list (choose-view value0)))
       #:attrs (list (cons 'data-we-widget "case-view")
                     (cons 'class "we-case-view"))))

    ;; tab-panel : (or/c any/c observable?) (listof (cons any/c view?)) [any/c] -> view?
    ;;   Construct a selected-tab branch view keyed by tab id, with optional style variants.
    ;;   Optional parameter variants defaults to 'default.
    (define/component tab-panel
      #:root-tag 'tab-panel
      #:positional ([selected]
                    [tabs]
                    [variants 'default])
      #:component-keywords ([#:variants variants-kw #f])
      #:root-attrs attrs/final
      (define final-variants
        (if (eq? variants-kw #f) variants variants-kw))
      (define @selected
        (observable-or-const selected))
      (define tabs/raw
        (ensure-list/internal tabs 'tab-panel "tabs"))
      (define tabs/normalized
        (map normalize-tab-entry/internal tabs/raw))
      (define panel-id
        (next-tab-panel-id/internal))
      (define enabled-tab-ids
        (map car
             (filter (lambda (tab-entry)
                       (not (list-ref tab-entry 2)))
                     tabs/normalized)))
      (define tab-variants
        (normalize-tab-variants/internal final-variants))
      (define tab-variant-class
        (tab-variant-class/internal tab-variants))
      (define tab-panel-class
        (string-append
         "we-tab-panel"
         (if (string=? tab-variant-class "")
             ""
             (string-append " " tab-variant-class))))
      (define (selected-derived/internal proc)
        (if (obs? @selected)
            (~> @selected proc)
            (proc @selected)))
      (define (tab-disabled?/internal tab-id)
        (let loop ([ts tabs/normalized])
          (cond
            [(null? ts) #t]
            [else
             (define tab-entry (car ts))
             (if (equal? (car tab-entry) tab-id)
                 (list-ref tab-entry 2)
                 (loop (cdr ts)))])))
      (define (index-of-enabled/internal selected0)
        (let loop ([i 0]
                   [ids enabled-tab-ids])
          (cond
            [(null? ids) 0]
            [(equal? (car ids) selected0) i]
            [else (loop (add1 i) (cdr ids))])))
      (define (enabled-at/internal idx)
        (list-ref enabled-tab-ids idx))
      (define (next-enabled/internal selected0)
        (if (null? enabled-tab-ids)
            #f
            (let* ([count (length enabled-tab-ids)]
                   [i (index-of-enabled/internal selected0)]
                   [j (modulo (+ i 1) count)])
              (enabled-at/internal j))))
      (define (prev-enabled/internal selected0)
        (if (null? enabled-tab-ids)
            #f
            (let* ([count (length enabled-tab-ids)]
                   [i (index-of-enabled/internal selected0)]
                   [j (modulo (+ i (- count 1)) count)])
              (enabled-at/internal j))))
      (define (first-enabled/internal)
        (if (null? enabled-tab-ids) #f (car enabled-tab-ids)))
      (define (last-enabled/internal)
        (if (null? enabled-tab-ids)
            #f
            (list-ref enabled-tab-ids (- (length enabled-tab-ids) 1))))
      (define (choose-tab-view/internal selected0)
        (define selected-view
          (let loop ([ts tabs/normalized])
            (cond
              [(null? ts) #f]
              [else
               (define tab-entry (car ts))
               (if (and (equal? (car tab-entry) selected0)
                        (not (list-ref tab-entry 2)))
                   (list-ref tab-entry 1)
                   (loop (cdr ts)))])))
        (cond
          [selected-view selected-view]
          [(null? enabled-tab-ids)
           (if (null? tabs/normalized)
               (spacer)
               (list-ref (car tabs/normalized) 1))]
          [else
           (let loop ([ts tabs/normalized])
             (define tab-entry (car ts))
             (if (equal? (car tab-entry) (car enabled-tab-ids))
                 (list-ref tab-entry 1)
                 (loop (cdr ts))))]))
      (define (selected-button-id/internal selected0)
        (let loop ([entries tabs/normalized]
                   [idx 0])
          (cond
            [(null? entries) ""]
            [else
             (define tab-entry (car entries))
             (if (and (equal? (car tab-entry) selected0)
                      (not (list-ref tab-entry 2)))
                 (string-append panel-id "-tab-" (number->string idx))
                 (loop (cdr entries) (add1 idx)))])))
      (define (select-if-possible/internal selected0 tab-id)
        (when (and tab-id
                   (obs? @selected)
                   (not (tab-disabled?/internal tab-id)))
          (obs-set! @selected tab-id)))
      (define tab-buttons
        (let loop ([entries tabs/normalized]
                   [idx 0])
          (cond
            [(null? entries)
             '()]
            [else
             (define tab-entry (car entries))
             (define tab-id (list-ref tab-entry 0))
             (define tab-disabled? (list-ref tab-entry 2))
             (define button-id
               (string-append panel-id "-tab-" (number->string idx)))
             (define @selected?
               (selected-derived/internal
                (lambda (selected0)
                  (and (equal? tab-id selected0)
                       (not tab-disabled?)))))
             (define @tabindex
               (selected-derived/internal
                (lambda (selected0)
                  (if (and (equal? tab-id selected0)
                           (not tab-disabled?))
                      0
                      -1))))
             (define @class
               (selected-derived/internal
                (lambda (selected0)
                  (cond
                    [tab-disabled? "we-tab-btn is-disabled"]
                    [(equal? tab-id selected0) "we-tab-btn is-selected"]
                    [else "we-tab-btn"]))))
             (define button-attrs
               (list (cons 'tab-id tab-id)
                     (cons 'id button-id)
                     (cons 'role 'tab)
                     (cons 'data-we-widget "tab-button")
                     (cons 'aria-controls panel-id)
                     (cons 'aria-disabled tab-disabled?)
                     (cons 'aria-selected @selected?)
                     (cons 'tabindex @tabindex)
                     (cons 'class @class)))
             (define dynamic-button-attrs
               (if (obs? selected)
                   (list (cons 'on-click-action
                               (lambda ()
                                 (unless tab-disabled?
                                   (obs-set! selected tab-id))))
                         (cons 'on-change-action
                               (lambda (key)
                                 (unless tab-disabled?
                                   (define selected0 (obs-peek selected))
                                   (case (string->symbol key)
                                     [(ArrowRight)
                                      (select-if-possible/internal selected0
                                                                   (next-enabled/internal selected0))]
                                     [(ArrowLeft)
                                      (select-if-possible/internal selected0
                                                                   (prev-enabled/internal selected0))]
                                     [(Home)
                                      (select-if-possible/internal selected0
                                                                   (first-enabled/internal))]
                                     [(End)
                                      (select-if-possible/internal selected0
                                                                   (last-enabled/internal))]
                                     [else
                                      (void)])))))
                   '()))
             (define button-attrs/final
               (append button-attrs dynamic-button-attrs))
             (cons (Button tab-id
                           #:attrs button-attrs/final)
                   (loop (cdr entries) (add1 idx)))])))
      (define tablist-view
        (apply Div
               (append tab-buttons
                       (list #:attrs (list (cons 'role 'tablist)
                                           (cons 'data-we-widget "tab-list")
                                           (cons 'class "we-tab-list"))))))
      (define @labelledby
        (selected-derived/internal selected-button-id/internal))
      (define content-view
        (observable-element-children
         'div
         @selected
         (lambda (selected0)
           (list (choose-tab-view/internal selected0)))
         #:attrs (list (cons 'role 'tabpanel)
                       (cons 'id panel-id)
                       (cons 'data-we-widget "tab-content")
                       (cons 'aria-labelledby @labelledby)
                       (cons 'class "we-tab-content"))))
      (define attrs/final
        (list (cons 'selected @selected)
              (cons 'data-we-widget "tab-panel")
              (cons 'class tab-panel-class)))
      (html-element-children
       'tab-panel
       tablist-view
       content-view
       #:attrs attrs/final))

    ;; collapse : (or/c boolean? observable?) view? -> view?
    ;;   Construct a container view that shows child only when open is true.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    (define/component collapse
      #:root-tag 'div
      #:positional ([open]
                    [child])
      #:root-attrs attrs/final
      (define @open
        (observable-or-const open))
      (define @class
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "we-collapse"
                  "we-collapse is-open"))))
      (define @aria-hidden
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "true"
                  "false"))))
      (define attrs/final
        (list (cons 'data-we-widget "collapse")
              (cons 'class @class)
              (cons 'aria-hidden @aria-hidden)))
      (Div child
           #:attrs attrs/final))

    ;; accordion : (or/c any/c observable?) list? -> view?
    ;;   Construct a single-open accordion from section rows: (list id label view).
    (define/component accordion
      #:root-tag 'div
      #:positional ([selected]
                    [sections])
      #:root-attrs attrs/final
      (define sections/raw
        (ensure-list/internal sections 'accordion "sections"))
      (define sections/normalized
        (map (lambda (section)
               (unless (list? section)
                 (raise-arguments-error 'accordion
                                        "expected section row as list"
                                        "section"
                                        section))
               (unless (= (length section) 3)
                 (raise-arguments-error 'accordion
                                        "expected section row of arity 3: (list id label view)"
                                        "section"
                                        section))
               (list (list-ref section 0)
                     (list-ref section 1)
                     (list-ref section 2)
                     (next-accordion-panel-id/internal)))
             sections/raw))
      (define section-ids
        (map car sections/normalized))
      (define (select-section!/internal section-id)
        (when (obs? selected)
          (obs-set! selected section-id)))
      (define (toggle-section!/internal section-id)
        (when (obs? selected)
          (if (equal? (obs-peek selected) section-id)
              (obs-set! selected #f)
              (obs-set! selected section-id))))
      (define (move-selection!/internal section-id delta)
        (when (and (obs? selected)
                   (pair? section-ids))
          (define selected0
            (obs-peek selected))
          (define index
            (let loop ([ids section-ids]
                       [i 0])
              (cond
                [(null? ids) 0]
                [(equal? (car ids) selected0) i]
                [else (loop (cdr ids) (add1 i))])))
          (define count (length section-ids))
          (define next-index (modulo (+ index delta count) count))
          (define next-id (list-ref section-ids next-index))
          (select-section!/internal next-id)))
      (define attrs/final
        (list (cons 'data-we-widget "accordion")
              (cons 'class "we-accordion")))
      (apply Div
             (append
              (map
               (lambda (section-entry)
                 (define section-id (list-ref section-entry 0))
                 (define section-label (list-ref section-entry 1))
                 (define section-view (list-ref section-entry 2))
                 (define panel-id (list-ref section-entry 3))
                 (define open-state
                   (if (obs? selected)
                       (~> selected
                           (lambda (selected0)
                             (equal? selected0 section-id)))
                       (equal? selected section-id)))
                 (define @expanded
                   (~> (observable-or-const open-state)
                       (lambda (open0)
                         (if open0 "true" "false"))))
                 (define @trigger-class
                   (~> (observable-or-const open-state)
                       (lambda (open0)
                         (if open0
                             "we-accordion-trigger is-open"
                             "we-accordion-trigger"))))
                 (define @collapse-class
                   (~> (observable-or-const open-state)
                       (lambda (open0)
                         (if open0
                             "we-collapse is-open we-accordion-content"
                             "we-collapse we-accordion-content"))))
                 (define @collapse-hidden
                   (~> (observable-or-const open-state)
                       (lambda (open0)
                         (if open0 "false" "true"))))
                 (define trigger-attrs
                   (list (cons 'role 'button)
                         (cons 'data-we-widget "accordion-trigger")
                         (cons 'aria-controls panel-id)
                         (cons 'aria-expanded @expanded)
                         (cons 'class @trigger-class)))
                 (define trigger-attrs/final
                   (if (obs? selected)
                       (append trigger-attrs
                               (list (cons 'on-click-action
                                           (lambda ()
                                             (toggle-section!/internal section-id)))
                                     (cons 'on-change-action
                                           (lambda (key)
                                             (case (string->symbol key)
                                               [(ArrowDown)
                                                (move-selection!/internal section-id 1)]
                                               [(ArrowUp)
                                                (move-selection!/internal section-id -1)]
                                               [(Home)
                                                (when (pair? section-ids)
                                                  (select-section!/internal (car section-ids)))]
                                               [(End)
                                                (when (pair? section-ids)
                                                  (select-section!/internal
                                                   (list-ref section-ids
                                                             (- (length section-ids) 1))))]
                                               [else
                                                (void)])))))
                       trigger-attrs))
                 (Div
                  (Button section-label
                          #:attrs trigger-attrs/final)
                  (Div section-view
                       #:attrs (list (cons 'id panel-id)
                                     (cons 'data-we-widget "collapse")
                                     (cons 'class @collapse-class)
                                     (cons 'aria-hidden @collapse-hidden)))
                  #:attrs (list (cons 'data-we-widget "accordion-section")
                                (cons 'class "we-accordion-section"))))
               sections/normalized)
              (list #:attrs attrs/final))))

    ;; offcanvas : (or/c boolean? observable?) (-> any/c) [view?] ... -> view?
    ;;   Construct an offcanvas side panel with open flag, close action, optional #:side, and body children.
    (define/component offcanvas
      #:root-tag 'div
      #:component-keywords ([#:side side-kw #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (when (< (length all-positional) 2)
        (error 'offcanvas
               "wrong number of positional arguments (expected at least 2, got ~a)"
               (length all-positional)))
      (define open
        (list-ref all-positional 0))
      (define on-close
        (list-ref all-positional 1))
      (define args
        (cddr all-positional))
      (define parsed
        (parse-offcanvas-args/internal args side-kw))
      (define raw-side
        (list-ref parsed 0))
      (define children
        (list-ref parsed 1))
      (define @open
        (observable-or-const open))
      (define @side
        (observable-or-const raw-side))
      (define @root-class
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "we-offcanvas"
                  "we-offcanvas is-open"))))
      (define @root-aria-hidden
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "true"
                  "false"))))
      (define @panel-class
        (~> @side
            (lambda (side0)
              (define final-side
                (normalize-offcanvas-side/internal side0))
              (string-append "we-offcanvas-panel is-" (symbol->string final-side)))))
      (define offcanvas-change-action
        (lambda (key)
          (when (string=? key "Escape")
            (invoke-close-callback/internal on-close 'escape))))
      (define make-offcanvas-children
        (lambda (_state)
          (list
           (apply Div
                  (list #:attrs (list (cons 'data-we-widget "offcanvas-backdrop")
                                      (cons 'class "we-offcanvas-backdrop")
                                      (cons 'on-click-action
                                            (lambda ()
                                              (invoke-close-callback/internal on-close 'backdrop))))))
           (apply Div
                  (append (list (close-button
                                 (lambda ()
                                   (invoke-close-callback/internal on-close 'button))
                                 "Close panel"))
                          children
                          (list #:attrs (list (cons 'data-we-widget "offcanvas-panel")
                                              (cons 'class @panel-class)
                                              (cons 'tabindex -1))))))))
      (define attrs/final
        (list (cons 'role 'dialog)
              (cons 'data-we-widget "offcanvas")
              (cons 'open @open)
              (cons 'class @root-class)
              (cons 'tabindex -1)
              (cons 'aria-modal "true")
              (cons 'aria-hidden @root-aria-hidden)
              (cons 'on-change-action offcanvas-change-action)))
      (observable-element-children
       'div
       @side
       make-offcanvas-children
       #:attrs attrs/final))

    ;; dialog : (or/c boolean? observable?) (-> any/c) [view?] ... -> view?
    ;;   Construct a modal dialog that is visible when open is true and closes via on-close.
    ;;   Optional keyword #:size defaults to 'md.
    ;;   Optional keywords #:title, #:description, #:footer, #:show-close?, #:close-label, #:tone,
    ;;   and #:tone-style configure dialog chrome.
    (define/component dialog
      #:root-tag 'dialog
      #:component-keywords ([#:size size-kw #f]
                            [#:title title #f]
                            [#:description description #f]
                            [#:footer footer #f]
                            [#:show-close? show-close? #f]
                            [#:close-label close-label "Close dialog"]
                            [#:tone tone #f]
                            [#:tone-style tone-style #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (when (< (length all-positional) 2)
        (error 'dialog
               "wrong number of positional arguments (expected at least 2, got ~a)"
               (length all-positional)))
      (define open
        (list-ref all-positional 0))
      (define on-close
        (list-ref all-positional 1))
      (define args
        (cddr all-positional))
      
      (define parsed
        (parse-dialog-options/internal args
                                       size-kw
                                       title
                                       description
                                       footer
                                       show-close?
                                       close-label
                                       tone
                                       tone-style
                                       "Close dialog"))
      (define final-size      (list-ref parsed 0))
      (define rest/args       (list-ref parsed 1))
      (define raw-title       (list-ref parsed 2))
      (define raw-description (list-ref parsed 3))
      (define raw-footer      (list-ref parsed 4))
      (define raw-show-close? (list-ref parsed 5))
      (define raw-close-label (list-ref parsed 6))
      (define raw-tone        (list-ref parsed 7))
      (define raw-tone-style  (list-ref parsed 8))
      (define @open
        (observable-or-const open))
      (define @title
        (observable-or-const raw-title))
      (define @description
        (observable-or-const raw-description))
      (define @footer
        (observable-or-const raw-footer))
      (define @show-close?
        (observable-or-const raw-show-close?))
      (define @close-label
        (observable-or-const raw-close-label))
      (define @tone
        (observable-or-const raw-tone))
      (define @tone-style
        (observable-or-const raw-tone-style))
      (define @state
        (obs-combine list
                     @open
                     @title
                     @description
                     @footer
                     @show-close?
                     @close-label
                     @tone
                     @tone-style))
      (define @root-class
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "we-dialog"
                  "we-dialog is-open"))))
      (define @root-aria-hidden
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "true"
                  "false"))))
      (define dialog-change-action
        (lambda (key)
          (when (string=? key "Escape")
            (invoke-close-callback/internal on-close 'escape))))
      (define make-dialog-children
        (lambda (state)
          (define title0
            (list-ref state 1))
          (define description0
            (list-ref state 2))
          (define footer0
            (list-ref state 3))
          (define show-close?0
            (not (eq? (list-ref state 4) #f)))
          (define close-label0
            (list-ref state 5))
          (define tone0
            (normalize-card-tone/internal (list-ref state 6)))
          (define tone-style0
            (normalize-card-tone-style/internal (list-ref state 7)))
          (define panel-class
            (string-append
             "we-dialog-panel we-dialog-size-" (symbol->string final-size)
             (if tone0
                 (string-append " we-dialog-tone-" (symbol->string tone0))
                 "")
             (if tone-style0
                 (string-append " we-dialog-tone-" (symbol->string tone-style0))
                 "")))
          (define dialog-desc-id #f)
          (define body-content
            (if (and (eq? description0 #f)
                     (pair? rest/args)
                     (text-view?/internal (car rest/args)))
                (begin
                  (set! dialog-desc-id (next-dialog-body-id/internal))
                  (cons (apply-extra-attrs/internal (car rest/args)
                                                    (list (cons 'id dialog-desc-id)))
                        (cdr rest/args)))
                rest/args))
          (define body-children
            (append
             (if (eq? description0 #f)
                 '()
                 (begin
                   (set! dialog-desc-id (next-dialog-body-id/internal))
                   (list (P description0
                            #:id dialog-desc-id
                            #:data-we-widget "dialog-description"
                            #:class "we-dialog-description"))))
             body-content))
          (define header-children
            (append
             (if (eq? title0 #f)
                 '()
                 (list (H2 title0
                           #:data-we-widget "dialog-title"
                           #:class "we-dialog-title")))
             (if show-close?0
                 (list
                  (Button "×"
                          #:attrs (list (cons 'role 'button)
                                        (cons 'data-we-widget "dialog-close")
                                        (cons 'class "we-close-button we-dialog-close")
                                        (cons 'aria-label close-label0)
                                        (cons 'on-click-action
                                              (lambda ()
                                                (invoke-close-callback/internal on-close 'button))))))
                 '())))
          (define footer-children
            (cond
              [(eq? footer0 #f)
               '()]
              [(view? footer0)
               (list footer0)]
              [else
               (list (Span footer0
                           #:data-we-widget "dialog-footer-text"
                           #:class "we-dialog-footer-text"))]))
          (define panel-attrs
            (append (list (cons 'class panel-class)
                          (cons 'data-we-widget "dialog-panel")
                          (cons 'tabindex -1))
                    (if dialog-desc-id
                        (list (cons 'aria-describedby dialog-desc-id))
                        '())))
          (list
           (apply Div
                  (append
                   (if (null? header-children)
                       '()
                       (list (apply Div
                                    (append header-children
                                            (list #:data-we-widget "dialog-header"
                                                  #:class "we-dialog-header")))))
                   (list (apply Div
                                (append body-children
                                        (list #:data-we-widget "dialog-body"
                                              #:class "we-dialog-body"))))
                   (if (null? footer-children)
                       '()
                       (list (apply Div
                                    (append footer-children
                                            (list #:data-we-widget "dialog-footer"
                                                  #:class "we-dialog-footer")))))
                   (list #:attrs panel-attrs))))))
      (define attrs/final
        (list (cons 'role 'dialog)
              (cons 'data-we-widget "dialog")
              (cons 'open @open)
              (cons 'class @root-class)
              (cons 'tabindex -1)
              (cons 'aria-modal "true")
              (cons 'aria-hidden @root-aria-hidden)
              (cons 'on-change-action dialog-change-action)))
      (observable-element-children
       'dialog
       @state
       make-dialog-children
       #:attrs attrs/final))

    ;; modal : (or/c boolean? observable?) (-> any/c) [view?] ... -> view?
    ;;   Construct a modal container that mirrors dialog behavior.
    ;;   Optional keyword #:size defaults to 'md.
    ;;   Optional keywords #:title, #:description, #:footer, #:show-close?, #:close-label, #:tone,
    ;;   and #:tone-style configure modal chrome.
    (define/component modal
      #:root-tag 'dialog
      #:component-keywords ([#:size size-kw #f]
                            [#:title title #f]
                            [#:description description #f]
                            [#:footer footer #f]
                            [#:show-close? show-close? #f]
                            [#:close-label close-label "Close modal"]
                            [#:tone tone #f]
                            [#:tone-style tone-style #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (when (< (length all-positional) 2)
        (error 'modal
               "wrong number of positional arguments (expected at least 2, got ~a)"
               (length all-positional)))
      (define open
        (list-ref all-positional 0))
      (define on-close
        (list-ref all-positional 1))
      (define args
        (cddr all-positional))
      
      (define parsed
        (parse-dialog-options/internal args
                                       size-kw
                                       title
                                       description
                                       footer
                                       show-close?
                                       close-label
                                       tone
                                       tone-style
                                       "Close modal"))
      (define final-size      (list-ref parsed 0))
      (define rest/args       (list-ref parsed 1))
      (define raw-title       (list-ref parsed 2))
      (define raw-description (list-ref parsed 3))
      (define raw-footer      (list-ref parsed 4))
      (define raw-show-close? (list-ref parsed 5))
      (define raw-close-label (list-ref parsed 6))
      (define raw-tone        (list-ref parsed 7))
      (define raw-tone-style  (list-ref parsed 8))
      (define @open
        (observable-or-const open))
      (define @title
        (observable-or-const raw-title))
      (define @description
        (observable-or-const raw-description))
      (define @footer
        (observable-or-const raw-footer))
      (define @show-close?
        (observable-or-const raw-show-close?))
      (define @close-label
        (observable-or-const raw-close-label))
      (define @tone
        (observable-or-const raw-tone))
      (define @tone-style
        (observable-or-const raw-tone-style))
      (define @state
        (obs-combine list
                     @open
                     @title
                     @description
                     @footer
                     @show-close?
                     @close-label
                     @tone
                     @tone-style))
      (define @root-class
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "we-modal"
                  "we-modal is-open"))))
      (define @root-aria-hidden
        (~> @open
            (lambda (open0)
              (if (eq? open0 #f)
                  "true"
                  "false"))))
      (define modal-change-action
        (lambda (key)
          (when (string=? key "Escape")
            (invoke-close-callback/internal on-close 'escape))))
      (define make-modal-children
        (lambda (state)
          (define title0
            (list-ref state 1))
          (define description0
            (list-ref state 2))
          (define footer0
            (list-ref state 3))
          (define show-close?0
            (not (eq? (list-ref state 4) #f)))
          (define close-label0
            (list-ref state 5))
          (define tone0
            (normalize-card-tone/internal (list-ref state 6)))
          (define tone-style0
            (normalize-card-tone-style/internal (list-ref state 7)))
          (define panel-class
            (string-append
             "we-dialog-panel we-dialog-size-" (symbol->string final-size)
             (if tone0
                 (string-append " we-dialog-tone-" (symbol->string tone0))
                 "")
             (if tone-style0
                 (string-append " we-dialog-tone-" (symbol->string tone-style0))
                 "")))
          (define modal-desc-id #f)
          (define body-content
            (if (and (eq? description0 #f)
                     (pair? rest/args)
                     (text-view?/internal (car rest/args)))
                (begin
                  (set! modal-desc-id (next-dialog-body-id/internal))
                  (cons (apply-extra-attrs/internal (car rest/args)
                                                    (list (cons 'id modal-desc-id)))
                        (cdr rest/args)))
                rest/args))
          (define body-children
            (append
             (if (eq? description0 #f)
                 '()
                 (begin
                   (set! modal-desc-id (next-dialog-body-id/internal))
                   (list (P description0
                            #:id modal-desc-id
                            #:data-we-widget "modal-description"
                            #:class "we-modal-description"))))
             body-content))
          (define header-children
            (append
             (if (eq? title0 #f)
                 '()
                 (list (H2 title0
                           #:data-we-widget "modal-title"
                           #:class "we-modal-title")))
             (if show-close?0
                 (list
                  (Button "×"
                          #:attrs (list (cons 'role 'button)
                                        (cons 'data-we-widget "modal-close")
                                        (cons 'class "we-close-button we-modal-close")
                                        (cons 'aria-label close-label0)
                                        (cons 'on-click-action
                                              (lambda ()
                                                (invoke-close-callback/internal on-close 'button))))))
                 '())))
          (define footer-children
            (cond
              [(eq? footer0 #f)
               '()]
              [(view? footer0)
               (list footer0)]
              [else
               (list (Span footer0
                           #:data-we-widget "modal-footer-text"
                           #:class "we-modal-footer-text"))]))
          (define panel-attrs
            (append (list (cons 'class panel-class)
                          (cons 'data-we-widget "modal-panel")
                          (cons 'tabindex -1))
                    (if modal-desc-id
                        (list (cons 'aria-describedby modal-desc-id))
                        '())))
          (list
           (apply Div
                  (append
                   (if (null? header-children)
                       '()
                       (list (apply Div
                                    (append header-children
                                            (list #:data-we-widget "modal-header"
                                                  #:class "we-modal-header")))))
                   (list (apply Div
                                (append body-children
                                        (list #:data-we-widget "modal-body"
                                              #:class "we-modal-body"))))
                   (if (null? footer-children)
                       '()
                       (list (apply Div
                                    (append footer-children
                                            (list #:data-we-widget "modal-footer"
                                                  #:class "we-modal-footer")))))
                   (list #:attrs panel-attrs))))))
      (define attrs/final
        (list (cons 'role 'dialog)
              (cons 'data-we-widget "modal")
              (cons 'open @open)
              (cons 'class @root-class)
              (cons 'tabindex -1)
              (cons 'aria-modal "true")
              (cons 'aria-hidden @root-aria-hidden)
              (cons 'on-change-action modal-change-action)))
      (observable-element-children
       'dialog
       @state
       make-modal-children
       #:attrs attrs/final))

    ;; observable-view : (or/c any/c observable?) (-> any/c view?) [(-> any/c any/c boolean?)] -> view?
    ;;   Construct a dynamic single-child view from value using make-view.
    ;;   Optional parameter equal-proc defaults to equal?.
    (define (observable-view data make-view [equal-proc equal?])
      (observable-element-children
       'div
       data
       (lambda (value)
         (list (make-view value)))
       equal-proc
       #:attrs (list (cons 'data-we-widget "observable-view")
                     (cons 'class "we-observable-view"))))

    ;; observable-element-children : symbol? (or/c any/c observable?) (-> any/c list?) [(-> any/c any/c boolean?)] [list?] [(or/c #f procedure?)] -> view?
    ;;   Construct a primitive element view with dynamic children and stable root identity.
    ;;   Optional parameter equal-proc defaults to equal?.
    ;;   Optional parameter attrs defaults to '().
    ;;   Optional parameter after-render defaults to #f and is invoked by renderer after each children update.
    (define/key (observable-element-children tag
                                             data
                                             make-children
                                             [equal-proc equal?]
                                             #:attrs [attrs '()]
                                             #:after-render [after-render #f])
      (view kind/observable-element-children
            (list (cons 'tag tag)
                  (cons 'data data)
                  (cons 'make-children make-children)
                  (cons 'equal-proc equal-proc)
                  (cons 'after-render after-render)
                  (cons 'extra-attrs attrs))
            '()))

    ;; normalize-spacer-grow/internal : any/c -> number?
    ;;   Normalize spacer grow factor to a positive numeric value.
    (define (normalize-spacer-grow/internal grow)
      (cond
        [(and (number? grow) (> grow 0)) grow]
        [else 1]))

    ;; normalize-divider-orientation/internal : any/c -> symbol?
    ;;   Normalize divider orientation to 'horizontal or 'vertical.
    (define (normalize-divider-orientation/internal orientation)
      (if (symbol? orientation)
          (case orientation
            [(horizontal vertical) orientation]
            [else 'horizontal])
          'horizontal))

    ;; menu-popup-counter/internal : number?
    ;;   Monotonic counter for menu popup ids.
    (define menu-popup-counter/internal 0)

    ;; tooltip-counter/internal : number?
    ;;   Monotonic counter for tooltip bubble ids.
    (define tooltip-counter/internal 0)

    ;; popover-panel-counter/internal : number?
    ;;   Monotonic counter for popover panel ids.
    (define popover-panel-counter/internal 0)

    ;; active-menu-close/internal : (or/c #f (-> void?))
    ;;   Thunk closing currently open popup menu.
    (define active-menu-close/internal #f)

    ;; next-menu-popup-id/internal : -> string?
    ;;   Allocate a unique id for menu popup element.
    (define (next-menu-popup-id/internal)
      (set! menu-popup-counter/internal
            (add1 menu-popup-counter/internal))
      (string-append "menu-popup-"
                     (number->string menu-popup-counter/internal)))

    ;; next-tooltip-id/internal : -> string?
    ;;   Allocate a unique id for tooltip bubble element.
    (define (next-tooltip-id/internal)
      (set! tooltip-counter/internal
            (add1 tooltip-counter/internal))
      (string-append "tooltip-"
                     (number->string tooltip-counter/internal)))

    ;; next-popover-panel-id/internal : -> string?
    ;;   Allocate a unique id for popover panel element.
    (define (next-popover-panel-id/internal)
      (set! popover-panel-counter/internal
            (add1 popover-panel-counter/internal))
      (string-append "popover-panel-"
                     (number->string popover-panel-counter/internal)))

    ;; close-active-menu/internal : -> void?
    ;;   Close currently open menu popup when available.
    (define (close-active-menu/internal)
      (when active-menu-close/internal
        (active-menu-close/internal)))

    ;; normalize-nav-orientation/internal : any/c -> symbol?
    ;;   Normalize navigation-bar orientation to horizontal/vertical.
    (define (normalize-nav-orientation/internal orientation)
      (if (symbol? orientation)
          (case orientation
            [(horizontal vertical) orientation]
            [else                  'horizontal])
          'horizontal))

    ;; normalize-dropdown-placement/internal : any/c -> symbol?
    ;;   Normalize dropdown placement to one of down/up/start/end.
    (define (normalize-dropdown-placement/internal raw-placement)
      (if (symbol? raw-placement)
          (case raw-placement
            [(down up start end) raw-placement]
            [else                'down])
          'down))

    ;; normalize-overlay-placement/internal : any/c symbol? -> symbol?
    ;;   Normalize tooltip/popover placement symbols, with provided default.
    (define (normalize-overlay-placement/internal raw-placement default-placement)
      (define placement0
        (if (obs? raw-placement)
            (obs-peek raw-placement)
            raw-placement))
      (if (memq placement0 '(top right bottom left))
          placement0
          default-placement))

    ;; normalized-option-pairs/internal : list? -> list?
    ;;   Normalize option rows to (cons id label) pairs.
    (define (normalized-option-pairs/internal rows)
      (map (lambda (row)
             (define id
               (cond
                 [(and (list? row)
                       (pair? row)
                       (pair? (cdr row)))
                  (car row)]
                 [(pair? row)
                  (car row)]
                 [else
                  row]))
             (define label
               (cond
                 [(and (list? row)
                       (pair? row)
                       (pair? (cdr row)))
                  (cadr row)]
                 [(pair? row)
                  (cdr row)]
                 [else
                  row]))
             (cons id label))
           rows))

    ;; carousel-item-label/internal : any/c -> any/c
    ;;   Extract carousel item label from (list id label view) row.
    (define (carousel-item-label/internal entry)
      (cadr (ensure-list/internal entry 'carousel "entry")))

    ;; carousel-item-view/internal : any/c -> view?
    ;;   Extract carousel item view from (list id label view) row.
    (define (carousel-item-view/internal entry)
      (caddr (ensure-list/internal entry 'carousel "entry")))

    ;; scrollspy-section-id/internal : any/c -> any/c
    ;;   Extract scrollspy section id from (list id label) row.
    (define (scrollspy-section-id/internal entry)
      (car (ensure-list/internal entry 'scrollspy "section")))

    ;; scrollspy-section-label/internal : any/c -> any/c
    ;;   Extract scrollspy section label from (list id label) row.
    (define (scrollspy-section-label/internal entry)
      (cadr (ensure-list/internal entry 'scrollspy "section")))

    ;; scrollspy-section-content/internal : any/c -> view?
    ;;   Extract optional section view from (list id label [view]); fallback to text label.
    (define (scrollspy-section-content/internal entry)
      (define section (ensure-list/internal entry 'scrollspy "section"))
      (cond
        [(>= (length section) 3)
         (define maybe-view (caddr section))
         (if (view? maybe-view)
             maybe-view
             (text (scrollspy-section-label/internal entry)))]
        [else
         (text (scrollspy-section-label/internal entry))]))

    ;; scrollspy-section-dom-id/internal : any/c -> string?
    ;;   Build deterministic DOM id for a scrollspy section identifier.
    (define (scrollspy-section-dom-id/internal section-id)
      (define section-text
        (cond
          [(symbol? section-id) (symbol->string section-id)]
          [(string? section-id) section-id]
          [else                 (format "~a" section-id)]))
      (string-append "we-scrollspy-section-" section-text))

    ;; ensure-list/internal : any/c symbol? string? -> list?
    ;;   Ensure v is a list, otherwise raise an argument error for who/what.
    (define (ensure-list/internal v who what)
      (if (list? v)
          v
          (raise-arguments-error who
                                 (string-append "expected list? for " what)
                                 what
                                 v)))

    ;; normalize-page-count/internal : any/c -> number?
    ;;   Normalize page-count to a positive integer.
    (define (normalize-page-count/internal page-count)
      (if (and (number? page-count)
               (integer? page-count)
               (> page-count 0))
          page-count
          1))

    ;; clamp-current-page/internal : any/c number? -> number?
    ;;   Clamp current page to [1, page-count].
    (define (clamp-current-page/internal current-page page-count)
      (if (and (number? current-page)
               (integer? current-page))
          (min page-count (max 1 current-page))
          1))

    ;; contains-equal/internal : list? any/c -> boolean?
    ;;   Check whether xs contains v using equal?.
    (define (contains-equal/internal xs v)
      (cond
        [(null? xs) #f]
        [else
         (if (equal? (car xs) v)
             #t
             (contains-equal/internal (cdr xs) v))]))

    ;; unique-sorted-numbers/internal : list? -> list?
    ;;   Sort numeric values and remove duplicates.
    (define (unique-sorted-numbers/internal nums)
      (define sorted
        (sort (filter number? nums) <))
      (let loop ([rest sorted]
                 [acc '()])
        (cond
          [(null? rest)
           (reverse acc)]
          [else
           (define n (car rest))
           (if (contains-equal/internal acc n)
               (loop (cdr rest) acc)
               (loop (cdr rest) (cons n acc)))])))

    ;; pagination-visible-pages/internal : number? number? -> list?
    ;;   Return page number list with 'ellipsis markers for compact rendering.
    (define (pagination-visible-pages/internal page-count current-page)
      (if (<= page-count 7)
          (let loop ([n 1])
            (if (> n page-count)
                '()
                (cons n (loop (add1 n)))))
          (let* ([base-pages (unique-sorted-numbers/internal
                              (list 1
                                    page-count
                                    (- current-page 1)
                                    current-page
                                    (+ current-page 1)))]
                 [bounded-pages (filter (lambda (n)
                                          (and (>= n 1)
                                               (<= n page-count)))
                                        base-pages)])
            (let loop ([rest bounded-pages]
                       [prev #f]
                       [acc '()])
              (cond
                [(null? rest)
                 (reverse acc)]
                [else
                 (define n (car rest))
                 (define next-acc
                   (cond
                     [(eq? prev #f)
                      (cons n acc)]
                     [(= n (+ prev 1))
                      (cons n acc)]
                     [else
                      (cons n (cons 'ellipsis acc))]))
                 (loop (cdr rest) n next-acc)])))))

    ;; list-group-id/internal : any/c -> any/c
    ;;   Extract list-group entry id from (list id label) row.
    (define (list-group-id/internal entry)
      (car (ensure-list/internal entry 'list-group "entry")))

    ;; list-group-label/internal : any/c -> any/c
    ;;   Extract list-group entry label from (list id label) row.
    (define (list-group-label/internal entry)
      (cadr (ensure-list/internal entry 'list-group "entry")))

    ;; breadcrumb-id/internal : any/c -> any/c
    ;;   Extract breadcrumb entry id from (list id label) row.
    (define (breadcrumb-id/internal entry)
      (car (ensure-list/internal entry 'breadcrumb "entry")))

    ;; breadcrumb-label/internal : any/c -> any/c
    ;;   Extract breadcrumb entry label from (list id label) row.
    (define (breadcrumb-label/internal entry)
      (cadr (ensure-list/internal entry 'breadcrumb "entry")))

    ;; spacer : [number?] -> view?
    ;;   Construct an empty spacer view with optional grow factor.
    ;;   Optional parameter grow defaults to 1.
    ;;   Accepts global HTML attributes for the root <span> via keyword arguments.
    (define/component spacer
      #:root-tag 'span
      #:positional ([grow 1])
      #:component-keywords ([#:grow grow-kw #f])
      #:root-attrs attrs/final
      (define final-grow (if (eq? grow-kw #f) grow grow-kw))
      (define @grow (observable-or-const final-grow))
      (define @style
        (~> @grow
            (lambda (grow0)
              (string-append "flex-grow:"
                             (number->string (normalize-spacer-grow/internal grow0))
                             ";"))))
      (define attrs/final
        (list (cons 'data-we-widget "spacer")
              (cons 'class "we-spacer")
              (cons 'style @style)))
      (Span ""
            #:attrs attrs/final))

    ;; divider : [symbol?] -> view?
    ;;   Construct a divider with orientation 'horizontal or 'vertical.
    ;;   Optional parameter orientation defaults to 'horizontal.
    ;;   Accepts global HTML attributes for the root <hr> via keyword arguments.
    (define/component divider
      #:root-tag 'hr
      #:positional ([orientation 'horizontal])
      #:component-keywords ([#:orientation orientation-kw #f])
      #:root-attrs attrs/final
      (define final-orientation (if (eq? orientation-kw #f) orientation orientation-kw))
      (define @orientation (observable-or-const final-orientation))
      (define @normalized-orientation
        (~> @orientation normalize-divider-orientation/internal))
      (define @aria-orientation
        (~> @normalized-orientation
            (lambda (orientation0)
              (if (eq? orientation0 'vertical)
                  "vertical"
                  "horizontal"))))
      (define @class
        (~> @normalized-orientation
            (lambda (orientation0)
              (if (eq? orientation0 'vertical)
                  "we-divider we-divider-vertical"
                  "we-divider we-divider-horizontal"))))
      (define attrs/final
        (list (cons 'role 'separator)
              (cons 'data-we-widget "divider")
              (cons 'aria-orientation @aria-orientation)
              (cons 'class @class)))
      (Hr #:attrs attrs/final))

    ;; normalize-table-density/internal : any/c -> symbol?
    ;;   Normalize table density to normal/compact.
    (define (normalize-table-density/internal density)
      (if (symbol? density)
          (case density
            [(normal compact) density]
            [else             'normal])
          'normal))

    ;; normalize-table-align/internal : any/c -> symbol?
    ;;   Normalize table alignment to left/center/right.
    (define (normalize-table-align/internal align)
      (if (symbol? align)
          (case align
            [(left center right) align]
            [else                'left])
          'left))

    ;; normalize-table-column/internal : any/c -> list?
    ;;   Normalize column specification to (list label align).
    (define (normalize-table-column/internal column)
      (if (and (list? column)
               (>= (length column) 2))
          (list (list-ref column 0)
                (normalize-table-align/internal (list-ref column 1)))
          (list column 'left)))

    ;; density-class/internal : symbol? -> string?
    ;;   Return CSS class token for table density.
    (define (density-class/internal density)
      (case density
        [(compact) "we-density-compact"]
        [else      "we-density-normal"]))

    ;; normalize-table-variants/internal : any/c -> list?
    ;;   Normalize table variants to accepted symbols.
    (define (normalize-table-variants/internal raw)
      (define (allowed-variant? v)
        (and (symbol? v)
             (case v
               [(striped hover borderless sm) #t]
               [else #f])))
      (define (loop xs)
        (cond
          [(null? xs) '()]
          [(allowed-variant? (car xs))
           (cons (car xs) (loop (cdr xs)))]
          [else
           (loop (cdr xs))]))
      (cond
        [(allowed-variant? raw)
         (list raw)]
        [(list? raw)
         (loop raw)]
        [else
         '()]))

    ;; table-variant-class/internal : list? -> string?
    ;;   Build CSS class fragment from table variants.
    (define (table-variant-class/internal variants)
      (define striped?    (contains-equal/internal variants 'striped))
      (define hover?      (contains-equal/internal variants 'hover))
      (define borderless? (contains-equal/internal variants 'borderless))
      (define small?      (contains-equal/internal variants 'sm))
      (string-append
       (if striped? " we-table-striped" "")
       (if hover? " we-table-hover" "")
       (if borderless? " we-table-borderless" "")
       (if small? " we-table-sm" "")))

    ;; normalize-table-row-variant/internal : any/c -> any/c
    ;;   Normalize row variant to accepted symbol or #f.
    (define (normalize-table-row-variant/internal raw)
      (if (symbol? raw)
          (case raw
            [(active primary secondary success danger warning info light dark) raw]
            [else #f])
          #f))

    ;; normalize-table-row-variants/internal : any/c -> list?
    ;;   Normalize row-variants option to list.
    (define (normalize-table-row-variants/internal raw)
      (if (list? raw)
          (map normalize-table-row-variant/internal raw)
          '()))

    ;; normalize-table-row-header-column/internal : any/c -> any/c
    ;;   Normalize row-header-column to non-negative index or #f.
    (define (normalize-table-row-header-column/internal raw)
      (if (and (number? raw)
               (exact-integer? raw)
               (>= raw 0))
          raw
          #f))

    ;; table-row-variant-class/internal : any/c -> string?
    ;;   Return CSS class token for row variant or empty string.
    (define (table-row-variant-class/internal variant)
      (if variant
          (string-append "we-table-row-" (symbol->string variant))
          ""))

    ;; table-align-class/internal : symbol? -> string?
    ;;   Convert normalized alignment symbol to CSS class token.
    (define (table-align-class/internal align)
      (case align
        [(center) "we-align-center"]
        [(right)  "we-align-right"]
        [else     "we-align-left"]))

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
    (define/component table
      #:root-tag 'table
      #:positional ([columns]
                    [rows]
                    [density 'normal]
                    [options-pos '()])
      #:component-keywords ([#:density density-kw #f]
                            [#:caption caption #f]
                            [#:variants variants #f]
                            [#:row-variants row-variants #f]
                            [#:row-header-column row-header-column #f])
      #:root-attrs attrs/final
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
      (define raw-caption
        (options-ref/internal options 'caption #f))
      (define raw-variants
        (options-ref/internal options 'variants #f))
      (define raw-row-variants
        (options-ref/internal options 'row-variants '()))
      (define raw-row-header-column
        (options-ref/internal options 'row-header-column #f))
      (define normalized-columns
        (map normalize-table-column/internal columns))
      (define @rows
        (observable-or-const rows))
      (define @caption
        (observable-or-const raw-caption))
      (define @variants
        (~> (observable-or-const raw-variants)
            normalize-table-variants/internal))
      (define @row-variants
        (~> (observable-or-const raw-row-variants)
            normalize-table-row-variants/internal))
      (define @row-header-column
        (~> (observable-or-const raw-row-header-column)
            normalize-table-row-header-column/internal))
      (define @density
        (~> (observable-or-const final-density)
            normalize-table-density/internal))
      (define @root-class
        (obs-combine
         (lambda (density0 variants0)
           (string-append "we-table "
                          (density-class/internal density0)
                          (table-variant-class/internal variants0)))
         @density
         @variants))
      (define @state
        (obs-combine list
                     @rows
                     @caption
                     @density
                     @row-variants
                     @row-header-column))
      (define (make-table-children state)
        (define rows0
          (ensure-list/internal (list-ref state 0) 'table "rows"))
        (define caption0
          (list-ref state 1))
        (define density0
          (list-ref state 2))
        (define row-variants0
          (list-ref state 3))
        (define row-header-column0
          (list-ref state 4))
        (define density-css
          (density-class/internal density0))
        (define caption-node
          (if (eq? caption0 #f)
              '()
              (list (html-element 'caption
                                  caption0
                                  #:attrs (list (cons 'data-we-widget "table-caption")
                                                (cons 'class "we-table-caption"))))))
        (define header-row
          (if (null? normalized-columns)
              '()
              (list
               (apply html-element-children
                      (append
                       (list 'tr)
                       (map (lambda (column-spec)
                              (define align-css
                                (table-align-class/internal
                                 (list-ref column-spec 1)))
                              (html-element 'th
                                            (list-ref column-spec 0)
                                            #:attrs (list (cons 'data-we-widget "table-header-cell")
                                                          (cons 'class
                                                                (string-append
                                                                 "we-table-header-cell "
                                                                 density-css
                                                                 " "
                                                                 align-css)))))
                            normalized-columns)
                       (list #:attrs (list (cons 'data-we-widget "table-row"))))))))
        (define (row-values row)
          (if (list? row)
              row
              (list row)))
        (define (row-node row0 row-index)
          (define row-variant
            (if (< row-index (length row-variants0))
                (list-ref row-variants0 row-index)
                #f))
          (define variant-css
            (table-row-variant-class/internal row-variant))
          (define row-attrs
            (if (string=? variant-css "")
                (list (cons 'data-we-widget "table-row"))
                (list (cons 'data-we-widget "table-row")
                      (cons 'class variant-css))))
          (define (cells-loop cells cell-index)
            (cond
              [(null? cells)
               '()]
              [else
               (define cell-value (car cells))
               (define align
                 (if (< cell-index (length normalized-columns))
                     (list-ref (list-ref normalized-columns cell-index) 1)
                     'left))
               (define align-css (table-align-class/internal align))
               (define cell-node
                 (if (and row-header-column0
                          (= cell-index row-header-column0))
                     (html-element
                      'th
                      cell-value
                      #:attrs (list (cons 'data-we-widget "table-row-header-cell")
                                    (cons 'scope "row")
                                    (cons 'class
                                          (string-append "we-table-data-cell "
                                                         density-css
                                                         " "
                                                         align-css))))
                     (html-element
                      'td
                      cell-value
                      #:attrs (list (cons 'data-we-widget "table-data-cell")
                                    (cons 'class
                                          (string-append "we-table-data-cell "
                                                         density-css
                                                         " "
                                                         align-css))))))
               (cons cell-node
                     (cells-loop (cdr cells) (add1 cell-index)))]))
          (apply html-element-children
                 (append (list 'tr)
                         (cells-loop (row-values row0) 0)
                         (list #:attrs row-attrs))))
        (define data-rows
          (let loop ([remaining rows0]
                     [row-index 0])
            (cond
              [(null? remaining)
               '()]
              [else
               (cons (row-node (car remaining) row-index)
                     (loop (cdr remaining) (add1 row-index)))])))
        (append caption-node header-row data-rows))
      (define attrs/final
        (list (cons 'data-we-widget "table")
              (cons 'columns columns)
              (cons 'variants @variants)
              (cons 'row-variants @row-variants)
              (cons 'row-header-column @row-header-column)
              (cons 'caption @caption)
              (cons 'density @density)
              (cons 'class @root-class)))
      (observable-element-children
       'table
       @state
       make-table-children
       #:attrs attrs/final))

    ;; radios : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a radio-choice control with choices and selected value.
    ;;   Accepts global HTML attributes for the root container via keyword arguments.
    (define/component radios
      #:root-tag 'div
      #:positional ([choices]
                    [selected]
                    [action])
      #:root-attrs attrs/final
      (define radio-entries
        (map (lambda (row)
               (define id-value
                 (if (and (list? row) (pair? row))
                     (car row)
                     row))
               (define label-value
                 (if (and (list? row)
                          (pair? row)
                          (pair? (cdr row)))
                     (cadr row)
                     row))
               (define disabled?
                 (if (and (list? row)
                          (pair? row)
                          (pair? (cdr row))
                          (pair? (cddr row)))
                     (not (not (caddr row)))
                     #f))
               (list id-value label-value disabled?))
             choices))
      (define (radio-text v)
        (format "~a" v))
      (define encoded->id
        (map (lambda (entry)
               (cons (radio-text (list-ref entry 0))
                     (list-ref entry 0)))
             radio-entries))
      (define first-enabled-id
        (let loop ([remaining radio-entries])
          (cond
            [(null? remaining) #f]
            [(list-ref (car remaining) 2)
             (loop (cdr remaining))]
            [else
             (list-ref (car remaining) 0)])))
      (define (decode-selection selected0)
        (define selected-text
          (if (string? selected0)
              selected0
              (radio-text selected0)))
        (define p (assoc selected-text encoded->id))
        (if p
            (cdr p)
            selected0))
      (define @selected
        (observable-or-const selected))
      (define @effective-selected
        (~> @selected
            (lambda (selected0)
              (define selected-text (radio-text selected0))
              (define matches-enabled?
                (let loop ([remaining radio-entries])
                  (cond
                    [(null? remaining) #f]
                    [else
                     (define entry (car remaining))
                     (define entry-id (list-ref entry 0))
                     (define disabled? (list-ref entry 2))
                     (if (and (not disabled?)
                              (string=? selected-text (radio-text entry-id)))
                         #t
                         (loop (cdr remaining)))])))
              (if matches-enabled?
                  selected0
                  first-enabled-id))))
      (define group-name
        (symbol->string (gensym 'we-radios-group-)))
      (define attrs/final
        (list (cons 'data-we-widget "radios")
              (cons 'class "we-radios")
              (cons 'choices choices)
              (cons 'selected @effective-selected)
              (cons 'on-change-action
                    (lambda (new-selected)
                      (action (decode-selection new-selected))))))
      (define option-views
        (map (lambda (entry)
               (define id-value (list-ref entry 0))
               (define label-value (list-ref entry 1))
               (define disabled? (list-ref entry 2))
               (define encoded-id (radio-text id-value))
               (define @checked
                 (~> @effective-selected
                     (lambda (selected0)
                       (and (not disabled?)
                            (string=? encoded-id
                                      (radio-text selected0))))))
               (define input-attrs
                 (list (cons 'class "we-radio-input")
                       (cons 'on-change-action
                             (lambda (new-selected)
                               (action (decode-selection new-selected))))))
               (define input-attrs/final
                 (if disabled?
                     (cons (cons 'disabled #t) input-attrs)
                     input-attrs))
               (Div (Input #:type "radio"
                           #:name group-name
                           #:value encoded-id
                           #:checked @checked
                           #:attrs input-attrs/final)
                    (Span label-value
                          #:data-we-widget "text"
                          #:class "we-text")
                    #:data-we-widget "radio-option"
                    #:class "we-radio-option"))
             radio-entries))
      (apply Div (append option-views
                         (list #:attrs attrs/final))))

    ;; image : (or/c string? observable?) [any/c] [any/c] -> view?
    ;;   Construct an image view from a source path/string with optional width/height attrs.
    ;;   Optional parameter width defaults to #f.
    ;;   Optional parameter height defaults to #f.
    ;;   Accepts global and img-specific HTML attributes for the root <img> via keyword arguments.
    (define/component image
      #:root-tag 'img
      #:positional ([src]
                    [width #f]
                    [height #f])
      #:component-keywords ([#:width width-kw #f]
                            [#:height height-kw #f])
      #:root-attrs attrs/final
      (define final-width (if (eq? width-kw #f) width width-kw))
      (define final-height (if (eq? height-kw #f) height height-kw))
      (define attrs/final
        (list (cons 'data-we-widget "image")
              (cons 'class "we-image")))
      (cond
        [(and (eq? final-width #f)
              (eq? final-height #f))
         (Img #:src src
              #:attrs attrs/final)]
        [(eq? final-height #f)
         (Img #:src src
              #:width final-width
              #:attrs attrs/final)]
        [(eq? final-width #f)
         (Img #:src src
              #:height final-height
              #:attrs attrs/final)]
        [else
         (Img #:src src
              #:width final-width
              #:height final-height
              #:attrs attrs/final)]))

    ;; dropdown : (or/c string? observable?) list? (-> any/c any/c) [symbol?] -> view?
    ;;   Construct a dropdown menu from a label and entry rows: (list id label).
    ;;   Optional parameter placement defaults to 'down.
    (define/component dropdown
      #:root-tag 'div
      #:positional ([label]
                    [entries]
                    [action]
                    [placement 'down])
      #:component-keywords ([#:placement placement-kw #f])
      #:root-attrs attrs/final
      (define final-placement
        (if (eq? placement-kw #f) placement placement-kw))
      (define normalized-placement
        (normalize-dropdown-placement/internal final-placement))
      (define dropdown-class
        (string-append "we-dropdown"
                       (if (eq? normalized-placement 'down)
                           ""
                           (string-append " we-dropdown-"
                                          (symbol->string normalized-placement)))))
      (define rows
        (ensure-list/internal entries 'dropdown "entries"))
      (define option-pairs
        (normalized-option-pairs/internal rows))
      (define menu-items
        (map (lambda (entry)
               (define entry-id (car entry))
               (define entry-label (cdr entry))
               (menu-item entry-label
                          (lambda ()
                            (action entry-id))))
             option-pairs))
      (define dropdown-view
        (Div (apply menu
                    (cons label menu-items))
             #:attrs (list (cons 'data-we-widget "dropdown")
                           (cons 'class dropdown-class))))
      (define attrs/final
        (list (cons 'data-we-widget "dropdown")
              (cons 'class dropdown-class)))
      (Div (apply menu
                  (cons label menu-items))
           #:attrs attrs/final))

    ;; carousel : list? (or/c number? observable?) (-> any/c any/c) [boolean?] [boolean?] -> view?
    ;;   Construct a carousel from item rows, current index, and index-change action with optional wrap and autoplay flags.
    ;;   Optional parameter wrap? defaults to #t.
    ;;   Optional parameter autoplay? defaults to #f.
    (define/component carousel
      #:root-tag 'div
      #:positional ([items]
                    [current-index]
                    [action]
                    [wrap? #t]
                    [autoplay? #f])
      #:component-keywords ([#:wrap? wrap-kw keyword-not-given]
                            [#:autoplay? autoplay-kw keyword-not-given])
      #:root-attrs attrs/final
      (define final-wrap?
        (if (keyword-given? wrap-kw) wrap-kw wrap?))
      (define final-autoplay?
        (if (keyword-given? autoplay-kw) autoplay-kw autoplay?))
      (define @items
        (observable-or-const items))
      (define @current-index
        (observable-or-const current-index))
      (define @wrap?
        (observable-or-const final-wrap?))
      (define @autoplay?
        (observable-or-const final-autoplay?))
      (define (normalize-carousel-state items0 current-index0 wrap?0 autoplay?0)
        (define items/list
          (ensure-list/internal items0 'carousel "items"))
        (define count
          (length items/list))
        (define has-items?
          (> count 0))
        (define min-index 0)
        (define max-index
          (if has-items? (- count 1) 0))
        (define wrap?/normalized
          (not (eq? wrap?0 #f)))
        (define autoplay?/normalized
          (not (eq? autoplay?0 #f)))
        (define current-index/normalized
          (if (and (number? current-index0)
                   (integer? current-index0)
                   has-items?)
              (min max-index (max min-index current-index0))
              0))
        (define at-first?
          (or (not has-items?)
              (<= current-index/normalized min-index)))
        (define at-last?
          (or (not has-items?)
              (>= current-index/normalized max-index)))
        (define prev-disabled?
          (or (not has-items?)
              (and (not wrap?/normalized) at-first?)))
        (define next-disabled?
          (or (not has-items?)
              (and (not wrap?/normalized) at-last?)))
        (list (cons 'items items/list)
              (cons 'count count)
              (cons 'has-items? has-items?)
              (cons 'current-index current-index/normalized)
              (cons 'min-index min-index)
              (cons 'max-index max-index)
              (cons 'wrap? wrap?/normalized)
              (cons 'autoplay? autoplay?/normalized)
              (cons 'at-first? at-first?)
              (cons 'at-last? at-last?)
              (cons 'prev-disabled? prev-disabled?)
              (cons 'next-disabled? next-disabled?)))
      (define (carousel-state-ref state key)
        (define p (assq key state))
        (if p
            (cdr p)
            (error 'carousel "missing carousel state key: ~a" key)))
      (define @state
        (obs-combine
         normalize-carousel-state
         @items
         @current-index
         @wrap?
         @autoplay?))
      (define (normalize-next-index state next-index)
        (define has-items?
          (carousel-state-ref state 'has-items?))
        (define wrap?/normalized
          (carousel-state-ref state 'wrap?))
        (define min-index
          (carousel-state-ref state 'min-index))
        (define max-index
          (carousel-state-ref state 'max-index))
        (define count
          (carousel-state-ref state 'count))
        (cond
          [(not has-items?) 0]
          [wrap?/normalized
           (modulo (+ next-index count) count)]
          [else
           (min max-index (max min-index next-index))]))
      (define (set-carousel-index! next-index)
        (define state
          (obs-peek @state))
        (define has-items?
          (carousel-state-ref state 'has-items?))
        (when has-items?
          (define current-index/normalized
            (carousel-state-ref state 'current-index))
          (define normalized-index
            (normalize-next-index state next-index))
          (unless (= normalized-index current-index/normalized)
            (action normalized-index))))
      (define (carousel-keydown! event-key)
        (define state
          (obs-peek @state))
        (define prev-disabled?
          (carousel-state-ref state 'prev-disabled?))
        (define next-disabled?
          (carousel-state-ref state 'next-disabled?))
        (define at-first?
          (carousel-state-ref state 'at-first?))
        (define at-last?
          (carousel-state-ref state 'at-last?))
        (define current-index/normalized
          (carousel-state-ref state 'current-index))
        (define min-index
          (carousel-state-ref state 'min-index))
        (define max-index
          (carousel-state-ref state 'max-index))
        (case (string->symbol event-key)
          [(ArrowLeft)
           (unless prev-disabled?
             (set-carousel-index! (- current-index/normalized 1)))]
          [(ArrowRight)
           (unless next-disabled?
             (set-carousel-index! (+ current-index/normalized 1)))]
          [(Home)
           (unless at-first?
             (set-carousel-index! min-index))]
          [(End)
           (unless at-last?
             (set-carousel-index! max-index))]
          [else
           (void)]))
      (define (make-carousel-children state)
        (define items/list
          (carousel-state-ref state 'items))
        (define has-items?
          (carousel-state-ref state 'has-items?))
        (define current-index/normalized
          (carousel-state-ref state 'current-index))
        (define prev-disabled?
          (carousel-state-ref state 'prev-disabled?))
        (define next-disabled?
          (carousel-state-ref state 'next-disabled?))
        (define viewport
          (if has-items?
              (carousel-item-view/internal
               (list-ref items/list current-index/normalized))
              (Span "No slides"
                    #:data-we-widget "carousel-empty")))
        (define indicators
          (let loop ([idx 0]
                     [rest items/list])
            (if (null? rest)
                '()
                (let ()
                  (define entry
                    (car rest))
                  (define label/text
                    (format "~a" (carousel-item-label/internal entry)))
                  (define is-current
                    (= idx current-index/normalized))
                  (define indicator-node
                    (html-element 'button
                                  ""
                                  #:attrs (append
                                           (list (cons 'role 'button)
                                                 (cons 'data-we-widget "carousel-indicator")
                                                 (cons 'class (string-append "we-carousel-indicator"
                                                                             (if is-current " is-current" "")))
                                                 (cons 'aria-label label/text)
                                                 (cons 'on-click-action
                                                       (lambda ()
                                                         (action idx))))
                                           '())))
                  (cons indicator-node
                        (loop (add1 idx) (cdr rest)))))))
        (define prev-node
          (html-element 'button
                        "Prev"
                        #:attrs (append
                                 (list (cons 'role 'button)
                                       (cons 'data-we-widget "carousel-prev")
                                       (cons 'class (string-append "we-button we-carousel-nav we-carousel-prev"
                                                                   (if prev-disabled? " is-disabled" "")))
                                       (cons 'aria-disabled (if prev-disabled? "true" "false"))
                                       (cons 'on-click-action
                                             (lambda ()
                                               (unless prev-disabled?
                                                 (set-carousel-index! (- current-index/normalized 1))))))
                                 (if prev-disabled?
                                     (list (cons 'disabled #t))
                                     '()))))
        (define next-node
          (html-element 'button
                        "Next"
                        #:attrs (append
                                 (list (cons 'role 'button)
                                       (cons 'data-we-widget "carousel-next")
                                       (cons 'class (string-append "we-button we-carousel-nav we-carousel-next"
                                                                   (if next-disabled? " is-disabled" "")))
                                       (cons 'aria-disabled (if next-disabled? "true" "false"))
                                       (cons 'on-click-action
                                             (lambda ()
                                               (unless next-disabled?
                                                 (set-carousel-index! (+ current-index/normalized 1))))))
                                 (if next-disabled?
                                     (list (cons 'disabled #t))
                                     '()))))
        (list (Div viewport
                   #:data-we-widget "carousel-viewport"
                   #:class "we-carousel-viewport")
              (Div prev-node
                   (apply Div
                          (append indicators
                                  (list #:data-we-widget "carousel-indicators"
                                        #:class "we-carousel-indicators")))
                   next-node
                   #:data-we-widget "carousel-controls"
                   #:class "we-carousel-controls")))
      (define carousel-timeout-handle #f)
      (define carousel-cleanup-registered? #f)
      (define (carousel-after-render root-node state register-cleanup! api)
        (define (api-proc key)
          (define p (assq key api))
          (if (and p (procedure? (cdr p)))
              (cdr p)
              (raise-arguments-error 'carousel-after-render
                                     "missing callback API procedure"
                                     "key"
                                     key)))
        (define find-node-by-widget
          (api-proc 'find-node-by-widget))
        (define dom-node-on-click
          (api-proc 'dom-node-on-click))
        (define backend-set-timeout!
          (api-proc 'backend-set-timeout!))
        (define backend-clear-timeout!
          (api-proc 'backend-clear-timeout!))
        (define (clear-carousel-timeout!)
          (when carousel-timeout-handle
            (backend-clear-timeout! carousel-timeout-handle)
            (set! carousel-timeout-handle #f)))
        (unless carousel-cleanup-registered?
          (set! carousel-cleanup-registered? #t)
          (register-cleanup!
           (lambda ()
             (clear-carousel-timeout!))))
        (clear-carousel-timeout!)
        (define autoplay?
          (not (eq? (carousel-state-ref state 'autoplay?) #f)))
        (define has-items?
          (not (eq? (carousel-state-ref state 'has-items?) #f)))
        (define count
          (carousel-state-ref state 'count))
        (define wrap?
          (not (eq? (carousel-state-ref state 'wrap?) #f)))
        (define at-last?
          (not (eq? (carousel-state-ref state 'at-last?) #f)))
        (when (and autoplay?
                   has-items?
                   (> count 1)
                   (or wrap? (not at-last?)))
          (set! carousel-timeout-handle
                (backend-set-timeout!
                 2500
                 (lambda ()
                   (set! carousel-timeout-handle #f)
                   (define next-node
                     (find-node-by-widget root-node "carousel-next"))
                   (when next-node
                     (define on-click
                       (dom-node-on-click next-node))
                     (when on-click
                       (on-click))))))))
      (define carousel-view
        (observable-element-children
         'div
         @state
         make-carousel-children
         #:after-render carousel-after-render
         #:attrs (list (cons 'data-we-widget "carousel")
                       (cons 'class "we-carousel")
                       (cons 'tabindex 0)
                       (cons 'on-change-action carousel-keydown!))))
      (define attrs/final
        (list (cons 'data-we-widget "carousel")
              (cons 'class "we-carousel")
              (cons 'tabindex 0)
              (cons 'on-change-action carousel-keydown!)))
      (observable-element-children
       'div
       @state
       make-carousel-children
       #:after-render carousel-after-render
       #:attrs attrs/final))

    ;; scrollspy : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct scroll-tracking section navigation from rows: (list id label [content-view]).
    (define (scrollspy sections current action)
      (define @sections
        (observable-or-const sections))
      (define @current
        (observable-or-const current))
      (define @state
        (obs-combine list @sections @current))
      (define (scrollspy-after-render root-node state register-cleanup! api)
        (define (api-proc key)
          (define p (assq key api))
          (if (and p (procedure? (cdr p)))
              (cdr p)
              (raise-arguments-error 'scrollspy-after-render
                                     "missing callback API procedure"
                                     "key"
                                     key)))
        (define find-node-by-widget
          (api-proc 'find-node-by-widget))
        (define dom-node-attr-ref
          (api-proc 'dom-node-attr-ref))
        (define dom-node-children
          (api-proc 'dom-node-children))
        (define dom-node-on-click
          (api-proc 'dom-node-on-click))
        (define set-dom-node-on-click!
          (api-proc 'set-dom-node-on-click!))
        (define backend-scrollspy-scroll-into-view!
          (api-proc 'backend-scrollspy-scroll-into-view!))
        (define backend-scrollspy-active-id
          (api-proc 'backend-scrollspy-active-id))
        (define backend-scrollspy-observe-scroll!
          (api-proc 'backend-scrollspy-observe-scroll!))
        (define sections-node
          (find-node-by-widget root-node "scrollspy-sections"))
        (define nav-node
          (find-node-by-widget root-node "scrollspy-nav"))
        (when (and sections-node nav-node)
          (define section-bindings
            (map (lambda (section-node)
                   (cons (dom-node-attr-ref section-node
                                            'data-we-scrollspy-id
                                            #f)
                         section-node))
                 (dom-node-children sections-node)))
          (define nav-action-bindings '())
          (for-each
           (lambda (nav-item)
             (define section-id
               (dom-node-attr-ref nav-item 'data-we-scrollspy-id #f))
             (define section-node-pair
               (assq section-id section-bindings))
             (define section-node
               (if section-node-pair
                   (cdr section-node-pair)
                   #f))
             (define on-click0
               (dom-node-on-click nav-item))
             (when on-click0
               (set! nav-action-bindings
                     (cons (cons section-id on-click0)
                           nav-action-bindings))
               (set-dom-node-on-click!
                nav-item
                (lambda ()
                  (on-click0)
                  (when section-node
                    (backend-scrollspy-scroll-into-view! section-node))))))
           (dom-node-children nav-node))
          (define current-id
            (if (and (list? state)
                     (pair? (cdr state)))
                (cadr state)
                #f))
          (define (sync-current-from-scroll!)
            (define active-id
              (backend-scrollspy-active-id section-bindings))
            (when (and active-id
                       (not (equal? active-id current-id)))
              (define p
                (assq active-id nav-action-bindings))
              (when p
                ((cdr p)))))
          (backend-scrollspy-observe-scroll!
           sections-node
           sync-current-from-scroll!
           register-cleanup!)
          (when (eq? current-id #f)
            (sync-current-from-scroll!))))
      (define (make-scrollspy-children state)
        (define sections/list
          (ensure-list/internal (car state) 'scrollspy "sections"))
        (define current/value
          (cadr state))
        (define nav-items
          (map (lambda (entry)
                 (define section-id
                   (scrollspy-section-id/internal entry))
                 (define label
                   (scrollspy-section-label/internal entry))
                 (define current?
                   (equal? section-id current/value))
                 (html-element 'button
                               (format "~a" label)
                               #:attrs (list (cons 'role 'button)
                                             (cons 'data-we-widget "scrollspy-item")
                                             (cons 'data-we-scrollspy-id section-id)
                                             (cons 'aria-current (if current? "true" "false"))
                                             (cons 'class (string-append "we-scrollspy-item"
                                                                         (if current? " is-current" "")))
                                             (cons 'on-click-action
                                                   (lambda ()
                                                     (action section-id))))))
               sections/list))
        (define section-nodes
          (map (lambda (entry)
                 (Section (scrollspy-section-content/internal entry)
                          #:data-we-widget "scrollspy-section"
                          #:data-we-scrollspy-id (scrollspy-section-id/internal entry)
                          #:class "we-scrollspy-section"
                          #:id (scrollspy-section-dom-id/internal
                                (scrollspy-section-id/internal entry))))
               sections/list))
        (list (apply Nav
                     (append nav-items
                             (list #:data-we-widget "scrollspy-nav"
                                   #:class "we-scrollspy-nav")))
              (apply Div
                     (append section-nodes
                             (list #:data-we-widget "scrollspy-sections"
                                   #:class "we-scrollspy-sections")))))
      (observable-element-children
       'div
       @state
       make-scrollspy-children
       #:after-render scrollspy-after-render
       #:attrs (list (cons 'role 'navigation)
                     (cons 'data-we-widget "scrollspy")
                     (cons 'class "we-scrollspy"))))

    ;; tooltip : text-content/c view? -> view?
    ;;   Construct a tooltip container with message, trigger child view, optional #:placement,
    ;;   and optional #:title/#:footer text.
    (define/component tooltip
      #:root-tag 'div
      #:component-keywords ([#:placement placement-kw #f]
                            [#:title title #f]
                            [#:footer footer #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (when (< (length all-positional) 2)
        (error 'tooltip
               "wrong number of positional arguments (expected at least 2, got ~a)"
               (length all-positional)))
      (define message
        (list-ref all-positional 0))
      (define child
        (list-ref all-positional 1))
      (define args
        (cddr all-positional))
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
        (normalize-overlay-placement/internal
         (if (eq? placement-kw #f) placement placement-kw)
         'top))
      (define old-title
        (let ([p (assq 'title old-options)])
          (if p (cdr p) #f)))
      (define old-footer
        (let ([p (assq 'footer old-options)])
          (if p (cdr p) #f)))
      (define final-title
        (if (eq? title #f) old-title title))
      (define final-footer
        (if (eq? footer #f) old-footer footer))
      (define bubble-id
        (next-tooltip-id/internal))
      (define @open
        (obs #f))
      (define @message
        (observable-or-const message))
      (define @title
        (observable-or-const final-title))
      (define @footer
        (observable-or-const final-footer))
      (define @root-class
        (~> @open
            (lambda (open0)
              (string-append
               "we-tooltip we-tooltip-"
               (symbol->string final-placement)
               (if open0 " is-open" "")))))
      (define @bubble-aria-hidden
        (~> @open
            (lambda (open0)
              (if open0 "false" "true"))))
      (define (tooltip-change-action key)
        (case (string->symbol key)
          [(mouseenter)
           (:= @open #t)]
          [(mouseleave focusout Escape)
           (:= @open #f)]
          [else
           (void)]))
      (define bubble-view
        (html-element-children
         'span
         (Span (~> @title
                   (lambda (title0)
                     (if (eq? title0 #f) "" (format "~a" title0))))
               #:data-we-widget "tooltip-header"
               #:class "we-tooltip-header"
               #:hidden (~> @title
                            (lambda (title0)
                              (if (eq? title0 #f) "hidden" #f))))
         (Span (~> @message
                   (lambda (message0)
                     (format "~a" message0)))
               #:data-we-widget "tooltip-body"
               #:class "we-tooltip-body")
         (Span (~> @footer
                   (lambda (footer0)
                     (if (eq? footer0 #f) "" (format "~a" footer0))))
               #:data-we-widget "tooltip-footer"
               #:class "we-tooltip-footer"
               #:hidden (~> @footer
                            (lambda (footer0)
                              (if (eq? footer0 #f) "hidden" #f))))
         #:attrs (list (cons 'role 'tooltip)
                       (cons 'id bubble-id)
                       (cons 'data-we-widget "tooltip-bubble")
                       (cons 'class "we-tooltip-bubble")
                       (cons 'aria-hidden @bubble-aria-hidden))))
      (define attrs/final
        (list (cons 'data-we-widget "tooltip")
              (cons 'class @root-class)
              (cons 'on-change-action tooltip-change-action)))
      (define trigger-child
        (if (view? child)
            (apply-extra-attrs/internal child
                                        (list (cons 'aria-describedby bubble-id)))
            child))
      (html-element-children
       'div
       (html-element-children
        'div
        trigger-child
        #:attrs (list (cons 'data-we-widget "tooltip-trigger")
                      (cons 'class "we-tooltip-trigger")
                      (cons 'on-change-action tooltip-change-action)))
       bubble-view
       #:attrs attrs/final))

    ;; popover : text-content/c [view?] ... -> view?
    ;;   Construct a click-toggle popover with trigger label, optional #:placement,
    ;;   optional #:title/#:footer text, and body children.
    (define/component popover
      #:root-tag 'div
      #:component-keywords ([#:placement placement-kw #f]
                            [#:title title #f]
                            [#:footer footer #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (when (null? all-positional)
        (error 'popover
               "wrong number of positional arguments (expected at least 1, got 0)"))
      (define label
        (car all-positional))
      (define args
        (cdr all-positional))
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
        (normalize-overlay-placement/internal
         (if (eq? placement-kw #f) placement placement-kw)
         'bottom))
      (define old-title
        (let ([p (assq 'title old-options)])
          (if p (cdr p) #f)))
      (define old-footer
        (let ([p (assq 'footer old-options)])
          (if p (cdr p) #f)))
      (define final-title
        (if (eq? title #f) old-title title))
      (define final-footer
        (if (eq? footer #f) old-footer footer))
      (define panel-id
        (next-popover-panel-id/internal))
      (define @open
        (obs #f))
      (define @label
        (observable-or-const label))
      (define @title
        (observable-or-const final-title))
      (define @footer
        (observable-or-const final-footer))
      (define @trigger-aria-expanded
        (~> @open
            (lambda (open0)
              (if open0 "true" "false"))))
      (define @panel-aria-hidden
        (~> @open
            (lambda (open0)
              (if open0 "false" "true"))))
      (define @backdrop-aria-hidden
        (~> @open
            (lambda (open0)
              (if open0 "false" "true"))))
      (define @panel-class
        (~> @open
            (lambda (open0)
              (if open0
                  "we-popover-panel is-open"
                  "we-popover-panel"))))
      (define @backdrop-class
        (~> @open
            (lambda (open0)
              (if open0
                  "we-popover-backdrop is-open"
                  "we-popover-backdrop"))))
      (define (set-open! next-open?)
        (:= @open (not (not next-open?))))
      (define attrs/final
        (list (cons 'data-we-widget "popover")
              (cons 'class (string-append "we-popover we-popover-"
                                          (symbol->string final-placement)))))
      (html-element-children
       'div
       (html-element
        'button
        @label
        #:attrs (list (cons 'role 'button)
                      (cons 'data-we-widget "popover-trigger")
                      (cons 'class "we-popover-trigger")
                      (cons 'tabindex 0)
                      (cons 'aria-haspopup "dialog")
                      (cons 'aria-controls panel-id)
                      (cons 'aria-expanded @trigger-aria-expanded)
                      (cons 'on-click-action
                            (lambda ()
                              (set-open! (not (obs-peek @open)))))
                      (cons 'on-change-action
                            (lambda (key)
                              (case (string->symbol key)
                                [(Escape)
                                 (set-open! #f)]
                                [else
                                 (void)])))))
       (html-element
        'div
        ""
        #:attrs (list (cons 'data-we-widget "popover-backdrop")
                      (cons 'aria-hidden @backdrop-aria-hidden)
                      (cons 'class @backdrop-class)
                      (cons 'on-click-action
                            (lambda ()
                              (set-open! #f)))))
       (html-element-children
        'div
        (html-element
         'div
         (~> @title
             (lambda (title0)
               (if (eq? title0 #f) "" title0)))
         #:attrs (list (cons 'data-we-widget "popover-header")
                       (cons 'class "we-popover-header")
                       (cons 'hidden (~> @title
                                         (lambda (title0)
                                           (if (eq? title0 #f) "hidden" #f))))))
        (apply Div
               (append children
                       (list #:data-we-widget "popover-body"
                             #:class "we-popover-body")))
        (html-element
         'div
         (~> @footer
             (lambda (footer0)
               (if (eq? footer0 #f) "" footer0)))
         #:attrs (list (cons 'data-we-widget "popover-footer")
                       (cons 'class "we-popover-footer")
                       (cons 'hidden (~> @footer
                                         (lambda (footer0)
                                           (if (eq? footer0 #f) "hidden" #f))))))
        #:attrs (list (cons 'role 'dialog)
                      (cons 'id panel-id)
                      (cons 'tabindex -1)
                      (cons 'aria-hidden @panel-aria-hidden)
                      (cons 'data-we-widget "popover-panel")
                      (cons 'class @panel-class)
                      (cons 'on-change-action
                            (lambda (key)
                              (case (string->symbol key)
                                [(Escape)
                                 (set-open! #f)]
                                [else
                                 (void)])))))
       #:attrs attrs/final))

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
    (define/component card
      #:root-tag 'div
      #:component-keywords ([#:variants variants-kw #f]
                            [#:subtitle subtitle #f]
                            [#:media media #f]
                            [#:actions actions #f]
                            [#:tone tone #f]
                            [#:tone-style tone-style #f])
      #:rest all-positional
      #:root-attrs attrs/final
      (define positional-count
        (length all-positional))
      (define title
        (if (>= positional-count 1)
            (list-ref all-positional 0)
            #f))
      (define footer
        (if (>= positional-count 2)
            (list-ref all-positional 1)
            #f))
      (define args
        (cond
          [(>= positional-count 2) (cddr all-positional)]
          [(>= positional-count 1) (cdr all-positional)]
          [else '()]))
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
      (define raw-subtitle
        (options-ref/internal options 'subtitle #f))
      (define raw-media
        (options-ref/internal options 'media #f))
      (define raw-actions
        (options-ref/internal options 'actions '()))
      (define raw-tone
        (options-ref/internal options 'tone #f))
      (define raw-tone-style
        (options-ref/internal options 'tone-style #f))
      (define @title
        (observable-or-const title))
      (define @footer
        (observable-or-const footer))
      (define @subtitle
        (observable-or-const raw-subtitle))
      (define @media
        (observable-or-const raw-media))
      (define @actions
        (observable-or-const raw-actions))
      (define @variants
        (observable-or-const final-variants))
      (define @tone
        (observable-or-const raw-tone))
      (define @tone-style
        (observable-or-const raw-tone-style))
      (define @state
        (obs-combine list
                     @title
                     @footer
                     @subtitle
                     @media
                     @actions
                     @variants
                     @tone
                     @tone-style))
      (define @root-class
        (~> @state
            (lambda (state)
              (define variants0
                (normalize-card-variants/internal (list-ref state 5)))
              (define tone0
                (normalize-card-tone/internal (list-ref state 6)))
              (define tone-style0
                (normalize-card-tone-style/internal (list-ref state 7)))
              (string-append
               (card-variant-class/internal variants0)
               (if tone0
                   (string-append " we-card-tone-" (symbol->string tone0))
                   "")
               (if tone-style0
                   (string-append " we-card-tone-" (symbol->string tone-style0))
                   "")))))
      (define (card-action-view action0)
        (if (view? action0)
            action0
            (Span action0
                  #:data-we-widget "card-action-text"
                  #:class "we-card-action-text")))
      (define (make-card-children state)
        (define title0
          (list-ref state 0))
        (define footer0
          (list-ref state 1))
        (define subtitle0
          (list-ref state 2))
        (define media0
          (list-ref state 3))
        (define actions0
          (list-ref state 4))
        (define variants0
          (normalize-card-variants/internal (list-ref state 5)))
        (define headerless?
          (contains-equal/internal variants0 'headerless))
        (define media-children
          (cond
            [(eq? media0 #f)
             '()]
            [(view? media0)
             (list media0)]
            [else
             (list (Span media0
                         #:data-we-widget "card-media-text"
                         #:class "we-card-media-text"))]))
        (define action-list0
          (if (list? actions0) actions0 '()))
        (append
         (if (or headerless? (eq? title0 #f))
             '()
             (list (html-element 'div
                                 title0
                                 #:attrs (list (cons 'data-we-widget "card-header")
                                               (cons 'class "we-card-header")))))
         (if (or headerless? (eq? subtitle0 #f))
             '()
             (list (Small subtitle0
                          #:data-we-widget "card-subtitle"
                          #:class "we-card-subtitle")))
         (if (null? media-children)
             '()
             (list (apply Div
                          (append media-children
                                  (list #:attrs (list (cons 'data-we-widget "card-media")
                                                      (cons 'class "we-card-media")))))))
         (list (apply Div
                      (append rest/args
                              (list #:attrs (list (cons 'data-we-widget "card-body")
                                                  (cons 'class "we-card-body"))))))
         (if (null? action-list0)
             '()
             (list (apply Div
                          (append (map card-action-view action-list0)
                                  (list #:attrs (list (cons 'data-we-widget "card-actions")
                                                      (cons 'class "we-card-actions")))))))
         (if (eq? footer0 #f)
             '()
             (list (html-element 'div
                                 footer0
                                 #:attrs (list (cons 'data-we-widget "card-footer")
                                               (cons 'class "we-card-footer")))))))
      (define attrs/final
        (list (cons 'role 'group)
              (cons 'data-we-widget "card")
              (cons 'class @root-class)))
      (observable-element-children
       'div
       @state
       make-card-children
       #:attrs attrs/final))

    ;; top-bar : view? ... -> view?
    ;;   Construct a top bar container for page-level header content.
    ;;   Accepts global HTML attributes for the root <header> via keyword arguments.
    (define/component top-bar
      #:root-tag 'header
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'role 'banner)
              (cons 'data-we-widget "top-bar")
              (cons 'class "we-top-bar")))
      (apply Header
             (append children
                     (list #:attrs attrs/final))))

    ;; navigation-bar : view? ... -> view?
    ;;   Construct a navigation bar with optional #:orientation, #:collapsed?, and #:expand keywords plus children.
    (define/component navigation-bar
      #:root-tag 'nav
      #:component-keywords ([#:orientation orientation-kw #f]
                            [#:collapsed? collapsed?-kw #f]
                            [#:expand expand-kw #f])
      #:rest args
      #:root-attrs attrs/final
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
      (define @orientation
        (observable-or-const final-orientation))
      (define @collapsed-state
        (if (obs? final-collapsed?)
            final-collapsed?
            (obs (not (not final-collapsed?)))))
      (define @root-class
        (obs-combine
         (lambda (orientation0 collapsed0)
           (define normalized-orientation
             (normalize-nav-orientation/internal orientation0))
           (string-append "we-navigation-bar"
                          (if (eq? normalized-orientation 'vertical) " is-vertical" "")
                          (if (not (not collapsed0)) " is-collapsed" "")))
         @orientation
         @collapsed-state))
      (define @toggle-aria-expanded
        (~> @collapsed-state
            (lambda (collapsed0)
              (if (not (not collapsed0)) "false" "true"))))
      (define show-toggle?
        (eq? (if (obs? final-expand)
                 (obs-peek final-expand)
                 final-expand)
             'always))
      (define (toggle-navigation!)
        (define next-collapsed?
          (not (not (obs-peek @collapsed-state))))
        (:= @collapsed-state (not next-collapsed?)))
      (define attrs/final
        (list (cons 'role 'navigation)
              (cons 'data-we-widget "navigation-bar")
              (cons 'class @root-class)))
      (apply html-element-children
             (append
              (list 'nav)
              (if show-toggle?
                  (list (html-element 'button
                                      "Menu"
                                      #:attrs (list (cons 'role 'button)
                                                    (cons 'data-we-widget "navigation-bar-toggle")
                                                    (cons 'class "we-button we-navigation-bar-toggle")
                                                    (cons 'aria-expanded @toggle-aria-expanded)
                                                    (cons 'aria-label "Toggle navigation")
                                                    (cons 'on-click-action toggle-navigation!))))
                  '())
              (list (apply Div
                           (append children
                                   (list #:data-we-widget "navigation-bar-items"
                                         #:class "we-navigation-bar-items"))))
              (list #:attrs attrs/final))))

    ;; menu-bar : view? ... -> view?
    ;;   Construct a menu bar containing menu children.
    ;;   Accepts global HTML attributes for the root <menu-bar> via keyword arguments.
    (define/component menu-bar
      #:root-tag 'menu-bar
      #:rest children
      #:root-attrs attrs/final
      (define attrs/final
        (list (cons 'class "we-menu-bar")
              (cons 'data-we-widget "menu-bar")
              (cons 'role 'menubar)
              (cons 'aria-orientation "horizontal")))
      (apply html-element-children
             (append (list 'menu-bar)
                     children
                     (list #:attrs attrs/final))))

    ;; menu : text-content/c view? ... -> view?
    ;;   Construct a labeled menu containing menu-item children.
    ;;   Accepts global HTML attributes for the root <menu> via keyword arguments.
    (define/component menu
      #:root-tag 'menu
      #:rest args
      #:root-attrs attrs/final
      (unless (pair? args)
        (error 'menu
               "wrong number of positional arguments (expected at least 1, got ~a)"
               0))
      (define label (car args))
      (define children (cdr args))
      (define popup-id
        (next-menu-popup-id/internal))
      (define @open
        (obs #f))
      (define (set-open! next-open?)
        (when (and next-open?
                   active-menu-close/internal
                   (not (eq? active-menu-close/internal close-self!)))
          (active-menu-close/internal))
        (:= @open (not (not next-open?)))
        (if next-open?
            (set! active-menu-close/internal close-self!)
            (when (and active-menu-close/internal
                       (eq? active-menu-close/internal close-self!))
              (set! active-menu-close/internal #f))))
      (define (close-self!)
        (set-open! #f))
      (define @aria-expanded
        (~> @open
            (lambda (open0)
              (if open0 "true" "false"))))
      (define @popup-class
        (~> @open
            (lambda (open0)
              (if open0
                  "we-menu-popup is-open"
                  "we-menu-popup"))))
      (define attrs/final
        (list (cons 'class "we-menu")
              (cons 'data-we-widget "menu")))
      (html-element-children
       'menu
       (html-element 'button
                     label
                     #:attrs (list (cons 'role 'button)
                                   (cons 'class "we-menu-label")
                                   (cons 'data-we-widget "menu-label")
                                   (cons 'menu-trigger #t)
                                   (cons 'tabindex 0)
                                   (cons 'aria-haspopup "menu")
                                   (cons 'aria-controls popup-id)
                                   (cons 'aria-expanded @aria-expanded)
                                   (cons 'on-click-action
                                         (lambda ()
                                           (set-open! (not (obs-peek @open)))))
                                   (cons 'on-change-action
                                         (lambda (key)
                                           (case (string->symbol key)
                                             [(ArrowDown ArrowUp)
                                              (set-open! #t)]
                                             [(mouseenter)
                                              (when (and active-menu-close/internal
                                                         (not (obs-peek @open)))
                                                (set-open! #t))]
                                             [(focusout Escape)
                                              (set-open! #f)]
                                             [else
                                              (void)])))))
       (apply html-element-children
              (append (list 'vpanel)
                      children
                      (list #:attrs (list (cons 'role 'menu)
                                          (cons 'id popup-id)
                                          (cons 'data-we-widget "menu-popup")
                                          (cons 'class @popup-class)))))
       #:attrs attrs/final))

    ;; menu-item : text-content/c (-> any/c) [(or/c #f content/c)] [(or/c #f content/c)] -> view?
    ;;   Construct a menu item with optional leading/trailing icon labels.
    ;;   Optional parameter leading-icon defaults to #f.
    ;;   Optional parameter trailing-icon defaults to #f.
    ;;   Accepts global HTML attributes for the root <menu-item> via keyword arguments.
    (define/component menu-item
      #:root-tag 'menu-item
      #:rest args
      #:root-attrs attrs/final
      (unless (and (>= (length args) 2)
                   (<= (length args) 4))
        (error 'menu-item
               "wrong number of positional arguments (expected 2 to 4, got ~a)"
               (length args)))
      (define label (list-ref args 0))
      (define action (list-ref args 1))
      (define leading-icon (if (>= (length args) 3) (list-ref args 2) #f))
      (define trailing-icon (if (>= (length args) 4) (list-ref args 3) #f))
      (define attrs/final
        (list (cons 'role 'menuitem)
              (cons 'class "we-menu-item")
              (cons 'data-we-widget "menu-item")
              (cons 'tabindex 0)
              (cons 'on-click-action
                    (lambda ()
                      (action)
                      (close-active-menu/internal)))
              (cons 'on-change-action
                    (lambda (key)
                      (case (string->symbol key)
                        [(focusout Escape)
                         (close-active-menu/internal)]
                        [else
                         (void)])))))
      (define iconized?
        (or (obs? leading-icon)
            (obs? trailing-icon)
            (not (eq? leading-icon #f))
            (not (eq? trailing-icon #f))))
      (if (eq? iconized? #f)
          (html-element 'menu-item label #:attrs attrs/final)
          (let ()
            (define leading-view
              (if (obs? leading-icon)
                  (Span (~> leading-icon
                            (lambda (v)
                              (if (eq? v #f) "" v)))
                        #:data-we-widget "menu-item-icon"
                        #:class "we-menu-item-icon we-menu-item-icon-leading"
                        #:hidden (~> leading-icon
                                     (lambda (v)
                                       (if (eq? v #f) "hidden" #f))))
                  (if (eq? leading-icon #f)
                      #f
                      (Span leading-icon
                            #:data-we-widget "menu-item-icon"
                            #:class "we-menu-item-icon we-menu-item-icon-leading"))))
            (define trailing-view
              (if (obs? trailing-icon)
                  (Span (~> trailing-icon
                            (lambda (v)
                              (if (eq? v #f) "" v)))
                        #:data-we-widget "menu-item-icon"
                        #:class "we-menu-item-icon we-menu-item-icon-trailing"
                        #:hidden (~> trailing-icon
                                     (lambda (v)
                                       (if (eq? v #f) "hidden" #f))))
                  (if (eq? trailing-icon #f)
                      #f
                      (Span trailing-icon
                            #:data-we-widget "menu-item-icon"
                            #:class "we-menu-item-icon we-menu-item-icon-trailing"))))
            (apply html-element-children
                   (append
                    (list 'menu-item)
                    (if (eq? leading-view #f) '() (list leading-view))
                    (list (Span label
                                #:data-we-widget "menu-item-label"
                                #:class "we-menu-item-label"))
                    (if (eq? trailing-view #f) '() (list trailing-view))
                    (list #:attrs attrs/final))))))

    ;; list-view : (or/c list? observable?) (-> any/c any/c view?) [(-> any/c any/c)] -> view?
    ;;   Construct a keyed dynamic list container.
    ;;   Optional parameter key defaults to values.
    (define (list-view entries make-view [key values])
      (define items '())
      (define (list-view-after-render node entries0 register-cleanup! api)
        (define backend-replace-children!/api
          (cdr (assq 'backend-replace-children! api)))
        (define build-node/api
          (cdr (assq 'build-node api)))
        (define normalized-entries
          (ensure-list/internal entries0 'list-view "entries"))
        (define new-items
          (map (lambda (entry)
                 (define entry-key
                   (key entry))
                 (define old-item
                   (assoc entry-key items))
                 (if (and old-item
                          (equal? (list-ref old-item 1) entry))
                     (list entry-key
                           entry
                           (list-ref old-item 2))
                     (list entry-key
                           entry
                           (build-node/api (make-view entry-key entry)))))
               normalized-entries))
        (set! items new-items)
        (backend-replace-children!/api node (map (lambda (item) (list-ref item 2))
                                                 new-items)))
      (observable-element-children
       'div
       entries
       (lambda (_value) '())
       #:attrs (list (cons 'data-we-widget "list-view")
                     (cons 'class "we-list-view"))
       #:after-render list-view-after-render))


    ;; rich-list-group : list? (or/c any/c observable?) (-> any/c any/c) -> view?
    ;;   Construct a selectable list-group from entries, current id, and selection action.
    ;;   Entry labels may be rich views, not just strings.
    ;;   Accepts global HTML attributes for the root <div> via keyword arguments.
    ;; TODO : Improve list-group and the remove `rich-list-group`.
    (define/component rich-list-group
      #:root-tag 'div
      #:positional ([entries]
                    [current]
                    [action])
      #:root-attrs attrs/final
      (define @entries
        (observable-or-const entries))
      (define @current
        (observable-or-const current))
      (define @state
        (obs-combine list @entries @current))

      (define (make-list-group-children state)
        (define entries0
          (ensure-list/internal (car state) 'rich-list-group "entries"))
        (define current0
          (cadr state))
        (map (lambda (entry)
               (define item-id
                 (list-group-id/internal entry))
               (define item-label
                 (list-group-label/internal entry))
               (define current?
                 (equal? item-id current0))
               (define attrs0
                 (list (cons 'role 'listitem)
                       (cons 'data-we-widget "rich-list-group-item")
                       (cons 'class (if current?
                                        "we-list-group-item is-current"
                                        "we-list-group-item"))
                       (cons 'aria-current (if current? "true" "false"))))
               (button item-label
                       (lambda ()
                         (action item-id))
                       #:attrs attrs0))
             entries0))

      (define attrs/final
        (list (cons 'role 'list)
              (cons 'data-we-widget "rich-list-group")
              (cons 'class "we-list-group")))

      (observable-element-children 'div
                                   @state
                                   make-list-group-children
                                   #:attrs attrs/final))


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
            Fragment
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
            B
            I
            U
            S
            Mark
            Sub
            Sup
            Kbd
            Samp
            Var
            Q
            Cite
            Dfn
            Abbr
            Time
            Data
            Del
            Ins
            Br
            Wbr
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
            Menu
            Ol
            Li
            Dl
            Dt
            Dd
            Table
            Caption
            Thead
            Tbody
            Tfoot
            Tr
            Th
            Td
            Audio
            Video
            Source
            Track
            Canvas
            Iframe
            Embed
            Object
            Input
            Select
            Option
            Textarea
            Details
            Dialog
            Summary
            Figure
            Figcaption
            Hgroup
            Address
            Blockquote
            Ruby
            Rt
            Rp
            Bdi
            Bdo
            Progress
            Meter
            Output
            Fieldset
            Legend
            Datalist
            Optgroup
            Colgroup
            Col
            Map
            Area
            Script
            Link
            Meta
            Title
            Base
            Style
            Slot
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
            menu-item
            rich-list-group)))
