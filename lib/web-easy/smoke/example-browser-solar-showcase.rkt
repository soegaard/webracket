;;;
;;; web-easy Browser Solar Showcase
;;;

;; Minimal, user-facing Solar-inspired showcase page.
;; Scope: Bootswatch-style section showcase using supported web-easy widgets.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader    "smoke-format.rkt" read-syntax/skip-first-line)

;; Constants for page state.
(define @theme        (@ 'solar2))
(define @theme-status (@ "solar2"))
(define @geometry-mode (@ 'exact))
(define @selected-1   (@ 'home))
(define @selected-2   (@ 'home))
(define @selected-3   (@ 'home))
(define @selected-4   (@ 'home))
(define @query-1      (@ ""))
(define @query-2      (@ ""))
(define @query-3      (@ ""))
(define @query-4      (@ ""))
(define @button-log   (@ "No button clicks yet."))
(define @name         (@ ""))
(define @select-value (@ "1"))
(define @radio-value  (@ "Option one is this and that - be sure to include why it's great"))
(define @notes        (@ ""))
(define @accept?      (@ #f))
(define @crumb-1      (@ 'home))
(define @crumb-2      (@ 'library))
(define @crumb-3      (@ 'data))
(define @page         (@ 1))
(define @level        (@ 42))
(define @progress-kind (@ 'info))
(define @list-current (@ 'current))
(define @table-rows   (@ '(("alpha" "stable" "today")
                           ("beta" "pending" "tomorrow")
                           ("gamma" "blocked" "later"))))
(define @accordion    (@ 'what))
(define @dialog-open? (@ #f))
(define @offcanvas-open? (@ #f))
(define @modal-open?  (@ #f))
(define @toast-open?  (@ #t))
(define @toast-level  (@ 'info))
(define @toast-top-open? (@ #f))
(define @toast-left-open? (@ #f))
(define @tab          (@ "Home"))
(define @pill         (@ "Alpha"))
(define @underline-tab (@ "Active"))
(define @pill-vertical (@ "Active"))

;; normalize-theme-id : any/c -> symbol?
;;   Normalize incoming theme id to one of the supported symbols.
(define (normalize-theme-id v)
  (if (member v '(solar solar2 light dark))
      v
      'light))

;; theme-class-name : any/c -> string?
;;   Map theme id to html class used by external theme stylesheets.
(define (theme-class-name theme)
  (case theme
    [(light)   "we-theme-light"]
    [(dark)    "we-theme-dark"]
    [(solar)   "we-theme-solar"]
    [(solar2)  "we-theme-solar2"]
    [else      "we-theme-light"]))

;; theme-css-path/general : any/c -> string?
;;   Map theme id to general stylesheet path.
(define (theme-css-path/general theme)
  (case theme
    [(light)  "../theme-external-light.css"]
    [(dark)   "../theme-external-dark.css"]
    [(solar)  "../theme-external-solar.css"]
    [(solar2) "../theme-solar-2.css"]
    [else     "../theme-external-light.css"]))

;; theme-css-path/showcase : any/c -> string?
;;   Map theme id to page-specific stylesheet path.
(define (theme-css-path/showcase theme)
  (case theme
    [(light)  "../theme-showcase-light.css"]
    [(dark)   "../theme-showcase-dark.css"]
    [(solar)  "../theme-showcase-solar.css"]
    [(solar2) "../theme-showcase-solar2.css"]
    [else     "../theme-showcase-light.css"]))

;; theme-css-path/core : -> string?
;;   Path to shared web-easy structural core stylesheet.
(define (theme-css-path/core)
  "../web-easy-core.css")

;; install-theme-link! : string? -> any/c
;;   Create and attach a stylesheet <link> with the given id.
(define (install-theme-link! link-id)
  (define doc  (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id" link-id)
  (js-set-attribute! link "rel" "stylesheet")
  (js-append-child! head link)
  link)

;; apply-theme! : any/c any/c any/c any/c -> void?
;;   Update html class and stylesheet hrefs for the selected theme.
(define (apply-theme! core-link general-link showcase-link theme)
  ;; Cache-bust external CSS so browser refresh always picks up latest local edits.
  (define cache-token "20260310b")
  (define (with-cache-bust path)
    (~a path "?v=" cache-token))
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (js-set-attribute! html-node     "class" (theme-class-name theme))
  (js-set-attribute! core-link     "href"  (theme-css-path/core))
  (js-set-attribute! general-link  "href"  (with-cache-bust (theme-css-path/general theme)))
  (js-set-attribute! showcase-link "href"  (with-cache-bust (theme-css-path/showcase theme)))
  (void))

;; apply-geometry-mode! : any/c -> void?
;;   Set html data attribute for showcase geometry mode.
(define (apply-geometry-mode! mode)
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (define value
    (case mode
      [(exact) "exact"]
      [else    "comfortable"]))
  (js-set-attribute! html-node "data-showcase-geometry" value)
  (void))

;; nav-item-button : observable? symbol? string? -> view?
;;   Build a navbar item that updates the selected id when clicked.
(define (nav-item-button @selected id label)
  (button label
          (lambda ()
            (:= @selected id))))

;; navbar-demo : observable? observable? -> view?
;;   Build one navbar row with brand, links, dropdown, and search form.
(define (navbar-demo @selected @query)
  (navigation-bar
   (with-class "we-navbar-brand"
     (text "Navbar"))
   (nav-item-button @selected 'home     "Home")
   (nav-item-button @selected 'features "Features")
   (nav-item-button @selected 'pricing  "Pricing")
   (nav-item-button @selected 'about    "About")
   (menu-bar
    (menu "Dropdown"
          (menu-item "Action"         (lambda () (void)))
          (menu-item "Another action" (lambda () (void)))
          (menu-item "Something else here" (lambda () (void)))
          (divider 'horizontal)
          (menu-item "Separated link" (lambda () (void)))))
   (spacer)
   (hpanel
    (with-attrs '((placeholder "Search"))
      (input @query (lambda (v) (:= @query v))))
    (button "Search" (lambda () (void))))))

;; log-button! : string? -> void?
;;   Record a showcase button click in the status line.
(define (log-button! label)
  (:= @button-log (~a "Clicked: " label)))

;; showcase-button : string? string? boolean? -> view?
;;   Build one showcase button with a variant class and optional disabled state.
(define (showcase-button label variant-class disabled?)
  (with-class variant-class
    (if disabled?
        (with-attrs '((disabled "disabled"))
          (button label (lambda () (void))))
        (button label
                (lambda ()
                  (log-button! label))))))

;; progress-fill-class : symbol? boolean? boolean? -> string?
;;   Build fill class string for showcase progress bars.
(define (progress-fill-class variant striped? animated?)
  (define variant-class
    (case variant
      [(success) "showcase-progress-fill-success"]
      [(info)    "showcase-progress-fill-info"]
      [(warn)    "showcase-progress-fill-warn"]
      [(error)   "showcase-progress-fill-error"]
      [else      "showcase-progress-fill-default"]))
  (string-append "showcase-progress-fill "
                 variant-class
                 (if striped? " showcase-progress-fill-striped" "")
                 (if animated? " showcase-progress-fill-animated" "")))

;; progress-fill-color : symbol? -> string?
;;   Map progress variant to Solar showcase fill class suffix.
(define (progress-fill-color variant)
  (case variant
    [(success) "showcase-progress-fill-success"]
    [(info)    "showcase-progress-fill-info"]
    [(warn)    "showcase-progress-fill-warn"]
    [(error)   "showcase-progress-fill-error"]
    [else      "showcase-progress-fill-default"]))

;; progress-width-class : number? -> string?
;;   Map known showcase percentage values to width utility classes.
(define (progress-width-class pct)
  (case pct
    [(10)  "showcase-progress-w-10"]
    [(15)  "showcase-progress-w-15"]
    [(20)  "showcase-progress-w-20"]
    [(25)  "showcase-progress-w-25"]
    [(30)  "showcase-progress-w-30"]
    [(50)  "showcase-progress-w-50"]
    [(75)  "showcase-progress-w-75"]
    [(100) "showcase-progress-w-100"]
    [else  "showcase-progress-w-0"]))

;; progress-fill : number? symbol? [boolean?] [boolean?] -> view?
;;   Build one showcase progress fill segment with width percentage and variant class.
;;   Optional parameter striped? defaults to #f.
;;   Optional parameter animated? defaults to #f.
(define (progress-fill pct variant [striped? #f] [animated? #f])
  (with-class (string-append (progress-fill-class variant striped? animated?)
                             " "
                             (progress-fill-color variant)
                             " "
                             (progress-width-class pct))
    (text " ")))

;; progress-track : view? ... -> view?
;;   Build one showcase progress track containing one or more fill segments.
(define (progress-track . fills)
  (with-class "showcase-progress-track"
    (with-class "showcase-progress-track-inner"
      (with-class "showcase-progress-fill-row"
        (apply inline fills)))))

;; showcase-list-group-row : string? number? -> view?
;;   Build one list-group row with a right-aligned badge count.
(define (showcase-list-group-row label count)
  (with-class "we-list-group-item showcase-list-group-row"
    (inline
     (text label)
     (spacer)
     (with-class "showcase-list-group-badge"
       (badge (~a count) 'primary)))))

;; showcase-static-list-group : (listof (list string? number?)) [string?] -> view?
;;   Build a static list-group with badge rows; optional extra class decorates the group.
;;   Optional parameter extra-class defaults to #f.
(define (showcase-static-list-group entries [extra-class #f])
  (define rows
    (map (lambda (entry)
           (showcase-list-group-row (first entry) (second entry)))
         entries))
  (define group-view
    (with-class "showcase-list-group-panel"
      (apply stack rows)))
  (if extra-class
      (with-class (string-append "we-list-group " extra-class)
        group-view)
      (with-class "we-list-group"
        group-view)))

;; showcase-rich-list-item : string? string? string? string? [boolean?] -> view?
;;   Build one rich list-group row with heading, meta text, body text and muted footnote.
;;   Optional parameter active? defaults to #f.
(define (showcase-rich-list-item heading meta body footnote [active? #f])
  (with-class (if active?
                  "we-list-group-item showcase-rich-list-item is-active"
                  "we-list-group-item showcase-rich-list-item")
    (stack
     (inline
      (with-class "showcase-rich-list-heading"
        (text heading))
      (spacer)
      (with-class "showcase-rich-list-meta"
        (text meta)))
     (with-class "showcase-rich-list-body"
       (text body))
     (with-class "showcase-rich-list-footnote"
       (text footnote)))))

;; showcase-rich-list-group : -> view?
;;   Build the rich list-group sample used in the Containers section.
(define (showcase-rich-list-group)
  (with-class "we-list-group showcase-rich-list-group"
    (with-class "showcase-list-group-panel"
      (stack
       (showcase-rich-list-item
        "List group item heading"
        "3 days ago"
        "Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit."
        "Donec id elit non mi porta."
        #t)
       (showcase-rich-list-item
        "List group item heading"
        "3 days ago"
        "Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit."
        "Donec id elit non mi porta.")))))

;; showcase-accordion-body : string? string? -> view?
;;   Build accordion panel text with lead sentence plus full reference-style copy.
(define (showcase-accordion-body lead copy)
  (stack
   (with-class "showcase-accordion-lead"
     (text lead))
   (with-class "showcase-accordion-copy"
     (text copy))))

(define app-renderer
  (render
   (window
    (with-class "showcase-shell"
      (container
       (stack
        ;;;
        ;;; Top Bar
        ;;;
        (with-id "theme-showcase-hero"
          (with-class "showcase-topbar"
            (card #f #f
             (with-class "showcase-topbar-inner"
               (inline
                (with-class "showcase-brand"
                  (stack
                   (with-class "showcase-brand-title"
                     (text "web-easy"))
                   (with-class "showcase-brand-subtitle"
                     (text "Solar showcase"))))
                (spacer)
                (with-class "showcase-topbar-controls"
                  (stack
                   (with-class "showcase-controls-label"
                     (text "Theme"))
                   (with-class "showcase-theme-choice"
                     (choice '((solar2 "Solar 2") (solar "Solar") (light "Light") (dark "Dark"))
                             @theme
                             (lambda (next-theme)
                               (:= @theme (normalize-theme-id next-theme))
                               (:= @theme-status (~a next-theme))))
                   )
                   (with-class "showcase-controls-label"
                     (text "Geometry"))
                   (with-class "showcase-theme-choice"
                     (choice '((comfortable "Comfortable") (exact "Exact"))
                             @geometry-mode
                             (lambda (next-mode)
                               (:= @geometry-mode next-mode))))))))))))
        ;;;
        ;;; Hero
        ;;;
        (with-class "showcase-hero"
          (card #f #f
                (with-class "showcase-hero-title"
                  (text "Solar 2 Theme"))
                (with-class "showcase-hero-lead"
                  (text "Top-level sections aligned with the Bootswatch Solar showcase layout."))))

        ;;;
        ;;; Main
        ;;;
        
        (with-class "showcase-main"
          (stack
           ;; Navbars
           (with-id "solar2-navbars"
             (with-class "showcase-section-title"
              (heading 1 "Navbars")))
           (with-class "we-flow"
             (stack
              (with-class "we-variant-primary"
                (navbar-demo @selected-1 @query-1))
              (with-class "we-variant-dark"
                (navbar-demo @selected-2 @query-2))
              (with-class "we-variant-light"
                (navbar-demo @selected-3 @query-3))
              (with-class "we-variant-subtle"
                (navbar-demo @selected-4 @query-4))))

           ;; Buttons
           (with-id "solar2-buttons"
             (with-class "showcase-section-title"
              (heading 1 "Buttons")))
           (with-class "we-button-row"
             (inline
              (showcase-button "Primary"   "we-btn-primary"   #f)
              (showcase-button "Secondary" "we-btn-secondary" #f)
              (showcase-button "Success"   "we-btn-success"   #f)
              (showcase-button "Info"      "we-btn-info"      #f)
              (showcase-button "Warning"   "we-btn-warning"   #f)
              (showcase-button "Danger"    "we-btn-danger"    #f)
              (showcase-button "Light"     "we-btn-light"     #f)
              (showcase-button "Dark"      "we-btn-dark"      #f)
              (showcase-button "Link"      "we-btn-link"      #f)))
           (with-class "we-button-row"
             (inline
              (showcase-button "Primary"   "we-btn-primary"   #t)
              (showcase-button "Secondary" "we-btn-secondary" #t)
              (showcase-button "Success"   "we-btn-success"   #t)
              (showcase-button "Info"      "we-btn-info"      #t)
              (showcase-button "Warning"   "we-btn-warning"   #t)
              (showcase-button "Danger"    "we-btn-danger"    #t)
              (showcase-button "Light"     "we-btn-light"     #t)
              (showcase-button "Dark"      "we-btn-dark"      #t)
              (showcase-button "Link"      "we-btn-link"      #t)))
           (with-class "we-button-row"
             (inline
              (showcase-button "Primary"   "we-btn-outline-primary"   #f)
              (showcase-button "Secondary" "we-btn-outline-secondary" #f)
              (showcase-button "Success"   "we-btn-outline-success"   #f)
              (showcase-button "Info"      "we-btn-outline-info"      #f)
              (showcase-button "Warning"   "we-btn-outline-warning"   #f)
              (showcase-button "Danger"    "we-btn-outline-danger"    #f)
              (showcase-button "Light"     "we-btn-outline-light"     #f)
              (showcase-button "Dark"      "we-btn-outline-dark"      #f)))
           (with-class "we-button-row"
             (inline
              (with-class "we-btn-lg"
                (showcase-button "Large button" "we-btn-primary" #f))
              (showcase-button "Default button" "we-btn-primary" #f)
              (with-class "we-btn-sm"
                (showcase-button "Small button" "we-btn-primary" #f))))
           (with-class "we-button-row"
             (button-group
              (button "Left"   (lambda () (log-button! "Group/Left")))
              (button "Middle" (lambda () (log-button! "Group/Middle")))
              (button "Right"  (lambda () (log-button! "Group/Right")))))
           (with-class "we-button-row"
             (button-toolbar
              (toolbar-group
               (button "1" (lambda () (log-button! "Toolbar/1")))
               (button "2" (lambda () (log-button! "Toolbar/2"))))
              (toolbar-group
               (button "A" (lambda () (log-button! "Toolbar/A")))
               (button "B" (lambda () (log-button! "Toolbar/B"))))))
           (with-class "we-button-row showcase-block-button-row"
             (showcase-button "Block button" "we-btn-primary" #f))

           ;; Typography
           (with-id "solar2-typography"
             (with-class "showcase-section-title"
              (heading 1 "Typography")))
           (with-class "showcase-typography-grid"
             (grid
              3
              (vpanel
               (heading 1 "Heading 1")
               (heading 2 "Heading 2")
               (heading 3 "Heading 3")
               (heading 4 "Heading 4")
               (heading 5 "Heading 5")
               (heading 6 "Heading 6")
               (heading-with-subtitle 3 "Heading" "with faded secondary text")
               (with-class "showcase-typography-lead"
                 (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus dolor auctor.")))
              (vpanel
               (heading 2 "Example body text")
               (inline
                (text "Nullam quis risus eget ")
                (link "urna mollis ornare" "#")
                (text " vel eu leo. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."))
               (with-class "showcase-fine-print"
                 (text "This line of text is meant to be treated as fine print."))
               (inline
                (text "The following is ")
                (with-class "showcase-text-strong"
                  (text "rendered as bold text"))
                (text "."))
               (inline
                (text "The following is ")
                (with-class "showcase-text-emphasis"
                  (text "rendered as italicized text"))
                (text "."))
               (inline
                (text "An abbreviation of the word attribute is ")
                (with-attrs '((title "attribute"))
                  (with-class "showcase-abbr"
                    (text "attr")))
                (text ".")))
              (vpanel
               (heading 2 "Emphasis classes")
               (with-class "showcase-text-primary"
                 (text "text-primary"))
               (with-class "showcase-text-primary-emphasis"
                 (text "text-primary-emphasis"))
               (with-class "showcase-text-secondary"
                 (text "text-secondary"))
               (with-class "showcase-text-secondary-emphasis"
                 (text "text-secondary-emphasis"))
               (with-class "showcase-text-success"
                 (text "text-success"))
               (with-class "showcase-text-success-emphasis"
                 (text "text-success-emphasis"))
               (with-class "showcase-text-danger"
                 (text "text-danger"))
               (with-class "showcase-text-danger-emphasis"
                 (text "text-danger-emphasis"))
               (with-class "showcase-text-warning"
                 (text "text-warning"))
               (with-class "showcase-text-warning-emphasis"
                 (text "text-warning-emphasis"))
               (with-class "showcase-text-info"
                 (text "text-info"))
               (with-class "showcase-text-info-emphasis"
                 (text "text-info-emphasis"))
               (with-class "showcase-text-light"
                 (text "text-light"))
               (with-class "showcase-text-light-emphasis"
                 (text "text-light-emphasis"))
               (with-class "showcase-text-dark"
                 (text "text-dark"))
               (with-class "showcase-text-dark-emphasis"
                 (text "text-dark-emphasis"))
               (with-class "showcase-text-body"
                 (text "text-body"))
               (with-class "showcase-text-body-emphasis"
                 (text "text-body-emphasis"))
               (with-class "showcase-text-body-secondary"
                 (text "text-body-secondary"))
               (with-class "showcase-text-tertiary"
                 (text "text-body-tertiary")))))
           (heading 2 "Blockquotes")
           (with-class "showcase-typography-grid"
             (grid
              3
              (with-class "showcase-blockquote"
                (blockquote
                 "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
                 "Someone famous in Source Title"))
              (with-class "showcase-blockquote showcase-blockquote-center"
                (blockquote
                 "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
                 "Someone famous in Source Title"))
              (with-class "showcase-blockquote showcase-blockquote-right"
                (blockquote
                 "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
                 "Someone famous in Source Title"))
             )
           )
           (with-class "showcase-inline-code"
             (text "(render (window (text \"hello\")))"))
           (with-class "showcase-pre"
             (text "(define app\n  (render\n   (window\n    (vpanel (text \"A\") (text \"B\")))))"))

           ;; Tables
           (with-id "solar2-tables"
             (with-class "showcase-section-title"
              (heading 1 "Tables")))
           (table '("Type" "Column heading" "Column heading" "Column heading")
                  '(("Active"    "Column content" "Column content" "Column content")
                    ("Default"   "Column content" "Column content" "Column content")
                    ("Primary"   "Column content" "Column content" "Column content")
                    ("Secondary" "Column content" "Column content" "Column content")
                    ("Success"   "Column content" "Column content" "Column content")
                    ("Danger"    "Column content" "Column content" "Column content")
                    ("Warning"   "Column content" "Column content" "Column content")
                    ("Info"      "Column content" "Column content" "Column content")
                    ("Light"     "Column content" "Column content" "Column content")
                    ("Dark"      "Column content" "Column content" "Column content"))
                  'normal
                  '((variants . (hover))
                    (row-variants . (active #f primary secondary success danger warning info light dark))
                    (row-header-column . 0)))

           ;; Forms
           (with-id "solar2-forms"
             (with-class "showcase-section-title"
              (heading 1 "Forms")))
           (with-class "showcase-forms-grid"
             (grid
              2
              (with-class "showcase-forms-left"
                (stack
                 (group "Legend"
                   (with-class "we-form-row showcase-static-row"
                     (inline
                      (with-class "showcase-static-label"
                        (text "Email"))
                      (with-class "showcase-static-value"
                        (with-class "we-form-control-plaintext"
                          (with-attrs '((readonly "readonly") (value "email@example.com"))
                            (input "" (lambda (_v) (void))))))))
                   (with-class "we-form-row we-form-label"
                     (text "Email address"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleInputEmail1")
                                   (placeholder "Enter email")
                                   (type "email"))
                       (input @name (lambda (v) (:= @name v)))))
                   (with-class "we-form-help"
                     (text "We'll never share your email with anyone else."))
                   (with-class "we-form-row we-form-label"
                     (text "Password"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleInputPassword1")
                                   (placeholder "Password")
                                   (type "password")
                                   (autocomplete "off"))
                       (input @notes (lambda (v) (:= @notes v)))))
                   (with-class "we-form-row we-form-label"
                     (text "Example select"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleSelect1"))
                       (choice '("1" "2" "3" "4" "5")
                               @select-value
                               (lambda (v) (:= @select-value v)))))
                   (with-class "we-form-row we-form-label"
                     (text "Example disabled select"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleDisabledSelect1")
                                   (disabled "disabled"))
                       (choice '("1" "2" "3" "4" "5")
                               "1"
                               (lambda (_v) (void)))))
                   (with-class "we-form-row we-form-label"
                     (text "Example multiple select"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleSelect2")
                                   (multiple "multiple")
                                   (size "5"))
                       (choice '("1" "2" "3" "4" "5")
                               "1"
                               (lambda (_v) (void)))))
                   (with-class "we-form-row we-form-label"
                     (text "Example textarea"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((id "exampleTextarea"))
                       (textarea @notes
                                 (lambda (v) (:= @notes v))
                                 3
                                 '())))
                   (with-class "we-form-row"
                     (text "Default file input example"))
                   (with-class "we-form-row we-form-control-block"
                     (with-attrs '((type "file"))
                       (input "" (lambda (_v) (void))))
                   )
                   (with-class "we-form-row"
                     (group "Radio buttons"
                       (radios '("Option one is this and that - be sure to include why it's great"
                                 "Option two can be something else and selecting it will deselect option one"
                                 ("Option three is disabled"
                                  "Option three is disabled"
                                  #t))
                               @radio-value
                               (lambda (v) (:= @radio-value v)))))
                   (with-class "we-form-row"
                     (group "Checkboxes"
                       (stack
                        (with-class "we-form-check we-checkbox-row"
                          (inline
                           (checkbox @accept?
                                     (lambda (v) (:= @accept? v)))
                           (text "Default checkbox")))
                        (with-class "we-form-check we-checkbox-row"
                          (inline
                           (checkbox #t (lambda (_v) (void)))
                           (text "Checked checkbox"))))))
                   (with-class "we-form-row"
                     (group "Switches"
                       (stack
                        (with-class "we-form-check we-switch-row"
                          (inline
                           (with-class "we-switch-control"
                             (checkbox #f (lambda (_v) (void))))
                           (text "Default switch checkbox input")))
                        (with-class "we-form-check we-switch-row"
                          (inline
                           (with-class "we-switch-control"
                             (checkbox #t (lambda (_v) (void))))
                           (text "Checked switch checkbox input"))))))
                   (with-class "we-form-row"
                     (heading 3 "Ranges"))
                   (with-class "we-form-row"
                     (text "Example range"))
                   (with-class "we-form-row we-form-control-block we-range-default"
                     (with-attrs '((id "customRange1"))
                       (slider @level (lambda (v) (:= @level v)) 0 100)))
                   (with-class "we-form-row"
                     (text "Disabled range"))
                   (with-class "we-form-row we-range-disabled"
                     (with-attrs '((disabled "disabled"))
                       (slider 50 (lambda (_v) (void)) 0 100)))
                   (with-class "we-form-row"
                     (text "Example range"))
                   (with-class "we-form-row we-range-step"
                     (with-attrs '((min "0") (max "5") (step "0.5"))
                       (slider 3 (lambda (_v) (void)) 0 5)))
                   (with-class "we-form-row"
                     (with-class "we-btn-primary"
                       (inline
                        (button "Submit" (lambda () (void)))))))))
                 (with-class "showcase-forms-right"
                (stack
                 (with-class "we-form-row"
                   (text "Disabled input"))
                 (with-class "we-form-row we-form-control-block we-form-state-disabled"
                   (with-attrs '((id "disabledInput")
                                 (placeholder "Disabled input here...")
                                 (disabled "disabled"))
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-row"
                   (text "Readonly input"))
                 (with-class "we-form-row we-form-control-block"
                   (with-attrs '((id "readOnlyInput")
                                 (placeholder "Readonly input here...")
                                 (readonly "readonly"))
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-row"
                   (text "Valid input"))
                 (with-class "we-form-state-valid we-form-control-block showcase-field-valid"
                   (with-attrs '((id "inputValid")
                                 (value "correct value"))
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-feedback we-form-feedback-valid"
                   (text "Success! You've done it."))
                 (with-class "we-form-row"
                   (text "Invalid input"))
                 (with-class "we-form-state-invalid we-form-control-block showcase-field-invalid"
                   (with-attrs '((id "inputInvalid")
                                 (value "wrong value"))
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-feedback we-form-feedback-invalid"
                   (text "Sorry, that username's taken. Try another?"))
                 (with-class "we-form-row"
                   (text "Large input"))
                 (with-attrs '((placeholder ".form-control-lg"))
                   (with-class "we-input-lg"
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-row"
                   (text "Default input"))
                 (with-class "showcase-field-default"
                   (with-attrs '((placeholder "Default input"))
                   (input "" (lambda (_v) (void)))
                   ))
                 (with-class "we-form-row"
                   (text "Small input"))
                 (with-attrs '((placeholder ".form-control-sm"))
                   (with-class "we-input-sm"
                     (input "" (lambda (_v) (void))))
                 )
                 (with-class "we-form-row"
                   (text "Input addons"))
                 (with-class "we-input-group"
                   (inline
                    (with-class "we-input-group-prefix"
                      (text "$"))
                    (input "" (lambda (_v) (void)))
                    (with-class "we-input-group-prefix"
                      (text ".00"))))
                 (with-class "we-input-group"
                   (inline
                    (input "" (lambda (_v) (void)))
                    (button "Button" (lambda () (void)))))
                 (with-class "we-form-row"
                   (text "Floating labels"))
                 (with-class "we-floating-field"
                   (stack
                    (text "Email address")
                    (with-attrs '((placeholder "name@example.com"))
                      (input "" (lambda (_v) (void)))))
                 )
                 (with-class "we-floating-field"
                   (stack
                    (text "Password")
                    (with-attrs '((placeholder "Password") (type "password") (autocomplete "off"))
                      (input "" (lambda (_v) (void)))))
                 )))))

           ;; Navs
           (with-id "solar2-navs-section"
             (stack
              (with-id "solar2-navs"
                (with-class "showcase-section-title"
                 (heading 1 "Navs")))
              (grid
               2
               (stack
                (heading 2 "Tabs")
                (tab-panel
                 @tab
                 (list (cons "Home"
                             (text "Raw denim you probably haven't heard of them jean shorts Austin. Nesciunt tofu stumptown aliqua, retro synth master cleanse. Mustache cliche tempor, williamsburg carles vegan helvetica. Reprehenderit butcher retro keffiyeh dreamcatcher synth. Cosby sweater eu banh mi, qui irure terry richardson ex squid. Aliquip placeat salvia cillum iphone. Seitan aliquip quis cardigan american apparel, butcher voluptate nisi qui."))
                       (cons "Profile"
                             (text "Food truck fixie locavore, accusamus mcsweeney's marfa nulla single-origin coffee squid. Exercitation +1 labore velit, blog sartorial PBR leggings next level wes anderson artisan four loko farm-to-table craft beer twee. Qui photo booth letterpress, commodo enim craft beer mlkshk aliquip jean shorts ullamco ad vinyl cillum PBR. Homo nostrud organic, assumenda labore aesthetic magna delectus mollit."))
                       (cons "Disabled"
                             (text "Etsy mixtape wayfarers, ethical wes anderson tofu before they sold out mcsweeney's organic lomo retro fanny pack lo-fi farm-to-table readymade. Messenger bag gentrify pitchfork tattooed craft beer, iphone skateboard locavore carles etsy salvia banksy hoodie helvetica. DIY synth PBR banksy irony. Leggings gentrify squid 8-bit cred pitchfork."))
                       (cons "Dropdown ▾"
                             (text "Trust fund seitan letterpress, keytar raw denim keffiyeh etsy art party before they sold out master cleanse gluten-free squid scenester freegan cosby sweater. Fanny pack portland seitan DIY, art party locavore wolf cliche high life echo park Austin. Cred vinyl keffiyeh DIY salvia PBR, banh mi before they sold out farm-to-table VHS viral locavore cosby sweater.")))))
               (stack
                (heading 2 "Pills")
                (with-class "we-tab-style-pills"
                  (tab-panel
                   @pill
                   (list (cons "Active"   (text "Active pill content."))
                         (cons "Dropdown ▾" (text "Dropdown pill content."))
                         (cons "Link"     (text "Link pill content."))
                         (cons "Disabled" (text "Disabled pill content.")))))
                (with-class "we-tab-style-pills we-nav-pills-vertical"
                  (tab-panel
                   @pill-vertical
                   (list (cons "Active"   (text "Active vertical pill content."))
                         (cons "Dropdown ▾" (text "Dropdown vertical content."))
                         (cons "Link"     (text "Link vertical content."))
                         (cons "Disabled" (text "Disabled vertical content.")))))))
              (grid
               2
               (stack
                (heading 2 "Breadcrumbs")
                (breadcrumb '((home "Home"))
                            @crumb-1
                            (lambda (v) (:= @crumb-1 v)))
                (breadcrumb '((home "Home")
                              (library "Library"))
                            @crumb-2
                            (lambda (v) (:= @crumb-2 v)))
                (breadcrumb '((home "Home")
                              (library "Library")
                              (data "Data"))
                            @crumb-3
                            (lambda (v) (:= @crumb-3 v))))
               (stack
                (heading 2 "Pagination")
                (pagination 5 @page (lambda (v) (:= @page v)))
                (with-class "we-pagination-lg"
                  (pagination 5 @page (lambda (v) (:= @page v))))
                (with-class "we-pagination-sm"
                  (pagination 5 @page (lambda (v) (:= @page v))))))
              (heading 2 "Underline")
              (with-class "we-tab-style-underline"
                (tab-panel
                 @underline-tab
                 (list (cons "Active"   (text "Underline active content."))
                       (cons "Link"     (text "Underline link content."))
                       (cons "Link​"    (text "Underline link two content."))
                       (list "Disabled" (text "Underline disabled content.") #t))))))

           ;; Indicators
           (with-id "solar2-indicators-section"
             (stack
              (with-id "solar2-indicators"
                (with-class "showcase-section-title"
                 (heading 1 "Indicators")))
              (heading 2 "Alerts")
              (with-class "showcase-alert-heading showcase-alert-major"
                (alert-rich
                 "Best check yo self, you're not looking too good. Nulla vitae elit libero, a pharetra augue. Praesent commodo cursus magna,"
                 "Warning!"
                 "vel scelerisque nisl consectetur et"
                 "#"
                 'warn))
              (grid
               3
               (with-class "showcase-alert-heading showcase-alert-compact"
                 (alert-rich "Change a few things up"
                             "Oh snap!"
                             "and try submitting again."
                             "#"
                             'error))
               (with-class "showcase-alert-heading showcase-alert-compact"
                 (alert-rich "You successfully read"
                             "Well done!"
                             "this important alert message"
                             "#"
                             'success))
               (with-class "showcase-alert-heading showcase-alert-compact"
                 (alert-rich "This"
                             "Heads up!"
                             "alert needs your attention, but it's not super important."
                             "#"
                             'info)))
              (grid
               3
               (with-class "showcase-alert-heading showcase-alert-compact showcase-alert-primary"
                 (alert-rich "Change a few things up"
                             "Oh snap!"
                             "and try submitting again."
                             "#"
                             'info))
               (with-class "showcase-alert-heading showcase-alert-compact showcase-alert-secondary"
                 (alert-rich "You successfully read"
                             "Well done!"
                             "this important alert message"
                             "#"
                             'info))
               (with-class "showcase-alert-heading showcase-alert-compact showcase-alert-light"
                 (alert-rich "This"
                             "Heads up!"
                             "alert needs your attention, but it's not super important."
                             "#"
                             'info)))
              (heading 2 "Badges")
              (with-class "we-button-row showcase-badge-row-square"
                (inline
                 (badge "Primary" 'primary)
                 (badge "Secondary" 'secondary)
                 (badge "Success" 'success)
                 (badge "Danger" 'danger)
                 (badge "Warning" 'warning)
                 (badge "Info" 'info)
                 (badge "Light" 'light)
                 (badge "Dark" 'dark)))
              (with-class "we-button-row showcase-badge-row-pill"
                (inline
                 (badge "Primary" 'primary)
                 (badge "Secondary" 'secondary)
                 (badge "Success" 'success)
                 (badge "Danger" 'danger)
                 (badge "Warning" 'warning)
                 (badge "Info" 'info)
                 (badge "Light" 'light)
                 (badge "Dark" 'dark)))))

           ;; Progress
           (with-id "solar2-progress-section"
             (stack
              (with-id "solar2-progress"
                (with-class "showcase-section-title"
                 (heading 1 "Progress")))
              (heading 3 "Basic")
              (progress-track
               (progress-fill 25 'default))
              (heading 3 "Contextual alternatives")
              (progress-track (progress-fill 25 'success))
              (progress-track (progress-fill 50 'info))
              (progress-track (progress-fill 75 'warn))
              (progress-track (progress-fill 100 'error))
              (heading 3 "Multiple bars")
              (progress-track
               (progress-fill 15 'default)
               (progress-fill 30 'success)
               (progress-fill 20 'info))
              (heading 3 "Striped")
              (progress-track (progress-fill 10 'default #t))
              (progress-track (progress-fill 25 'success #t))
              (progress-track (progress-fill 50 'info #t))
              (progress-track (progress-fill 75 'warn #t))
              (progress-track (progress-fill 100 'error #t))
              (heading 3 "Animated")
              (progress-track (progress-fill 75 'default #t #t))))

           ;; Containers
           (with-id "solar2-containers"
             (with-class "showcase-section-title"
              (heading 1 "Containers")))
           (heading 2 "List groups")
           (grid
            3
            (stack
             (showcase-static-list-group '(("Cras justo odio" 14)
                                           ("Dapibus ac facilisis in" 2)
                                           ("Morbi leo risus" 1)))
             (showcase-static-list-group '(("Primary row" 14)
                                           ("Secondary row" 2)
                                           ("Success row" 1)
                                           ("Info row" 5)
                                           ("Warning row" 4)
                                           ("Danger row" 9)
                                           ("Light row" 8)
                                           ("Dark row" 0))
                                       "showcase-list-variant"))
            (stack
             (list-group '((active "Active")
                           (link "Link")
                           (disabled "Disabled"))
                         @list-current
                         (lambda (v) (:= @list-current v))))
            (stack
             (showcase-rich-list-group)))

           ;; Cards
           (with-id "solar2-cards"
             (with-class "showcase-section-title"
              (heading 1 "Cards")))
           (heading 2 "Cards")
           (with-id "solar2-cards-body"
            (grid
             3
            (stack
             (with-class "showcase-card-bg-primary"
               (card "Header" #f
                     (heading 4 "Primary card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-secondary"
               (card "Header" #f
                     (heading 4 "Secondary card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-success"
               (card "Header" #f
                     (heading 4 "Success card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-danger"
               (card "Header" #f
                     (heading 4 "Danger card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-warning"
               (card "Header" #f
                     (heading 4 "Warning card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-info"
               (card "Header" #f
                     (heading 4 "Info card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-light"
               (card "Header" #f
                     (heading 4 "Light card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-bg-dark"
               (card "Header" #f
                     (heading 4 "Dark card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content."))))
            (stack
             (with-class "showcase-card-border-primary"
               (card "Header" #f
                     (heading 4 "Primary card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-secondary"
               (card "Header" #f
                     (heading 4 "Secondary card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-success"
               (card "Header" #f
                     (heading 4 "Success card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-danger"
               (card "Header" #f
                     (heading 4 "Danger card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-warning"
               (card "Header" #f
                     (heading 4 "Warning card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-info"
               (card "Header" #f
                     (heading 4 "Info card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-light"
               (card "Header" #f
                     (heading 4 "Light card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content.")))
             (with-class "showcase-card-border-dark"
               (card "Header" #f
                     (heading 4 "Dark card title")
                     (text "Some quick example text to build on the card title and make up the bulk of the card's content."))))
            (stack
             (card "Card header" "2 days ago"
                   (stack
                    (heading 5 "Special title treatment")
                    (with-class "showcase-card-subtitle"
                      (text "Support card subtitle"))
                    (with-class "showcase-card-image-cap"
                      (text "Image cap"))
                    (text "Some quick example text to build on the card title and make up the bulk of the card's content.")
                    (with-class "we-list-group"
                      (stack
                       (with-class "we-list-group-item"
                         (text "Cras justo odio"))
                       (with-class "we-list-group-item"
                         (text "Dapibus ac facilisis in"))
                       (with-class "we-list-group-item"
                         (text "Vestibulum at eros"))))
                    (with-class "showcase-card-links"
                      (inline
                       (link "Card link" "#")
                       (link "Another link" "#")))))
             (card #f #f
                   (heading 4 "Card title")
                   (with-class "showcase-card-subtitle"
                     (text "Card subtitle"))
                   (text "Some quick example text to build on the card title and make up the bulk of the card's content.")
                   (with-class "showcase-card-links"
                     (inline
                      (link "Card link" "#")
                      (link "Another link" "#")))))))

           ;; Accordions
           (with-id "solar2-accordions"
             (with-class "showcase-section-title"
              (heading 1 "Accordions")))
           (with-id "solar2-accordions-body"
            (with-class "showcase-accordion-wrap"
             (accordion
             @accordion
             (list
               (list 'what
                     "Accordion Item #1"
                     (showcase-accordion-body
                      "This is the first item's accordion body."
                      "It is shown by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. It's also worth noting that just about any HTML can go within the .accordion-body, though the transition does limit overflow."))
               (list 'themes
                     "Accordion Item #2"
                     (showcase-accordion-body
                      "This is the second item's accordion body."
                      "It is hidden by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. It's also worth noting that just about any HTML can go within the .accordion-body, though the transition does limit overflow."))
               (list 'third
                     "Accordion Item #3"
                     (showcase-accordion-body
                      "This is the third item's accordion body."
                      "It is hidden by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. It's also worth noting that just about any HTML can go within the .accordion-body, though the transition does limit overflow."))))))

           ;; Dialogs
           (with-id "solar2-dialogs"
             (with-class "showcase-section-title"
              (heading 1 "Dialogs")))
           (with-id "solar2-dialogs-body"
            (with-class "showcase-dialogs-grid"
             (grid
              2
              (stack
               (heading 2 "Modals")
               (with-class "we-modal-panel showcase-static-modal"
                 (stack
                  (with-class "showcase-static-modal-header"
                    (inline
                     (heading 5 "Modal title")
                     (spacer)
                     (text "×")))
                  (with-class "showcase-static-modal-body"
                    (text "Modal body text goes here."))
                  (with-class "we-button-row"
                    (inline
                     (spacer)
                     (with-class "we-btn-primary"
                       (button "Save changes" (lambda () (void))))
                     (with-class "we-btn-secondary"
                       (button "Close" (lambda () (void))))))))
               (heading 2 "Offcanvas")
               (with-class "we-button-row"
                 (inline
                  (with-class "we-btn-primary"
                    (button "Link with href" (lambda () (:= @offcanvas-open? #t))))
                  (with-class "we-btn-primary"
                    (button "Button with data-bs-target" (lambda () (:= @offcanvas-open? #t))))))
               (dialog
                @dialog-open?
                (lambda (_reason) (:= @dialog-open? #f))
                (heading 3 "Confirm Changes")
                (text "Apply updated Solar 2 showcase settings?")
                (with-class "we-button-row"
                  (inline
                   (button "Cancel" (lambda () (:= @dialog-open? #f)))
                   (button "Confirm" (lambda () (:= @dialog-open? #f)))))
               )
               (offcanvas
                @offcanvas-open?
                (lambda (_reason) (:= @offcanvas-open? #f))
                'end
                (heading 5 "Offcanvas")
                (text "Some text as placeholder. In real life you can have the elements you have chosen. Like, text, images, lists, etc."))
               )

              (stack
               (heading 2 "Popovers")
               (with-class "showcase-dialogs-row"
                 (inline
                  (with-class "showcase-popover-left"
                    (with-class "we-btn-secondary"
                      (popover "Left" (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."))))
                  (with-class "showcase-popover-top"
                    (with-class "we-btn-secondary"
                      (popover "Top" (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."))))
                  (with-class "showcase-popover-bottom"
                    (with-class "we-btn-secondary"
                      (popover "Bottom" (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."))))
                  (with-class "showcase-popover-right"
                    (with-class "we-btn-secondary"
                      (popover "Right" (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."))))))

               (heading 2 "Tooltips")
               (with-class "showcase-dialogs-row"
                 (inline
                  (with-class "showcase-tooltip-left"
                    (tooltip "Tooltip on left"
                             (with-class "we-btn-secondary"
                               (button "Left" (lambda () (void))))))
                  (with-class "showcase-tooltip-top"
                    (tooltip "Tooltip on top"
                             (with-class "we-btn-secondary"
                               (button "Top" (lambda () (void))))))
                  (with-class "showcase-tooltip-bottom"
                    (tooltip "Tooltip on bottom"
                             (with-class "we-btn-secondary"
                               (button "Bottom" (lambda () (void))))))
                  (with-class "showcase-tooltip-right"
                    (tooltip "Tooltip on right"
                             (with-class "we-btn-secondary"
                               (button "Right" (lambda () (void))))))))

               (heading 2 "Toasts")
               (with-class "showcase-static-toast"
                 (stack
                  (with-class "showcase-static-toast-header"
                    (inline
                     (with-class "showcase-static-toast-title"
                       (text "Bootstrap"))
                     (spacer)
                     (with-class "showcase-static-toast-time"
                       (text "11 mins ago"))
                     (text "×")))
                  (with-class "showcase-static-toast-body"
                    (text "Hello, world! This is a toast message."))))))))

           (with-class "we-button-status"
             (text @button-log))

           ;; Appendix
           (with-id "solar2-appendix"
             (with-class "showcase-section-title"
              (heading 1 "Appendix: Table Variants")))
           (table '("Name" "Status" "ETA")
                  @table-rows
                  'compact
                  '((caption . "Compact table")))
           (table '(("Service" left) ("Health" center) ("Latency" right))
                  '(("API" "OK" "120ms")
                    ("DB" "WARN" "220ms")
                    ("Queue" "OK" "98ms"))
                  'normal
                  '((variants . (striped))
                    (caption . "Striped rows")))
           (table '("Region" "Errors" "SLA")
                  '(("eu-west" "2" "99.9%")
                    ("us-east" "0" "99.99%")
                    ("ap-south" "4" "99.5%"))
                  'normal
                  '((variants . (hover))
                    (caption . "Hover rows")))
           (table '("Task" "Owner" "Status")
                  '(("design" "Alice" "done")
                    ("build" "Bob" "running")
                    ("ship" "Carol" "blocked"))
                  'normal
                  '((variants . (borderless))
                    (caption . "Borderless")))
           (table '("k" "v")
                  '(("A" "1") ("B" "2") ("C" "3"))
                  'compact
                  '((variants . (sm))
                    (caption . "Small table")))

           )))))))

(define theme-core-link-node     (install-theme-link! "we-theme-core-css"))
(define theme-general-link-node  (install-theme-link! "we-theme-external-css"))
(define theme-showcase-link-node (install-theme-link! "we-theme-showcase-css"))

(apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node (obs-peek @theme))
(apply-geometry-mode! (obs-peek @geometry-mode))
(obs-observe! @theme
              (lambda (next-theme)
                (apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node next-theme)))
(obs-observe! @geometry-mode
              (lambda (next-mode)
                (apply-geometry-mode! next-mode)))

(mount-renderer! app-renderer)
