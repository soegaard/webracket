;;;
;;; web-easy Browser Solar Showcase
;;;

;; Minimal, user-facing Solar-inspired showcase page.
;; Scope: Bootswatch-style section showcase using supported web-easy widgets.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader    "smoke-format.rkt" read-syntax/skip-first-line)

;; Constants for page state.
(define @theme              (@ 'solar2))
(define @theme-status       (@ "solar2"))
(define @selected-1         (@ 'home))
(define @selected-2         (@ 'home))
(define @selected-3         (@ 'home))
(define @selected-4         (@ 'home))
(define @query-1            (@ ""))
(define @query-2            (@ ""))
(define @query-3            (@ ""))
(define @query-4            (@ ""))
(define @button-log         (@ "No button clicks yet."))
(define @name               (@ ""))
(define @select-value       (@ "1"))
(define @radio-value        (@ "Option one is this and that - be sure to include why it's great"))
(define @notes              (@ ""))
(define @accept?            (@ #f))
(define @crumb-1            (@ 'home))
(define @crumb-2            (@ 'library))
(define @crumb-3            (@ 'data))
(define @page               (@ 1))
(define @level              (@ 42))
(define @progress-kind      (@ 'info))
(define @list-current       (@ 'current))
(define @accordion          (@ 'what))
(define @dialog-open?       (@ #f))
(define @offcanvas-open?    (@ #f))
(define @modal-open?        (@ #f))
(define @toast-open?        (@ #t))
(define @toast-level        (@ 'info))
(define @toast-top-open?    (@ #f))
(define @toast-left-open?   (@ #f))
(define @tab                (@ "Home"))
(define @pill               (@ "Alpha"))
(define @underline-tab      (@ "Active"))
(define @pill-vertical      (@ "Active"))
(define @btn-check-selected (@ '(c1)))
(define @btn-radio-selected (@ 'r1))

;;;
;;; CSS Files - Themes
;;;

;; The shared CSS file is web-easy-core.css.

;; theme-css-path/core : -> string?
;;   Path to shared web-easy structural core stylesheet.
(define (theme-css-path/core)
  "../themes/web-easy-core.css")

;; The themes build upon the core CSS file.

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
    [(light)  "../themes/theme-external-light.css"]
    [(dark)   "../themes/theme-external-dark.css"]
    [(solar)  "../themes/theme-external-solar.css"]
    [(solar2) "../themes/theme-solar-2.css"]
    [else     "../themes/theme-external-light.css"]))

;; theme-css-path/showcase : any/c -> string?
;;   Map theme id to page-specific stylesheet path.
(define (theme-css-path/showcase theme)
  (case theme
    [(light)  "../themes/theme-showcase-light.css"]
    [(dark)   "../themes/theme-showcase-dark.css"]
    [(solar)  "../themes/theme-showcase-solar.css"]
    [(solar2) "../themes/theme-showcase-solar2.css"]
    [else     "../themes/theme-showcase-light.css"]))


;; install-theme-link! : string? -> any/c
;;   Create and attach a stylesheet <link> with the given id.
(define (install-theme-link! link-id)
  (define doc  (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id"  link-id)
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

;;;
;;; Showcase Specific Components
;;;

;;; Navbars

;; nav-item-button : observable? symbol? string? -> view?
;;   Build a navbar item that updates the selected id when clicked.
(define (nav-item-button @selected id label)
  (button label
          (lambda () (:= @selected id))))

;; navbar-demo : observable? observable? string? -> view?
;;   Build one navbar row with brand, links, dropdown, and search form.
(define (navbar-demo @selected @query variant-class)
  (navigation-bar
   (text "Navbar" #:class "we-navbar-brand")
   (nav-item-button @selected 'home     "Home")
   (nav-item-button @selected 'features "Features")
   (nav-item-button @selected 'pricing  "Pricing")
   (nav-item-button @selected 'about    "About")
   (menu-bar
    (menu "Dropdown"
          (menu-item "Action"         (lambda () (void)))
          (menu-item "Another action" (lambda () (void)))
          (menu-item "Something else here" (lambda () (void)))
          (divider #:orientation 'horizontal)
          (menu-item "Separated link" (lambda () (void)))))
   (spacer)
   (hpanel
    (input
              @query
              (lambda (v) (:= @query v))
              #:attrs '((placeholder "Search")))
    (button "Search" (lambda () (void))))
   #:class variant-class))

;;; Buttons

;; log-button! : string? -> void?
;;   Record a showcase button click in the status line.
(define (log-button! label)
  (:= @button-log (~a "Clicked: " label)))

;; showcase-button : string? string? boolean? -> view?
;;   Build one showcase button with a variant class and optional disabled state.
(define (showcase-button label variant-class disabled?)
  (if disabled?
      (button label
              (lambda () (void))
              #:class variant-class
              #:attrs '((disabled "disabled")))
      (button label
              (lambda () (log-button! label))
              #:class variant-class)))


;;; Progress Bars

;; progress-fill-class : symbol? boolean? boolean? -> string?
;;   Build fill class string for showcase progress bars.
(define (progress-fill-class variant striped? animated?)
  (define variant-class
    (case variant
      [(success) "we-progress-fill-success"]
      [(info)    "we-progress-fill-info"]
      [(warning) "we-progress-fill-warning"]
      [(danger)  "we-progress-fill-danger"]
      [else      "we-progress-fill-default"]))
  (string-append "we-progress-fill "
                 variant-class
                 (if striped?  " we-progress-fill-striped"  "")
                 (if animated? " we-progress-fill-animated" "")))

;; progress-fill-color : symbol? -> string?
;;   Map progress variant to Solar showcase fill class suffix.
(define (progress-fill-color variant)
  (case variant
    [(success) "we-progress-fill-success"]
    [(info)    "we-progress-fill-info"]
    [(warning) "we-progress-fill-warning"]
    [(danger)  "we-progress-fill-danger"]
    [else      "we-progress-fill-default"]))

;; progress-width-class : number? -> string?
;;   Map known showcase percentage values to width utility classes.
(define (progress-width-class pct)
  (case pct
    [(10)  "we-progress-w-10"]
    [(15)  "we-progress-w-15"]
    [(20)  "we-progress-w-20"]
    [(25)  "we-progress-w-25"]
    [(30)  "we-progress-w-30"]
    [(50)  "we-progress-w-50"]
    [(75)  "we-progress-w-75"]
    [(100) "we-progress-w-100"]
    [else  "we-progress-w-0"]))

;; progress-fill : number? symbol? [boolean?] [boolean?] -> view?
;;   Build one showcase progress fill segment with width percentage and variant class.
;;   Optional parameter striped? defaults to #f.
;;   Optional parameter animated? defaults to #f.
(define (progress-fill pct variant [striped? #f] [animated? #f])
  (text " "
        #:class (string-append     (progress-fill-class variant striped? animated?)
                               " " (progress-fill-color variant)
                               " " (progress-width-class pct))))

;; progress-track : view? ... -> view?
;;   Build one showcase progress track containing one or more fill segments.
(define (progress-track . fills)
  (inline (inline (apply inline fills)
                  #:class "we-progress-fill-row")
          #:class "we-progress-track we-progress-track-inner"))


;;; List Groups

;; showcase-list-group-row : string? number? -> view?
;;   Build one list-group row with a right-aligned badge count.
(define (showcase-list-group-row label count)
  (inline (text label)
          (spacer)
          (badge (~a count) 'primary #:class "showcase-list-group-badge")
          #:class "we-list-group-item showcase-list-group-row"))

;; showcase-static-list-group : (listof (list string? number?)) [string?] -> view?
;;   Build a static list-group with badge rows; optional extra class decorates the group.
;;   Optional parameter extra-class defaults to #f.
(define (showcase-static-list-group entries [extra-class #f])
  (define rows
    (map (lambda (entry)
           (showcase-list-group-row (first entry) (second entry)))
         entries))
  (define group-view
    (stack (apply stack rows)
           #:class "we-stack-gap-0"))
  (if extra-class
      (stack group-view #:class (string-append "we-list-group " extra-class))
      (stack group-view #:class "we-list-group")))

;; showcase-rich-list-item : string? string? string? string? [boolean?] -> view?
;;   Build one rich list-group row with heading, meta text, body text and muted footnote.
;;   Optional parameter active? defaults to #f.
(define (showcase-rich-list-item heading meta body footnote [active? #f])
  (stack
   (inline (text heading #:class "showcase-rich-list-heading")
           (spacer)
           (text meta    #:class "showcase-rich-list-meta"))
   (text body     #:class "showcase-rich-list-body")
   (text footnote #:class "showcase-rich-list-footnote")
   #:class (string-append (if active?
                              "we-list-group-item showcase-rich-list-item is-active"
                              "we-list-group-item showcase-rich-list-item")
                          " we-stack-gap-1")))

;; showcase-rich-list-group : -> view?
;;   Build the rich list-group sample used in the Containers section.
(define (showcase-rich-list-group)
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
    "Donec id elit non mi porta.")
   #:class "we-list-group showcase-rich-list-group we-stack-gap-0"))

;; showcase-accordion-body : string? string? -> view?
;;   Build accordion panel text with lead sentence plus full reference-style copy.
(define (showcase-accordion-body lead copy)
  (inline
   (text lead #:class "showcase-accordion-lead")
   (text copy #:class "showcase-accordion-copy")
   #:class "showcase-accordion-body-row showcase-accordion-inline"))

;; section-heading : string? string? -> view?
;;   Build a standardized top-level showcase section heading.
(define (section-heading section-id title)
  (heading 1 title #:id section-id))

;; tone-card : symbol? symbol? string? -> view?
;;   Build a tone-styled card sample row with title/copy content.
(define (tone-card tone tone-style title)
  (card
   "Header"
   #f
   (heading 4 title)
   (text "Some quick example text to build on the card title and make up the bulk of the card's content.")
   #:tone tone
   #:tone-style tone-style))

(define app-renderer
  (render
   (window
    (stack
     ;;;
     ;;; Top Bar
     ;;;
     (top-bar
      (inline
       (stack
        (text "web-easy"       #:class "showcase-brand-title")
        (text "Solar showcase" #:class "showcase-brand-subtitle")
        #:class "showcase-brand")
       (spacer)
       (stack
        (text "Theme" #:class "showcase-controls-label")
        (choice '((solar2 "Solar 2") (solar "Solar") (light "Light") (dark "Dark"))
                @theme
                (lambda (next-theme)
                  (:= @theme        (normalize-theme-id next-theme))
                  (:= @theme-status (~a next-theme)))
                #:class "showcase-theme-choice")
        #:class "showcase-topbar-controls")
       #:class "showcase-topbar-inner")
      #:id "theme-showcase-hero"
      #:class "showcase-topbar")
     
     (container
      (stack
       ;;;
       ;;; Main
       ;;;
       
       (stack
        ;; Navbars
        (section-heading "solar2-navbars" "Navbars")
        (stack
         (navbar-demo @selected-1 @query-1 "we-variant-primary")
         (navbar-demo @selected-2 @query-2 "we-variant-dark")
         (navbar-demo @selected-3 @query-3 "we-variant-light")
         (navbar-demo @selected-4 @query-4 "we-variant-subtle")
         #:class "we-flow")
        
        ;; Buttons
        (section-heading "solar2-buttons" "Buttons")
        (grid '(60 40)
              ;; First Column
              (stack
               ; Variants of enabled buttons 
               (inline
                (showcase-button "Primary"   "we-button-primary"   #f)
                (showcase-button "Secondary" "we-button-secondary" #f)
                (showcase-button "Success"   "we-button-success"   #f)
                (showcase-button "Info"      "we-button-info"      #f)
                (showcase-button "Warning"   "we-button-warning"   #f)
                (showcase-button "Danger"    "we-button-danger"    #f)
                (showcase-button "Light"     "we-button-light"     #f)
                (showcase-button "Dark"      "we-button-dark"      #f)
                (showcase-button "Link"      "we-button-link"      #f)
                #:class "we-button-row")
               ; Variants of disabled buttons 
               (inline
                (showcase-button "Primary"   "we-button-primary"   #t)
                (showcase-button "Secondary" "we-button-secondary" #t)
                (showcase-button "Success"   "we-button-success"   #t)
                (showcase-button "Info"      "we-button-info"      #t)
                (showcase-button "Warning"   "we-button-warning"   #t)
                (showcase-button "Danger"    "we-button-danger"    #t)
                (showcase-button "Light"     "we-button-light"     #t)
                (showcase-button "Dark"      "we-button-dark"      #t)
                (showcase-button "Link"      "we-button-link"      #t)
                #:class "we-button-row")
               ; Variants of outline buttons
               (inline
                (showcase-button "Primary"   "we-button-outline-primary"   #f)
                (showcase-button "Secondary" "we-button-outline-secondary" #f)
                (showcase-button "Success"   "we-button-outline-success"   #f)
                (showcase-button "Info"      "we-button-outline-info"      #f)
                (showcase-button "Warning"   "we-button-outline-warning"   #f)
                (showcase-button "Danger"    "we-button-outline-danger"    #f)
                (showcase-button "Light"     "we-button-outline-light"     #f)
                (showcase-button "Dark"      "we-button-outline-dark"      #f)
                #:class "we-button-row")
               ; Dropdown buttons
               (inline
                (dropdown "Dropdown button"
                          '((action  "Action")
                            (another "Another action")
                            (more    "Something else here"))
                          (lambda (id)
                            (log-button! (~a "Dropdown/Primary/" id)))
                          #:class "we-button-primary")
                (dropdown "Dropdown button"
                          '((action  "Action")
                            (another "Another action")
                            (more    "Something else here"))
                          (lambda (id)
                            (log-button! (~a "Dropdown/Secondary/" id)))
                          #:class "we-button-secondary")
                #:class "we-button-row")
               ; Button sizes
               (inline
                (showcase-button "Large button"   "we-button-primary we-button-lg" #f)
                (showcase-button "Default button" "we-button-primary"           #f)
                (showcase-button "Small button"   "we-button-primary we-button-sm" #f)
                #:class "we-button-row"))
              
              ;; Second Column
              (stack
               ;; Block buttons
               (stack
                (showcase-button "Block button" "we-button-primary we-button-lg showcase-block-button-row" #f)
                (showcase-button "Block button" "we-button-primary we-button-lg showcase-block-button-row" #f)
                #:class "showcase-block-button-grid")
               ;; Check buttons
               (toggle-button-group 'checkbox
                                    '((c1 "Checkbox 1")
                                      (c2 "Checkbox 2")
                                      (c3 "Checkbox 3"))
                                    @btn-check-selected
                                    (lambda (next)
                                      (:= @btn-check-selected next)
                                      (log-button! (~a "Check/" next)))
                                    #:class "showcase-btn-check-group")
               ;; Radio Buttons
               (toggle-button-group 'radio
                                    '((r1 "Radio 1")
                                      (r2 "Radio 2")
                                      (r3 "Radio 3"))
                                    @btn-radio-selected
                                    (lambda (next)
                                      (:= @btn-radio-selected next)
                                      (log-button! (~a "Radio/" next)))
                                    #:class "showcase-btn-radio-group")
               ;; Vertical grouped buttons
               (stack
                (button "Button" (lambda () (log-button! "Vertical/1")) #:class "we-button-primary")
                (button "Button" (lambda () (log-button! "Vertical/2")) #:class "we-button-primary")
                (button "Button" (lambda () (log-button! "Vertical/3")) #:class "we-button-primary")
                (button "Button" (lambda () (log-button! "Vertical/4")) #:class "we-button-primary")
                (button "Button" (lambda () (log-button! "Vertical/5")) #:class "we-button-primary")
                (button "Button" (lambda () (log-button! "Vertical/6")) #:class "we-button-primary")
                #:class "we-button-group-vertical")
               ;; Horizontally grouped buttons
               (inline
                (button-group
                 (button "Left"   (lambda () (log-button! "Group/Left")))
                 (button "Middle" (lambda () (log-button! "Group/Middle")))
                 (button "Right"  (lambda () (log-button! "Group/Right")))
                 #:class "we-button-secondary")
                #:class "we-button-row")
               ;; Button toolbras
               (inline
                (button-toolbar
                 (toolbar-group
                  (button "1" (lambda () (log-button! "Toolbar/1")))
                  (button "2" (lambda () (log-button! "Toolbar/2")))
                  (button "3" (lambda () (log-button! "Toolbar/3")))
                  (button "4" (lambda () (log-button! "Toolbar/4"))))
                 (toolbar-group
                  (button "5" (lambda () (log-button! "Toolbar/5")))
                  (button "6" (lambda () (log-button! "Toolbar/6")))
                  (button "7" (lambda () (log-button! "Toolbar/7"))))
                 (toolbar-group
                  (button "8" (lambda () (log-button! "Toolbar/8"))))
                 #:class "we-button-secondary")
                #:class "we-button-row"))
              #:class "showcase-buttons-grid")

        ;; Typography
        (section-heading "solar2-typography" "Typography")
        (grid 3
              ;; Headings
              (vpanel
               (heading 1 "Heading 1")
               (heading 2 "Heading 2")
               (heading 3 "Heading 3")
               (heading 4 "Heading 4")
               (heading 5 "Heading 5")
               (heading 6 "Heading 6")
               (heading-with-subtitle 3 "Heading" "with faded secondary text")
               (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus dolor auctor."
                     #:class "showcase-typography-lead"))
              ;; Body text
              (vpanel
               (heading 2 "Example body text")
               (inline
                (text "Nullam quis risus eget ")
                (link "urna mollis ornare" "#")
                (text " vel eu leo. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."))
               (text "This line of text is meant to be treated as fine print." #:class "we-text-fine-print")
               (inline
                (text "The following is ")
                (text "rendered as bold text."       #:class "we-text-strong"))
               (inline
                (text "The following is ")
                (text "rendered as italicized text." #:class "we-text-emphasis"))
               (inline
                (text "An abbreviation of the word attribute is ")
                (text "attr." #:class "we-text-abbr" #:attrs '((title "attribute")))))
              ;; Emphasis classes
              (vpanel
               (heading 2 "Emphasis classes")
               (text "text-primary"            #:class "we-text-primary showcase-text-primary")
               (text "text-primary-emphasis"   #:class "we-text-primary-emphasis showcase-text-primary-emphasis")
               (text "text-secondary"          #:class "we-text-secondary showcase-text-secondary")
               (text "text-secondary-emphasis" #:class "we-text-secondary-emphasis showcase-text-secondary-emphasis")
               (text "text-success"            #:class "we-text-success showcase-text-success")
               (text "text-success-emphasis"   #:class "we-text-success-emphasis showcase-text-success-emphasis")
               (text "text-danger"             #:class "we-text-danger showcase-text-danger")
               (text "text-danger-emphasis"    #:class "we-text-danger-emphasis showcase-text-danger-emphasis")
               (text "text-warning"            #:class "we-text-warning showcase-text-warning")
               (text "text-warning-emphasis"   #:class "we-text-warning-emphasis showcase-text-warning-emphasis")
               (text "text-info"               #:class "we-text-info showcase-text-info")
               (text "text-info-emphasis"      #:class "we-text-info-emphasis showcase-text-info-emphasis")
               (text "text-light"              #:class "we-text-light showcase-text-light")
               (text "text-light-emphasis"     #:class "we-text-light-emphasis showcase-text-light-emphasis")
               (text "text-dark"               #:class "we-text-dark showcase-text-dark")
               (text "text-dark-emphasis"      #:class "we-text-dark-emphasis showcase-text-dark-emphasis")
               (text "text-body"               #:class "we-text-body showcase-text-body")
               (text "text-body-emphasis"      #:class "we-text-body-emphasis showcase-text-body-emphasis")
               (text "text-body-secondary"     #:class "we-text-body-secondary showcase-text-body-secondary")
               (text "text-body-tertiary"      #:class "we-text-body-tertiary showcase-text-tertiary"))
              #:class "showcase-typography-grid")
        ;; Blockquotes
        (heading 2 "Blockquotes")
        (grid 3
              (blockquote
               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
               "Someone famous in Source Title")
              (blockquote
               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
               "Someone famous in Source Title"
               #:align 'center)
              (blockquote
               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante."
               "Someone famous in Source Title"
               #:align 'right)
              #:class "showcase-typography-grid")
        ;; Tables
        (section-heading "solar2-tables" "Tables")
        (table '( "Type"      "Column heading" "Column heading" "Column heading")
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
               #:density           'normal
               #:variants          '(hover)
               #:row-variants      '(active #f primary secondary success danger warning info light dark)
               #:row-header-column 0)

        ;; Forms
        (section-heading "solar2-forms" "Forms")
        (grid '(60 40) "3rem"
              (stack
               (group "Legend"
                      ;; Email with label before, prefilled
                      (inline
                       (text "Email" #:class "showcase-static-label")
                       (input ""
                              (lambda (_v) (void))
                              #:input-attrs '((readonly "readonly")
                                              (value    "email@example.com"))
                              #:class "showcase-static-value we-form-control-plaintext")
                       #:class "we-form-row showcase-static-row")
                      ;; Email with label above and placeholder input
                      (text "Email address" #:class "we-form-row we-form-label")
                      (input @name
                             (lambda (v) (:= @name v))
                             #:input-attrs '((id          "exampleInputEmail1")
                                             (placeholder "Enter email")
                                             (type        "email"))
                             #:class "we-form-row we-form-control-block")                      
                      (text "We'll never share your email with anyone else." #:class "we-form-help")
                      ;; Password field
                      (text "Password" #:class "we-form-row we-form-label")
                      (input @notes
                             (lambda (v) (:= @notes v))
                             #:input-attrs '((id           "exampleInputPassword1")
                                             (placeholder  "Password")
                                             (type         "password")
                                             (autocomplete "off"))
                             #:class "we-form-row we-form-control-block")
                      ;; Select 
                      (text "Example select" #:class "we-form-row we-form-label")
                      (choice '("1" "2" "3" "4" "5")
                              @select-value
                              (lambda (v) (:= @select-value v))
                              #:attrs '((id "exampleSelect1"))
                              #:class "we-form-row we-form-control-block")
                      ;; Disabled select
                      (text "Example disabled select" #:class "we-form-row we-form-label")
                      (choice '("1" "2" "3" "4" "5")
                              "1"
                              (lambda (_v) (void))
                              #:attrs '((id "exampleDisabledSelect1")
                                        (disabled "disabled"))
                              #:class "we-form-row we-form-control-block")
                      ;; Select mutliple
                      (text "Example multiple select"
                            #:class "we-form-row we-form-label")
                      (choice '("1" "2" "3" "4" "5")
                              "1"
                              (lambda (_v) (void))
                              #:attrs '((id       "exampleSelect2")
                                        (multiple "multiple")
                                        (size     "5"))
                              #:class "we-form-row we-form-control-block")
                      ;; Text Area
                      (text "Example textarea"
                            #:class "we-form-row we-form-label")
                      (textarea @notes
                                (lambda (v) (:= @notes v))
                                #:rows           3
                                #:textarea-attrs '((id "exampleTextarea"))
                                #:class          "we-form-row we-form-control-block")
                      ;; File input
                      (text "Default file input example" #:class "we-form-row")
                      (input ""
                             (lambda (_v) (void))
                             #:input-attrs '((type "file"))
                             #:class       "we-form-row we-form-control-block")
                      ;; Radio buttons
                      (group "Radio buttons"
                       (radios '("Option one is this and that - be sure to include why it's great"
                                 "Option two can be something else and selecting it will deselect option one"
                                 ("Option three is disabled"
                                  "Option three is disabled"
                                  #t))
                               @radio-value
                               (lambda (v) (:= @radio-value v)))
                       #:class "we-form-row")
                      ;; Check boxes
                      (group "Checkboxes"
                             (stack
                              (inline
                               (checkbox @accept?
                                         (lambda (v) (:= @accept? v)))
                               (text "Default checkbox")
                               #:class "we-form-check we-checkbox-row")
                              (inline
                               (checkbox #t (lambda (_v) (void)))
                               (text "Checked checkbox")
                               #:class "we-form-check we-checkbox-row"))
                             #:class "we-form-row")
                      ;; Switches
                      (group
                       "Switches"
                       (stack
                        (inline
                         (checkbox #f
                                   (lambda (_v) (void))
                                   #:class "we-switch-control")
                         (text "Default switch checkbox input")
                         #:class "we-form-check we-switch-row")
                        (inline
                         (checkbox #t
                                   (lambda (_v) (void))
                                   #:class "we-switch-control")
                         (text "Checked switch checkbox input")
                         #:class "we-form-check we-switch-row"))
                       #:class "we-form-row")
                      ;; Ranges
                      (heading 3 "Ranges"   #:class "we-form-row")
                      (text "Example range" #:class "we-form-row")
                      (slider @level
                              (lambda (v) (:= @level v))
                              #:min   0
                              #:max   100
                              #:id    "customRange1"
                              #:class "we-form-row we-form-control-block we-range-default")
                      (text "Disabled range" #:class "we-form-row")
                      (slider 50
                              (lambda (_v) (void))
                              #:min 0
                              #:max 100
                              #:attrs '((disabled "disabled"))
                              #:class "we-form-row we-range-disabled")
                      (text "Example range" #:class "we-form-row")
                      (slider 3
                              (lambda (_v) (void))
                              #:min 0
                              #:max 5
                              #:attrs '((min "0") (max "5") (step "0.5"))
                              #:class "we-form-row we-range-step")
                      ;; Submit button
                      (inline
                       (button "Submit" (lambda () (void))
                               #:class "we-button-primary")
                       #:class "we-form-row"))
               #:class "showcase-forms-left")
              ;; Input - states
              (stack
               ;; Disabled
               (text "Disabled input"
                     #:class "we-form-row")
               (input
                ""
                (lambda (_v) (void))
                #:input-attrs '((id "disabledInput")
                                (placeholder "Disabled input here...")
                                (disabled "disabled"))
                #:class "we-form-row we-form-control-block we-form-state-disabled")
               ;; Readonly
               (text "Readonly input"
                     #:class "we-form-row")
               (input
                ""
                (lambda (_v) (void))
                #:input-attrs '((id "readOnlyInput")
                                (placeholder "Readonly input here...")
                                (readonly "readonly"))
                #:class "we-form-row we-form-control-block")
               ;; Valid Input
               (text "Valid input" #:class "we-form-row")
               (input ""
                      (lambda (_v) (void))
                      #:input-attrs '((id "inputValid")
                                      (value "correct value"))
                      #:class "we-form-state-valid we-form-control-block showcase-field-valid")               
               (text "Success! You've done it."
                     #:class "we-form-feedback we-form-feedback-valid")
               ;; Invalid
               (text "Invalid input" #:class "we-form-row")
               (input ""
                      (lambda (_v) (void))
                      #:input-attrs '((id "inputInvalid")
                                      (value "wrong value"))
                      #:class "we-form-state-invalid we-form-control-block showcase-field-invalid")
               (text "Sorry, that username's taken. Try another?"
                     #:class "we-form-feedback we-form-feedback-invalid")
               ;; Sizes
               (text "Large input" #:class "we-form-row")
               (input ""
                      (lambda (_v) (void))
                      #:input-attrs '((placeholder ".form-control-lg"))
                      #:class "we-input-lg")
               (text "Default input" #:class "we-form-row")
               (input ""
                      (lambda (_v) (void))
                      #:input-attrs '((placeholder "Default input"))
                      #:class "showcase-field-default")
               (text "Small input" #:class "we-form-row")
               (input ""
                      (lambda (_v) (void))
                      #:input-attrs '((placeholder ".form-control-sm"))
                      #:class "we-input-sm")
               ;; Addons (?)
               (text "Input addons" #:class "we-form-row")
               (inline
                (text "$" #:class "we-input-group-prefix")
                (input "" (lambda (_v) (void)))
                (text ".00" #:class "we-input-group-prefix")
                #:class "we-input-group")
               
               (inline
                (input   ""      (lambda (_v) (void)))
                (button "Button" (lambda () (void)))
                #:class "we-input-group")
               
               (text "Floating labels" #:class "we-form-row")
               (stack
                (text "Email address")
                (input ""
                       (lambda (_v) (void))
                       #:input-attrs '((placeholder "name@example.com")))
                #:class "we-floating-field")
               (stack
                (text "Password")
                (input ""
                       (lambda (_v) (void))
                       #:input-attrs '((placeholder "Password")
                                       (type "password")
                                       (autocomplete "off")))
                #:class "we-floating-field")
               #:class "showcase-forms-right"))

        ;; Navs
        (stack
         (section-heading "solar2-navs" "Navs")
         (grid 2
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
                             (text "Trust fund seitan letterpress, keytar raw denim keffiyeh etsy art party before they sold out master cleanse gluten-free squid scenester freegan cosby sweater. Fanny pack portland seitan DIY, art party locavore wolf cliche high life echo park Austin. Cred vinyl keffiyeh DIY salvia PBR, banh mi before they sold out farm-to-table VHS viral locavore cosby sweater."))))
                #:class "we-stack-gap-1")
               (stack
                (heading 2 "Pills")
                (tab-panel
                 @pill
                 (list (cons "Active"     (text "Active pill content."))
                       (cons "Dropdown ▾" (text "Dropdown pill content."))
                       (cons "Link"       (text "Link pill content."))
                       (cons "Disabled"   (text "Disabled pill content.")))
                 #:class "we-tab-style-pills")
                (tab-panel
                 @pill-vertical
                 (list (cons "Active"     (text "Active vertical pill content."))
                       (cons "Dropdown ▾" (text "Dropdown vertical content."))
                       (cons "Link"       (text "Link vertical content."))
                       (cons "Disabled"   (text "Disabled vertical content.")))
                 #:class "we-tab-style-pills we-nav-pills-vertical")
                #:class "we-stack-gap-1"))
         (grid 2
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
                (pagination 5 @page (lambda (v) (:= @page v)) #:class "we-pagination-lg")
                (pagination 5 @page (lambda (v) (:= @page v)) #:class "we-pagination-sm")))
         (heading 2 "Underline")
         (tab-panel @underline-tab
                    (list (cons "Active"   (text "Underline active content."))
                          (cons "Link"     (text "Underline link content."))
                          (cons "Link​"    (text "Underline link two content."))
                          (list "Disabled" (text "Underline disabled content.") #t))
                    #:class "we-tab-style-underline")
         #:class "we-section-break-xl"
         #:id "solar2-navs-section")

        ;; Indicators
        (stack
         (section-heading "solar2-indicators" "Indicators")
         ;; Alerts
         (heading 2 "Alerts")
         (alert-rich
          "Best check yo self, you're not looking too good. Nulla vitae elit libero, a pharetra augue. Praesent commodo cursus magna,"
          "Warning!"
          "vel scelerisque nisl consectetur et."
          "#"
          #:level 'warning
          #:scale 'major
          #:dismiss-action (lambda () (void)))
         (grid 3
               (alert-rich
                "Change a few things up and"
                "Oh snap!"
                "try submitting again."
                "#"
                #:level 'danger
                #:layout 'inline
                #:dismiss-action (lambda () (void)))
               (alert-rich
                "You successfully read"
                "Well done!"
                "this important alert message."
                "#"
                #:level 'success
                #:layout 'inline
                #:dismiss-action (lambda () (void)))
               (alert-rich
                "This alert needs your attention, but it's not super important."
                "Heads up!"
                #f
                #f
                #:level 'info
                #:layout 'inline
                #:dismiss-action (lambda () (void))))
         (grid 3
               (alert-rich
                "Change a few things up and"
                "Oh snap!"
                "try submitting again."
                "#"
                #:level 'info
                #:layout 'inline
                #:tone 'primary
                #:dismiss-action (lambda () (void)))
               (alert-rich
                "You successfully read"
                "Well done!"
                "this important alert message."
                "#"
                #:level 'info
                #:layout 'inline
                #:tone 'secondary
                #:dismiss-action (lambda () (void)))
               (alert-rich
                "This alert needs your attention, but it's not super important."
                "Heads up!"
                #f
                #f
                #:level 'info
                #:layout 'inline
                #:tone 'light
                #:dismiss-action (lambda () (void))))
         ;; Badges
         (heading 2 "Badges")
         (inline
          (badge "Primary" 'primary)
          (badge "Secondary" 'secondary)
          (badge "Success" 'success)
          (badge "Danger" 'danger)
          (badge "Warning" 'warning)
          (badge "Info" 'info)
          (badge "Light" 'light)
          (badge "Dark" 'dark)
          #:class "we-button-row we-badge-shape-square")
         (inline
          (badge "Primary" 'primary)
          (badge "Secondary" 'secondary)
          (badge "Success" 'success)
          (badge "Danger" 'danger)
          (badge "Warning" 'warning)
          (badge "Info" 'info)
          (badge "Light" 'light)
          (badge "Dark" 'dark)
          #:class "we-button-row we-badge-shape-pill")
         #:class "we-section-break-xl"
         #:id "solar2-indicators-section")

        ;; Progress
        (stack
         (section-heading "solar2-progress" "Progress")
         (heading 3 "Basic")
         (progress-track
          (progress-fill 25 'default))
         (heading 3 "Contextual alternatives")
         (progress-track (progress-fill 25 'success))
         (progress-track (progress-fill 50 'info))
         (progress-track (progress-fill 75 'warning))
         (progress-track (progress-fill 100 'danger))
         (heading 3 "Multiple bars")
         (progress-track
          (progress-fill 15 'default)
          (progress-fill 30 'success)
          (progress-fill 20 'info))
         (heading 3 "Striped")
         (progress-track (progress-fill 10 'default #t))
         (progress-track (progress-fill 25 'success #t))
         (progress-track (progress-fill 50 'info #t))
         (progress-track (progress-fill 75 'warning #t))
         (progress-track (progress-fill 100 'danger #t))
         (heading 3 "Animated")
         (progress-track (progress-fill 75 'default #t #t))
         #:class "we-section-break-xl"
         #:id "solar2-progress-section")

        ;; Containers
        (section-heading "solar2-containers" "Containers")
        (heading 2 "List groups")
        (grid 3
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
        (heading 1 "Cards"
                 #:id    "solar2-cards"
                 #:class "we-section-break")
        (heading 2 "Cards")
        (grid 3
              (stack
               (tone-card 'primary   'fill    "Primary card title")
               (tone-card 'secondary 'fill    "Secondary card title")
               (tone-card 'success   'fill    "Success card title")
               (tone-card 'danger    'fill    "Danger card title")
               (tone-card 'warning   'fill    "Warning card title")
               (tone-card 'info      'fill    "Info card title")
               (tone-card 'light     'fill    "Light card title")
               (tone-card 'dark      'fill    "Dark card title"))
              (stack
               (tone-card 'primary   'outline "Primary card title")
               (tone-card 'secondary 'outline "Secondary card title")
               (tone-card 'success   'outline "Success card title")
               (tone-card 'danger    'outline "Danger card title")
               (tone-card 'warning   'outline "Warning card title")
               (tone-card 'info      'outline "Info card title")
               (tone-card 'light     'outline "Light card title")
               (tone-card 'dark      'outline "Dark card title"))
              (stack
               (card
                "Card header"
                "2 days ago"
                (stack
                 (heading 5 "Special title treatment")
                 (text "Support card subtitle"
                       #:class "we-card-subtitle")
                 (text "Image cap"
                       #:class "showcase-card-image-cap")
                 (text "Some quick example text to build on the card title and make up the bulk of the card's content.")
                 (stack
                  (text "Cras justo odio"
                        #:class "we-list-group-item")
                  (text "Dapibus ac facilisis in"
                        #:class "we-list-group-item")
                  (text "Vestibulum at eros"
                        #:class "we-list-group-item")
                  #:class "we-list-group"))
                #:actions (list (link "Card link" "#")
                                (link "Another link" "#")))
               (card
                "Card title"
                #f
                (stack
                 (text "Card subtitle"
                       #:class "we-card-subtitle")
                 (text "Some quick example text to build on the card title and make up the bulk of the card's content."))
                #:actions (list (link "Card link" "#")
                                (link "Another link" "#"))))
              #:id "solar2-cards-body")

        ;; Accordions
        (heading 1 "Accordions"
                 #:id    "solar2-accordions"
                 #:class "we-section-break")
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
                 "It is hidden by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. It's also worth noting that just about any HTML can go within the .accordion-body, though the transition does limit overflow.")))
         #:id "solar2-accordions-body")

        ;; Dialogs
        (heading 1 "Dialogs"
                 #:id    "solar2-dialogs"
                 #:class "we-section-break")
        (grid 2
              (stack
               (heading 2 "Modals")
               (stack
                (inline
                 (text "Modal title"
                       #:class "we-modal-title")
                 (spacer)
                 (text "×"
                       #:class "we-modal-close")
                 #:class "we-modal-header")
                (text "Modal body text goes here."
                      #:class "we-modal-body")
                (inline
                 (button "Save changes"
                         (lambda () (void))
                         #:class "we-button-primary")
                 (button "Close"
                         (lambda () (void))
                         #:class "we-button-secondary")
                 #:class "we-modal-footer")
                #:class "showcase-static-modal-wrap we-modal-panel showcase-static-modal")
               (heading 2 "Offcanvas")
               (inline
                (button
                 "Link with href"
                 (lambda () (:= @offcanvas-open? #t))
                 #:class "we-button-primary")
                (button
                 "Button with data-bs-target"
                 (lambda () (:= @offcanvas-open? #t))
                 #:class "we-button-primary")
                #:class "we-button-row")
               (offcanvas
                @offcanvas-open?
                (lambda (_reason) (:= @offcanvas-open? #f))
                (heading 5 "Offcanvas")
                (text "Some text as placeholder. In real life you can have the elements you have chosen. Like, text, images, lists, etc.")
                #:side 'end))
              (stack
               (heading 2 "Popovers")
               (inline
                (popover
                 "Left"
                 (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.")
                 #:placement 'left
                 #:title  "Popover title"
                 #:footer "Popover footer"
                 #:class  "we-button-secondary")
                (popover
                 "Top"
                 (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.")
                 #:placement 'top
                 #:title  "Popover title"
                 #:footer "Popover footer"
                 #:class  "we-button-secondary")
                (popover
                 "Bottom"
                 (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.")
                 #:placement 'bottom
                 #:title  "Popover title"
                 #:footer "Popover footer"
                 #:class  "we-button-secondary")
                (popover
                 "Right"
                 (text "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.")
                 #:placement 'right
                 #:title  "Popover title"
                 #:footer "Popover footer"
                 #:class  "we-button-secondary"))
               (heading 2 "Tooltips")
               (inline
                (tooltip
                 "Tooltip on left"
                 (button "Left"
                         (lambda () (void))
                         #:class "we-button-secondary")
                 #:placement 'left
                 #:title  "Tooltip"
                 #:footer "Footer text")
                (tooltip
                 "Tooltip on top"
                 (button "Top"
                         (lambda () (void))
                         #:class "we-button-secondary")
                 #:placement 'top
                 #:title  "Tooltip"
                 #:footer "Footer text")
                (tooltip
                 "Tooltip on bottom"
                 (button "Bottom"
                         (lambda () (void))
                         #:class "we-button-secondary")
                 #:placement 'bottom
                 #:title  "Tooltip"
                 #:footer "Footer text")
                (tooltip
                 "Tooltip on right"
                 (button "Right"
                         (lambda () (void))
                         #:class "we-button-secondary")
                 #:placement 'right
                 #:title "Tooltip"
                 #:footer "Footer text"))
               (heading 2 "Toasts")
               (stack
                (inline
                 (text "Bootstrap"
                       #:class "showcase-static-toast-title")
                 (spacer)
                 (text "11 mins ago"
                       #:class "showcase-static-toast-time")
                 (text "×"
                       #:class "showcase-static-toast-close")
                 #:class "showcase-static-toast-header")
                (text "Hello, world! This is a toast message."
                      #:class "showcase-static-toast-body")
                #:class "showcase-static-toast"))
              #:class "we-grid-safe"
              #:id    "solar2-dialogs-body")

        (text @button-log
              #:class "we-button-status")

        )))))))

(define theme-core-link-node     (install-theme-link! "we-theme-core-css"))
(define theme-general-link-node  (install-theme-link! "we-theme-external-css"))
(define theme-showcase-link-node (install-theme-link! "we-theme-showcase-css"))

(apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node (obs-peek @theme))
(obs-observe! @theme
              (lambda (next-theme)
                (apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node next-theme)))

(mount-renderer! app-renderer)
