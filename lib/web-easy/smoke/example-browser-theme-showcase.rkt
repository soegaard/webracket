;;;
;;; web-easy Browser Theme Showcase
;;;

;; User-facing web-easy showcase page (no iframe, no test harness) with a
;; top-level theme chooser wired to external CSS files.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader "smoke-format.rkt" read-syntax/skip-first-line)

;; Constants for static table rows.
(define row/release-1 (list "Release 1.4" "In Progress" "2d")) ; First roadmap row.
(define row/api-clean (list "API Cleanup" "Review" "1d")) ; Second roadmap row.

;; Constants for initial observable values.
(define initial-table-rows (list row/release-1 row/api-clean)) ; Initial table model.

;; Constants for page state.
(define @theme          (@ 'solar))
(define @tab-style      (@ 'joined))
(define @showcase-css-enabled? (@ #t))
(define @theme-status   (@ "solar"))
(define @menu-status    (@ "none"))
(define @name           (@ ""))
(define @enabled?       (@ #t))
(define @role           (@ "Alpha"))
(define @level          (@ 62))
(define @progress-kind  (@ 'info))
(define @tab            (@ "Overview"))
(define @accordion      (@ 'what))
(define @toast-open?    (@ #t))
(define @toast-level    (@ 'info))
(define @toast-title    (@ "Toast"))
(define @toast-msg      (@ "Changes published."))
(define @page           (@ 2))
(define @crumb          (@ 'solar-demo))
(define @list-current   (@ 'current))
(define @off-open?      (@ #f))
(define @off-side       (@ 'end))
(define @table-rows     (@ initial-table-rows))

;; value->display-string : any/c -> string?
;;   Convert common demo values to readable text.
(define (value->display-string v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [(number? v) (number->string v)]
    [(boolean? v) (if v "true" "false")]
    [else "#<value>"]))

;; theme-class-name : symbol? -> string?
;;   Map theme id to html class used by external theme stylesheets.
(define (normalize-theme-id v)
  (define result
    (cond
      [(symbol? v)
       (case v
         [(solar light dark) v]
         [else 'solar])]
      [(string? v)
       (case (string->symbol v)
         [(solar light dark) (string->symbol v)]
         [else 'solar])]
      [else 'solar]))
  result)

;; theme-class-name : any/c -> string?
;;   Map theme id to html class used by external theme stylesheets.
(define (theme-class-name theme)
  (define normalized-theme (normalize-theme-id theme))
  (define class-name
    (case normalized-theme
      [(light) "we-theme-light"]
      [(dark)  "we-theme-dark"]
      [else    "we-theme-solar"]))
  class-name)

;; theme-css-path : any/c -> string?
;;   Map theme id to relative stylesheet path from generated showcase page.
(define (theme-css-path/general theme)
  (define normalized-theme (normalize-theme-id theme))
  (define path
    (case normalized-theme
      [(light) "../themes/theme-external-light.css"]
      [(dark)  "../themes/theme-external-dark.css"]
      [else    "../themes/theme-external-solar.css"]))
  path)

;; theme-css-path/showcase : any/c -> string?
;;   Map theme id to showcase page stylesheet path.
(define (theme-css-path/showcase theme)
  (define normalized-theme (normalize-theme-id theme))
  (define path
    (case normalized-theme
      [(light) "../themes/theme-showcase-light.css"]
      [(dark)  "../themes/theme-showcase-dark.css"]
      [else    "../themes/theme-showcase-solar.css"]))
  path)

;; theme-css-path/core : -> string?
;;   Path to shared web-easy structural core stylesheet.
(define (theme-css-path/core)
  "../themes/web-easy-core.css")

;; normalize-tab-style-id : any/c -> symbol?
;;   Normalize tab style id for root class switching.
(define (normalize-tab-style-id v)
  (define result
    (cond
      [(symbol? v)
       (case v
         [(connected joined pills underline soft rail cards mono signal notch) v]
         [else 'connected])]
      [(string? v)
       (case (string->symbol v)
         [(connected joined pills underline soft rail cards mono signal notch) (string->symbol v)]
         [else 'connected])]
      [else 'connected]))
  result)

;; tab-style-class-name : any/c -> string?
;;   Map tab style id to root class.
(define (tab-style-class-name tab-style)
  (case (normalize-tab-style-id tab-style)
    [(joined)    "we-tab-style-joined"]
    [(pills)     "we-tab-style-pills"]
    [(underline) "we-tab-style-underline"]
    [(soft)      "we-tab-style-soft"]
    [(rail)      "we-tab-style-rail"]
    [(cards)     "we-tab-style-cards"]
    [(mono)      "we-tab-style-mono"]
    [(signal)    "we-tab-style-signal"]
    [(notch)     "we-tab-style-notch"]
    [else        "we-tab-style-connected"]))

;; install-theme-link! : string? -> any/c
;;   Create and attach a stylesheet <link> with the given id.
(define (install-theme-link! link-id)
  (define doc (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id" link-id)
  (js-set-attribute! link "rel" "stylesheet")
  (js-append-child! head link)
  link)

;; apply-theme! : any/c any/c any/c any/c any/c -> void?
;;   Update html class and stylesheet hrefs for theme and tab style.
(define (apply-theme! core-link general-link showcase-link theme tab-style)
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (define class-name
    (string-append (theme-class-name theme)
                   " "
                   (tab-style-class-name tab-style)))
  (define core-path (theme-css-path/core))
  (define general-path (theme-css-path/general theme))
  (define showcase-path (theme-css-path/showcase theme))
  (js-set-attribute! html-node "class" class-name)
  (js-set-attribute! core-link "href" core-path)
  (js-set-attribute! general-link  "href" general-path)
  (js-set-attribute! showcase-link "href" showcase-path)
  (void))

;; apply-showcase-css-enabled! : any/c boolean? -> void?
;;   Enable/disable showcase page-specific stylesheet.
(define (apply-showcase-css-enabled! showcase-link enabled?)
  (js-set-attribute! showcase-link "media" (if enabled? "all" "not all"))
  (void))

;; set-progress-variant! : symbol? -> void?
;;   Update progress variant and keep level in-range.
(define (set-progress-variant! variant)
  (:= @progress-kind variant)
  (when (> (obs-peek @level) 100)
    (:= @level 100))
  (void))

;; show-toast! : symbol? string? string? -> void?
;;   Configure toast payload and open it.
(define (show-toast! level title msg)
  (:= @toast-level level)
  (:= @toast-title title)
  (:= @toast-msg msg)
  (:= @toast-open? #t)
  (void))

;; reset-table! : -> void?
;;   Restore demo table rows to initial data.
(define (reset-table!)
  (:= @table-rows initial-table-rows)
  (void))

(define app-renderer
  (render
   (window
    (container
       (stack
        ;;;
        ;;; TOP BAR
        ;;;
        (card #f #f
             (inline
                ;; Brand (left)
                (stack
                   (text "web-easy Theme Showcase" #:class "showcase-brand-title")
                   (text "Solar-inspired style demo page (no iframes, no test harness)" #:class "showcase-brand-subtitle")
                   #:class "showcase-brand")
                (spacer)
                ;; Controls (right)
                (stack
                   ;; Label
                   (text "Theme" #:class "showcase-controls-label")
                   ;; Select theme
                   (choice '((solar "Solar") (light "Light") (dark "Dark"))
                             @theme
                             (lambda (next-theme)
                               (:= @theme (normalize-theme-id next-theme))
                               (:= @theme-status (value->display-string next-theme)))
                             #:class "showcase-theme-choice")
                   ;; Label
                   (text "Tab style" #:class "showcase-controls-label")
                   ;; Select tab style variant
                   (choice '((connected "Connected")
                               (joined "Joined")
                               (pills "Pills")
                               (underline "Underline")
                               (soft "Soft")
                               (rail "Rail")
                               (cards "Cards")
                               (mono "Mono")
                               (signal "Signal")
                             (notch "Notch"))
                             @tab-style
                             (lambda (next-style)
                               (:= @tab-style (normalize-tab-style-id next-style)))
                             #:class "showcase-theme-choice")
                   ;; Toogle the showcase specific css file on/off
                   (inline
                      (button "Toggle Showcase CSS"
                              (lambda ()
                                (:= @showcase-css-enabled? (not (obs-peek @showcase-css-enabled?)))))
                      (text (~> @showcase-css-enabled?
                                (lambda (enabled?)
                                  (if enabled?
                                      "Showcase CSS: on"
                                      "Showcase CSS: off"))))
                      #:class "showcase-controls-download-row")
                   ;; Download links
                   (toolbar
                      (toolbar-group
                       (link "Download Solar CSS" "../themes/theme-external-solar.css" #t #:class "showcase-link-button")
                       (link "Download Light CSS" "../themes/theme-external-light.css" #t #:class "showcase-link-button")
                       (link "Download Dark CSS" "../themes/theme-external-dark.css" #t #:class "showcase-link-button"))
                      #:class "showcase-controls-download-row")
                   #:class "showcase-topbar-controls")
                #:class "showcase-topbar-inner")
             #:id "theme-showcase-hero"
             #:class "showcase-topbar")

        ;;;
        ;;; Hero Lead
        ;;;

        (card #f #f
           (text "Solar Theme" #:class "showcase-hero-title")
           (text
              (~a
               "This page is a user-facing component gallery inspired by the Solar Bootswatch look. "
               "Use the theme selector above to switch between external stylesheets.")
              #:class "showcase-hero-lead")
           #:class "showcase-hero")

        ;;;
        ;;; Main Contents
        ;;;

        (stack
           (grid 'auto
             (card
               "Buttons, Alerts, Badge"
               #f
               'compact
               (toolbar
                 (toolbar-group
                  (button "Primary"
                            (lambda ()
                              (show-toast! 'info "Buttons" "Primary clicked."))
                            #:class "we-button-primary")
                  (button "Secondary"
                            (lambda ()
                              (show-toast! 'warning "Buttons" "Secondary clicked."))
                            #:class "we-button-secondary")
                  (button "Ghost"
                            (lambda ()
                              (show-toast! 'success "Buttons" "Ghost clicked."))
                            #:class "we-button-ghost"))
                 #:class "showcase-button-row")
               (alert "Info alert: configuration saved." 'info)
               (alert "Success alert: build finished." 'success)
               (alert "Warning alert: pending review." 'warning)
               (alert "Error alert: deploy failed." 'danger)
               (inline
                 (badge "Badge" 'info)
                 (badge "42" 'success)
                 #:class "showcase-button-row")
               #:class "showcase-card")

             (card
               "Forms & Menu"
               #f
               (input
                 @name
                 (lambda (v) (:= @name v))
                 #:attrs '((placeholder "Your name")))
               (choice '("Alpha" "Beta" "Gamma")
                       @role
                       (lambda (v) (:= @role v)))
               (textarea
                 ""
                 (lambda (_v) (void))
                 3
                 #:attrs '((placeholder "Notes")))
               (menu-bar
                (menu "Project"
                      (menu-item "Open"
                                 (lambda () (:= @menu-status "Project/Open"))
                                 "📂")
                      (menu-item "Save"
                                 (lambda () (:= @menu-status "Project/Save"))
                                 "💾")
                      (menu-item "Close"
                                 (lambda () (:= @menu-status "Project/Close"))
                                 "×"))
                (menu "Help"
                      (menu-item "Docs"
                                 (lambda () (:= @menu-status "Help/Docs"))
                                 "📘")
                      (menu-item "FAQ"
                                 (lambda () (:= @menu-status "Help/FAQ"))
                                 "?"
                                 "›")))
               #:class "showcase-card")

             (card
               "Tabs, List Group, Table"
               #f
               (tab-panel
                @tab
                (list (cons "Overview" (text "Overview content."))
                      (cons "Settings" (text "Settings content."))
                      (cons "History"  (text "History content."))))
               (list-group '((current "Current item")
                             (secondary "Secondary item")
                             (third "Third item"))
                           @list-current
                           (lambda (v) (:= @list-current v)))
               (table
                 '("Name" "Status" "ETA")
                 @table-rows
                 #:density 'compact)
               #:class "showcase-card")
             #:class "showcase-grid-row")

           (grid 'auto
             (card
               "Breadcrumb, Pagination, Progress, Spinner, Toast"
               #f
               (breadcrumb '((home "Home")
                             (projects "Projects")
                             (solar-demo "Solar Demo"))
                           @crumb
                           (lambda (v) (:= @crumb v)))
               (pagination 3
                           @page
                           (lambda (v) (:= @page v)))
               (progress
                 @level
                 #:min 0
                 #:max 100
                 #:variant @progress-kind)
               (spinner "Loading activity...")
               (toast @toast-open?
                      (lambda () (:= @toast-open? #f))
                      "Toast: Changes published."
                      @toast-level
                      #f
                      #t)
               #:class "showcase-card")

             (card
               "Accordion"
               #f
               (accordion
                @accordion
                (list
                 (list 'what
                       "What is web-easy?"
                       (text "web-easy is a web-rendered gui-easy style toolkit for WebRacket."))
                 (list 'themes
                       "How do themes work?"
                       (text "Theme files override CSS tokens and widget classes only."))))
               (offcanvas
                 @off-open?
                 (lambda () (:= @off-open? #f))
                 (text "Offcanvas panel with extra context.")
                 #:side @off-side)
               #:class "showcase-card")
             #:class "showcase-grid-row")
           #:class "showcase-main"))
       #:class "showcase-shell"))))
(define theme-core-link-node     (install-theme-link! "we-theme-core-css"))
(define theme-general-link-node  (install-theme-link! "we-theme-external-css"))
(define theme-showcase-link-node (install-theme-link! "we-theme-showcase-css"))

(apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node (obs-peek @theme) (obs-peek @tab-style))
(apply-showcase-css-enabled! theme-showcase-link-node (obs-peek @showcase-css-enabled?))
(obs-observe! @theme (lambda (next-theme)
                       (apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node next-theme (obs-peek @tab-style))))
(obs-observe! @tab-style (lambda (next-style)
                           (apply-theme! theme-core-link-node theme-general-link-node theme-showcase-link-node (obs-peek @theme) next-style)))
(obs-observe! @showcase-css-enabled?
              (lambda (enabled?)
                (apply-showcase-css-enabled! theme-showcase-link-node enabled?)))

(mount-renderer! app-renderer)
