;;;
;;; web-easy Browser Theme Showcase
;;;

;; User-facing web-easy showcase page (no iframe, no test harness) with a
;; top-level theme chooser wired to external CSS files.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; Constants for static table rows.
(define row/release-1 (list "Release 1.4" "In Progress" "2d")) ; First roadmap row.
(define row/api-clean (list "API Cleanup" "Review" "1d")) ; Second roadmap row.
(define row/docs      (list "Docs Sweep" "Queued" "5d")) ; Third roadmap row.

;; Constants for initial observable values.
(define initial-table-rows (list row/release-1 row/api-clean row/docs)) ; Initial table model.

;; Constants for page state.
(define @theme          (@ 'solar))
(define @theme-status   (@ "solar"))
(define @menu-status    (@ "none"))
(define @name           (@ ""))
(define @enabled?       (@ #t))
(define @role           (@ "Alpha"))
(define @level          (@ 62))
(define @progress-kind  (@ 'info))
(define @tab            (@ 'overview))
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
  (cond
    [(symbol? v) v]
    [(string? v) (string->symbol v)]
    [else 'solar]))

;; theme-class-name : any/c -> string?
;;   Map theme id to html class used by external theme stylesheets.
(define (theme-class-name theme)
  (case (normalize-theme-id theme)
    [(light) "we-theme-light"]
    [(dark)  "we-theme-dark"]
    [else    "we-theme-solar"]))

;; theme-css-path : any/c -> string?
;;   Map theme id to relative stylesheet path from generated showcase page.
(define (theme-css-path theme)
  (case (normalize-theme-id theme)
    [(light) "../theme-external-light.css"]
    [(dark)  "../theme-external-dark.css"]
    [else    "../theme-external-solar.css"]))

;; install-theme-link! : -> any/c
;;   Create and attach external theme <link> to document head.
(define (install-theme-link!)
  (define doc (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id" "we-theme-external-css")
  (js-set-attribute! link "rel" "stylesheet")
  (js-append-child! head link)
  link)

;; apply-theme! : any/c symbol? -> void?
;;   Update html class and external stylesheet href for theme id.
(define (apply-theme! link theme)
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (js-set-attribute! html-node "class" (theme-class-name theme))
  (js-set-attribute! link "href" (theme-css-path theme))
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

;; add-delta-row! : -> void?
;;   Append a fourth row once to show dynamic table updates.
(define (add-delta-row!)
  (define rows (obs-peek @table-rows))
  (define has-delta?
    (cond
      [(null? rows) #f]
      [(string=? (car (car rows)) "delta") #t]
      [else
       (let loop ([rest (cdr rows)])
         (cond
           [(null? rest) #f]
           [(string=? (car (car rest)) "delta") #t]
           [else (loop (cdr rest))]))]))
  (unless has-delta?
    (:= @table-rows (append rows (list (list "delta" "new" "11d")))))
  (void))

;; next-list-item! : -> void?
;;   Rotate list-group selection through demo rows.
(define (next-list-item!)
  (case (obs-peek @list-current)
    [(current)   (:= @list-current 'secondary)]
    [(secondary) (:= @list-current 'third)]
    [else        (:= @list-current 'current)])
  (void))

(define app-renderer
  (render
   (window
    (with-class "showcase-shell"
     (container
      (stack
       (with-id "theme-showcase-hero"
        (with-class "showcase-topbar"
         (card
          #f
          #f
          (with-class "showcase-topbar-inner"
           (inline
            (with-class "showcase-brand"
             (stack
              (with-class "showcase-brand-title"
               (text "web-easy Theme Showcase"))
              (with-class "showcase-brand-subtitle"
               (text "Solar-inspired style demo page (no iframes, no test harness)"))))
            (spacer)
            (with-class "showcase-topbar-controls"
             (stack
              (with-class "showcase-controls-label"
               (text "Theme"))
              (with-class "showcase-theme-choice"
               (choice '(solar light dark)
                       @theme
                       (lambda (next-theme)
                         (:= @theme next-theme)
                         (:= @theme-status (value->display-string next-theme)))))
              (with-class "showcase-controls-download-row"
               (toolbar
                (toolbar-group
                 (with-class "showcase-link-button"
                  (link "Download Solar CSS" "../theme-external-solar.css" #t))
                 (with-class "showcase-link-button"
                  (link "Download Light CSS" "../theme-external-light.css" #t))
                 (with-class "showcase-link-button"
                  (link "Download Dark CSS" "../theme-external-dark.css" #t))))))))))))

       (with-class "showcase-hero"
        (card
         #f
         #f
         (with-class "showcase-hero-title"
          (text "Solar Theme"))
         (with-class "showcase-hero-lead"
          (text
           "This page is a user-facing component gallery inspired by the Solar Bootswatch look. Use the theme selector above to switch external stylesheets."))))

       (with-class "showcase-main"
        (stack
         (with-class "showcase-grid-row"
          (grid 'auto
           (with-class "showcase-card"
            (card
             "Buttons, Alerts, Badge"
             #f
             'compact
             (with-class "showcase-button-row"
              (toolbar
               (toolbar-group
                (with-class "we-button-primary"
                 (button "Primary"
                         (lambda ()
                           (show-toast! 'info "Buttons" "Primary clicked."))))
                (with-class "we-button-secondary"
                 (button "Secondary"
                         (lambda ()
                           (show-toast! 'warn "Buttons" "Secondary clicked."))))
                (with-class "we-button-ghost"
                 (button "Ghost"
                         (lambda ()
                           (show-toast! 'success "Buttons" "Ghost clicked.")))))))
             (alert "Info alert: configuration saved." 'info)
             (alert "Success alert: build finished." 'success)
             (alert "Warning alert: pending review." 'warn)
             (alert "Error alert: deploy failed." 'error)
             (with-class "showcase-button-row"
              (inline
               (badge "Badge" 'info)
               (badge "42" 'success)))))

           (with-class "showcase-card"
            (card
             "Forms & Menu"
             #f
             (with-attrs
              '((placeholder "Your name"))
              (input @name (lambda (v) (:= @name v))))
             (choice '("Alpha" "Beta" "Gamma")
                     @role
                     (lambda (v) (:= @role v)))
             (dropdown "Actions"
                       '((alpha "Alpha")
                         (beta "Beta")
                         (gamma "Gamma"))
                       (lambda (v) (:= @menu-status (value->display-string v))))
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
             (text (~> @menu-status
                       (lambda (s)
                         (string-append "menu-action:" (value->display-string s)))))))))

         (with-class "showcase-grid-row"
          (grid 'auto
           (with-class "showcase-card"
            (card
             "Tabs, List Group, Table"
             #f
             (tab-panel
              @tab
              (list (cons 'overview (text "Overview content."))
                    (cons 'settings (text "Settings content."))
                    (cons 'history  (text "History content."))))
             (list-group '((current "Current item")
                           (secondary "Secondary item")
                           (third "Third item"))
                         @list-current
                         (lambda (v) (:= @list-current v)))
             (table '(name status eta)
                    @table-rows
                    'compact)
             (with-class "showcase-button-row"
              (inline
               (button "add-row" add-delta-row!)
               (button "reset-table" reset-table!)))))

           (with-class "showcase-card"
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
             (slider @level
                     (lambda (v) (:= @level v))
                     0
                     100)
             (progress @level 0 100 @progress-kind)
             (inline
              (spinner "Loading activity...")
              (button "next list item" next-list-item!))
             (button-toolbar
              (button-group
               (button "info"    (lambda () (set-progress-variant! 'info)))
               (button "success" (lambda () (set-progress-variant! 'success)))
               (button "warn"    (lambda () (set-progress-variant! 'warn)))
               (button "error"   (lambda () (set-progress-variant! 'error))))
              (button-group
               (button "toast success"
                       (lambda ()
                         (show-toast! 'success "Toast" "Changes published.")))
               (button "toast error"
                       (lambda ()
                         (show-toast! 'error "Toast" "Publish failed.")))))
             (toast @toast-open?
                    (lambda () (:= @toast-open? #f))
                    @toast-msg
                    @toast-level
                    @toast-title
                    #t)))))

         (with-class "showcase-card"
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
           (offcanvas @off-open?
                      (lambda () (:= @off-open? #f))
                      @off-side
                      (text "Offcanvas panel with extra context."))))))))))))
(define theme-link-node (install-theme-link!))
(apply-theme! theme-link-node (obs-peek @theme))
(obs-observe! @theme (lambda (next-theme)
                       (apply-theme! theme-link-node next-theme)))

(mount-renderer! app-renderer)
