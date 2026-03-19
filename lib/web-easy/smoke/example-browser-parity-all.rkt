;;;
;;; web-easy Browser Parity-All Driver
;;;

;; Single compiled parity driver for isolated parity capsules selected by `?test=...`.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader "smoke-format.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-hello.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-counter.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dynamic-list.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-counters.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs-disabled.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs-dynamic.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-profile.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-settings.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-progress.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-table.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-menu-keys.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-menu-full.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dropdown.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-choice-decode.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-navigation-bar.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-navigation-bar-advanced.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-button-group.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-button-toolbar.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-card.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-alert.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-badge.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-spinner.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-pagination.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-breadcrumb.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-list-group.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-toast.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-toast-timing.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-collapse.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-accordion.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-offcanvas.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dialog.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-modal.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dialog-no-desc.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-table-align.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tooltip.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-popover.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-close-button.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-placeholder.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-headings.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-primitive-tags.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-hero.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-carousel.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-carousel-advanced.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-scrollspy.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-scrollspy-docs.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-list.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-todo.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-incident.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-release.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-workspace.rkt" read-syntax/skip-first-line)

;; Constants used by parity-all query dispatch and test registry.
(define query/test-prefix "?test=") ; Prefix used in query dispatch.

;; parity-tests : list?
;;   Registry from test id to (make-page run-test cleanup) triple.
(define parity-tests
  (list (cons 'parity-hello
              (list parity-hello-make-page parity-hello-run-test parity-hello-cleanup))
        (cons 'parity-counter
              (list parity-counter-make-page parity-counter-run-test parity-counter-cleanup))
        (cons 'parity-dynamic-list
              (list parity-dynamic-list-make-page
                    parity-dynamic-list-run-test
                    parity-dynamic-list-cleanup))
        (cons 'parity-counters
              (list parity-counters-make-page parity-counters-run-test parity-counters-cleanup))
        (cons 'parity-tabs
              (list parity-tabs-make-page parity-tabs-run-test parity-tabs-cleanup))
        (cons 'parity-tabs-disabled
              (list parity-tabs-disabled-make-page
                    parity-tabs-disabled-run-test
                    parity-tabs-disabled-cleanup))
        (cons 'parity-tabs-dynamic
              (list parity-tabs-dynamic-make-page
                    parity-tabs-dynamic-run-test
                    parity-tabs-dynamic-cleanup))
        (cons 'parity-profile
              (list parity-profile-make-page parity-profile-run-test parity-profile-cleanup))
        (cons 'parity-settings
              (list parity-settings-make-page parity-settings-run-test parity-settings-cleanup))
        (cons 'parity-progress
              (list parity-progress-make-page parity-progress-run-test parity-progress-cleanup))
        (cons 'parity-table
              (list parity-table-make-page parity-table-run-test parity-table-cleanup))
        (cons 'parity-menu-keys
              (list parity-menu-keys-make-page
                    parity-menu-keys-run-test
                    parity-menu-keys-cleanup))
        (cons 'parity-menu-full
              (list parity-menu-full-make-page
                    parity-menu-full-run-test
                    parity-menu-full-cleanup))
        (cons 'parity-dropdown
              (list parity-dropdown-make-page
                    parity-dropdown-run-test
                    parity-dropdown-cleanup))
        (cons 'parity-choice-decode
              (list parity-choice-decode-make-page
                    parity-choice-decode-run-test
                    parity-choice-decode-cleanup))
        (cons 'parity-navigation-bar
              (list parity-navigation-bar-make-page
                    parity-navigation-bar-run-test
                    parity-navigation-bar-cleanup))
        (cons 'parity-navigation-bar-advanced
              (list parity-navigation-bar-advanced-make-page
                    parity-navigation-bar-advanced-run-test
                    parity-navigation-bar-advanced-cleanup))
        (cons 'parity-button-group
              (list parity-button-group-make-page
                    parity-button-group-run-test
                    parity-button-group-cleanup))
        (cons 'parity-button-toolbar
              (list parity-button-toolbar-make-page
                    parity-button-toolbar-run-test
                    parity-button-toolbar-cleanup))
        (cons 'parity-card
              (list parity-card-make-page
                    parity-card-run-test
                    parity-card-cleanup))
        (cons 'parity-alert
              (list parity-alert-make-page
                    parity-alert-run-test
                    parity-alert-cleanup))
        (cons 'parity-badge
              (list parity-badge-make-page
                    parity-badge-run-test
                    parity-badge-cleanup))
        (cons 'parity-spinner
              (list parity-spinner-make-page
                    parity-spinner-run-test
                    parity-spinner-cleanup))
        (cons 'parity-pagination
              (list parity-pagination-make-page
                    parity-pagination-run-test
                    parity-pagination-cleanup))
        (cons 'parity-breadcrumb
              (list parity-breadcrumb-make-page
                    parity-breadcrumb-run-test
                    parity-breadcrumb-cleanup))
        (cons 'parity-list-group
              (list parity-list-group-make-page
                    parity-list-group-run-test
                    parity-list-group-cleanup))
        (cons 'parity-toast
              (list parity-toast-make-page
                    parity-toast-run-test
                    parity-toast-cleanup))
        (cons 'parity-toast-timing
              (list parity-toast-timing-make-page
                    parity-toast-timing-run-test
                    parity-toast-timing-cleanup))
        (cons 'parity-collapse
              (list parity-collapse-make-page
                    parity-collapse-run-test
                    parity-collapse-cleanup))
        (cons 'parity-accordion
              (list parity-accordion-make-page
                    parity-accordion-run-test
                    parity-accordion-cleanup))
        (cons 'parity-offcanvas
              (list parity-offcanvas-make-page
                    parity-offcanvas-run-test
                    parity-offcanvas-cleanup))
        (cons 'parity-dialog
              (list parity-dialog-make-page
                    parity-dialog-run-test
                    parity-dialog-cleanup))
        (cons 'parity-modal
              (list parity-modal-make-page
                    parity-modal-run-test
                    parity-modal-cleanup))
        (cons 'parity-dialog-no-desc
              (list parity-dialog-no-desc-make-page
                    parity-dialog-no-desc-run-test
                    parity-dialog-no-desc-cleanup))
        (cons 'parity-table-align
              (list parity-table-align-make-page
                    parity-table-align-run-test
                    parity-table-align-cleanup))
        (cons 'parity-tooltip
              (list parity-tooltip-make-page
                    parity-tooltip-run-test
                    parity-tooltip-cleanup))
        (cons 'parity-popover
              (list parity-popover-make-page
                    parity-popover-run-test
                    parity-popover-cleanup))
        (cons 'parity-close-button
              (list parity-close-button-make-page
                    parity-close-button-run-test
                    parity-close-button-cleanup))
        (cons 'parity-placeholder
              (list parity-placeholder-make-page
                    parity-placeholder-run-test
                    parity-placeholder-cleanup))
        (cons 'parity-headings
              (list parity-headings-make-page
                    parity-headings-run-test
                    parity-headings-cleanup))
        (cons 'parity-primitive-tags
              (list parity-primitive-tags-make-page
                    parity-primitive-tags-run-test
                    parity-primitive-tags-cleanup))
        (cons 'parity-hero
              (list parity-hero-make-page
                    parity-hero-run-test
                    parity-hero-cleanup))
        (cons 'parity-carousel
              (list parity-carousel-make-page
                    parity-carousel-run-test
                    parity-carousel-cleanup))
        (cons 'parity-carousel-advanced
              (list parity-carousel-advanced-make-page
                    parity-carousel-advanced-run-test
                    parity-carousel-advanced-cleanup))
        (cons 'parity-scrollspy
              (list parity-scrollspy-make-page
                    parity-scrollspy-run-test
                    parity-scrollspy-cleanup))
        (cons 'parity-scrollspy-docs
              (list parity-scrollspy-docs-make-page
                    parity-scrollspy-docs-run-test
                    parity-scrollspy-docs-cleanup))
        (cons 'parity-list
              (list parity-list-make-page parity-list-run-test parity-list-cleanup))
        (cons 'parity-todo
              (list parity-todo-make-page parity-todo-run-test parity-todo-cleanup))
        (cons 'parity-incident
              (list parity-incident-make-page
                    parity-incident-run-test
                    parity-incident-cleanup))
        (cons 'parity-release
              (list parity-release-make-page
                    parity-release-run-test
                    parity-release-cleanup))
        (cons 'parity-workspace
              (list parity-workspace-make-page
                    parity-workspace-run-test
                    parity-workspace-cleanup))))

;; query->test-id : string? -> symbol?
;;   Parse `?test=...` query into a registered symbol id.
(define (query->test-id search)
  (define prefix-len (string-length query/test-prefix))
  (cond
    [(and (>= (string-length search) (+ prefix-len 1))
          (string=? (substring search 0 prefix-len) query/test-prefix))
     (string->symbol (substring search prefix-len))]
    [else
     'parity-hello]))

;; selected-test-id : symbol?
;;   Resolve current test id from browser location search.
(define selected-test-id
  (let ()
    (define loc (js-window-location))
    (define search (js-value->string
                    (js-ref/extern loc "search")))
    (query->test-id search)))

;; selected-entry : any/c
;;   Lookup selected test entry in the parity registry.
(define selected-entry
  (let ([p (assq selected-test-id parity-tests)])
    (if p (cdr p) #f)))

;; root : any/c
;;   Dedicated root container for mounted parity capsule page.
(define root
  (let ()
    (define r (js-create-element "div"))
    (js-set-attribute! r "id" "parity-root")
    (js-append-child! (js-document-body) r)
    r))

;; render-danger! : string? -> void?
;;   Render a plain-text danger in the root container.
(define (render-danger! msg)
  (js-replace-children! root (js-create-text-node msg)))

(cond
  [selected-entry
   (define make-page (car selected-entry))
   (define run-test (cadr selected-entry))
   (define _cleanup (caddr selected-entry))
   (make-page root)
   (unless (run-test root)
     (render-danger! "parity-all test setup failed"))]
  [else
   (render-danger! "unknown parity-all test id")])
