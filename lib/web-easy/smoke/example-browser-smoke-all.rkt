;;;
;;; web-easy Browser Smoke-All Driver
;;;

;; Single compiled smoke driver for isolated test capsules selected by `?test=...`.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader "smoke-format.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-smoke.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-input.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-checkbox.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-list.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-branch.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-destroy.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-controls.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-progress.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-width.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-menu-keys.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-menu-full.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-dropdown.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-navigation-bar.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-group.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-button-group.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-button-toolbar.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-card.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-alert.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-badge.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-spinner.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-pagination.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-breadcrumb.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-list-group.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-toast.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-collapse.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-accordion.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-tooltip.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-popover.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-dialog.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-operators.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-tab-panel.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-tab-panel-disabled.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-tab-panel-dynamic.rkt" read-syntax/skip-first-line)

;; Constants used by smoke-all query dispatch and test registry.
(define query/test-prefix "?test=") ; Prefix used in query dispatch.

;; smoke-tests : list?
;;   Registry from test id to (make-page run-test cleanup) triple.
(define smoke-tests
  (list (cons 'smoke    (list smoke-make-page    smoke-run-test    smoke-cleanup))
        (cons 'input    (list input-make-page    input-run-test    input-cleanup))
        (cons 'checkbox (list checkbox-make-page checkbox-run-test checkbox-cleanup))
        (cons 'list     (list list-make-page     list-run-test     list-cleanup))
        (cons 'branch   (list branch-make-page   branch-run-test   branch-cleanup))
        (cons 'destroy  (list destroy-make-page  destroy-run-test  destroy-cleanup))
        (cons 'controls (list controls-make-page controls-run-test controls-cleanup))
        (cons 'progress (list progress-make-page progress-run-test progress-cleanup))
        (cons 'width    (list width-make-page    width-run-test    width-cleanup))
        (cons 'menu-keys (list menu-keys-make-page menu-keys-run-test menu-keys-cleanup))
        (cons 'menu-full (list menu-full-make-page menu-full-run-test menu-full-cleanup))
        (cons 'dropdown (list dropdown-make-page dropdown-run-test dropdown-cleanup))
        (cons 'navigation-bar
              (list navigation-bar-make-page navigation-bar-run-test navigation-bar-cleanup))
        (cons 'group    (list group-make-page group-run-test group-cleanup))
        (cons 'button-group
              (list button-group-make-page button-group-run-test button-group-cleanup))
        (cons 'button-toolbar
              (list button-toolbar-make-page button-toolbar-run-test button-toolbar-cleanup))
        (cons 'card     (list card-make-page card-run-test card-cleanup))
        (cons 'alert    (list alert-make-page alert-run-test alert-cleanup))
        (cons 'badge    (list badge-make-page badge-run-test badge-cleanup))
        (cons 'spinner  (list spinner-make-page spinner-run-test spinner-cleanup))
        (cons 'pagination (list pagination-make-page pagination-run-test pagination-cleanup))
        (cons 'breadcrumb (list breadcrumb-make-page breadcrumb-run-test breadcrumb-cleanup))
        (cons 'list-group (list list-group-make-page list-group-run-test list-group-cleanup))
        (cons 'toast    (list toast-make-page toast-run-test toast-cleanup))
        (cons 'collapse (list collapse-make-page collapse-run-test collapse-cleanup))
        (cons 'accordion (list accordion-make-page accordion-run-test accordion-cleanup))
        (cons 'tooltip  (list tooltip-make-page tooltip-run-test tooltip-cleanup))
        (cons 'popover  (list popover-make-page popover-run-test popover-cleanup))
        (cons 'dialog   (list dialog-make-page dialog-run-test dialog-cleanup))
        (cons 'operators (list operators-make-page operators-run-test operators-cleanup))
        (cons 'tab-panel
              (list tab-panel-make-page tab-panel-run-test tab-panel-cleanup))
        (cons 'tab-panel-disabled
              (list tab-panel-disabled-make-page
                    tab-panel-disabled-run-test
                    tab-panel-disabled-cleanup))
        (cons 'tab-panel-dynamic
              (list tab-panel-dynamic-make-page
                    tab-panel-dynamic-run-test
                    tab-panel-dynamic-cleanup))))

;; query->test-id : string? -> symbol?
;;   Parse `?test=...` query into a registered symbol id.
(define (query->test-id search)
  (define prefix-len (string-length query/test-prefix))
  (cond
    [(and (>= (string-length search) (+ prefix-len 1))
          (string=? (substring search 0 prefix-len) query/test-prefix))
     (string->symbol (substring search prefix-len))]
    [else
     'smoke]))

;; selected-test-id : symbol?
;;   Resolve current test id from browser location search.
(define selected-test-id
  (let ()
    (define loc (js-window-location))
    (define search (js-value->string
                    (js-ref/extern loc "search")))
    (query->test-id search)))

;; selected-entry : any/c
;;   Lookup selected test entry in the smoke registry.
(define selected-entry
  (let ([p (assq selected-test-id smoke-tests)])
    (if p (cdr p) #f)))

;; root : any/c
;;   Dedicated root container for mounted smoke capsule page.
(define root
  (let ()
    (define r (js-create-element "div"))
    (js-set-attribute! r "id" "smoke-root")
    (js-append-child! (js-document-body) r)
    r))

;; render-error! : string? -> void?
;;   Render a plain-text error in the root container.
(define (render-error! msg)
  (js-replace-children! root (js-create-text-node msg)))

(cond
  [selected-entry
   (define make-page (car selected-entry))
   (define run-test (cadr selected-entry))
   (define _cleanup (caddr selected-entry))
   (make-page root)
   (unless (run-test root)
     (render-error! "smoke-all test setup failed"))]
  [else
   (render-error! "unknown smoke-all test id")])
