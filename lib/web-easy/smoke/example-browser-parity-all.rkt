;;;
;;; web-easy Browser Parity-All Driver
;;;

;; Single compiled parity driver for isolated parity capsules selected by `?test=...`.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-hello.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-counter.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dynamic-list.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-counters.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs-disabled.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-tabs-dynamic.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-profile.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-settings.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-table.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-menu-keys.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-menu-full.rkt" read-syntax/skip-first-line)
(include/reader "smoke-capsule-parity-dialog.rkt" read-syntax/skip-first-line)
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
        (cons 'parity-dialog
              (list parity-dialog-make-page
                    parity-dialog-run-test
                    parity-dialog-cleanup))
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
     (render-error! "parity-all test setup failed"))]
  [else
   (render-error! "unknown parity-all test id")])
