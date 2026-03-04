#lang webracket

;;;
;;; Smoke Capsule: Tab Panel Dynamic
;;;

;; Isolated smoke capsule for dynamic tab add/remove with selected preservation.
;;
;; Exports:
;;   tab-panel-dynamic-make-page     Build and mount the dynamic tab page under root.
;;   tab-panel-dynamic-run-test      Execute capsule-local setup checks.
;;   tab-panel-dynamic-cleanup       Destroy mounted renderer state for this capsule.

(define-values (tab-panel-dynamic-make-page
                tab-panel-dynamic-run-test
                tab-panel-dynamic-cleanup)
  (let ()
    ;; Constants for dynamic tab-panel capsule state.
    (define tab-panel-dynamic-renderer #f) ; Mounted renderer for this capsule.

    ;; tab-id : pair? -> symbol?
    ;;   Extract tab id from tab descriptor.
    (define (tab-id tab)
      (car tab))

    ;; tab-label : pair? -> string?
    ;;   Extract tab label from tab descriptor.
    (define (tab-label tab)
      (cdr tab))

    ;; has-tab-id? : list? symbol? -> boolean?
    ;;   Check whether tabs contains id.
    (define (has-tab-id? tabs id)
      (cond
        [(null? tabs) #f]
        [else
         (if (eq? (tab-id (car tabs)) id)
             #t
             (has-tab-id? (cdr tabs) id))]))

    ;; first-tab-id : list? -> (or/c symbol? #f)
    ;;   Return first tab id or #f.
    (define (first-tab-id tabs)
      (if (null? tabs) #f (tab-id (car tabs))))

    ;; panel-entries : list? -> list?
    ;;   Convert tabs to tab-panel entries.
    (define (panel-entries tabs)
      (map (lambda (tab)
             (cons (tab-id tab)
                   (text (string-append (tab-label tab) " panel"))))
           tabs))

    ;; tab-panel-dynamic-make-page : any/c -> void?
    ;;   Build and mount the dynamic tab page under root.
    (define (tab-panel-dynamic-make-page root)
      ;; Constants for observable state.
      (define @tabs
        (@ (list (cons 'overview "Overview")
                 (cons 'details "Details")
                 (cons 'help "Help"))))
      (define @selected (@ 'overview))

      ;; remove-tab! : symbol? -> void?
      ;;   Remove tab id and keep selected valid.
      (define (remove-tab! id)
        (define selected-id (obs-peek @selected))
        (obs-update! @tabs
                     (lambda (tabs)
                       (define new-tabs
                         (filter (lambda (tab)
                                   (not (eq? (tab-id tab) id)))
                                 tabs))
                       (when (eq? selected-id id)
                         (obs-set! @selected (first-tab-id new-tabs)))
                       new-tabs)))

      ;; remove-selected! : -> void?
      ;;   Remove currently selected tab.
      (define (remove-selected!)
        (define selected-id (obs-peek @selected))
        (when selected-id
          (remove-tab! selected-id)))

      ;; add-faq! : -> void?
      ;;   Append faq tab if missing.
      (define (add-faq!)
        (obs-update! @tabs
                     (lambda (tabs)
                       (if (has-tab-id? tabs 'faq)
                           tabs
                           (append tabs (list (cons 'faq "FAQ")))))))

      (set! tab-panel-dynamic-renderer
        (render
         (window
          (vpanel
           (hpanel
            (button "add-faq" add-faq!)
            (button "remove-details" (lambda () (remove-tab! 'details)))
            (button "remove-selected" remove-selected!))
           (observable-view
            @tabs
            (lambda (tabs)
              (if (pair? tabs)
                  (tab-panel @selected (panel-entries tabs))
                  (text "No tabs"))))))))
      (mount-renderer! tab-panel-dynamic-renderer root)
      (void))

    ;; tab-panel-dynamic-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (tab-panel-dynamic-run-test _root)
      (and tab-panel-dynamic-renderer #t))

    ;; tab-panel-dynamic-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (tab-panel-dynamic-cleanup _root)
      (when tab-panel-dynamic-renderer
        (renderer-destroy tab-panel-dynamic-renderer)
        (set! tab-panel-dynamic-renderer #f))
      (void))

    (values tab-panel-dynamic-make-page
            tab-panel-dynamic-run-test
            tab-panel-dynamic-cleanup)))
