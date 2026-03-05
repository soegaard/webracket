#lang webracket

;;;
;;; Smoke Capsule: Parity tabs dynamic
;;;

;; Isolated parity capsule for parity-tabs-dynamic.
;;
;; Exports:
;;   parity-tabs-dynamic-make-page      Build and mount the parity page under root.
;;   parity-tabs-dynamic-run-test       Execute capsule-local setup checks.
;;   parity-tabs-dynamic-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-tabs-dynamic-make-page parity-tabs-dynamic-run-test parity-tabs-dynamic-cleanup)
  (let ()
    ;; Constants for parity-tabs-dynamic capsule state.
    (define parity-tabs-dynamic-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-tabs-dynamic-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-tabs-dynamic-make-page root)
      ;;;
      ;;; web-easy Browser Parity Dynamic Tabs Example
      ;;;
      
      ;; Parity example: dynamic tab add/remove with selected-tab preservation.
      
      
      (struct parity-tab (id label) #:transparent)
      
      ;; Constants for example-local observable state.
      (define @tabs
        (@ (list (parity-tab 'overview "Overview")
                 (parity-tab 'details "Details")
                 (parity-tab 'help "Help"))))
      (define @selected (@ 'overview))
      
      ;; has-tab-id? : list? symbol? -> boolean?
      ;;   Check whether tabs contains a tab with id.
      (define (has-tab-id? tabs id)
        (cond
          [(null? tabs)
           #f]
          [else
           (if (eq? (parity-tab-id (car tabs)) id)
               #t
               (has-tab-id? (cdr tabs) id))]))
      
      ;; first-tab-id : list? -> (or/c symbol? #f)
      ;;   Return the id of the first tab or #f when tabs is empty.
      (define (first-tab-id tabs)
        (if (null? tabs) #f (parity-tab-id (car tabs))))
      
      ;; panel-entries : list? -> list?
      ;;   Convert tab records to tab-panel entry pairs.
      (define (panel-entries tabs)
        (map (lambda (tab)
               (cons (parity-tab-id tab)
                     (text (~a (parity-tab-label tab) " panel"))))
             tabs))
      
      ;; add-tab! : symbol? string? -> void?
      ;;   Append a tab if id does not exist.
      (define (add-tab! id label)
        (obs-update! @tabs
                     (lambda (tabs)
                       (if (has-tab-id? tabs id)
                           tabs
                           (append tabs (list (parity-tab id label)))))))
      
      ;; remove-tab! : symbol? -> void?
      ;;   Remove tab id and preserve selected value when possible.
      (define (remove-tab! id)
        (define selected-id (obs-peek @selected))
        (obs-update! @tabs
                     (lambda (tabs)
                       (define new-tabs
                         (filter (lambda (tab)
                                   (not (eq? (parity-tab-id tab) id)))
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
      
      (define app-renderer
        (render
         (window
          (vpanel
           (hpanel
            (button "add-faq" (lambda ()
                                (add-tab! 'faq "FAQ")))
            (button "remove-details" (lambda ()
                                       (remove-tab! 'details)))
            (button "remove-selected" remove-selected!))
           (text "Keyboard: focus a tab header (overview/details/help/faq), then use ArrowLeft/ArrowRight.")
           (observable-view @tabs
                            (lambda (tabs)
                              (if (pair? tabs)
                                  (tab-panel @selected
                                             (panel-entries tabs))
                                  (text "No tabs"))))))))
      
      (set! parity-tabs-dynamic-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-tabs-dynamic-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-tabs-dynamic-run-test _root)
      (and parity-tabs-dynamic-renderer #t))

    ;; parity-tabs-dynamic-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-tabs-dynamic-cleanup _root)
      (when parity-tabs-dynamic-renderer
        (renderer-destroy parity-tabs-dynamic-renderer)
        (set! parity-tabs-dynamic-renderer #f))
      (void))

    (values parity-tabs-dynamic-make-page parity-tabs-dynamic-run-test parity-tabs-dynamic-cleanup)))
