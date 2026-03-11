#lang webracket

;;;
;;; Smoke Capsule: Surface API
;;;

;; Isolated smoke capsule for placement/size/tab-variant API coverage in smoke-all.
;;
;; Exports:
;;   surface-api-make-page   Build and mount the surface-api page under root.
;;   surface-api-run-test    Execute capsule-local setup checks.
;;   surface-api-cleanup     Destroy mounted renderer state for this capsule.

(define-values (surface-api-make-page surface-api-run-test surface-api-cleanup)
  (let ()
    ;; Constants for surface-api capsule state.
    (define surface-api-renderer #f) ; Mounted renderer for this capsule.

    ;; surface-api-make-page : any/c -> void?
    ;;   Build and mount the surface-api page under root.
    (define (surface-api-make-page root)
      ;; Constants for observable state.
      (define @dialog-open (@ #f))
      (define @modal-open  (@ #f))
      (define @tab         (@ 'alpha))
      (define @alert-open? (@ #t))

      ;; open-dialog! : -> void?
      ;;   Open the large dialog test fixture.
      (define (open-dialog!)
        (:= @dialog-open #t))

      ;; close-dialog! : -> void?
      ;;   Close the large dialog test fixture.
      (define (close-dialog!)
        (:= @dialog-open #f))

      ;; open-modal! : -> void?
      ;;   Open the small modal test fixture.
      (define (open-modal!)
        (:= @modal-open #t))

      ;; close-modal! : -> void?
      ;;   Close the small modal test fixture.
      (define (close-modal!)
        (:= @modal-open #f))

      ;; dismiss-alert! : -> void?
      ;;   Hide the alert-rich dismiss action marker.
      (define (dismiss-alert!)
        (:= @alert-open? #f))

      (set! surface-api-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "open-dialog-lg" open-dialog!)
                (button "open-modal-sm" open-modal!))
               (tooltip "right tooltip"
                        (button "tooltip-target" (lambda () (void)))
                        'right)
               (popover "popover-left"
                        'left
                        (text "popover body"))
               (if-view @alert-open?
                        (alert-rich "Server warning"
                                    "Warning"
                                    "Details"
                                    "/alerts"
                                    'warning
                                    (list (cons 'dismiss-action dismiss-alert!)
                                          (cons 'dismiss-label "Close warning")))
                        (text "alert-closed"))
               (card "card-title"
                     "card-footer"
                     (list (cons 'subtitle "card-subtitle")
                           (cons 'media (text "card-media"))
                           (cons 'actions (list (button "card-action" (lambda () (void))))))
                     (text "card-body"))
               (dropdown "dropdown-end"
                         '((open "Open") (close "Close"))
                         (lambda (_id) (void))
                         'end)
               (tab-panel @tab
                          (list (cons 'alpha (text "alpha-tab"))
                                (cons 'beta  (text "beta-tab")))
                          'joined)
               (dialog @dialog-open
                       close-dialog!
                       'lg
                       (list (cons 'title "Dialog title")
                             (cons 'description "Dialog description")
                             (cons 'show-close? #t)
                             (cons 'footer (button "dialog-footer-action" (lambda () (void)))))
                       (vpanel
                        (text "dialog-lg")
                        (button "close-dialog" close-dialog!)))
               (modal @modal-open
                      close-modal!
                      'sm
                      (list (cons 'title "Modal title")
                            (cons 'description "Modal description")
                            (cons 'show-close? #t)
                            (cons 'footer (button "modal-footer-action" (lambda () (void)))))
                      (vpanel
                       (text "modal-sm")
                       (button "close-modal" close-modal!)))))))
      (mount-renderer! surface-api-renderer root)
      (void))

    ;; surface-api-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (surface-api-run-test _root)
      (and surface-api-renderer #t))

    ;; surface-api-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (surface-api-cleanup _root)
      (when surface-api-renderer
        (renderer-destroy surface-api-renderer)
        (set! surface-api-renderer #f))
      (void))

    (values surface-api-make-page surface-api-run-test surface-api-cleanup)))
