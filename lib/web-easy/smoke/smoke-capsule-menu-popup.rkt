#lang webracket

;;;
;;; Smoke Capsule: Menu Popup
;;;

;; Isolated smoke capsule for standalone menu-popup keyboard entry.
;;
;; Exports:
;;   menu-popup-make-page   Build and mount the popup page under root.
;;   menu-popup-run-test    Execute capsule-local setup checks.
;;   menu-popup-cleanup     Destroy mounted renderer state for this capsule.

(define-values (menu-popup-make-page menu-popup-run-test menu-popup-cleanup)
  (let ()
    ;; Constants for popup capsule state.
    (define menu-popup-renderer #f) ; Mounted renderer for this capsule.

    ;; menu-popup-make-page : any/c -> void?
    ;;   Build and mount the popup page under root.
    (define (menu-popup-make-page root)
      ;; Constants for observable state.
      (define @open?  (@ #f))
      (define @status (@ "idle"))

      ;; open-popup! : -> void?
      ;;   Show the popup and track open status.
      (define (open-popup!)
        (:= @open?  #t)
        (:= @status "open"))

      ;; close-popup! : string? -> void?
      ;;   Hide the popup and track closing reason.
      (define (close-popup! why)
        (:= @open?  #f)
        (:= @status why))

      ;; popup-key! : event? -> void?
      ;;   Close the popup on Escape.
      (define (popup-key! evt)
        (define key (keyboard-event-key evt))
        (when (string=? key "Escape")
          (prevent-default! evt)
          (close-popup! "escape"))
        (void))

      ;; mark! : string? -> void?
      ;;   Record which popup action ran.
      (define (mark! label)
        (:= @status label)
        (:= @open? #f))

      (set! menu-popup-renderer
            (render
             (window
              (vpanel
               (button "open-popup" open-popup!)
               (observable-view
                @open?
                (lambda (open?)
                  (if open?
                      (Div #:id "popup-menu"
                           #:autofocus #t
                           #:attrs '((role "menu")
                                     (tabindex "0")
                                     (data-we-widget "menu-popup")
                                     (class "we-menu-popup is-open"))
                           #:on-keydown popup-key!
                           (menu-item "Alpha"  (lambda () (mark! "alpha")))
                           (menu-item "Bravo"
                                      (lambda () (mark! "bravo"))
                                      #:disabled #t)
                           (menu-item "Delete" (lambda () (mark! "delete"))))
                      (text "popup-hidden"))))
               (text (~> @status
                         (lambda (status)
                           (~a "status:" status))))))))
      (mount-renderer! menu-popup-renderer root)
      (void))

    ;; menu-popup-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (menu-popup-run-test _root)
      (and menu-popup-renderer #t))

    ;; menu-popup-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (menu-popup-cleanup _root)
      (when menu-popup-renderer
        (renderer-destroy menu-popup-renderer)
        (set! menu-popup-renderer #f))
      (void))

    (values menu-popup-make-page menu-popup-run-test menu-popup-cleanup)))
