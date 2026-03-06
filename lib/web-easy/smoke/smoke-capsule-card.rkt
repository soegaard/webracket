#lang webracket

;;;
;;; Smoke Capsule: Card
;;;

;; Isolated smoke capsule for card header/footer/body behavior in smoke-all.
;;
;; Exports:
;;   card-make-page   Build and mount the card page under root.
;;   card-run-test    Execute capsule-local setup checks.
;;   card-cleanup     Destroy mounted renderer state for this capsule.

(define-values (card-make-page card-run-test card-cleanup)
  (let ()
    ;; Constants for card capsule state.
    (define card-renderer #f) ; Mounted renderer for this capsule.

    ;; card-make-page : any/c -> void?
    ;;   Build and mount the card page under root.
    (define (card-make-page root)
      (define @title  (@ "Profile"))
      (define @footer (@ "saved"))
      (set! card-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "retitle"
                        (lambda ()
                          (:= @title "Account")))
                (button "footnote"
                        (lambda ()
                          (:= @footer "updated"))))
               (card @title
                     @footer
                     (text "body-line"))))))
      (mount-renderer! card-renderer root)
      (void))

    ;; card-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (card-run-test _root)
      (and card-renderer #t))

    ;; card-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (card-cleanup _root)
      (when card-renderer
        (renderer-destroy card-renderer)
        (set! card-renderer #f))
      (void))

    (values card-make-page card-run-test card-cleanup)))
