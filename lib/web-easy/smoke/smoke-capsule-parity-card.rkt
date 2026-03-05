#lang webracket

;;;
;;; Smoke Capsule: Parity Card
;;;

;; Isolated parity capsule for card header/footer/body behavior in parity-all.
;;
;; Exports:
;;   parity-card-make-page   Build and mount the parity page under root.
;;   parity-card-run-test    Execute capsule-local setup checks.
;;   parity-card-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-card-make-page parity-card-run-test parity-card-cleanup)
  (let ()
    ;; Constants for parity card capsule state.
    (define parity-card-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-card-make-page : any/c -> void?
    ;;   Build and mount the parity card page under root.
    (define (parity-card-make-page root)
      (define @title  (@ "Status"))
      (define @footer (@ "ok"))
      (set! parity-card-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "retitle"
                        (lambda ()
                          (:= @title "Summary")))
                (button "footnote"
                        (lambda ()
                          (:= @footer "updated"))))
               (card @title
                     @footer
                     (text "parity-body"))))))
      (mount-renderer! parity-card-renderer root)
      (void))

    ;; parity-card-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-card-run-test _root)
      (and parity-card-renderer #t))

    ;; parity-card-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-card-cleanup _root)
      (when parity-card-renderer
        (renderer-destroy parity-card-renderer)
        (set! parity-card-renderer #f))
      (void))

    (values parity-card-make-page parity-card-run-test parity-card-cleanup)))

