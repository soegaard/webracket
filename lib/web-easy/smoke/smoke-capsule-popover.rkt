#lang webracket

;;;
;;; Smoke Capsule: Popover
;;;

;; Isolated smoke capsule for popover open/close and body content in smoke-all.
;;
;; Exports:
;;   popover-make-page   Build and mount the popover page under root.
;;   popover-run-test    Execute capsule-local setup checks.
;;   popover-cleanup     Destroy mounted renderer state for this capsule.

(define-values (popover-make-page popover-run-test popover-cleanup)
  (let ()
    ;; Constants for popover capsule state.
    (define popover-renderer #f) ; Mounted renderer for this capsule.

    ;; popover-make-page : any/c -> void?
    ;;   Build and mount the popover page under root.
    (define (popover-make-page root)
      (define @title (@ "Deploy now?"))
      (set! popover-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "retitle"
                        (lambda ()
                          (:= @title "Deploy to staging?"))))
               (popover "actions"
                        (text @title)
                        (hpanel
                         (button "cancel" (lambda () (void)))
                         (button "confirm" (lambda () (void)))))))))
      (mount-renderer! popover-renderer root)
      (void))

    ;; popover-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (popover-run-test _root)
      (and popover-renderer #t))

    ;; popover-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (popover-cleanup _root)
      (when popover-renderer
        (renderer-destroy popover-renderer)
        (set! popover-renderer #f))
      (void))

    (values popover-make-page popover-run-test popover-cleanup)))
