#lang webracket

;;;
;;; Smoke Capsule: Tooltip
;;;

;; Isolated smoke capsule for tooltip trigger and message updates in smoke-all.
;;
;; Exports:
;;   tooltip-make-page   Build and mount the tooltip page under root.
;;   tooltip-run-test    Execute capsule-local setup checks.
;;   tooltip-cleanup     Destroy mounted renderer state for this capsule.

(define-values (tooltip-make-page tooltip-run-test tooltip-cleanup)
  (let ()
    ;; Constants for tooltip capsule state.
    (define tooltip-renderer #f) ; Mounted renderer for this capsule.

    ;; tooltip-make-page : any/c -> void?
    ;;   Build and mount the tooltip page under root.
    (define (tooltip-make-page root)
      (define @hint (@ "Click to apply"))
      (set! tooltip-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-short"
                        (lambda ()
                          (:= @hint "Click to apply")))
                (button "set-long"
                        (lambda ()
                          (:= @hint "Click to apply deployment profile"))))
               (tooltip @hint
                        (button "apply"
                                (lambda ()
                                  (void))))))))
      (mount-renderer! tooltip-renderer root)
      (void))

    ;; tooltip-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (tooltip-run-test _root)
      (and tooltip-renderer #t))

    ;; tooltip-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (tooltip-cleanup _root)
      (when tooltip-renderer
        (renderer-destroy tooltip-renderer)
        (set! tooltip-renderer #f))
      (void))

    (values tooltip-make-page tooltip-run-test tooltip-cleanup)))
