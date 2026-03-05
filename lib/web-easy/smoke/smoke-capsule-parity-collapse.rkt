#lang webracket

;;;
;;; Smoke Capsule: Parity Collapse
;;;

;; Isolated parity capsule for collapse open/close rendering in parity-all.
;;
;; Exports:
;;   parity-collapse-make-page   Build and mount the parity page under root.
;;   parity-collapse-run-test    Execute capsule-local setup checks.
;;   parity-collapse-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-collapse-make-page parity-collapse-run-test parity-collapse-cleanup)
  (let ()
    ;; Constants for parity collapse capsule state.
    (define parity-collapse-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-collapse-make-page : any/c -> void?
    ;;   Build and mount the parity collapse page under root.
    (define (parity-collapse-make-page root)
      (define @open (@ #f))
      (set! parity-collapse-renderer
            (render
             (window
              (vpanel
               (button "toggle"
                       (lambda ()
                         (obs-update! @open not)))
               (collapse @open
                         (text "parity-secret"))))))
      (mount-renderer! parity-collapse-renderer root)
      (void))

    ;; parity-collapse-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-collapse-run-test _root)
      (and parity-collapse-renderer #t))

    ;; parity-collapse-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-collapse-cleanup _root)
      (when parity-collapse-renderer
        (renderer-destroy parity-collapse-renderer)
        (set! parity-collapse-renderer #f))
      (void))

    (values parity-collapse-make-page parity-collapse-run-test parity-collapse-cleanup)))
