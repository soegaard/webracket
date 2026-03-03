#lang webracket

;;;
;;; Smoke Capsule: Parity tabs
;;;

;; Isolated parity capsule for parity-tabs.
;;
;; Exports:
;;   parity-tabs-make-page      Build and mount the parity page under root.
;;   parity-tabs-run-test       Execute capsule-local setup checks.
;;   parity-tabs-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-tabs-make-page parity-tabs-run-test parity-tabs-cleanup)
  (let ()
    ;; Constants for parity-tabs capsule state.
    (define parity-tabs-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-tabs-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-tabs-make-page root)
      ;;;
      ;;; web-easy Browser Parity Tabs Example
      ;;;
      
      ;; Parity example: tabs selection/content switching.
      
      
      (define @selected (@ 'overview))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (tab-panel
            @selected
            (list (cons 'overview (text "Overview panel"))
                  (cons 'details  (text "Details panel"))
                  (cons 'help     (text "Help panel"))))))))
      
      (set! parity-tabs-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-tabs-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-tabs-run-test _root)
      (and parity-tabs-renderer #t))

    ;; parity-tabs-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-tabs-cleanup _root)
      (when parity-tabs-renderer
        (renderer-destroy parity-tabs-renderer)
        (set! parity-tabs-renderer #f))
      (void))

    (values parity-tabs-make-page parity-tabs-run-test parity-tabs-cleanup)))
