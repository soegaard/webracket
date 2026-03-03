#lang webracket

;;;
;;; Smoke Capsule: Tab Panel Disabled
;;;

;; Isolated smoke capsule for tab-panel disabled-tab interaction behavior.
;;
;; Exports:
;;   tab-panel-disabled-make-page     Build and mount the disabled-tab page under root.
;;   tab-panel-disabled-run-test      Execute capsule-local setup checks.
;;   tab-panel-disabled-cleanup       Destroy mounted renderer state for this capsule.

(define-values (tab-panel-disabled-make-page
                tab-panel-disabled-run-test
                tab-panel-disabled-cleanup)
  (let ()
    ;; Constants for disabled tab-panel capsule state.
    (define tab-panel-disabled-renderer #f) ; Mounted renderer for this capsule.

    ;; tab-panel-disabled-make-page : any/c -> void?
    ;;   Build and mount the disabled-tab page under root.
    (define (tab-panel-disabled-make-page root)
      (define @tab (@ 'left))
      (set! tab-panel-disabled-renderer
        (render
         (window
          (vpanel
           (tab-panel @tab
                      (list (list 'left (text "Left tab") #f)
                            (list 'middle (text "Middle tab") #t)
                            (list 'right (text "Right tab") #f)))))))
      (mount-renderer! tab-panel-disabled-renderer root)
      (void))

    ;; tab-panel-disabled-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (tab-panel-disabled-run-test _root)
      (and tab-panel-disabled-renderer #t))

    ;; tab-panel-disabled-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (tab-panel-disabled-cleanup _root)
      (when tab-panel-disabled-renderer
        (renderer-destroy tab-panel-disabled-renderer)
        (set! tab-panel-disabled-renderer #f))
      (void))

    (values tab-panel-disabled-make-page
            tab-panel-disabled-run-test
            tab-panel-disabled-cleanup)))
