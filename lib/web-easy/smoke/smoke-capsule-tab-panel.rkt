#lang webracket

;;;
;;; Smoke Capsule: Tab Panel
;;;

;; Isolated smoke capsule for tab-panel selection and keyboard navigation.
;;
;; Exports:
;;   tab-panel-make-page      Build and mount the tab-panel page under root.
;;   tab-panel-run-test       Execute capsule-local setup checks.
;;   tab-panel-cleanup        Destroy mounted renderer state for this capsule.

(define-values (tab-panel-make-page tab-panel-run-test tab-panel-cleanup)
  (let ()
    ;; Constants for tab-panel capsule state.
    (define tab-panel-renderer #f) ; Mounted renderer for this capsule.

    ;; tab-panel-make-page : any/c -> void?
    ;;   Build and mount the tab-panel page under root.
    (define (tab-panel-make-page root)
      (define @tab (@ 'info))
      (set! tab-panel-renderer
        (render
         (window
          (vpanel
           (tab-panel @tab
                      (list (cons 'info (text "Info tab"))
                            (cons 'settings (text "Settings tab"))
                            (cons 'about (text "About tab"))))))))
      (mount-renderer! tab-panel-renderer root)
      (void))

    ;; tab-panel-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (tab-panel-run-test _root)
      (and tab-panel-renderer #t))

    ;; tab-panel-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (tab-panel-cleanup _root)
      (when tab-panel-renderer
        (renderer-destroy tab-panel-renderer)
        (set! tab-panel-renderer #f))
      (void))

    (values tab-panel-make-page tab-panel-run-test tab-panel-cleanup)))
