#lang webracket

;;;
;;; Smoke Capsule: Toolbar
;;;

;; Isolated smoke capsule for generic toolbar and toolbar-group behavior in smoke-all.
;;
;; Exports:
;;   toolbar-make-page   Build and mount the toolbar page under root.
;;   toolbar-run-test    Execute capsule-local setup checks.
;;   toolbar-cleanup     Destroy mounted renderer state for this capsule.

(define-values (toolbar-make-page toolbar-run-test toolbar-cleanup)
  (let ()
    ;; Constants for toolbar capsule state.
    (define toolbar-renderer #f) ; Mounted renderer for this capsule.

    ;; toolbar-make-page : any/c -> void?
    ;;   Build and mount the toolbar page under root.
    (define (toolbar-make-page root)
      (define @status (@ "ready"))

      (define (set-status! s)
        (:= @status s))

      (set! toolbar-renderer
            (render
             (window
              (vpanel
               (toolbar
                (toolbar-group
                 (button "refresh" (lambda () (set-status! "refresh"))))
                (divider #:orientation 'vertical)
                (toolbar-group
                 (button "save" (lambda () (set-status! "save")))
                 (button "publish" (lambda () (set-status! "publish")))))
               (text (~> @status (lambda (s) (~a "status:" s))))))))
      (mount-renderer! toolbar-renderer root)
      (void))

    ;; toolbar-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (toolbar-run-test _root)
      (and toolbar-renderer #t))

    ;; toolbar-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (toolbar-cleanup _root)
      (when toolbar-renderer
        (renderer-destroy toolbar-renderer)
        (set! toolbar-renderer #f))
      (void))

    (values toolbar-make-page toolbar-run-test toolbar-cleanup)))
