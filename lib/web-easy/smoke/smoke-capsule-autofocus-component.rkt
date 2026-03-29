#lang webracket

;;;
;;; Smoke Capsule: Autofocus Component
;;;

;; Isolated smoke capsule for mount-time autofocus forwarded through a component root.
;;
;; Exports:
;;   autofocus-component-make-page   Build and mount the autofocus component page under root.
;;   autofocus-component-run-test    Execute capsule-local setup checks.
;;   autofocus-component-cleanup     Destroy mounted renderer state for this capsule.

(define-values (autofocus-component-make-page
                autofocus-component-run-test
                autofocus-component-cleanup)
  (let ()
    ;; Constants for autofocus component capsule state.
    (define autofocus-component-renderer #f) ; Mounted renderer for this capsule.

    ;; autofocus-component-make-page : any/c -> void?
    ;;   Build and mount the autofocus component page under root.
    (define (autofocus-component-make-page root)
      ;; Constants for observable state.
      (define @open?  (@ #f))
      (define @status (@ "idle"))

      ;; open-banner! : -> void?
      ;;   Show the component-root autofocus target and track open status.
      (define (open-banner!)
        (:= @open?  #t)
        (:= @status "open"))

      (set! autofocus-component-renderer
            (render
             (window
              (vpanel
               (button "open-component-editor" open-banner!)
               (observable-view
                @open?
                (lambda (open?)
                  (if open?
                      (top-bar #:id "autofocus-component-root"
                               #:autofocus #t
                               #:attrs '((tabindex "0"))
                               (text "component-editor-ready"))
                      (text "component-editor-hidden"))))
               (text (~> @status
                         (lambda (status)
                           (~a "status:" status))))))))
      (mount-renderer! autofocus-component-renderer root)
      (void))

    ;; autofocus-component-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (autofocus-component-run-test _root)
      (and autofocus-component-renderer #t))

    ;; autofocus-component-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (autofocus-component-cleanup _root)
      (when autofocus-component-renderer
        (renderer-destroy autofocus-component-renderer)
        (set! autofocus-component-renderer #f))
      (void))

    (values autofocus-component-make-page
            autofocus-component-run-test
            autofocus-component-cleanup)))
