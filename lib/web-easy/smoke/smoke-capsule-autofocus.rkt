#lang webracket

;;;
;;; Smoke Capsule: Autofocus
;;;

;; Isolated smoke capsule for mount-time autofocus on dynamically mounted UI.
;;
;; Exports:
;;   autofocus-make-page   Build and mount the autofocus page under root.
;;   autofocus-run-test    Execute capsule-local setup checks.
;;   autofocus-cleanup     Destroy mounted renderer state for this capsule.

(define-values (autofocus-make-page autofocus-run-test autofocus-cleanup)
  (let ()
    ;; Constants for autofocus capsule state.
    (define autofocus-renderer #f) ; Mounted renderer for this capsule.

    ;; autofocus-make-page : any/c -> void?
    ;;   Build and mount the autofocus page under root.
    (define (autofocus-make-page root)
      ;; Constants for observable state.
      (define @open?   (@ #f))
      (define @status  (@ "idle"))

      ;; open-editor! : -> void?
      ;;   Show the editor and track open status.
      (define (open-editor!)
        (:= @open?  #t)
        (:= @status "open"))

      ;; close-editor! : string? -> void?
      ;;   Hide the editor and record the closing reason.
      (define (close-editor! why)
        (:= @open?  #f)
        (:= @status why))

      ;; handle-editor-key! : event? -> void?
      ;;   Close the mounted editor on Escape.
      (define (handle-editor-key! evt)
        (define key (keyboard-event-key evt))
        (when (string=? key "Escape")
          (prevent-default! evt)
          (close-editor! "escape"))
        (void))

      (set! autofocus-renderer
            (render
             (window
              (vpanel
               (button "open-editor" open-editor!)
               (observable-view
                @open?
                (lambda (open?)
                  (if open?
                      (Div #:id "autofocus-editor"
                           #:autofocus #t
                           #:attrs '((tabindex "0"))
                           #:on-keydown handle-editor-key!
                           (text "editor-ready"))
                      (text "editor-hidden"))))
               (text (~> @status
                         (lambda (status)
                           (~a "status:" status))))))))
      (mount-renderer! autofocus-renderer root)
      (void))

    ;; autofocus-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (autofocus-run-test _root)
      (and autofocus-renderer #t))

    ;; autofocus-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (autofocus-cleanup _root)
      (when autofocus-renderer
        (renderer-destroy autofocus-renderer)
        (set! autofocus-renderer #f))
      (void))

    (values autofocus-make-page autofocus-run-test autofocus-cleanup)))
