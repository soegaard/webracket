#lang webracket

;;;
;;; Smoke Capsule: Branch
;;;

;; Isolated smoke capsule for the branch toggle test page in the smoke-all driver.
;;
;; Exports:
;;   branch-make-page   Build and mount the branch page under root.
;;   branch-run-test    Execute capsule-local setup checks.
;;   branch-cleanup     Destroy mounted renderer state for this capsule.

(define-values (branch-make-page branch-run-test branch-cleanup)
  (let ()
    ;; Constants for branch capsule state.
    (define branch-renderer #f) ; Mounted renderer for this capsule.

    ;; branch-make-page : any/c -> void?
    ;;   Build and mount the branch page under root.
    (define (branch-make-page root)
      (define @show-on (@ #t))
      (set! branch-renderer
        (render
         (window
          (vpanel
           (button "toggle"
                   (lambda ()
                     (:= @show-on (not (obs-peek @show-on)))))
           (if-view @show-on
                    (text "ON")
                    (text "OFF"))))))
      (mount-renderer! branch-renderer root)
      (void))

    ;; branch-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (branch-run-test _root)
      (and branch-renderer #t))

    ;; branch-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (branch-cleanup _root)
      (when branch-renderer
        (renderer-destroy branch-renderer)
        (set! branch-renderer #f))
      (void))

    (values branch-make-page branch-run-test branch-cleanup)))
