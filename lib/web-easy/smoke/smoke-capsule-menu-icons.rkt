#lang webracket

;;;
;;; Smoke Capsule: Menu Icons
;;;

;; Isolated smoke capsule for menu-item icon slot rendering in smoke-all.
;;
;; Exports:
;;   menu-icons-make-page   Build and mount the menu-icons page under root.
;;   menu-icons-run-test    Execute capsule-local setup checks.
;;   menu-icons-cleanup     Destroy mounted renderer state for this capsule.

(define-values (menu-icons-make-page menu-icons-run-test menu-icons-cleanup)
  (let ()
    ;; Constants for menu-icons capsule state.
    (define menu-icons-renderer #f) ; Mounted renderer for this capsule.

    ;; menu-icons-make-page : any/c -> void?
    ;;   Build and mount the menu-icons page under root.
    (define (menu-icons-make-page root)
      (define @status (@ "idle"))

      (define (mark! s)
        (:= @status s))

      (set! menu-icons-renderer
            (render
             (window
              (vpanel
                (menu-bar
                (menu "File"
                      (menu-item "Plain" (lambda () (mark! "plain")))
                      (menu-item "Open" (lambda () (mark! "open")) "📂" "↗")
                      (menu-item "Save" (lambda () (mark! "save")) "💾" "✓")))
               (text (~> @status (lambda (s) (~a "status:" s))))))))
      (mount-renderer! menu-icons-renderer root)
      (void))

    ;; menu-icons-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (menu-icons-run-test _root)
      (and menu-icons-renderer #t))

    ;; menu-icons-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (menu-icons-cleanup _root)
      (when menu-icons-renderer
        (renderer-destroy menu-icons-renderer)
        (set! menu-icons-renderer #f))
      (void))

    (values menu-icons-make-page menu-icons-run-test menu-icons-cleanup)))
