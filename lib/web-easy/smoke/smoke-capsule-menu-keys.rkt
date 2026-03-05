#lang webracket

;;;
;;; Smoke Capsule: Menu Keys
;;;

;; Isolated smoke capsule for menu-item keyboard interaction in smoke-all.
;;
;; Exports:
;;   menu-keys-make-page   Build and mount the menu-keys page under root.
;;   menu-keys-run-test    Execute capsule-local setup checks.
;;   menu-keys-cleanup     Destroy mounted renderer state for this capsule.

(define-values (menu-keys-make-page menu-keys-run-test menu-keys-cleanup)
  (let ()
    ;; Constants for menu-keys capsule state.
    (define menu-keys-renderer #f) ; Mounted renderer for this capsule.

    ;; menu-keys-make-page : any/c -> void?
    ;;   Build and mount the menu-keyboard page under root.
    (define (menu-keys-make-page root)
      (define @count (@ 0))
      (define (inc!)
        (<~ @count add1))
      (set! menu-keys-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n) (~a "count:" n))))
           (menu-bar
            (menu "Actions"
                  (menu-item "inc" inc!)))))))
      (mount-renderer! menu-keys-renderer root)
      (void))

    ;; menu-keys-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (menu-keys-run-test _root)
      (and menu-keys-renderer #t))

    ;; menu-keys-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (menu-keys-cleanup _root)
      (when menu-keys-renderer
        (renderer-destroy menu-keys-renderer)
        (set! menu-keys-renderer #f))
      (void))

    (values menu-keys-make-page menu-keys-run-test menu-keys-cleanup)))
