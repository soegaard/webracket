#lang webracket

;;;
;;; Smoke Capsule: Parity menu keys
;;;

;; Isolated parity capsule for parity-menu-keys.
;;
;; Exports:
;;   parity-menu-keys-make-page      Build and mount the parity page under root.
;;   parity-menu-keys-run-test       Execute capsule-local setup checks.
;;   parity-menu-keys-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-menu-keys-make-page parity-menu-keys-run-test parity-menu-keys-cleanup)
  (let ()
    ;; Constants for parity-menu-keys capsule state.
    (define parity-menu-keys-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-menu-keys-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-menu-keys-make-page root)
      ;;;
      ;;; web-easy Browser Parity Menu Keyboard Example
      ;;;
      
      ;; Parity smoke example for menu-item keyboard focus and activation semantics.
      
      
      (define @count (@ 0))
      
      (define (inc!)
        (<~ @count add1))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n) (string-append "count:" (number->string n)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "inc" inc!)))))))
      
      (set! parity-menu-keys-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-menu-keys-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-menu-keys-run-test _root)
      (and parity-menu-keys-renderer #t))

    ;; parity-menu-keys-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-menu-keys-cleanup _root)
      (when parity-menu-keys-renderer
        (renderer-destroy parity-menu-keys-renderer)
        (set! parity-menu-keys-renderer #f))
      (void))

    (values parity-menu-keys-make-page parity-menu-keys-run-test parity-menu-keys-cleanup)))
