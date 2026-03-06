#lang webracket

;;;
;;; Smoke Capsule: Theme Token API
;;;

;; Isolated smoke capsule for runtime theme-token API behavior in smoke-all.
;;
;; Exports:
;;   theme-token-api-make-page   Build and mount the theme-token-api page under root.
;;   theme-token-api-run-test    Execute capsule-local setup checks.
;;   theme-token-api-cleanup     Destroy mounted renderer state for this capsule.

(define-values
  (theme-token-api-make-page theme-token-api-run-test theme-token-api-cleanup)
  (let ()
    ;; Constants for theme-token-api capsule state.
    (define theme-token-api-renderer #f) ; Mounted renderer for this capsule.
    (define token/fg 'we-fg) ; Foreground token key used by this capsule.

    ;; theme-token-api-make-page : any/c -> void?
    ;;   Build and mount the theme-token-api page under root.
    (define (theme-token-api-make-page root)
      (define @applied (@ ""))
      (define @mixed   (@ ""))

      (define (apply-theme!)
        (theme-token-set! token/fg "rgb(64, 128, 32)")
        (theme-token-set-many! '((we-bg-subtle . "rgb(240, 248, 230)")
                                 (we-border . "rgb(112, 144, 80)")))
        (:= @applied (theme-token-ref token/fg "")))

      (define (apply-mixed!)
        (theme-token-set! 'we-radius 6)
        (theme-token-set! 'we-mode   'solar)
        (:= @mixed (~a (theme-token-ref 'we-radius "")
                       "|"
                       (theme-token-ref 'we-mode ""))))

      (set! theme-token-api-renderer
            (render
             (window
              (vpanel
               (button "apply tokens" apply-theme!)
               (button "apply mixed" apply-mixed!)
               (text (~> @applied (lambda (v) (~a "fg:" v))))
               (text (~> @mixed   (lambda (v) (~a "mixed:" v))))))))
      (mount-renderer! theme-token-api-renderer root)
      (void))

    ;; theme-token-api-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (theme-token-api-run-test _root)
      (and theme-token-api-renderer #t))

    ;; theme-token-api-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (theme-token-api-cleanup _root)
      (when theme-token-api-renderer
        (renderer-destroy theme-token-api-renderer)
        (set! theme-token-api-renderer #f))
      (void))

    (values theme-token-api-make-page
            theme-token-api-run-test
            theme-token-api-cleanup)))
