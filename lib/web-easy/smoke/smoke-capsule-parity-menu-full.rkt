#lang webracket

;;;
;;; Smoke Capsule: Parity menu full
;;;

;; Isolated parity capsule for parity-menu-full.
;;
;; Exports:
;;   parity-menu-full-make-page      Build and mount the parity page under root.
;;   parity-menu-full-run-test       Execute capsule-local setup checks.
;;   parity-menu-full-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-menu-full-make-page parity-menu-full-run-test parity-menu-full-cleanup)
  (let ()
    ;; Constants for parity-menu-full capsule state.
    (define parity-menu-full-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-menu-full-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-menu-full-make-page root)
      (define @build-count (@ 0))
      (define @deploy-count (@ 0))
      (define @docs-count (@ 0))
      (define @about-count (@ 0))
      (define @status (@ "ready"))

      (define (build!)
        (<~ @build-count add1)
        (:= @status "build"))

      (define (deploy!)
        (<~ @deploy-count add1)
        (:= @status "deploy"))

      (define (docs!)
        (<~ @docs-count add1)
        (:= @status "docs"))

      (define (about!)
        (<~ @about-count add1)
        (:= @status "about"))

      (define app-renderer
        (render
         (window
          (vpanel
           (text (~> @status (lambda (s) (string-append "status:" s))))
           (text (~> @build-count (lambda (n) (string-append "build:" (number->string n)))))
           (text (~> @deploy-count (lambda (n) (string-append "deploy:" (number->string n)))))
           (text (~> @docs-count (lambda (n) (string-append "docs:" (number->string n)))))
           (text (~> @about-count (lambda (n) (string-append "about:" (number->string n)))))
           (menu-bar
            (menu "Project"
                  (menu-item "build" build!)
                  (menu-item "deploy" deploy!))
            (menu "Help"
                  (menu-item "docs" docs!)
                  (menu-item "about" about!)))))))

      (set! parity-menu-full-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-menu-full-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-menu-full-run-test _root)
      (and parity-menu-full-renderer #t))

    ;; parity-menu-full-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-menu-full-cleanup _root)
      (when parity-menu-full-renderer
        (renderer-destroy parity-menu-full-renderer)
        (set! parity-menu-full-renderer #f))
      (void))

    (values parity-menu-full-make-page parity-menu-full-run-test parity-menu-full-cleanup)))
