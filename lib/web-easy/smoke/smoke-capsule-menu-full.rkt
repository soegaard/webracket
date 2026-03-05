#lang webracket

;;;
;;; Smoke Capsule: Menu Full
;;;

;; Isolated smoke capsule for richer menu interaction coverage in smoke-all.
;;
;; Exports:
;;   menu-full-make-page   Build and mount the menu-full page under root.
;;   menu-full-run-test    Execute capsule-local setup checks.
;;   menu-full-cleanup     Destroy mounted renderer state for this capsule.

(define-values (menu-full-make-page menu-full-run-test menu-full-cleanup)
  (let ()
    ;; Constants for menu-full capsule state.
    (define menu-full-renderer #f) ; Mounted renderer for this capsule.

    ;; menu-full-make-page : any/c -> void?
    ;;   Build and mount the richer menu page under root.
    (define (menu-full-make-page root)
      (define @open-count (@ 0))
      (define @save-count (@ 0))
      (define @undo-count (@ 0))
      (define @redo-count (@ 0))
      (define @status (@ "ready"))

      (define (open!)
        (<~ @open-count add1)
        (:= @status "open"))

      (define (save!)
        (<~ @save-count add1)
        (:= @status "save"))

      (define (undo!)
        (<~ @undo-count add1)
        (:= @status "undo"))

      (define (redo!)
        (<~ @redo-count add1)
        (:= @status "redo"))

      (set! menu-full-renderer
        (render
         (window
          (vpanel
           (text (~> @status (lambda (s) (~a "status:" s))))
           (text (~> @open-count (lambda (n) (~a "open:" n))))
           (text (~> @save-count (lambda (n) (~a "save:" n))))
           (text (~> @undo-count (lambda (n) (~a "undo:" n))))
           (text (~> @redo-count (lambda (n) (~a "redo:" n))))
           (menu-bar
            (menu "File"
                  (menu-item "open" open!)
                  (menu-item "save" save!))
            (menu "Edit"
                  (menu-item "undo" undo!)
                  (menu-item "redo" redo!)))))))
      (mount-renderer! menu-full-renderer root)
      (void))

    ;; menu-full-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (menu-full-run-test _root)
      (and menu-full-renderer #t))

    ;; menu-full-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (menu-full-cleanup _root)
      (when menu-full-renderer
        (renderer-destroy menu-full-renderer)
        (set! menu-full-renderer #f))
      (void))

    (values menu-full-make-page menu-full-run-test menu-full-cleanup)))
