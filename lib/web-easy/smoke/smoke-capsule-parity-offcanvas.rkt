#lang webracket

;;;
;;; Smoke Capsule: Parity Offcanvas
;;;

;; Isolated parity capsule for offcanvas open/close and side behavior in parity-all.
;;
;; Exports:
;;   parity-offcanvas-make-page   Build and mount the parity page under root.
;;   parity-offcanvas-run-test    Execute capsule-local setup checks.
;;   parity-offcanvas-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-offcanvas-make-page parity-offcanvas-run-test parity-offcanvas-cleanup)
  (let ()
    ;; Constants for parity offcanvas capsule state.
    (define parity-offcanvas-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-offcanvas-make-page : any/c -> void?
    ;;   Build and mount the parity offcanvas page under root.
    (define (parity-offcanvas-make-page root)
      (define @open (@ #t))
      (define @side (@ 'start))
      (set! parity-offcanvas-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "open-start" (lambda () (:= @side 'start) (:= @open #t)))
                (button "open-end" (lambda () (:= @side 'end) (:= @open #t)))
                (button "close" (lambda () (:= @open #f))))
               (offcanvas
                         @open
                         (lambda () (:= @open #f))
                         (text "parity-offcanvas-body")
                         #:side @side)
               (text (~> @open
                         (lambda (b)
                           (~a "parity-open:" b))))
               (text (~> @side
                         (lambda (s)
                           (~a "parity-side:" s))))))))
      (mount-renderer! parity-offcanvas-renderer root)
      (void))

    ;; parity-offcanvas-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-offcanvas-run-test _root)
      (and parity-offcanvas-renderer #t))

    ;; parity-offcanvas-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-offcanvas-cleanup _root)
      (when parity-offcanvas-renderer
        (renderer-destroy parity-offcanvas-renderer)
        (set! parity-offcanvas-renderer #f))
      (void))

    (values parity-offcanvas-make-page parity-offcanvas-run-test parity-offcanvas-cleanup)))
