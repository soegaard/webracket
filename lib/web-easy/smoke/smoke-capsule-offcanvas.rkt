#lang webracket

;;;
;;; Smoke Capsule: Offcanvas
;;;

;; Isolated smoke capsule for offcanvas open/close and side behavior in smoke-all.
;;
;; Exports:
;;   offcanvas-make-page   Build and mount the offcanvas page under root.
;;   offcanvas-run-test    Execute capsule-local setup checks.
;;   offcanvas-cleanup     Destroy mounted renderer state for this capsule.

(define-values (offcanvas-make-page offcanvas-run-test offcanvas-cleanup)
  (let ()
    ;; Constants for offcanvas capsule state.
    (define offcanvas-renderer #f) ; Mounted renderer for this capsule.

    ;; offcanvas-make-page : any/c -> void?
    ;;   Build and mount the offcanvas page under root.
    (define (offcanvas-make-page root)
      (define @open (@ #f))
      (define @side (@ 'end))
      (set! offcanvas-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "open-end" (lambda () (:= @side 'end) (:= @open #t)))
                (button "open-start" (lambda () (:= @side 'start) (:= @open #t)))
                (button "close" (lambda () (:= @open #f))))
               (offcanvas
                         @open
                         (lambda () (:= @open #f))
                         (text "offcanvas-body")
                         #:side @side)
               (text (~> @open
                         (lambda (b)
                           (~a "open:" b))))
               (text (~> @side
                         (lambda (s)
                           (~a "side:" s))))))))
      (mount-renderer! offcanvas-renderer root)
      (void))

    ;; offcanvas-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (offcanvas-run-test _root)
      (and offcanvas-renderer #t))

    ;; offcanvas-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (offcanvas-cleanup _root)
      (when offcanvas-renderer
        (renderer-destroy offcanvas-renderer)
        (set! offcanvas-renderer #f))
      (void))

    (values offcanvas-make-page offcanvas-run-test offcanvas-cleanup)))
