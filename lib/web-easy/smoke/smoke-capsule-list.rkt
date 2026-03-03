#lang webracket

;;;
;;; Smoke Capsule: List
;;;

;; Isolated smoke capsule for the list reorder test page in the smoke-all driver.
;;
;; Exports:
;;   list-make-page   Build and mount the list page under root.
;;   list-run-test    Execute capsule-local setup checks.
;;   list-cleanup     Destroy mounted renderer state for this capsule.

(define-values (list-make-page list-run-test list-cleanup)
  (let ()
    ;; Constants for list capsule state.
    (define list-renderer #f) ; Mounted renderer for this capsule.

    ;; list-make-page : any/c -> void?
    ;;   Build and mount the list page under root.
    (define (list-make-page root)
      (define @items (@ '((1 . "a") (2 . "b") (3 . "c"))))
      (set! list-renderer
        (render
         (window
          (vpanel
           (button "reorder"
                   (lambda ()
                     (:= @items '((3 . "c") (1 . "a") (2 . "b")))))
           (list-view @items
                      (lambda (_key entry)
                        (text (string-append "item:" (cdr entry))))
                      car)))))
      (mount-renderer! list-renderer root)
      (void))

    ;; list-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (list-run-test _root)
      (and list-renderer #t))

    ;; list-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (list-cleanup _root)
      (when list-renderer
        (renderer-destroy list-renderer)
        (set! list-renderer #f))
      (void))

    (values list-make-page list-run-test list-cleanup)))
