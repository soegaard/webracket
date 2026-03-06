#lang webracket

;;;
;;; Smoke Capsule: Choice Labeled
;;;

;; Isolated smoke capsule for labeled choice option decoding in smoke-all.
;;
;; Exports:
;;   choice-labeled-make-page   Build and mount the choice-labeled page under root.
;;   choice-labeled-run-test    Execute capsule-local setup checks.
;;   choice-labeled-cleanup     Destroy mounted renderer state for this capsule.

(define-values (choice-labeled-make-page choice-labeled-run-test choice-labeled-cleanup)
  (let ()
    ;; Constants for choice-labeled capsule state.
    (define choice-labeled-renderer #f) ; Mounted renderer for this capsule.

    ;; choice-labeled-make-page : any/c -> void?
    ;;   Build and mount the choice-labeled page under root.
    (define (choice-labeled-make-page root)
      (define @selected (@ 'green))
      (set! choice-labeled-renderer
            (render
             (window
              (vpanel
               (choice '((red "Red") (green "Green") (blue "Blue"))
                       @selected
                       (lambda (new-id)
                         (:= @selected new-id)))
               (text (~> @selected (lambda (id) (~a "selected:" id))))))))
      (mount-renderer! choice-labeled-renderer root)
      (void))

    ;; choice-labeled-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (choice-labeled-run-test _root)
      (and choice-labeled-renderer #t))

    ;; choice-labeled-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (choice-labeled-cleanup _root)
      (when choice-labeled-renderer
        (renderer-destroy choice-labeled-renderer)
        (set! choice-labeled-renderer #f))
      (void))

    (values choice-labeled-make-page
            choice-labeled-run-test
            choice-labeled-cleanup)))
