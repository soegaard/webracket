#lang webracket

;;;
;;; Smoke Capsule: Checkbox
;;;

;; Isolated smoke capsule for the checkbox test page in the smoke-all driver.
;;
;; Exports:
;;   checkbox-make-page   Build and mount the checkbox page under root.
;;   checkbox-run-test    Execute capsule-local setup checks.
;;   checkbox-cleanup     Destroy mounted renderer state for this capsule.

(define-values (checkbox-make-page checkbox-run-test checkbox-cleanup)
  (let ()
    ;; Constants for checkbox capsule state.
    (define checkbox-renderer #f) ; Mounted renderer for this capsule.

    ;; checkbox-make-page : any/c -> void?
    ;;   Build and mount the checkbox page under root.
    (define (checkbox-make-page root)
      (define @enabled (@ #f))
      (set! checkbox-renderer
        (render
         (window
          (vpanel
           (checkbox @enabled
                     (lambda (new-value)
                       (:= @enabled new-value)))
           (text (~> @enabled (lambda (v) (if v "on" "off"))))))))
      (mount-renderer! checkbox-renderer root)
      (void))

    ;; checkbox-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (checkbox-run-test _root)
      (and checkbox-renderer #t))

    ;; checkbox-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (checkbox-cleanup _root)
      (when checkbox-renderer
        (renderer-destroy checkbox-renderer)
        (set! checkbox-renderer #f))
      (void))

    (values checkbox-make-page checkbox-run-test checkbox-cleanup)))
