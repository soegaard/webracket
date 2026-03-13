#lang webracket

;;;
;;; Smoke Capsule: Controls
;;;

;; Isolated smoke capsule for choice/slider/progress behavior in smoke-all.
;;
;; Exports:
;;   controls-make-page   Build and mount the controls page under root.
;;   controls-run-test    Execute capsule-local setup checks.
;;   controls-cleanup     Destroy mounted renderer state for this capsule.

(define-values (controls-make-page controls-run-test controls-cleanup)
  (let ()
    ;; Constants for controls capsule state.
    (define controls-renderer #f) ; Mounted renderer for this capsule.

    ;; controls-make-page : any/c -> void?
    ;;   Build and mount the controls page under root.
    (define (controls-make-page root)
      (define @color (@ 'red))
      (define @level (@ 10))
      (define @variant
        (~> @color
            (lambda (color)
              (case color
                [(green) 'success]
                [(red)   'danger]
                [else    'info]))))
      (set! controls-renderer
            (render
             (window
              (vpanel
               (choice '(red green blue)
                       @color
                       (lambda (new-value)
                         (:= @color (if (symbol? new-value)
                                        new-value
                                        (string->symbol new-value)))))
               (text (~> @color (lambda (s) (~a "color:" s))))
               (slider
                         @level
                         (lambda (new-value)
                           (:= @level new-value))
                         #:min 0
                         #:max 100)
               (text (~> @level (lambda (n) (~a "level:" n))))
               (progress
                         @level
                         #:min 0
                         #:max 100
                         #:variant @variant)))))
      (mount-renderer! controls-renderer root)
      (void))

    ;; controls-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (controls-run-test _root)
      (and controls-renderer #t))

    ;; controls-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (controls-cleanup _root)
      (when controls-renderer
        (renderer-destroy controls-renderer)
        (set! controls-renderer #f))
      (void))

    (values controls-make-page controls-run-test controls-cleanup)))
