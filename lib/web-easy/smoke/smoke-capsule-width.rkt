#lang webracket

;;;
;;; Smoke Capsule: Width
;;;

;; Isolated smoke capsule for width-policy behavior in smoke-all.
;;
;; Exports:
;;   width-make-page   Build and mount the width-policy page under root.
;;   width-run-test    Execute capsule-local setup checks.
;;   width-cleanup     Destroy mounted renderer state for this capsule.

(define-values (width-make-page width-run-test width-cleanup)
  (let ()
    ;; Constants for width capsule state.
    (define width-renderer #f) ; Mounted renderer for this capsule.

    ;; width-make-page : any/c -> void?
    ;;   Build and mount the width-policy page under root.
    (define (width-make-page root)
      (define @name (@ "Alice"))
      (define @enabled (@ #t))
      (define @choice (@ "one"))
      (define @level (@ 40))
      (define @rows (@ (list (list "a" "ok")
                             (list "b" "hold"))))
      (set! width-renderer
            (render
             (window
              (vpanel
               (group "Width Policy"
                      (input @name (lambda (v) (:= @name v)))
                      (checkbox @enabled (lambda (v) (:= @enabled v)))
                      (choice (list "one" "two" "three")
                              @choice
                              (lambda (v) (:= @choice v)))
                      (slider @level (lambda (v) (:= @level v))
                                #:min 0
                                #:max 100)
                      (progress @level
                                #:min 0
                                #:max 100)
                      (button "noop" (lambda () (void)))
                      (table '(item state) @rows #:density 'compact))))))
      (mount-renderer! width-renderer root)
      (void))

    ;; width-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (width-run-test _root)
      (and width-renderer #t))

    ;; width-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (width-cleanup _root)
      (when width-renderer
        (renderer-destroy width-renderer)
        (set! width-renderer #f))
      (void))

    (values width-make-page width-run-test width-cleanup)))
