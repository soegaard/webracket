#lang webracket

;;;
;;; Smoke Capsule: Parity Headings
;;;

;; Isolated parity capsule for heading, display-heading, and lead rendering in parity-all.
;;
;; Exports:
;;   parity-headings-make-page   Build and mount the parity page under root.
;;   parity-headings-run-test    Execute capsule-local setup checks.
;;   parity-headings-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-headings-make-page parity-headings-run-test parity-headings-cleanup)
  (let ()
    ;; Constants for parity headings capsule state.
    (define parity-headings-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-headings-make-page : any/c -> void?
    ;;   Build and mount the parity headings page under root.
    (define (parity-headings-make-page root)
      (define @level   (@ 4))
      (define @title   (@ "Parity Heading"))
      (define @display (@ "Parity Display"))
      (define @lead    (@ "Parity lead text"))
      (define @subtitle (@ "Parity subtitle"))
      (define @align    (@ 'left))
      (define @spacing  (@ 'normal))
      (set! parity-headings-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-level-2" (lambda () (:= @level 2)))
                (button "set-level-5" (lambda () (:= @level 5)))
                (button "set-title"   (lambda () (:= @title "Parity Updated Heading")))
                (button "center"      (lambda () (:= @align 'center)))
                (button "right"       (lambda () (:= @align 'right)))
                (button "space-loose" (lambda () (:= @spacing 'loose))))
               (h2 "Parity section")
               (heading @level @title @align @spacing)
               (heading-with-subtitle @level @title @subtitle @align @spacing)
               (display-heading @level @display @align @spacing)
               (display-heading-with-subtitle @level @display @subtitle @align @spacing)
               (display-3 "Parity Display Wrapper")
               (lead @lead)))))
      (mount-renderer! parity-headings-renderer root)
      (void))

    ;; parity-headings-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-headings-run-test _root)
      (and parity-headings-renderer #t))

    ;; parity-headings-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-headings-cleanup _root)
      (when parity-headings-renderer
        (renderer-destroy parity-headings-renderer)
        (set! parity-headings-renderer #f))
      (void))

    (values parity-headings-make-page parity-headings-run-test parity-headings-cleanup)))
