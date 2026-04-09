;;;
;;; web-easy Browser Width Policy Example
;;;

;; Smoke example that exposes fill-vs-fit width defaults for core widgets.

(include-lib web-easy)

(define @name (@ "Alice"))
(define @enabled (@ #t))
(define @choice (@ "one"))
(define @level (@ 40))
(define @rows (@ (list (list "a" "ok")
                       (list "b" "hold"))))

(define app-renderer
  (render
   (window
    (vpanel
     (group "Width Policy"
            (input @name (lambda (v) (:= @name v)))
            (checkbox @enabled (lambda (v) (:= @enabled v)))
            (choice (list "one" "two" "three")
                    @choice
                    (lambda (v) (:= @choice v)))
            (slider
                      @level
                      (lambda (v) (:= @level v))
                      #:min 0
                      #:max 100)
            (progress
                      @level
                      #:min 0
                      #:max 100)
            (button "noop" (lambda () (void)))
            (table
                      '(item state)
                      @rows
                      #:density 'compact))))))

(mount-renderer! app-renderer)
