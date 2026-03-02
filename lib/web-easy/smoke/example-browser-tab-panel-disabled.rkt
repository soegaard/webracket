;;;
;;; web-easy Browser Tab-Panel Disabled Smoke Example
;;;

;; Minimal browser app for tab-panel disabled-tab interaction smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @tab (@ 'left))

(define app-renderer
  (render
   (window
    (vpanel
     (tab-panel @tab
                (list (list 'left (text "Left tab") #f)
                      (list 'middle (text "Middle tab") #t)
                      (list 'right (text "Right tab") #f)))))))

(mount-renderer! app-renderer)
