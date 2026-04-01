#lang webracket

;;;
;;; Event wrappers
;;;

;; This library reuses the checked browser event helpers from
;; `lib/web-easy/event-browser.rkt` so the public `include-lib event`
;; surface stays aligned with the existing browser event layer.

(include/reader "../web-easy/event-browser.rkt" read-syntax/skip-first-line)
