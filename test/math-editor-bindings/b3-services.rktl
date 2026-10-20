#lang webracket/base

;; Reusable public-import consumer for the B3 service bindings.
(require browser/abort
         browser/document
         browser/element
         browser/event
         browser/event-target
         browser/font
         browser/range
         browser/segmenter
         browser/window
         clipboard
         js/promise
         resize-observer)

(provide b3-make-text-clipboard-item)

;; b3-make-text-clipboard-item : string? string? -> clipboard-item?
;;   Build one native ClipboardItem from a text Blob through public bindings.
(define (b3-make-text-clipboard-item mime-type text)
  (make-clipboard-item mime-type
                       (make-text-blob text mime-type)))
