#lang webracket/base

(require browser/element
         browser/event)

(provide install-synchronous-veto-listener!)

;; install-synchronous-veto-listener! : element? -> procedure?
;;   Install and return a listener that synchronously cancels its event.
(define (install-synchronous-veto-listener! target)
  (define listener
    (lambda (evt)
      (unless (event-cancelable? evt)
        (error 'install-synchronous-veto-listener! "expected a cancelable event"))
      (prevent-default! evt)
      (unless (event-default-prevented? evt)
        (error 'install-synchronous-veto-listener! "cancellation was not synchronous"))
      (void)))
  (element-add-event-listener! target 'webracket-b1-cancel listener)
  listener)
