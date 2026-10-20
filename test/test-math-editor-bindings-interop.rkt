#lang webracket/base

;; WB-07 browser interop probe. It publishes ordinary foreign callbacks through
;; globalThis without the console bridge or a root-module callback export.

(require browser/document
         browser/element
         browser/window
         (only-in webracket/primitives procedure->external)
         (only-in webracket/unsafe/standard
                  js-global-this
                  js-ref/extern
                  js-send/value
                  js-set!))

(define body (document-body))
(unless body
  (error 'test-math-editor-bindings-interop "document.body is unavailable"))

(element-set-attribute! body 'data-math-editor-bindings-interop-state "starting")

;; install-global-callback! : string? procedure? -> void?
;;   Publish an explicit test-only foreign callback for the external host lane.
(define (install-global-callback! name callback)
  (js-set! (js-global-this) name (procedure->external callback))
  (void))

;; nested-host-successor : exact-integer? -> exact-integer?
;;   Invoke the host callback during an external-to-Racket callback invocation.
(define (nested-host-successor value)
  (define host-callback
    (js-ref/extern (js-global-this) "__webracketBindingsHostSuccessor"))
  (js-send/value host-callback
                 "call"
                 (vector (js-global-this) value)))

(install-global-callback! "__webracketBindingsAdd"
                          (lambda (left right) (+ left right)))
(install-global-callback! "__webracketBindingsVeto" (lambda () #f))
(install-global-callback! "__webracketBindingsNested"
                          (lambda (value) (nested-host-successor value)))
(install-global-callback!
 "__webracketBindingsAsync"
 (lambda ()
   (window-queue-microtask
    (lambda ()
      (element-set-attribute! body 'data-math-editor-bindings-interop-state "async")))
   #t))
(install-global-callback! "__webracketBindingsFailure"
                          (lambda () (error 'webracket-interop "intentional failure")))

(element-set-attribute! body 'data-math-editor-bindings-interop-state "ready")
