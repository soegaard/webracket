#lang racket/base

;; Generated from a WebRacket .ffi declaration file.
;; Raw exports are compiler-recognized aliases for active .ffi primitives.

(require (only-in "../private/ffi.rkt" unsafe-ffi-primitive))

(provide
 js-add-event-listener!
 js-event-new
 js-event-new/extern
 js-event?
 js-event-type
 js-event-target
 js-event-current-target
 js-event-event-phase
 js-event-bubbles
 js-event-cancelable
 js-event-default-prevented
 js-event-composed
 js-event-is-trusted
 js-event-time-stamp
 js-event-composed-path
 js-event-prevent-default
 js-event-stop-propagation
 js-event-stop-immediate-propagation
 js-message-event?
 js-message-event-data
 js-message-event-origin
 js-message-event-last-event-id
 js-message-event-source
 js-message-event-ports
 js-close-event?
 js-close-event-was-clean
 js-close-event-code
 js-close-event-reason
 js-mouse-event?
 js-mouse-event-offset-x
 js-mouse-event-offset-y
 js-mouse-event-client-x
 js-mouse-event-client-y
 js-mouse-event-page-x
 js-mouse-event-page-y
 js-mouse-event-screen-x
 js-mouse-event-screen-y
 js-mouse-event-button
 js-mouse-event-buttons
 js-mouse-event-alt-key
 js-mouse-event-ctrl-key
 js-mouse-event-meta-key
 js-mouse-event-shift-key
 js-keyboard-event?
 js-keyboard-event-key
 js-keyboard-event-code
 js-keyboard-event-alt-key
 js-keyboard-event-ctrl-key
 js-keyboard-event-meta-key
 js-keyboard-event-shift-key
 js-keyboard-event-repeat
 js-keyboard-event-is-composing
 js-keyboard-event-get-modifier-state
 js-pointer-event?
 js-pointer-event-id
 js-pointer-event-type
 js-pointer-event-is-primary
 js-focus-event?
 js-focus-event-related-target
 js-input-event?
 js-input-event-input-type
 js-input-event-data
 js-input-event-is-composing
 js-input-event-data-transfer
 js-composition-event?
 js-composition-event-data
 js-clipboard-event?
 js-clipboard-event-data
 js-submit-event?
 js-wheel-event?
 js-touch-event?
 js-touch-event-touches
 js-touch-event-target-touches
 js-touch-event-changed-touches
 js-touch-list?
 js-touch-list-length
 js-touch-list-ref
 js-touch?
 js-touch-identifier
 js-touch-client-x
 js-touch-client-y
 js-touch-page-x
 js-touch-page-y
 js-touch-screen-x
 js-touch-screen-y)

;; js-add-event-listener! : ffi (extern string extern) -> ()
;;   Raw binding generated for webracket/unsafe/event.
(define js-add-event-listener! (unsafe-ffi-primitive 'js-add-event-listener!))

;; js-event-new : ffi (string value) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-new (unsafe-ffi-primitive 'js-event-new))

;; js-event-new/extern : ffi (string extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-new/extern (unsafe-ffi-primitive 'js-event-new/extern))

;; js-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event? (unsafe-ffi-primitive 'js-event?))

;; js-event-type : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-type (unsafe-ffi-primitive 'js-event-type))

;; js-event-target : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-target (unsafe-ffi-primitive 'js-event-target))

;; js-event-current-target : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-current-target (unsafe-ffi-primitive 'js-event-current-target))

;; js-event-event-phase : ffi (extern) -> (u32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-event-phase (unsafe-ffi-primitive 'js-event-event-phase))

;; js-event-bubbles : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-bubbles (unsafe-ffi-primitive 'js-event-bubbles))

;; js-event-cancelable : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-cancelable (unsafe-ffi-primitive 'js-event-cancelable))

;; js-event-default-prevented : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-default-prevented (unsafe-ffi-primitive 'js-event-default-prevented))

;; js-event-composed : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-composed (unsafe-ffi-primitive 'js-event-composed))

;; js-event-is-trusted : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-is-trusted (unsafe-ffi-primitive 'js-event-is-trusted))

;; js-event-time-stamp : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-time-stamp (unsafe-ffi-primitive 'js-event-time-stamp))

;; js-event-composed-path : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-composed-path (unsafe-ffi-primitive 'js-event-composed-path))

;; js-event-prevent-default : ffi (extern) -> ()
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-prevent-default (unsafe-ffi-primitive 'js-event-prevent-default))

;; js-event-stop-propagation : ffi (extern) -> ()
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-stop-propagation (unsafe-ffi-primitive 'js-event-stop-propagation))

;; js-event-stop-immediate-propagation : ffi (extern) -> ()
;;   Raw binding generated for webracket/unsafe/event.
(define js-event-stop-immediate-propagation (unsafe-ffi-primitive 'js-event-stop-immediate-propagation))

;; js-message-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event? (unsafe-ffi-primitive 'js-message-event?))

;; js-message-event-data : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event-data (unsafe-ffi-primitive 'js-message-event-data))

;; js-message-event-origin : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event-origin (unsafe-ffi-primitive 'js-message-event-origin))

;; js-message-event-last-event-id : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event-last-event-id (unsafe-ffi-primitive 'js-message-event-last-event-id))

;; js-message-event-source : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event-source (unsafe-ffi-primitive 'js-message-event-source))

;; js-message-event-ports : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-message-event-ports (unsafe-ffi-primitive 'js-message-event-ports))

;; js-close-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-close-event? (unsafe-ffi-primitive 'js-close-event?))

;; js-close-event-was-clean : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-close-event-was-clean (unsafe-ffi-primitive 'js-close-event-was-clean))

;; js-close-event-code : ffi (extern) -> (u32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-close-event-code (unsafe-ffi-primitive 'js-close-event-code))

;; js-close-event-reason : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-close-event-reason (unsafe-ffi-primitive 'js-close-event-reason))

;; js-mouse-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event? (unsafe-ffi-primitive 'js-mouse-event?))

;; js-mouse-event-offset-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-offset-x (unsafe-ffi-primitive 'js-mouse-event-offset-x))

;; js-mouse-event-offset-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-offset-y (unsafe-ffi-primitive 'js-mouse-event-offset-y))

;; js-mouse-event-client-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-client-x (unsafe-ffi-primitive 'js-mouse-event-client-x))

;; js-mouse-event-client-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-client-y (unsafe-ffi-primitive 'js-mouse-event-client-y))

;; js-mouse-event-page-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-page-x (unsafe-ffi-primitive 'js-mouse-event-page-x))

;; js-mouse-event-page-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-page-y (unsafe-ffi-primitive 'js-mouse-event-page-y))

;; js-mouse-event-screen-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-screen-x (unsafe-ffi-primitive 'js-mouse-event-screen-x))

;; js-mouse-event-screen-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-screen-y (unsafe-ffi-primitive 'js-mouse-event-screen-y))

;; js-mouse-event-button : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-button (unsafe-ffi-primitive 'js-mouse-event-button))

;; js-mouse-event-buttons : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-buttons (unsafe-ffi-primitive 'js-mouse-event-buttons))

;; js-mouse-event-alt-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-alt-key (unsafe-ffi-primitive 'js-mouse-event-alt-key))

;; js-mouse-event-ctrl-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-ctrl-key (unsafe-ffi-primitive 'js-mouse-event-ctrl-key))

;; js-mouse-event-meta-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-meta-key (unsafe-ffi-primitive 'js-mouse-event-meta-key))

;; js-mouse-event-shift-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-mouse-event-shift-key (unsafe-ffi-primitive 'js-mouse-event-shift-key))

;; js-keyboard-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event? (unsafe-ffi-primitive 'js-keyboard-event?))

;; js-keyboard-event-key : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-key (unsafe-ffi-primitive 'js-keyboard-event-key))

;; js-keyboard-event-code : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-code (unsafe-ffi-primitive 'js-keyboard-event-code))

;; js-keyboard-event-alt-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-alt-key (unsafe-ffi-primitive 'js-keyboard-event-alt-key))

;; js-keyboard-event-ctrl-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-ctrl-key (unsafe-ffi-primitive 'js-keyboard-event-ctrl-key))

;; js-keyboard-event-meta-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-meta-key (unsafe-ffi-primitive 'js-keyboard-event-meta-key))

;; js-keyboard-event-shift-key : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-shift-key (unsafe-ffi-primitive 'js-keyboard-event-shift-key))

;; js-keyboard-event-repeat : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-repeat (unsafe-ffi-primitive 'js-keyboard-event-repeat))

;; js-keyboard-event-is-composing : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-is-composing (unsafe-ffi-primitive 'js-keyboard-event-is-composing))

;; js-keyboard-event-get-modifier-state : ffi (extern string) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-keyboard-event-get-modifier-state (unsafe-ffi-primitive 'js-keyboard-event-get-modifier-state))

;; js-pointer-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-pointer-event? (unsafe-ffi-primitive 'js-pointer-event?))

;; js-pointer-event-id : ffi (extern) -> (u32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-pointer-event-id (unsafe-ffi-primitive 'js-pointer-event-id))

;; js-pointer-event-type : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-pointer-event-type (unsafe-ffi-primitive 'js-pointer-event-type))

;; js-pointer-event-is-primary : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-pointer-event-is-primary (unsafe-ffi-primitive 'js-pointer-event-is-primary))

;; js-focus-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-focus-event? (unsafe-ffi-primitive 'js-focus-event?))

;; js-focus-event-related-target : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-focus-event-related-target (unsafe-ffi-primitive 'js-focus-event-related-target))

;; js-input-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-input-event? (unsafe-ffi-primitive 'js-input-event?))

;; js-input-event-input-type : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-input-event-input-type (unsafe-ffi-primitive 'js-input-event-input-type))

;; js-input-event-data : ffi (extern) -> (value)
;;   Raw binding generated for webracket/unsafe/event.
(define js-input-event-data (unsafe-ffi-primitive 'js-input-event-data))

;; js-input-event-is-composing : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-input-event-is-composing (unsafe-ffi-primitive 'js-input-event-is-composing))

;; js-input-event-data-transfer : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-input-event-data-transfer (unsafe-ffi-primitive 'js-input-event-data-transfer))

;; js-composition-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-composition-event? (unsafe-ffi-primitive 'js-composition-event?))

;; js-composition-event-data : ffi (extern) -> (string)
;;   Raw binding generated for webracket/unsafe/event.
(define js-composition-event-data (unsafe-ffi-primitive 'js-composition-event-data))

;; js-clipboard-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-clipboard-event? (unsafe-ffi-primitive 'js-clipboard-event?))

;; js-clipboard-event-data : ffi (extern) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-clipboard-event-data (unsafe-ffi-primitive 'js-clipboard-event-data))

;; js-submit-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-submit-event? (unsafe-ffi-primitive 'js-submit-event?))

;; js-wheel-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-wheel-event? (unsafe-ffi-primitive 'js-wheel-event?))

;; js-touch-event? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-event? (unsafe-ffi-primitive 'js-touch-event?))

;; js-touch-event-touches : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-event-touches (unsafe-ffi-primitive 'js-touch-event-touches))

;; js-touch-event-target-touches : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-event-target-touches (unsafe-ffi-primitive 'js-touch-event-target-touches))

;; js-touch-event-changed-touches : ffi (extern) -> (extern/raw)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-event-changed-touches (unsafe-ffi-primitive 'js-touch-event-changed-touches))

;; js-touch-list? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-list? (unsafe-ffi-primitive 'js-touch-list?))

;; js-touch-list-length : ffi (extern) -> (u32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-list-length (unsafe-ffi-primitive 'js-touch-list-length))

;; js-touch-list-ref : ffi (extern u32) -> (extern)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-list-ref (unsafe-ffi-primitive 'js-touch-list-ref))

;; js-touch? : ffi (extern) -> (boolean)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch? (unsafe-ffi-primitive 'js-touch?))

;; js-touch-identifier : ffi (extern) -> (i32)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-identifier (unsafe-ffi-primitive 'js-touch-identifier))

;; js-touch-client-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-client-x (unsafe-ffi-primitive 'js-touch-client-x))

;; js-touch-client-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-client-y (unsafe-ffi-primitive 'js-touch-client-y))

;; js-touch-page-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-page-x (unsafe-ffi-primitive 'js-touch-page-x))

;; js-touch-page-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-page-y (unsafe-ffi-primitive 'js-touch-page-y))

;; js-touch-screen-x : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-screen-x (unsafe-ffi-primitive 'js-touch-screen-x))

;; js-touch-screen-y : ffi (extern) -> (f64)
;;   Raw binding generated for webracket/unsafe/event.
(define js-touch-screen-y (unsafe-ffi-primitive 'js-touch-screen-y))
