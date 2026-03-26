#lang webracket

;;;
;;; web-easy Browser Event Helpers
;;;

;; Thin convenience wrappers around raw DOM event FFI bindings.
;;
;; Exports:
;;   event?                          Check whether value is a DOM Event.
;;   mouse-event?                    Check whether value is a DOM MouseEvent.
;;   keyboard-event?                 Check whether value is a DOM KeyboardEvent.
;;   pointer-event?                  Check whether value is a DOM PointerEvent.
;;   focus-event?                    Check whether value is a DOM FocusEvent.
;;   input-event?                    Check whether value is a DOM InputEvent.
;;   submit-event?                   Check whether value is a DOM SubmitEvent.
;;   wheel-event?                    Check whether value is a DOM WheelEvent.
;;   event-type                       Read event type string.
;;   event-target                     Read original dispatch target.
;;   event-current-target             Read current listener target.
;;   prevent-default!                 Prevent default event action.
;;   stop-propagation!                Stop event propagation.
;;   stop-immediate-propagation!      Stop immediate event propagation.
;;   mouse-event-offset-x             Read target-relative mouse x coordinate.
;;   mouse-event-offset-y             Read target-relative mouse y coordinate.
;;   mouse-event-client-x             Read viewport-relative mouse x coordinate.
;;   mouse-event-client-y             Read viewport-relative mouse y coordinate.
;;   keyboard-event-key               Read logical key string.
;;   keyboard-event-code              Read physical key code string.
;;   keyboard-event-alt-key?          Read Alt modifier state as boolean.
;;   keyboard-event-ctrl-key?         Read Control modifier state as boolean.
;;   keyboard-event-meta-key?         Read Meta modifier state as boolean.
;;   keyboard-event-shift-key?        Read Shift modifier state as boolean.
;;   keyboard-event-repeat?           Read repeat state as boolean.

(define-values
  (event?
   mouse-event?
   keyboard-event?
   pointer-event?
   focus-event?
   input-event?
   submit-event?
   wheel-event?
   event-type
   event-target
   event-current-target
   prevent-default!
   stop-propagation!
   stop-immediate-propagation!
   mouse-event-offset-x
   mouse-event-offset-y
   mouse-event-client-x
   mouse-event-client-y
   keyboard-event-key
   keyboard-event-code
   keyboard-event-alt-key?
   keyboard-event-ctrl-key?
   keyboard-event-meta-key?
   keyboard-event-shift-key?
   keyboard-event-repeat?)
  (let ()
    ;; js-flag->boolean/internal : integer? -> boolean?
    ;;   Convert FFI i32 flag results to boolean.
    (define (js-flag->boolean/internal v)
      (not (zero? v)))

    ;; check-event : symbol? any/c -> void?
    ;;   Ensure evt is a DOM Event object.
    (define (check-event who evt)
      (unless (event? evt)
        (raise-argument-error who "event?" evt)))

    ;; check-mouse-event : symbol? any/c -> void?
    ;;   Ensure evt is a DOM MouseEvent object.
    (define (check-mouse-event who evt)
      (unless (mouse-event? evt)
        (raise-argument-error who "mouse-event?" evt)))

    ;; check-keyboard-event : symbol? any/c -> void?
    ;;   Ensure evt is a DOM KeyboardEvent object.
    (define (check-keyboard-event who evt)
      (unless (keyboard-event? evt)
        (raise-argument-error who "keyboard-event?" evt)))

    ;; event? : any/c -> boolean?
    ;;   Report whether evt is a DOM Event object.
    (define (event? evt)
      (and (external? evt)
           (js-event? evt)))

    ;; mouse-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM MouseEvent object.
    (define (mouse-event? evt)
      (and (external? evt)
           (js-mouse-event? evt)))

    ;; keyboard-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM KeyboardEvent object.
    (define (keyboard-event? evt)
      (and (external? evt)
           (js-keyboard-event? evt)))

    ;; pointer-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM PointerEvent object.
    (define (pointer-event? evt)
      (and (external? evt)
           (js-pointer-event? evt)))

    ;; focus-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM FocusEvent object.
    (define (focus-event? evt)
      (and (external? evt)
           (js-focus-event? evt)))

    ;; input-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM InputEvent object.
    (define (input-event? evt)
      (and (external? evt)
           (js-input-event? evt)))

    ;; submit-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM SubmitEvent object.
    (define (submit-event? evt)
      (and (external? evt)
           (js-submit-event? evt)))

    ;; wheel-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM WheelEvent object.
    (define (wheel-event? evt)
      (and (external? evt)
           (js-wheel-event? evt)))

    ;; event-type : any/c -> string?
    ;;   Return the browser event type string.
    (define (event-type evt)
      (check-event 'event-type evt)
      (js-event-type evt))

    ;; event-target : any/c -> any/c
    ;;   Return the original dispatch target.
    (define (event-target evt)
      (check-event 'event-target evt)
      (js-event-target evt))

    ;; event-current-target : any/c -> any/c
    ;;   Return the current listener target.
    (define (event-current-target evt)
      (check-event 'event-current-target evt)
      (js-event-current-target evt))

    ;; prevent-default! : any/c -> void?
    ;;   Prevent the default browser action for evt.
    (define (prevent-default! evt)
      (check-event 'prevent-default! evt)
      (js-event-prevent-default evt))

    ;; stop-propagation! : any/c -> void?
    ;;   Stop further propagation of evt.
    (define (stop-propagation! evt)
      (check-event 'stop-propagation! evt)
      (js-event-stop-propagation evt))

    ;; stop-immediate-propagation! : any/c -> void?
    ;;   Stop immediate propagation of evt.
    (define (stop-immediate-propagation! evt)
      (check-event 'stop-immediate-propagation! evt)
      (js-event-stop-immediate-propagation evt))

    ;; mouse-event-offset-x : any/c -> number?
    ;;   Return target-relative mouse x coordinate.
    (define (mouse-event-offset-x evt)
      (check-mouse-event 'mouse-event-offset-x evt)
      (js-mouse-event-offset-x evt))

    ;; mouse-event-offset-y : any/c -> number?
    ;;   Return target-relative mouse y coordinate.
    (define (mouse-event-offset-y evt)
      (check-mouse-event 'mouse-event-offset-y evt)
      (js-mouse-event-offset-y evt))

    ;; mouse-event-client-x : any/c -> number?
    ;;   Return viewport-relative mouse x coordinate.
    (define (mouse-event-client-x evt)
      (check-mouse-event 'mouse-event-client-x evt)
      (js-mouse-event-client-x evt))

    ;; mouse-event-client-y : any/c -> number?
    ;;   Return viewport-relative mouse y coordinate.
    (define (mouse-event-client-y evt)
      (check-mouse-event 'mouse-event-client-y evt)
      (js-mouse-event-client-y evt))

    ;; keyboard-event-key : any/c -> string?
    ;;   Return logical key string for keyboard evt.
    (define (keyboard-event-key evt)
      (check-keyboard-event 'keyboard-event-key evt)
      (js-keyboard-event-key evt))

    ;; keyboard-event-code : any/c -> string?
    ;;   Return physical key code string for keyboard evt.
    (define (keyboard-event-code evt)
      (check-keyboard-event 'keyboard-event-code evt)
      (js-keyboard-event-code evt))

    ;; keyboard-event-alt-key? : any/c -> boolean?
    ;;   Report whether Alt was active for keyboard evt.
    (define (keyboard-event-alt-key? evt)
      (check-keyboard-event 'keyboard-event-alt-key? evt)
      (js-flag->boolean/internal
       (js-keyboard-event-alt-key evt)))

    ;; keyboard-event-ctrl-key? : any/c -> boolean?
    ;;   Report whether Control was active for keyboard evt.
    (define (keyboard-event-ctrl-key? evt)
      (check-keyboard-event 'keyboard-event-ctrl-key? evt)
      (js-flag->boolean/internal
       (js-keyboard-event-ctrl-key evt)))

    ;; keyboard-event-meta-key? : any/c -> boolean?
    ;;   Report whether Meta was active for keyboard evt.
    (define (keyboard-event-meta-key? evt)
      (check-keyboard-event 'keyboard-event-meta-key? evt)
      (js-flag->boolean/internal
       (js-keyboard-event-meta-key evt)))

    ;; keyboard-event-shift-key? : any/c -> boolean?
    ;;   Report whether Shift was active for keyboard evt.
    (define (keyboard-event-shift-key? evt)
      (check-keyboard-event 'keyboard-event-shift-key? evt)
      (js-flag->boolean/internal
       (js-keyboard-event-shift-key evt)))

    ;; keyboard-event-repeat? : any/c -> boolean?
    ;;   Report whether keyboard evt is auto-repeating.
    (define (keyboard-event-repeat? evt)
      (check-keyboard-event 'keyboard-event-repeat? evt)
      (js-flag->boolean/internal
       (js-keyboard-event-repeat evt)))

    (values event?
            mouse-event?
            keyboard-event?
            pointer-event?
            focus-event?
            input-event?
            submit-event?
            wheel-event?
            event-type
            event-target
            event-current-target
            prevent-default!
            stop-propagation!
            stop-immediate-propagation!
            mouse-event-offset-x
            mouse-event-offset-y
            mouse-event-client-x
            mouse-event-client-y
            keyboard-event-key
            keyboard-event-code
            keyboard-event-alt-key?
            keyboard-event-ctrl-key?
            keyboard-event-meta-key?
            keyboard-event-shift-key?
            keyboard-event-repeat?)))
