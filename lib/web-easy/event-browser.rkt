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
;;   touch-event?                    Check whether value is a DOM TouchEvent.
;;   wheel-event?                    Check whether value is a DOM WheelEvent.
;;   touch-list?                     Check whether value is a DOM TouchList.
;;   touch?                          Check whether value is a DOM Touch.
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
;;   touch-event-touches              Read active touches list.
;;   touch-event-target-touches       Read target touches list.
;;   touch-event-changed-touches      Read changed touches list.
;;   touch-list-length                Read number of touches in a touch list.
;;   touch-list-ref                   Read touch at index from a touch list.
;;   touch-identifier                 Read touch identifier.
;;   touch-client-x                   Read touch client x coordinate.
;;   touch-client-y                   Read touch client y coordinate.
;;   touch-page-x                     Read touch page x coordinate.
;;   touch-page-y                     Read touch page y coordinate.
;;   touch-screen-x                   Read touch screen x coordinate.
;;   touch-screen-y                   Read touch screen y coordinate.

(define-values
  (event?
   mouse-event?
   keyboard-event?
   pointer-event?
   focus-event?
   input-event?
   submit-event?
   touch-event?
   wheel-event?
   touch-list?
   touch?
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
   keyboard-event-repeat?
   touch-event-touches
   touch-event-target-touches
   touch-event-changed-touches
   touch-list-length
   touch-list-ref
   touch-identifier
   touch-client-x
   touch-client-y
   touch-page-x
   touch-page-y
   touch-screen-x
   touch-screen-y)
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

    ;; check-touch-event : symbol? any/c -> void?
    ;;   Ensure evt is a DOM TouchEvent object.
    (define (check-touch-event who evt)
      (unless (touch-event? evt)
        (raise-argument-error who "touch-event?" evt)))

    ;; check-touch-list : symbol? any/c -> void?
    ;;   Ensure xs is a DOM TouchList object.
    (define (check-touch-list who xs)
      (unless (touch-list? xs)
        (raise-argument-error who "touch-list?" xs)))

    ;; check-touch : symbol? any/c -> void?
    ;;   Ensure t is a DOM Touch object.
    (define (check-touch who t)
      (unless (touch? t)
        (raise-argument-error who "touch?" t)))

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

    ;; touch-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM TouchEvent object.
    (define (touch-event? evt)
      (and (external? evt)
           (js-touch-event? evt)))

    ;; wheel-event? : any/c -> boolean?
    ;;   Report whether evt is a DOM WheelEvent object.
    (define (wheel-event? evt)
      (and (external? evt)
           (js-wheel-event? evt)))

    ;; touch-list? : any/c -> boolean?
    ;;   Report whether xs is a DOM TouchList object.
    (define (touch-list? xs)
      (and (external? xs)
           (js-touch-list? xs)))

    ;; touch? : any/c -> boolean?
    ;;   Report whether t is a DOM Touch object.
    (define (touch? t)
      (and (external? t)
           (js-touch? t)))

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

    ;; touch-event-touches : any/c -> external?
    ;;   Return the active touches list for a touch event.
    (define (touch-event-touches evt)
      (check-touch-event 'touch-event-touches evt)
      (js-touch-event-touches evt))

    ;; touch-event-target-touches : any/c -> external?
    ;;   Return the target touches list for a touch event.
    (define (touch-event-target-touches evt)
      (check-touch-event 'touch-event-target-touches evt)
      (js-touch-event-target-touches evt))

    ;; touch-event-changed-touches : any/c -> external?
    ;;   Return the changed touches list for a touch event.
    (define (touch-event-changed-touches evt)
      (check-touch-event 'touch-event-changed-touches evt)
      (js-touch-event-changed-touches evt))

    ;; touch-list-length : any/c -> exact-nonnegative-integer?
    ;;   Return the number of touches in xs.
    (define (touch-list-length xs)
      (check-touch-list 'touch-list-length xs)
      (js-touch-list-length xs))

    ;; touch-list-ref : any/c exact-nonnegative-integer? -> (or/c #f external)
    ;;   Return the touch at index i, or #f when out of range.
    (define (touch-list-ref xs i)
      (check-touch-list 'touch-list-ref xs)
      (js-touch-list-ref xs i))

    ;; touch-identifier : any/c -> exact-integer?
    ;;   Return the identifier of touch t.
    (define (touch-identifier t)
      (check-touch 'touch-identifier t)
      (js-touch-identifier t))

    ;; touch-client-x : any/c -> number?
    ;;   Return the client x coordinate of touch t.
    (define (touch-client-x t)
      (check-touch 'touch-client-x t)
      (js-touch-client-x t))

    ;; touch-client-y : any/c -> number?
    ;;   Return the client y coordinate of touch t.
    (define (touch-client-y t)
      (check-touch 'touch-client-y t)
      (js-touch-client-y t))

    ;; touch-page-x : any/c -> number?
    ;;   Return the page x coordinate of touch t.
    (define (touch-page-x t)
      (check-touch 'touch-page-x t)
      (js-touch-page-x t))

    ;; touch-page-y : any/c -> number?
    ;;   Return the page y coordinate of touch t.
    (define (touch-page-y t)
      (check-touch 'touch-page-y t)
      (js-touch-page-y t))

    ;; touch-screen-x : any/c -> number?
    ;;   Return the screen x coordinate of touch t.
    (define (touch-screen-x t)
      (check-touch 'touch-screen-x t)
      (js-touch-screen-x t))

    ;; touch-screen-y : any/c -> number?
    ;;   Return the screen y coordinate of touch t.
    (define (touch-screen-y t)
      (check-touch 'touch-screen-y t)
      (js-touch-screen-y t))

    (values event?
            mouse-event?
            keyboard-event?
            pointer-event?
            focus-event?
            input-event?
            submit-event?
            touch-event?
            wheel-event?
            touch-list?
            touch?
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
            keyboard-event-repeat?
            touch-event-touches
            touch-event-target-touches
            touch-event-changed-touches
            touch-list-length
            touch-list-ref
            touch-identifier
            touch-client-x
            touch-client-y
            touch-page-x
            touch-page-y
            touch-screen-x
            touch-screen-y)))
