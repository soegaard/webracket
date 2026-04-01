;;;
;;; dom.ffi
;;;

;; Focused tests for the Event wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-event.rkt

(include-lib event)

(define (check-equal got want label)
  (unless (if (and (number? got) (number? want))
              (= got want)
              (equal? got want))
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-false got label)
  (when got
    (error 'check-false label)))

(define (expect-contract-error thunk)
  (with-handlers ([exn:fail:contract? (lambda (_e) #t)])
    (thunk)
    #f))

(define (install!)
  (js-eval
   "class BaseEvent {
      constructor(type, init = {}) {
        this.type = type;
        this.target = init.target || null;
        this.currentTarget = init.currentTarget || null;
        this.defaultPrevented = false;
        this.cancelBubble = false;
        this.immediateStopped = false;
      }
      preventDefault() { this.defaultPrevented = true; }
      stopPropagation() { this.cancelBubble = true; }
      stopImmediatePropagation() { this.immediateStopped = true; }
    }
    class MouseEvent extends BaseEvent {
      constructor(type, init = {}) {
        super(type, init);
        this.offsetX = init.offsetX || 4;
        this.offsetY = init.offsetY || 5;
        this.clientX = init.clientX || 12;
        this.clientY = init.clientY || 34;
        this.pageX = init.pageX || 56;
        this.pageY = init.pageY || 78;
        this.screenX = init.screenX || 90;
        this.screenY = init.screenY || 91;
        this.button = init.button || 1;
        this.buttons = init.buttons || 2;
        this.altKey = !!init.altKey;
        this.ctrlKey = !!init.ctrlKey;
        this.metaKey = !!init.metaKey;
        this.shiftKey = !!init.shiftKey;
      }
    }
    class KeyboardEvent extends BaseEvent {
      constructor(type, init = {}) {
        super(type, init);
        this.key = init.key || 'Enter';
        this.code = init.code || 'Enter';
        this.altKey = !!init.altKey;
        this.ctrlKey = !!init.ctrlKey;
        this.metaKey = !!init.metaKey;
        this.shiftKey = !!init.shiftKey;
        this.repeat = !!init.repeat;
      }
    }
    globalThis.Event = BaseEvent;
    globalThis.MouseEvent = MouseEvent;
    globalThis.KeyboardEvent = KeyboardEvent;
    window.__domTest = window.__domTest || {};
    window.__domTest.event = new BaseEvent('submit', { target: { kind: 'form' }, currentTarget: { kind: 'listener' } });
    window.__domTest.mouseEvent = new MouseEvent('click', {
      target: { kind: 'button' },
      currentTarget: { kind: 'listener' },
      altKey: true,
      metaKey: true
    });
    window.__domTest.keyboardEvent = new KeyboardEvent('keydown', {
      target: { kind: 'input' },
      currentTarget: { kind: 'listener' },
      key: 'Escape',
      code: 'Escape',
      altKey: true,
      ctrlKey: true,
      shiftKey: true,
      repeat: true
    });"))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
 (list "Event wrappers"
       (let ()
         (install!)
         (define evt (dom-test-fixture "event"))
         (define mouse (dom-test-fixture "mouseEvent"))
         (define key (dom-test-fixture "keyboardEvent"))
         (check-true (event? evt) "event predicate")
         (check-true (mouse-event? mouse) "mouse event predicate")
         (check-true (keyboard-event? key) "keyboard event predicate")
         (check-equal (event-type evt) "submit" "event type")
         (check-equal (js-ref (event-target evt) "kind") "form" "event target")
         (check-equal (js-ref (event-current-target evt) "kind") "listener" "event current target")
         (prevent-default! evt)
         (stop-propagation! evt)
         (stop-immediate-propagation! evt)
         (check-true (js-ref evt "defaultPrevented") "event prevent default")
         (check-true (js-ref evt "cancelBubble") "event stop propagation")
         (check-true (js-ref evt "immediateStopped") "event stop immediate propagation")
         (check-equal (mouse-event-offset-x mouse) 4.0 "mouse offset x")
         (check-equal (mouse-event-offset-y mouse) 5.0 "mouse offset y")
         (check-equal (mouse-event-client-x mouse) 12.0 "mouse client x")
         (check-equal (mouse-event-client-y mouse) 34.0 "mouse client y")
         (check-equal (mouse-event-page-x mouse) 56.0 "mouse page x")
         (check-equal (mouse-event-page-y mouse) 78.0 "mouse page y")
         (check-equal (mouse-event-screen-x mouse) 90.0 "mouse screen x")
         (check-equal (mouse-event-screen-y mouse) 91.0 "mouse screen y")
         (check-equal (mouse-event-button mouse) 1.0 "mouse button")
         (check-equal (mouse-event-buttons mouse) 2.0 "mouse buttons")
         (check-true (mouse-event-alt-key? mouse) "mouse alt key")
         (check-false (mouse-event-ctrl-key? mouse) "mouse ctrl key")
         (check-true (mouse-event-meta-key? mouse) "mouse meta key")
         (check-false (mouse-event-shift-key? mouse) "mouse shift key")
         (check-equal (keyboard-event-key key) "Escape" "keyboard key")
         (check-equal (keyboard-event-code key) "Escape" "keyboard code")
         (check-true (keyboard-event-alt-key? key) "keyboard alt key")
         (check-true (keyboard-event-ctrl-key? key) "keyboard ctrl key")
         (check-false (keyboard-event-meta-key? key) "keyboard meta key")
         (check-true (keyboard-event-shift-key? key) "keyboard shift key")
         (check-true (keyboard-event-repeat? key) "keyboard repeat")
         (check-true (expect-contract-error (lambda () (event-type 1))) "event type validation")
         (check-true (expect-contract-error (lambda () (mouse-event-client-x 1))) "mouse event validation")
         (check-true (expect-contract-error (lambda () (keyboard-event-key 1))) "keyboard event validation")
         #t)))
