;;;
;;; websocket.ffi
;;;

;; This file contains focused tests for `websocket.ffi`.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/websocket.ffi -r test-websocket.rkt

(include-lib websocket)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

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

(list
 (list "WebSocket"
       (let* ([_  (js-eval
                   "globalThis.WebSocket = class WebSocket {
                      constructor(url, protocols) {
                        this.url = url;
                        this.protocols = protocols;
                        this.readyState = 1;
                        this.bufferedAmount = 7;
                        this.protocol = 'chat';
                        this.extensions = 'permessage-deflate';
                        this.sent = [];
                        this.closed = [];
                        this.handlers = [];
                      }
                      send(data) {
                        this.sent.push(data);
                      }
                      close(code, reason) {
                        this.closed.push([code, reason]);
                        if (this.onclose) {
                          this.onclose({type: 'close', code, reason});
                        }
                      }
                      addEventListener(type, listener, options) {
                        this.handlers.push([type, listener, options]);
                      }
                      removeEventListener(type, listener, options) {
                        this.handlers = this.handlers.filter((entry) =>
                          entry[0] !== type);
                      }
                      dispatch(type, evt) {
                        const handler = this['on' + type];
                        if (handler) {
                          handler(evt);
                        }
                        for (const entry of this.handlers) {
                          if (entry[0] === type) {
                            entry[1](evt);
                          }
                        }
                      }
                    };")]
              [ws (js-websocket-new "wss://example.invalid/socket" (vector "chat"))]
              [_  (js-websocket-send ws "hello")]
              [_  (js-websocket-close ws 1000 "done")])
         (and (equal? (js-websocket-url ws) "wss://example.invalid/socket")
              (equal? (js-websocket-ready-state ws) 1)
              (equal? (js-websocket-buffered-amount ws) 7)
              (equal? (js-websocket-protocol ws) "chat")
              (equal? (js-websocket-extensions ws) "permessage-deflate")
              (equal? (js-ref ws "protocols") (vector "chat"))
              (equal? (js-ref ws "sent") (vector "hello"))
              (equal? (js-ref ws "closed") (vector (vector 1000 "done"))))))
 (list "WebSocket wrappers"
       (let* ([_  (js-eval
                   "globalThis.WebSocket = class WebSocket {
                      constructor(url, protocols) {
                        this.url = url;
                        this.protocols = protocols;
                        this.readyState = 1;
                        this.bufferedAmount = 7;
                        this.protocol = 'chat';
                        this.extensions = 'permessage-deflate';
                        this.sent = [];
                        this.closed = [];
                        this.handlers = [];
                        this.onopen = null;
                        this.onmessage = null;
                        this.onclose = null;
                        this.onerror = null;
                      }
                      send(data) {
                        this.sent.push(data);
                      }
                      close(code, reason) {
                        this.closed.push([code, reason === undefined ? 'omitted' : reason]);
                      }
                      addEventListener(type, listener, options) {
                        this.lastAddOptions = options;
                        this.handlers.push(['add', type, listener, options]);
                      }
                      removeEventListener(type, listener, options) {
                        this.lastRemoveOptions = options;
                        this.handlers.push(['remove', type, listener, options]);
                      }
                      dispatch(type, evt) {
                        const handler = this['on' + type];
                        if (handler) {
                          handler(evt);
                        }
                        for (const entry of this.handlers) {
                          if (entry[0] === 'add' && entry[1] === type) {
                            entry[2](evt);
                          }
                        }
                      }
                    };")]
              [ws (websocket-new (string->symbol "wss://example.invalid/socket")
                                 "chat"
                                 'v2)]
              [open-count 0]
              [message-count 0]
              [close-count 0]
              [error-count 0]
              [listener-count 0]
              [open-handler
               (lambda (_evt)
                 (set! open-count (add1 open-count))
                 (void))]
              [message-handler
               (lambda (_evt)
                 (set! message-count (add1 message-count))
                 (void))]
              [close-handler
               (lambda (_evt)
                 (set! close-count (add1 close-count))
                 (void))]
              [error-handler
               (lambda (_evt)
                 (set! error-count (add1 error-count))
                 (void))]
              [listener
               (lambda (_evt)
                 (set! listener-count (add1 listener-count))
                 (void))]
              [listener* (websocket-add-event-listener! ws 'message listener #t)])
         (websocket-onopen! ws open-handler)
         (websocket-onmessage! ws message-handler)
         (websocket-onclose! ws close-handler)
         (websocket-onerror! ws error-handler)
         (websocket-send ws 'hello)
         (js-send/extern/nullish ws "dispatch" (vector "open" (js-eval "({type: 'open'})")))
         (js-send/extern/nullish ws "dispatch" (vector "message" (js-eval "({type: 'message', data: 'ping'})")))
         (js-send/extern/nullish ws "dispatch" (vector "error" (js-eval "({type: 'error'})")))
         (websocket-remove-event-listener! ws "message" listener #t)
         (js-send/extern/nullish ws "dispatch" (vector "close" (js-eval "({type: 'close'})")))
         (websocket-close ws 1000 'done)
         (check-true (websocket? ws) "wrapper websocket? true")
         (check-false (websocket? #f) "wrapper websocket? false")
         (check-equal (websocket-url ws) "wss://example.invalid/socket" "wrapper url")
         (check-equal (websocket-ready-state-number ws) 1 "wrapper readyState number")
         (check-equal (websocket-ready-state ws) 'open "wrapper readyState symbol")
         (check-equal (websocket-buffered-amount ws) 7 "wrapper bufferedAmount")
         (check-equal (websocket-protocol ws) "chat" "wrapper protocol")
         (check-equal (websocket-extensions ws) "permessage-deflate" "wrapper extensions")
         (check-equal (js-ref ws "protocols") (vector "chat" "v2") "wrapper protocols")
         (check-equal (js-ref ws "sent") (vector "hello") "wrapper send")
         (check-equal (js-ref ws "closed") (vector (vector 1000 "done")) "wrapper close")
         (check-equal (js-ref ws "lastAddOptions") #t "wrapper add listener options")
         (check-equal (js-ref ws "lastRemoveOptions") #t "wrapper remove listener options")
         (check-equal open-count 1 "wrapper open handler")
         (check-equal message-count 1 "wrapper message handler")
         (check-equal close-count 1 "wrapper close handler")
         (check-equal error-count 1 "wrapper error handler")
         (check-equal listener-count 1 "wrapper listener registration")
         (check-true (expect-contract-error (lambda () (websocket-new 0))) "wrapper constructor validation")
         (check-true (expect-contract-error (lambda () (websocket-send #f "x"))) "wrapper send validation")
         (check-true (expect-contract-error (lambda () (websocket-close #f))) "wrapper close validation")
         (check-true (expect-contract-error (lambda () (websocket-onopen! ws 0))) "wrapper onopen validation")
         (check-true (expect-contract-error (lambda () (websocket-add-event-listener! ws 0 listener))) "wrapper add listener validation")
         (check-true (expect-contract-error (lambda () (websocket-remove-event-listener! ws "message" 0))) "wrapper remove listener validation")
         #t)))
