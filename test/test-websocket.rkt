;;;
;;; websocket.ffi
;;;

;; This file contains focused tests for `websocket.ffi`.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/websocket.ffi -r test-websocket.rkt

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
                      }
                      send(data) {
                        this.sent.push(data);
                      }
                      close(code, reason) {
                        this.closed.push([code, reason]);
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
              (equal? (js-ref ws "closed") (vector (vector 1000 "done")))))))
