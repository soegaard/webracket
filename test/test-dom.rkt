;;;
;;; dom.ffi
;;;

;; Focused tests for the split DOM FFI families and wrapper libraries.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom.rkt

(include-lib window)
(include-lib document)
(include-lib element)
(include-lib canvas)
(include-lib domrect)
(include-lib media)
(include-lib image)
(include-lib event)

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

(define (install-window-fixtures!)
  (js-eval
   "window.name = 'dom-suite';
    window.confirm = function (message) {
      window.lastConfirm = message;
      return 1;
    };
    window.open = function (url, target, features, replace) {
      window.lastOpen = [url, target, features, replace];
      return { kind: 'popup', url, target, features, replace };
    };
    window.setTimeout = function (callback, delay) {
      window.lastTimeout = [callback, delay];
      callback();
      return 17;
    };
    window.clearTimeout = function (id) {
      window.lastClearedTimeout = id;
    };
    window.setInterval = function (callback, delay) {
      window.lastInterval = [callback, delay];
      return 23;
    };
    window.clearInterval = function (id) {
      window.lastClearedInterval = id;
    };
    window.requestAnimationFrame = function (callback) {
      window.lastAnimationFrame = callback;
      return 29;
    };
    window.cancelAnimationFrame = function (id) {
      window.lastCancelledAnimationFrame = id;
    };
    window.requestIdleCallback = function (callback, options) {
      window.lastIdleCallback = [callback, options];
      return 31;
    };
    window.cancelIdleCallback = function (id) {
      window.lastCancelledIdleCallback = id;
    };
    window.scrollTo = function (x, y, options) {
      window.lastScrollTo = [x, y, options];
    };
    window.scrollBy = function (x, y, options) {
      window.lastScrollBy = [x, y, options];
    };
    window.scroll = function (x, y, options) {
      window.lastScroll = [x, y, options];
    };
    window.resizeTo = function (w, h) {
      window.lastResizeTo = [w, h];
    };
    window.resizeBy = function (w, h) {
      window.lastResizeBy = [w, h];
    };
    window.moveTo = function (x, y) {
      window.lastMoveTo = [x, y];
    };
    window.moveBy = function (x, y) {
      window.lastMoveBy = [x, y];
    };
    window.getComputedStyle = function (element, pseudoElement) {
      return { element, pseudoElement };
    };
    window.matchMedia = function (query) {
      return { query, matches: true };
    };
    window.structuredClone = function (value, options) {
      return { value, options };
    };
    window.__domTest = window.__domTest || {};
    window.__domTest.rawWindow = window;"))

(define (install-document-fixtures!)
  (js-eval
   "document.body.innerHTML = '';
    const host = document.createElement('div');
    host.id = 'dom-host';
    host.setAttribute('data-state', 'ready');
    const leaf = document.createElement('span');
    leaf.className = 'leaf';
    leaf.textContent = 'leaf';
    host.appendChild(leaf);
    document.body.appendChild(host);
    document.__domTest = { host, leaf };"))

(define (install-canvas-fixtures!)
  (js-eval
   "window.__domTest = window.__domTest || {};
    const ctx = {
      canvas: null,
      direction: 'ltr',
      fillStyle: 'red',
      strokeStyle: 'blue',
      calls: [],
      fillRect(...args) { this.calls.push(['fillRect', ...args]); },
      strokeRect(...args) { this.calls.push(['strokeRect', ...args]); },
      beginPath() { this.calls.push(['beginPath']); },
      closePath() { this.calls.push(['closePath']); },
      moveTo(...args) { this.calls.push(['moveTo', ...args]); },
      lineTo(...args) { this.calls.push(['lineTo', ...args]); },
      arc(...args) { this.calls.push(['arc', ...args]); },
      clearRect(...args) { this.calls.push(['clearRect', ...args]); },
      fill(path, rule) { this.calls.push(['fill', path, rule]); },
      stroke(path) { this.calls.push(['stroke', path]); },
      save() { this.calls.push(['save']); },
      restore() { this.calls.push(['restore']); },
      translate(...args) { this.calls.push(['translate', ...args]); },
      scale(...args) { this.calls.push(['scale', ...args]); },
      rotate(angle) { this.calls.push(['rotate', angle]); },
      fillText(text, x, y, maxWidth) { this.calls.push(['fillText', text, x, y, maxWidth]); },
      strokeText(text, x, y, maxWidth) { this.calls.push(['strokeText', text, x, y, maxWidth]); },
      measureText(text) { this.calls.push(['measureText', text]); return { width: text.length }; },
      createLinearGradient(x0, y0, x1, y1) { this.calls.push(['createLinearGradient', x0, y0, x1, y1]); return { kind: 'linear-gradient' }; },
      createRadialGradient(x0, y0, r0, x1, y1, r1) { this.calls.push(['createRadialGradient', x0, y0, r0, x1, y1, r1]); return { kind: 'radial-gradient' }; }
    };
    const canvas = {
      width: 320,
      height: 200,
      calls: [],
      ctx,
      captureStream(frameRate) {
        this.calls.push(['captureStream', frameRate]);
        return { kind: 'stream', frameRate };
      },
      getContext(contextId, options) {
        this.calls.push(['getContext', contextId, options]);
        return this.ctx;
      },
      toDataURL(type, quality) {
        this.calls.push(['toDataURL', type, quality]);
        return `${type}:${quality}`;
      },
      toBlob(callback, type, quality) {
        this.calls.push(['toBlob', type, quality]);
        callback({ kind: 'blob', type, quality });
      },
      transferControlToOffscreen() {
        this.calls.push(['transferControlToOffscreen']);
        return { kind: 'offscreen' };
      }
    };
    ctx.canvas = canvas;
    window.__domTest.canvas = canvas;
    window.__domTest.ctx = ctx;"))

(define (install-media-image-fixtures!)
  (js-eval
   "window.__domTest = window.__domTest || {};
    const media = {
      currentTime: 1.5,
      volume: 0.25,
      muted: 1,
      defaultMuted: 0,
      defaultPlaybackRate: 1.25,
      playbackRate: 0.75,
      controls: 1,
      loop: 0,
      preload: 'metadata',
      src: 'song.ogg',
      currentSrc: 'resolved.ogg',
      decoding: 'async',
      loading: 'lazy',
      complete: 1,
      crossOrigin: 'anonymous',
      calls: [],
      play() {
        this.calls.push(['play']);
        return { kind: 'play-promise' };
      },
      pause() {
        this.calls.push(['pause']);
      },
      load() {
        this.calls.push(['load']);
      },
      fastSeek(t) {
        this.calls.push(['fastSeek', t]);
      },
      canPlayType(type) {
        this.calls.push(['canPlayType', type]);
        return 'maybe';
      },
      setSinkId(id) {
        this.calls.push(['setSinkId', id]);
        return { kind: 'sink', id };
      }
    };
    const image = {
      alt: 'fallback',
      src: 'image.png',
      width: 10,
      height: 20,
      currentSrc: 'resolved.png',
      decoding: 'async',
      loading: 'lazy',
      complete: 1,
      crossOrigin: 'anonymous'
    };
    globalThis.Image = class Image {
      constructor(width, height) {
        this.width = width;
        this.height = height;
        this.alt = '';
        this.src = '';
        this.calls = [];
      }
    };
    globalThis.HTMLImageElement = globalThis.Image;
    window.__domTest.media = media;
    window.__domTest.image = image;"))

(define (install-event-fixtures!)
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
      preventDefault() {
        this.defaultPrevented = true;
      }
      stopPropagation() {
        this.cancelBubble = true;
      }
      stopImmediatePropagation() {
        this.immediateStopped = true;
      }
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
    class EventTarget {
      constructor() {
        this.listeners = [];
      }
      addEventListener(type, listener, options) {
        this.listeners.push(['add', type, listener, options]);
      }
      removeEventListener(type, listener, options) {
        this.listeners.push(['remove', type, listener, options]);
      }
      dispatchEvent(evt) {
        evt.currentTarget = this;
        for (const entry of this.listeners) {
          if (entry[1] === evt.type && entry[0] === 'add') {
            entry[2](evt);
          }
        }
      }
    }
    globalThis.Event = BaseEvent;
    globalThis.MouseEvent = MouseEvent;
    globalThis.KeyboardEvent = KeyboardEvent;
    globalThis.EventTarget = EventTarget;
    window.__domTest = window.__domTest || {};
    window.__domTest.eventTarget = new EventTarget();
    window.__domTest.event = new BaseEvent('submit', { target: { kind: 'form' }, currentTarget: { kind: 'listener' } });
    window.__domTest.mouseEvent = new MouseEvent('click', {
      target: { kind: 'button' },
      currentTarget: { kind: 'listener' },
      altKey: true,
      ctrlKey: false,
      metaKey: true,
      shiftKey: false,
      clientX: 12,
      clientY: 34,
      pageX: 56,
      pageY: 78,
      screenX: 90,
      screenY: 91,
      button: 1,
      buttons: 2
    });
    window.__domTest.keyboardEvent = new KeyboardEvent('keydown', {
      target: { kind: 'input' },
      currentTarget: { kind: 'listener' },
      key: 'Escape',
      code: 'Escape',
      altKey: true,
      ctrlKey: true,
      metaKey: false,
      shiftKey: true,
      repeat: true
    });"))

(define (install!)
  (install-window-fixtures!)
  (install-document-fixtures!)
  (install-canvas-fixtures!)
  (install-media-image-fixtures!)
  (install-event-fixtures!))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
 (list "Raw DOM aggregate"
       (let ()
         (install!)
         (check-equal (js-window-name) "dom-suite" "raw window name")
         (check-true (external? (js-window-window)) "raw window object")
         (check-true (external? (js-document)) "raw document object")
         (check-equal (js-canvas-width (dom-test-fixture "canvas")) 320 "raw canvas width")
         (check-equal (js-media-current-time (dom-test-fixture "media")) 1.5 "raw media time")
         (check-equal (js-image-alt (dom-test-fixture "image")) "fallback" "raw image alt")
         #t))
 (list "Window wrappers"
       (let ()
         (install!)
         (define timeout-fired 0)
         (check-equal (window-name) "dom-suite" "window name")
         (window-set-name! "web-bracket")
         (check-equal (window-name) "web-bracket" "window set name")
         (check-true (window-confirm "proceed?") "window confirm")
         (check-equal (js-ref (js-var "window") "lastConfirm") "proceed?" "window confirm arg")
         (define opened (window-open "https://example.invalid" "tab" "noopener" #f))
         (check-equal (js-ref (js-var "window") "lastOpen")
                      (vector "https://example.invalid" "tab" "noopener" #f)
                      "window open args")
         (check-true (external? opened) "window open result")
         (check-equal (js-ref opened "kind") "popup" "window open payload")
         (check-equal
          (window-set-timeout/delay
           (procedure->external (lambda ()
                                  (set! timeout-fired (add1 timeout-fired))))
           10)
          17
          "window timeout handle")
         (check-equal timeout-fired 1 "window timeout callback")
         (check-equal (vector-ref (js-ref (js-var "window") "lastTimeout") 1) 10 "window timeout recorded")
         (check-equal (window-request-idle-callback (procedure->external (lambda (_deadline) (void)))
                                                    (js-eval "({ timeout: 1 })"))
                      31
                      "window idle handle")
         (check-true (expect-contract-error (lambda () (window-set-name! 1))) "window name validation")
         (check-true (expect-contract-error (lambda () (window-open 1))) "window open validation")
         (check-true (expect-contract-error (lambda () (window-confirm 1))) "window confirm validation")
         #t))
 (list "Document and element wrappers"
       (let ()
         (install!)
         (define body (document-body))
         (define host (document-create-element "div"))
         (define leaf (document-create-text-node "leaf"))
         (set-attribute! host "id" "dom-host")
         (set-attribute! host "data-state" "ready")
         (append-child! host leaf)
         (append-child! body host)
         (check-equal (get-attribute host "id") "dom-host" "element attribute")
         (check-equal (get-attribute host "data-state") "ready" "element attribute 2")
         (check-equal (get-attribute (document-get-element-by-id "dom-host") "data-state")
                      "ready"
                      "document lookup")
         (check-equal (get-attribute (document-query-selector "#dom-host") "id")
                      "dom-host"
                      "document query selector")
         (check-equal (js-ref (document-query-selector-all "#dom-host") "length") 1 "document query selector all")
         (check-true (toggle-attribute! host "data-flag") "element toggle on")
         (check-false (toggle-attribute! host "data-flag") "element toggle off")
         (replace-children! host (document-create-text-node "updated"))
         (check-equal (js-ref host "textContent") "updated" "element replace-children")
         (check-true (expect-contract-error (lambda () (document-create-element 1))) "document create-element validation")
         (check-true (expect-contract-error (lambda () (set-attribute! host 1 "value"))) "element set-attribute validation")
         #t))
 (list "Canvas and DOMRect wrappers"
       (let ()
         (install!)
         (define canvas (dom-test-fixture "canvas"))
         (define options (js-eval "({ alpha: false })"))
         (define ctx (canvas-get-context canvas "2d" options))
         (check-equal (canvas-width canvas) 320 "canvas width")
         (canvas-set-width! canvas 640)
         (check-equal (canvas-width canvas) 640 "canvas width set")
         (check-equal (canvas-height canvas) 200 "canvas height")
         (canvas-set-height! canvas 240)
         (check-equal (canvas-height canvas) 240 "canvas height set")
         (check-equal (js-ref (canvas-capture-stream canvas 30.0) "kind")
                      "stream"
                      "canvas capture stream")
         (check-equal (canvas-to-data-url canvas "image/jpeg" 0.8) "image/jpeg:0.8" "canvas to data url")
         (define blobs '())
         (canvas-to-blob canvas
                         (procedure->external (lambda (blob)
                                                (set! blobs (cons blob blobs))))
                         "image/png"
                         0.5)
         (check-equal (vector-length (js-ref canvas "calls")) 4 "canvas call count")
         (check-equal (vector-ref (js-ref canvas "calls") 0) (vector "getContext" "2d" options) "canvas get-context")
         (check-equal (js-ref ctx "direction") "ltr" "canvas context direction")
         (canvas-2d-set-direction! ctx "rtl")
         (check-equal (canvas-2d-direction ctx) "rtl" "canvas context direction set")
         (canvas-2d-set-fill-style! ctx "green")
         (check-equal (canvas-2d-fill-style ctx) "green" "canvas fill style")
         (canvas-2d-fill-rect ctx 1 2 3 4)
         (canvas-2d-stroke-rect ctx 5 6 7 8)
         (canvas-2d-begin-path ctx)
         (canvas-2d-close-path ctx)
         (canvas-2d-move-to ctx 9 10)
         (canvas-2d-line-to ctx 11 12)
         (canvas-2d-arc ctx 13 14 15 16 17)
         (canvas-2d-clear-rect ctx 18 19 20 21)
         (canvas-2d-fill ctx "path" "nonzero")
         (canvas-2d-stroke ctx "path")
         (canvas-2d-save ctx)
         (canvas-2d-restore ctx)
         (canvas-2d-translate ctx 22 23)
         (canvas-2d-scale ctx 24 25)
         (canvas-2d-rotate ctx 26)
         (canvas-2d-fill-text ctx "text" 27 28 29)
         (canvas-2d-stroke-text ctx "text" 30 31 32)
         (check-equal (js-ref (canvas-2d-measure-text ctx "measure") "width")
                      7
                      "canvas measure text")
         (check-equal (js-ref ctx "calls")
                      (vector (vector "fillRect" 1 2 3 4)
                              (vector "strokeRect" 5 6 7 8)
                              (vector "beginPath")
                              (vector "closePath")
                              (vector "moveTo" 9 10)
                              (vector "lineTo" 11 12)
                              (vector "arc" 13 14 15 16 17)
                              (vector "clearRect" 18 19 20 21)
                              (vector "fill" "path" "nonzero")
                              (vector "stroke" "path")
                              (vector "save")
                              (vector "restore")
                              (vector "translate" 22 23)
                              (vector "scale" 24 25)
                              (vector "rotate" 26)
                              (vector "fillText" "text" 27 28 29)
                              (vector "strokeText" "text" 30 31 32)
                              (vector "measureText" "measure"))
                      "canvas context calls")
         (check-equal (dom-rect-left (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })"))
                      1.25
                      "domrect left")
         (check-equal (dom-rect-top (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })"))
                      2.5
                      "domrect top")
         (check-equal (dom-rect-width (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })"))
                      3.75
                      "domrect width")
         (check-equal (dom-rect-height (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })"))
                      4.5
                      "domrect height")
         (check-true (expect-contract-error (lambda () (canvas-get-context canvas 1))) "canvas get-context validation")
         (check-true (expect-contract-error (lambda () (canvas-to-data-url canvas 1))) "canvas to-data-url validation")
         #t))
 (list "Media and image wrappers"
       (let ()
         (install!)
         (define media (dom-test-fixture "media"))
         (define image (dom-test-fixture "image"))
         (check-equal (media-current-time media) 1.5 "media current time")
         (media-set-current-time! media 2.75)
         (check-equal (media-current-time media) 2.75 "media current time set")
         (check-equal (media-volume media) 0.25 "media volume")
         (media-set-volume! media 0.5)
         (check-equal (media-volume media) 0.5 "media volume set")
         (check-true (media-muted media) "media muted")
         (media-set-muted! media #f)
         (check-false (media-muted media) "media muted set")
         (check-false (media-default-muted media) "media default muted")
         (media-set-default-muted! media #t)
         (check-true (media-default-muted media) "media default muted set")
         (check-equal (media-default-playback-rate media) 1.25 "media default playback rate")
         (media-set-default-playback-rate! media 1.5)
         (check-equal (media-default-playback-rate media) 1.5 "media default playback rate set")
         (check-equal (media-playback-rate media) 0.75 "media playback rate")
         (media-set-playback-rate! media 0.9)
         (check-equal (media-playback-rate media) 0.9 "media playback rate set")
         (check-true (media-controls? media) "media controls")
         (media-set-controls! media #f)
         (check-false (media-controls? media) "media controls set")
         (check-false (media-loop? media) "media loop")
         (media-set-loop! media #t)
         (check-true (media-loop? media) "media loop set")
         (check-equal (media-preload media) "metadata" "media preload")
         (media-set-preload! media "auto")
         (check-equal (media-preload media) "auto" "media preload set")
         (check-equal (media-src media) "song.ogg" "media src")
         (media-set-src! media "track.ogg")
         (check-equal (media-src media) "track.ogg" "media src set")
         (check-equal (media-can-play-type media "audio/ogg") "maybe" "media can-play-type")
         (check-equal (js-ref (media-play media) "kind") "play-promise" "media play")
         (media-pause media)
         (media-load! media)
         (media-fast-seek! media 42.0)
         (check-equal (js-ref (media-set-sink-id! media "speaker-1") "id") "speaker-1" "media set sink id")
         (check-equal (js-ref media "calls")
                      (vector (vector "play")
                              (vector "pause")
                              (vector "load")
                              (vector "fastSeek" 42.0)
                              (vector "canPlayType" "audio/ogg")
                              (vector "setSinkId" "speaker-1"))
                      "media calls")
         (check-equal (image-alt image) "fallback" "image alt")
         (image-set-alt! image "decorative")
         (check-equal (image-alt image) "decorative" "image alt set")
         (check-equal (image-src image) "image.png" "image src")
         (image-set-src! image "icon.png")
         (check-equal (image-src image) "icon.png" "image src set")
         (check-equal (image-width image) 10 "image width")
         (image-set-width! image 11)
         (check-equal (image-width image) 11 "image width set")
         (check-equal (image-height image) 20 "image height")
         (image-set-height! image 21)
         (check-equal (image-height image) 21 "image height set")
         (check-equal (image-current-src image) "resolved.png" "image current src")
         (check-equal (image-decoding image) "async" "image decoding")
         (image-set-decoding! image "sync")
         (check-equal (image-decoding image) "sync" "image decoding set")
         (check-equal (image-loading image) "lazy" "image loading")
         (image-set-loading! image "eager")
         (check-equal (image-loading image) "eager" "image loading set")
         (check-true (image-complete? image) "image complete")
         (check-equal (image-cross-origin image) "anonymous" "image cross origin")
         (image-set-cross-origin! image "use-credentials")
         (check-equal (image-cross-origin image) "use-credentials" "image cross origin set")
         (define img2 (image-new 64 48))
         (check-equal (image-width img2) 64 "image new width")
         (check-equal (image-height img2) 48 "image new height")
         (check-true (expect-contract-error (lambda () (image-set-alt! image 1))) "image alt validation")
         (check-true (expect-contract-error (lambda () (media-set-src! media 1))) "media src validation")
         #t))
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
         (check-equal (event-target evt) (js-ref evt "target") "event target")
         (check-equal (event-current-target evt) (js-ref evt "currentTarget") "event current target")
         (prevent-default! evt)
         (stop-propagation! evt)
         (stop-immediate-propagation! evt)
         (check-true (js-ref evt "defaultPrevented") "event prevent default")
         (check-true (js-ref evt "cancelBubble") "event stop propagation")
         (check-true (js-ref evt "immediateStopped") "event stop immediate propagation")
         (check-equal (mouse-event-offset-x mouse) 4 "mouse offset x")
         (check-equal (mouse-event-offset-y mouse) 5 "mouse offset y")
         (check-equal (mouse-event-client-x mouse) 12 "mouse client x")
         (check-equal (mouse-event-client-y mouse) 34 "mouse client y")
         (check-equal (mouse-event-page-x mouse) 56 "mouse page x")
         (check-equal (mouse-event-page-y mouse) 78 "mouse page y")
         (check-equal (mouse-event-screen-x mouse) 90 "mouse screen x")
         (check-equal (mouse-event-screen-y mouse) 91 "mouse screen y")
         (check-equal (mouse-event-button mouse) 1 "mouse button")
         (check-equal (mouse-event-buttons mouse) 2 "mouse buttons")
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
