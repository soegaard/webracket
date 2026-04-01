;;;
;;; dom.ffi
;;;

;; Focused tests for the Canvas, DOMRect, and Media wrapper libraries.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-canvas-media-image.rkt

(include-lib canvas)
(include-lib domrect)
(include-lib media)
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
      measureText(text) { this.calls.push(['measureText', text]); return { width: text.length }; }
    };
    const canvas = {
      width: 320,
      height: 200,
      calls: [],
      ctx,
      captureStream(frameRate) { this.calls.push(['captureStream', frameRate]); return { kind: 'stream', frameRate }; },
      getContext(contextId, options) { this.calls.push(['getContext', contextId, options]); return this.ctx; },
      toDataURL(type, quality) { this.calls.push(['toDataURL', type, quality]); return `${type}:${quality}`; },
      toBlob(callback, type, quality) { this.calls.push(['toBlob', type, quality]); callback({ kind: 'blob', type, quality }); }
    };
    ctx.canvas = canvas;
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
      calls: [],
      play() { this.calls.push(['play']); return { kind: 'play-promise' }; },
      pause() { this.calls.push(['pause']); },
      load() { this.calls.push(['load']); },
      fastSeek(t) { this.calls.push(['fastSeek', t]); },
      canPlayType(type) { this.calls.push(['canPlayType', type]); return 'maybe'; },
      setSinkId(id) { this.calls.push(['setSinkId', id]); return { kind: 'sink', id }; }
    };
    window.__domTest.canvas = canvas;
    window.__domTest.ctx = ctx;
    window.__domTest.media = media;"))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
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
         (check-equal (js-ref (canvas-capture-stream canvas 30.0) "kind") "stream" "canvas capture stream")
         (check-equal (canvas-to-data-url canvas "image/jpeg" 0.8) "image/jpeg:0.8" "canvas to data url")
         (define blobs '())
         (canvas-to-blob canvas
                         (procedure->external (lambda (blob) (set! blobs (cons blob blobs))))
                         "image/png"
                         0.5)
         (check-equal (vector-length (js-ref canvas "calls")) 4 "canvas call count")
         (define get-context-call (vector-ref (js-ref canvas "calls") 0))
         (check-equal (vector-ref get-context-call 0) "getContext" "canvas get-context method")
         (check-equal (vector-ref get-context-call 1) "2d" "canvas get-context id")
         (check-false (js-ref (vector-ref get-context-call 2) "alpha") "canvas get-context options")
         (check-equal (js-ref ctx "direction") "ltr" "canvas context direction")
         (canvas-2d-set-direction! ctx "rtl")
         (check-equal (canvas-2d-direction ctx) "rtl" "canvas context direction set")
         (canvas-2d-set-fill-style! ctx "green")
         (check-equal (js-ref (canvas-2d-measure-text ctx "measure") "width") 7 "canvas measure text")
         (check-equal (dom-rect-left (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })")) 1.25 "domrect left")
         (check-equal (dom-rect-top (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })")) 2.5 "domrect top")
         (check-equal (dom-rect-width (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })")) 3.75 "domrect width")
         (check-equal (dom-rect-height (js-eval "({ left: 1.25, top: 2.5, width: 3.75, height: 4.5 })")) 4.5 "domrect height")
         (check-true (expect-contract-error (lambda () (canvas-get-context canvas 1))) "canvas get-context validation")
         (check-true (expect-contract-error (lambda () (canvas-to-data-url canvas 1))) "canvas to-data-url validation")
         #t))
 (list "Media and image wrappers"
       (let ()
         (install!)
         (define media (dom-test-fixture "media"))
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
                      (vector (vector "canPlayType" "audio/ogg")
                              (vector "play")
                              (vector "pause")
                              (vector "load")
                              (vector "fastSeek" 42)
                              (vector "setSinkId" "speaker-1"))
                      "media calls")
         (check-true (expect-contract-error (lambda () (media-set-src! media 1))) "media src validation")
         #t)))
