;;;
;;; dom.ffi
;;;

;; Focused tests for the Canvas, DOMRect, and Media wrapper libraries.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-canvas-media-image.rkt

(include-lib canvas)
(include-lib domrect)
(include-lib element)
(include-lib image)
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
    const linearGradient = {
      kind: 'linear-gradient',
      calls: [],
      addColorStop(offset, color) { this.calls.push(['addColorStop', offset, color]); }
    };
    const radialGradient = {
      kind: 'radial-gradient',
      calls: [],
      addColorStop(offset, color) { this.calls.push(['addColorStop', offset, color]); }
    };
    const conicGradient = {
      kind: 'conic-gradient',
      calls: [],
      addColorStop(offset, color) { this.calls.push(['addColorStop', offset, color]); }
    };
    const pattern = {
      kind: 'pattern',
      calls: [],
      setTransform(matrix) { this.calls.push(['setTransform', matrix]); }
    };
    const transform = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 };
    const imageData = {
      kind: 'image-data',
      width: 4,
      height: 1,
      data: new Uint8ClampedArray([1, 2, 3, 4])
    };
    const offscreen = { kind: 'offscreen', width: 320, height: 200 };
    const ctx = {
      canvas: null,
      filter: 'none',
      font: '16px sans-serif',
      globalAlpha: 0.5,
      globalCompositeOperation: 'source-over',
      imageSmoothingEnabled: 1,
      imageSmoothingQuality: 'low',
      direction: 'ltr',
      lineCap: 'butt',
      lineDashOffset: 2,
      lineJoin: 'miter',
      lineWidth: 1,
      miterLimit: 10,
      shadowBlur: 0,
      shadowColor: '#000000',
      shadowOffsetX: 0,
      shadowOffsetY: 0,
      fillStyle: 'red',
      strokeStyle: 'blue',
      textAlign: 'start',
      textBaseline: 'alphabetic',
      textRendering: 'auto',
      fontKerning: 'auto',
      fontStretch: 'normal',
      fontVariantCaps: 'normal',
      fontVariantLigatures: 'normal',
      fontVariantNumeric: 'normal',
      letterSpacing: '0px',
      wordSpacing: '0px',
      calls: [],
      fillRect(...args) { this.calls.push(['fillRect', ...args]); },
      strokeRect(...args) { this.calls.push(['strokeRect', ...args]); },
      beginPath() { this.calls.push(['beginPath']); },
      closePath() { this.calls.push(['closePath']); },
      moveTo(...args) { this.calls.push(['moveTo', ...args]); },
      lineTo(...args) { this.calls.push(['lineTo', ...args]); },
      arc(...args) { this.calls.push(['arc', ...args]); },
      arcTo(...args) { this.calls.push(['arcTo', ...args]); },
      bezierCurveTo(...args) { this.calls.push(['bezierCurveTo', ...args]); },
      clearRect(...args) { this.calls.push(['clearRect', ...args]); },
      clip(...args) { this.calls.push(['clip', ...args]); },
      fill(path, rule) {
        if (arguments.length === 1) {
          this.calls.push(['fill', null, path]);
        } else {
          this.calls.push(['fill', path, rule]);
        }
      },
      stroke(path) { this.calls.push(['stroke', path]); },
      save() { this.calls.push(['save']); },
      restore() { this.calls.push(['restore']); },
      translate(...args) { this.calls.push(['translate', ...args]); },
      scale(...args) { this.calls.push(['scale', ...args]); },
      rotate(angle) { this.calls.push(['rotate', angle]); },
      setLineDash(segments) { this.calls.push(['setLineDash', segments]); },
      getLineDash() { this.calls.push(['getLineDash']); return [4, 2]; },
      reset() { this.calls.push(['reset']); },
      resetTransform() { this.calls.push(['resetTransform']); },
      roundRect(...args) { this.calls.push(['roundRect', ...args]); },
      rect(...args) { this.calls.push(['rect', ...args]); },
      quadraticCurveTo(...args) { this.calls.push(['quadraticCurveTo', ...args]); },
      transform(...args) { this.calls.push(['transform', ...args]); },
      setTransform(...args) { this.calls.push(['setTransform', ...args]); },
      getTransform() { this.calls.push(['getTransform']); return transform; },
      fillText(text, x, y, maxWidth) { this.calls.push(['fillText', text, x, y, maxWidth]); },
      strokeText(text, x, y, maxWidth) { this.calls.push(['strokeText', text, x, y, maxWidth]); },
      measureText(text) { this.calls.push(['measureText', text]); return { width: text.length }; },
      createLinearGradient(...args) { this.calls.push(['createLinearGradient', ...args]); return linearGradient; },
      createRadialGradient(...args) { this.calls.push(['createRadialGradient', ...args]); return radialGradient; },
      createConicGradient(...args) { this.calls.push(['createConicGradient', ...args]); return conicGradient; },
      createPattern(source, repetition) { this.calls.push(['createPattern', source, repetition]); return pattern; },
      createImageData(...args) {
        this.calls.push(['createImageData', ...args]);
        if (args.length === 1)
          return { kind: 'image-data-copy', width: args[0].width, height: args[0].height, data: new Uint8ClampedArray(args[0].data) };
        return { kind: 'image-data', width: args[0], height: args[1], data: new Uint8ClampedArray(args[0] * args[1] * 4) };
      },
      getImageData(...args) { this.calls.push(['getImageData', ...args]); return imageData; },
      putImageData(...args) { this.calls.push(['putImageData', ...args]); },
      drawFocusIfNeeded(...args) { this.calls.push(['drawFocusIfNeeded', ...args]); },
      drawImage(...args) { this.calls.push(['drawImage', ...args]); },
      ellipse(...args) { this.calls.push(['ellipse', ...args]); },
      isPointInPath(...args) { this.calls.push(['isPointInPath', ...args]); return 1; },
      isPointInStroke(...args) { this.calls.push(['isPointInStroke', ...args]); return 0; }
    };
    const canvas = {
      width: 320,
      height: 200,
      calls: [],
      ctx,
      captureStream(frameRate) { this.calls.push(['captureStream', frameRate]); return { kind: 'stream', frameRate, getTracks() { return []; } }; },
      getContext(contextId, options) { this.calls.push(['getContext', contextId, options]); return this.ctx; },
      transferControlToOffscreen() { this.calls.push(['transferControlToOffscreen']); return offscreen; },
      toDataURL(type, quality) { this.calls.push(['toDataURL', type, quality]); return `${type}:${quality}`; },
      toBlob(callback, type, quality) { this.calls.push(['toBlob', type, quality]); callback({ kind: 'blob', type, quality }); }
    };
    ctx.canvas = canvas;
    const controlsList = {
      value: 'nodownload',
      length: 1,
      contains(token) { return token === 'nodownload'; }
    };
    const audioTrackList = {
      length: 2,
      item(index) {
        if (index === 0) return { kind: 'audio', id: 'audio-0', label: 'Audio 0', language: 'en', enabled: true };
        if (index === 1) return { kind: 'audio', id: 'audio-1', label: 'Audio 1', language: 'fr', enabled: false };
        return null;
      }
    };
    const textTrackList = {
      length: 1,
      item(index) {
        if (index === 0) return { kind: 'subtitles', id: 'text-0', label: 'Captions', language: 'en', mode: 'disabled' };
        return null;
      }
    };
    const videoTrackList = {
      length: 1,
      item(index) {
        if (index === 0) return { kind: 'main', id: 'video-0', label: 'Main', language: 'en', selected: true };
        return null;
      }
    };
    const buffered = {
      length: 2,
      start(index) { return index === 0 ? 0.5 : 4.0; },
      end(index) { return index === 0 ? 1.25 : 7.5; }
    };
    const mediaError = {
      code: 3,
      message: 'media failure'
    };
    const mediaStream = {
      kind: 'stream',
      frameRate: null,
      getTracks() { return []; }
    };
    const mediaSource = {
      kind: 'source'
    };
    const mediaKeys = {
      kind: 'keys'
    };
    const media = {
      autoplay: 0,
      crossOrigin: 'anonymous',
      currentTime: 1.5,
      duration: 3.25,
      currentSrc: 'resolved.ogg',
      disableRemotePlayback: 0,
      ended: 0,
      paused: 1,
      seeking: 0,
      preservesPitch: 1,
      volume: 0.25,
      muted: 1,
      defaultMuted: 0,
      defaultPlaybackRate: 1.25,
      playbackRate: 0.75,
      controls: 1,
      loop: 0,
      preload: 'metadata',
      mediaGroup: 'group-a',
      src: 'song.ogg',
      currentSrc: 'resolved.ogg',
      networkState: 1,
      readyState: 1,
      audioTracks: audioTrackList,
      buffered,
      controlsList,
      error: mediaError,
      mediaKeys,
      played: buffered,
      calls: [],
      play() { this.calls.push(['play']); return { kind: 'play-promise' }; },
      pause() { this.calls.push(['pause']); },
      load() { this.calls.push(['load']); },
      fastSeek(t) { this.calls.push(['fastSeek', t]); },
      canPlayType(type) { this.calls.push(['canPlayType', type]); return 'maybe'; },
      seekable: buffered,
      sinkId: 'default',
      srcObject: mediaSource,
      textTracks: textTrackList,
      videoTracks: videoTrackList,
      captureStream(frameRate) { this.calls.push(['captureStream', frameRate]); return mediaStream; },
      addTextTrack(kind, label, language) {
        this.calls.push(['addTextTrack', kind, label, language]);
        return { kind, label, language, id: 'added-0', mode: 'hidden' };
      },
      setMediaKeys(keys) { this.calls.push(['setMediaKeys', keys]); return { kind: 'setMediaKeys-promise' }; },
      setSinkId(id) { this.calls.push(['setSinkId', id]); return { kind: 'sink', id }; }
    };
    const image = {
      alt: 'Alpha',
      src: 'photo.png',
      width: 64,
      height: 32,
      currentSrc: 'photo.png',
      decoding: 'auto',
      loading: 'eager',
      crossOrigin: 'anonymous',
      complete: 1
    };
    window.__domTest.canvas = canvas;
    window.__domTest.ctx = ctx;
    window.__domTest.image = image;
    window.__domTest.media = media;
    window.__domTest.mediaStream = mediaStream;"))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
 (list "Canvas and DOMRect wrappers"
       (let ()
         (install!)
         (js-eval "console.log('canvas/media/image browser test running')")
         (define canvas (canvas-wrap (dom-test-fixture "canvas")))
         (define options (js-eval "({ alpha: false })"))
         (define ctx (canvas-get-context canvas '2d options))
         (define captured (canvas-capture-stream canvas 30.0))
         (check-equal (canvas-width canvas) 320 "canvas width")
         (canvas-set-width! canvas 640)
         (check-equal (canvas-width canvas) 640 "canvas width set")
         (check-equal (canvas-height canvas) 200 "canvas height")
         (canvas-set-height! canvas 240)
         (check-equal (canvas-height canvas) 240 "canvas height set")
         (check-true (media-stream? captured) "canvas capture stream")
         (check-equal (js-ref (media-stream-raw captured) "kind") "stream" "canvas capture stream raw")
         (define offscreen (canvas-transfer-control-to-offscreen canvas))
         (check-true (offscreen-canvas? offscreen) "canvas offscreen wrapper")
         (check-equal (js-ref (offscreen-canvas-raw offscreen) "kind") "offscreen" "canvas offscreen raw")
         (check-equal (canvas-to-data-url canvas 'image/jpeg 0.8) "image/jpeg:0.8" "canvas to data url")
         (define blobs '())
         (canvas-to-blob canvas
                         (procedure->external (lambda (blob) (set! blobs (cons blob blobs))))
                         "image/png"
                         0.5)
         (check-equal (vector-length (js-ref (canvas-raw canvas) "calls")) 5 "canvas call count")
         (define get-context-call (vector-ref (js-ref (canvas-raw canvas) "calls") 0))
         (check-equal (vector-ref get-context-call 0) "getContext" "canvas get-context method")
         (check-equal (vector-ref get-context-call 1) "2d" "canvas get-context id")
         (check-false (js-ref (vector-ref get-context-call 2) "alpha") "canvas get-context options")
         (check-true (canvas-2d-context? ctx) "canvas context wrapper")
         (check-equal (js-ref (canvas-2d-context-raw ctx) "direction") "ltr" "canvas context direction")
         (canvas-2d-set-direction! ctx 'rtl)
         (check-equal (canvas-2d-direction ctx) "rtl" "canvas context direction set")
         (canvas-2d-set-fill-style! ctx "green")
         (canvas-2d-fill ctx #f 'evenodd)
         (define fill-call (vector-ref (js-ref (canvas-2d-context-raw ctx) "calls") 0))
         (check-equal (vector-ref fill-call 0) "fill" "canvas fill call")
         (check-equal (vector-ref fill-call 2) "evenodd" "canvas fill rule")
         (define dash (canvas-2d-get-line-dash ctx))
         (check-equal (vector->list dash) '(4 2) "canvas get line dash")
         (canvas-2d-set-line-dash ctx (vector 1 3 5))
         (define (canvas-call-recorded? name)
           (let ([calls (js-ref (canvas-2d-context-raw ctx) "calls")])
             (let loop ([i 0])
               (cond
                 [(>= i (vector-length calls)) #f]
                 [(equal? (vector-ref (vector-ref calls i) 0) name) #t]
                 [else (loop (add1 i))]))))
         (check-true (canvas-call-recorded? "setLineDash") "canvas set line dash")
         (define linear-gradient (canvas-2d-create-linear-gradient ctx 0 0 10 10))
         (check-true (canvas-gradient? linear-gradient) "canvas linear gradient wrapper")
         (canvas-gradient-add-color-stop! linear-gradient 0 'red)
         (check-equal (vector-ref (vector-ref (js-ref (canvas-gradient-raw linear-gradient) "calls") 0) 0)
                      "addColorStop"
                      "canvas gradient add color stop")
         (define pattern (canvas-2d-create-pattern ctx canvas "repeat"))
         (check-true (canvas-pattern? pattern) "canvas pattern wrapper")
         (canvas-pattern-set-transform! pattern (canvas-2d-get-transform ctx))
         (check-equal (vector-ref (vector-ref (js-ref (canvas-pattern-raw pattern) "calls") 0) 0)
                      "setTransform"
                      "canvas pattern transform")
         (define image-data (canvas-2d-create-image-data ctx 2 3))
         (check-true (canvas-image-data? image-data) "canvas image data wrapper")
         (check-equal (canvas-image-data-width image-data) 2 "canvas image data width")
         (check-equal (canvas-image-data-height image-data) 3 "canvas image data height")
         (check-equal (bytes-length (canvas-image-data-data image-data)) 24 "canvas image data bytes")
         (define copied-image-data (canvas-2d-create-image-data-from ctx image-data))
         (check-true (canvas-image-data? copied-image-data) "canvas image data copy wrapper")
         (check-equal (canvas-image-data-width copied-image-data) 2 "canvas image data copy width")
         (check-equal (canvas-image-data-height copied-image-data) 3 "canvas image data copy height")
         (check-equal (bytes-length (canvas-image-data-data copied-image-data)) 24 "canvas image data copy bytes")
         (define image-data-from-canvas (canvas-2d-get-image-data ctx 1 2 3 4))
         (check-true (canvas-image-data? image-data-from-canvas) "canvas get image data wrapper")
         (check-equal (canvas-image-data-width image-data-from-canvas) 4 "canvas get image data width")
         (check-equal (canvas-image-data-height image-data-from-canvas) 1 "canvas get image data height")
         (check-equal (bytes-length (canvas-image-data-data image-data-from-canvas)) 4 "canvas get image data bytes")
         (define matrix (canvas-2d-get-transform ctx))
         (check-true (canvas-dom-matrix? matrix) "canvas transform wrapper")
         (check-equal (canvas-dom-matrix-a matrix) 1 "canvas matrix a")
         (check-equal (canvas-dom-matrix-f matrix) 6 "canvas matrix f")
         (define measure-text (canvas-2d-measure-text ctx "measure"))
         (check-true (canvas-text-metrics? measure-text) "canvas measure text wrapper")
         (check-equal (canvas-text-metrics-width measure-text) 7 "canvas measure text")
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
         (define img (dom-test-fixture "image"))
         (check-false (media-autoplay? media) "media autoplay")
         (media-set-autoplay! media #t)
         (check-true (media-autoplay? media) "media autoplay set")
         (check-equal (media-current-src media) "resolved.ogg" "media current src")
         (check-equal (media-cross-origin media) "anonymous" "media cross origin")
         (media-set-cross-origin! media 'use-credentials)
         (check-equal (media-cross-origin media) "use-credentials" "media cross origin set")
         (check-false (media-disable-remote-playback? media) "media disable remote playback")
         (media-set-disable-remote-playback! media #t)
         (check-true (media-disable-remote-playback? media) "media disable remote playback set")
         (check-false (media-ended? media) "media ended")
         (check-true (media-paused? media) "media paused")
         (check-false (media-seeking? media) "media seeking")
         (check-equal (media-duration media) 3.25 "media duration")
         (check-equal (media-media-group media) "group-a" "media group")
         (media-set-media-group! media 'group-b)
         (check-equal (media-media-group media) "group-b" "media group set")
         (check-equal (media-network-state-number media) 1 "media network state number")
         (check-equal (media-network-state media) 'idle "media network state symbol")
         (check-equal (media-ready-state-number media) 1 "media ready state number")
         (check-equal (media-ready-state media) 'have-metadata "media ready state symbol")
         (check-equal (media-sink-id media) "default" "media sink id")
         (check-true (media-preserves-pitch? media) "media preserves pitch")
         (media-set-preserves-pitch! media #f)
         (check-false (media-preserves-pitch? media) "media preserves pitch set")
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
         (media-set-preload! media 'auto)
         (check-equal (media-preload media) "auto" "media preload set")
         (check-equal (media-src media) "song.ogg" "media src")
         (media-set-src! media 'track.ogg)
         (check-equal (media-src media) "track.ogg" "media src set")
         (check-equal (media-can-play-type media 'audio/ogg) "maybe" "media can-play-type")
         (check-true (media-keys-info? (media-keys media)) "media keys")
         (check-equal (js-ref (media-keys-info-raw (media-keys media)) "kind") "keys" "media keys raw")
         (check-equal (js-ref (media-set-media-keys! media (media-keys media)) "kind")
                      "setMediaKeys-promise"
                      "media set keys")
         (define src-object (media-src-object media))
         (check-true (media-source-info? src-object) "media src object")
         (check-equal (js-ref (media-source-info-raw src-object) "kind") "source" "media src object raw")
         (media-set-src-object! media (dom-test-fixture "mediaStream"))
         (define src-object-stream (media-src-object media))
         (check-true (media-stream? src-object-stream) "media src object stream")
         (check-equal (js-ref (media-stream-raw src-object-stream) "kind") "stream" "media src object stream raw")
         (define added-track (media-add-text-track! media 'subtitles 'captions 'en))
         (check-true (text-track? added-track) "media add text track")
         (check-equal (text-track-kind added-track) "subtitles" "media added track kind")
         (check-equal (text-track-label added-track) "captions" "media added track label")
         (check-equal (text-track-language added-track) "en" "media added track language")
         (check-equal (text-track-id added-track) "added-0" "media added track id")
         (check-equal (text-track-mode added-track) "hidden" "media added track mode")
         (text-track-set-mode! added-track 'showing)
         (check-equal (text-track-mode added-track) "showing" "media added track mode set")
         (define captured (media-capture-stream media 60.0))
         (check-equal (image-alt img) "Alpha" "image alt")
         (image-set-alt! img 'Beta)
         (check-equal (image-alt img) "Beta" "image alt set")
         (check-equal (image-src img) "photo.png" "image src")
         (image-set-src! img 'sprite.png)
         (check-equal (image-src img) "sprite.png" "image src set")
         (check-equal (image-current-src img) "photo.png" "image current src")
         (check-equal (image-width img) 64 "image width")
         (image-set-width! img 80)
         (check-equal (image-width img) 80 "image width set")
         (check-equal (image-height img) 32 "image height")
         (image-set-height! img 40)
         (check-equal (image-height img) 40 "image height set")
         (check-equal (image-decoding img) "auto" "image decoding")
         (image-set-decoding! img 'sync)
         (check-equal (image-decoding img) "sync" "image decoding set")
         (check-equal (image-loading img) "eager" "image loading")
         (image-set-loading! img 'lazy)
         (check-equal (image-loading img) "lazy" "image loading set")
         (check-true (image-complete? img) "image complete")
         (check-equal (image-cross-origin img) "anonymous" "image cross origin")
         (image-set-cross-origin! img 'use-credentials)
         (check-equal (image-cross-origin img) "use-credentials" "image cross origin set")
         (check-true (dom-token-list? (media-controls-list media)) "media controls list")
         (check-equal (dom-token-list-value (media-controls-list media)) "nodownload" "media controls list value")
         (check-true (dom-token-list-contains? (media-controls-list media) 'nodownload) "media controls list contains")
         (define audio-tracks (media-audio-tracks media))
         (check-true (audio-track-list? audio-tracks) "media audio tracks")
         (check-equal (audio-track-list-length audio-tracks) 2 "media audio track length")
         (define audio-track (audio-track-list-item audio-tracks 1))
         (check-true (audio-track? audio-track) "media audio track item")
         (check-equal (audio-track-id audio-track) "audio-1" "media audio track id")
         (check-equal (audio-track-kind audio-track) "audio" "media audio track kind")
         (check-equal (audio-track-label audio-track) "Audio 1" "media audio track label")
         (check-equal (audio-track-language audio-track) "fr" "media audio track language")
         (check-false (audio-track-enabled? audio-track) "media audio track enabled")
         (audio-track-set-enabled! audio-track #t)
         (check-true (audio-track-enabled? audio-track) "media audio track enabled set")
         (check-true (time-ranges? (media-buffered media)) "media buffered")
         (check-equal (time-ranges-length (media-buffered media)) 2 "media buffered length")
         (check-equal (time-ranges-start (media-buffered media) 0) 0.5 "media buffered start")
         (check-equal (time-ranges-end (media-buffered media) 1) 7.5 "media buffered end")
         (check-true (media-error-info? (media-error media)) "media error")
         (check-equal (media-error-info-code (media-error media)) 3 "media error code")
         (check-equal (media-error-info-message (media-error media)) "media failure" "media error message")
         (check-true (time-ranges? (media-played media)) "media played")
         (check-true (time-ranges? (media-seekable media)) "media seekable")
         (define text-tracks (media-text-tracks media))
         (check-true (text-track-list? text-tracks) "media text tracks")
         (check-equal (text-track-list-length text-tracks) 1 "media text track length")
         (define text-track (text-track-list-item text-tracks 0))
         (check-true (text-track? text-track) "media text track item")
         (check-equal (text-track-id text-track) "text-0" "media text track id")
         (check-equal (text-track-kind text-track) "subtitles" "media text track kind")
         (check-equal (text-track-label text-track) "Captions" "media text track label")
         (check-equal (text-track-language text-track) "en" "media text track language")
         (check-equal (text-track-mode text-track) "disabled" "media text track mode")
         (text-track-set-mode! text-track 'showing)
         (check-equal (text-track-mode text-track) "showing" "media text track mode set")
         (define video-tracks (media-video-tracks media))
         (check-true (video-track-list? video-tracks) "media video tracks")
         (check-equal (video-track-list-length video-tracks) 1 "media video track length")
         (define video-track (video-track-list-item video-tracks 0))
         (check-true (video-track? video-track) "media video track item")
         (check-equal (video-track-id video-track) "video-0" "media video track id")
         (check-equal (video-track-kind video-track) "main" "media video track kind")
         (check-equal (video-track-label video-track) "Main" "media video track label")
         (check-equal (video-track-language video-track) "en" "media video track language")
         (check-true (video-track-selected? video-track) "media video track selected")
         (check-true (media-stream? captured) "media capture stream")
         (check-equal (js-ref (media-stream-raw captured) "kind") "stream" "media capture stream raw")
         (check-equal (js-ref (media-play media) "kind") "play-promise" "media play")
         (media-pause media)
         (media-load! media)
         (media-fast-seek! media 42.0)
         (check-equal (js-ref (media-set-sink-id! media 'speaker-1) "id") "speaker-1" "media set sink id")
         (check-equal (js-ref media "autoplay") #t "media autoplay set raw")
         (check-equal (js-ref media "crossOrigin") "use-credentials" "media cross origin set raw")
         (check-equal (js-ref media "disableRemotePlayback") #t "media disable remote playback set raw")
         (check-equal (js-ref media "mediaGroup") "group-b" "media group set raw")
         (check-equal (js-ref media "sinkId") "default" "media sink id raw")
         (define media-calls (js-ref media "calls"))
         (check-equal (vector-ref (vector-ref media-calls 0) 0) "canPlayType" "media calls canPlayType")
         (check-equal (vector-ref (vector-ref media-calls 1) 0) "setMediaKeys" "media calls setMediaKeys")
         (check-true (external? (vector-ref (vector-ref media-calls 1) 1)) "media calls setMediaKeys arg")
         (check-equal (vector-ref (vector-ref media-calls 2) 0) "addTextTrack" "media calls addTextTrack")
         (check-equal (vector-ref (vector-ref media-calls 3) 0) "captureStream" "media calls captureStream")
         (check-equal (vector-ref (vector-ref media-calls 4) 0) "play" "media calls play")
         (check-equal (vector-ref (vector-ref media-calls 5) 0) "pause" "media calls pause")
         (check-equal (vector-ref (vector-ref media-calls 6) 0) "load" "media calls load")
         (check-equal (vector-ref (vector-ref media-calls 7) 0) "fastSeek" "media calls fastSeek")
         (check-equal (vector-ref (vector-ref media-calls 8) 0) "setSinkId" "media calls setSinkId")
         (check-true (expect-contract-error (lambda () (media-set-src! media 1))) "media src validation")
         #t)))
