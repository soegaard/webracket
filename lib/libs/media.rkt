#lang webracket

;;;
;;; Media wrappers
;;;

;; media-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (media-i32->boolean v)
  (not (zero? v)))

;; media-stringish->string : symbol/string -> string
;;   Normalize a browser string argument.
(define (media-stringish->string who value)
  (cond
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [else (raise-argument-error who "(or/c string? symbol?)" value)]))

;; media-resolve-optional : any/c -> any/c
;;   Treat #f as omitted and force thunks when a literal value is needed.
(define (media-resolve-optional value)
  (define resolved
    (cond
      [(eq? value #f) (void)]
      [(procedure? value) (value)]
      [else value]))
  (if (eq? resolved #f)
      (void)
      resolved))

;; media-resolve-stringish-optional : symbol? any/c -> any/c
;;   Resolve an optional string-like value and normalize strings and symbols.
(define (media-resolve-stringish-optional who value)
  (define resolved (media-resolve-optional value))
  (cond
    [(string? resolved) resolved]
    [(symbol? resolved) (symbol->string resolved)]
    [(void? resolved) resolved]
    [else (raise-argument-error who "(or/c string? symbol? #f procedure?)" value)]))

;; media-network-state->symbol : exact-nonnegative-integer? -> symbol?
;;   Convert a browser networkState code to a symbol.
(define (media-network-state->symbol code)
  (case code
    [(0) 'empty]
    [(1) 'idle]
    [(2) 'loading]
    [(3) 'no-source]
    [else (string->symbol (number->string code))]))

;; media-boolean->i32 : any/c boolean? -> exact-integer?
;;   Convert a Racket boolean to a browser i32 flag.
(define (media-boolean->i32 _who value)
  (if value 1 0))

;; media-value->value : any/c -> any/c
;;   Preserve a browser value unchanged.
(define (media-value->value value)
  value)

;; media-value->self : any/c any/c -> any/c
;;   Preserve a browser value unchanged in a setter helper.
(define (media-value->self _who value)
  value)

;; define-media-getter : syntax -> syntax
;;   Define a simple media property getter.
(define-syntax-rule (define-media-getter racket-name foreign-proc convert)
  (define (racket-name media)
    (convert (foreign-proc media))))

;; define-media-setter : syntax -> syntax
;;   Define a simple media property setter.
(define-syntax-rule (define-media-setter racket-name foreign-proc who convert)
  (define (racket-name media value)
    (foreign-proc media (convert who value))
    (void)))

;; define-media-wrapped-getter : syntax -> syntax
;;   Define a media property getter that wraps a browser object result.
(define-syntax-rule (define-media-wrapped-getter racket-name foreign-proc wrap)
  (define (racket-name media)
    (wrap (foreign-proc media))))

;; define-media-wrapped-setter : syntax -> syntax
;;   Define a media property setter that unwraps a wrapped browser object.
(define-syntax-rule (define-media-wrapped-setter racket-name foreign-proc unwrap)
  (define (racket-name media value)
    (foreign-proc media (unwrap value))
    (void)))

;; media-autoplay? : external? -> boolean?
;;   Read whether autoplay is enabled.
(define-media-getter media-autoplay? js-media-autoplay media-i32->boolean)

;; media-set-autoplay! : external? boolean? -> void?
;;   Enable or disable autoplay.
(define-media-setter media-set-autoplay! js-set-media-autoplay! 'media-set-autoplay! media-boolean->i32)

;; media-current-src : external? -> string?
;;   Read the resolved media source URL.
(define-media-getter media-current-src js-media-current-src media-value->value)

;; media-cross-origin : external? -> (or/c #f string?)
;;   Read the CORS mode for media requests.
(define-media-getter media-cross-origin js-media-cross-origin media-value->value)

;; media-set-cross-origin! : external? (or/c string? symbol?) -> void?
;;   Set the CORS mode for media requests.
(define-media-setter media-set-cross-origin! js-set-media-cross-origin! 'media-set-cross-origin! media-stringish->string)

;; media-current-time : external? -> real?
;;   Read the current playback time.
(define-media-getter media-current-time js-media-current-time media-value->value)

;; media-set-current-time! : external? real? -> void?
;;   Seek to a playback time.
(define-media-setter media-set-current-time! js-set-media-current-time! 'media-set-current-time! media-value->self)

;; media-duration : external? -> real?
;;   Read the media duration.
(define (media-duration media)
  (js-media-duration media))

;; media-volume : external? -> real?
;;   Read the volume level.
(define-media-getter media-volume js-media-volume media-value->value)

;; media-set-volume! : external? real? -> void?
;;   Set the volume level.
(define-media-setter media-set-volume! js-set-media-volume! 'media-set-volume! media-value->self)

;; media-muted : external? -> boolean?
;;   Read the muted flag.
(define-media-getter media-muted js-media-muted media-i32->boolean)

;; media-set-muted! : external? any/c -> void?
;;   Set the muted flag.
(define-media-setter media-set-muted! js-set-media-muted! 'media-set-muted! media-boolean->i32)

;; media-default-muted : external? -> boolean?
;;   Read the default-muted flag.
(define-media-getter media-default-muted js-media-default-muted media-i32->boolean)

;; media-set-default-muted! : external? any/c -> void?
;;   Set the default-muted flag.
(define-media-setter media-set-default-muted! js-set-media-default-muted! 'media-set-default-muted! media-boolean->i32)

;; media-default-playback-rate : external? -> real?
;;   Read the default playback rate.
(define-media-getter media-default-playback-rate js-media-default-playback-rate media-value->value)

;; media-set-default-playback-rate! : external? real? -> void?
;;   Set the default playback rate.
(define-media-setter media-set-default-playback-rate! js-set-media-default-playback-rate! 'media-set-default-playback-rate! media-value->self)

;; media-playback-rate : external? -> real?
;;   Read the playback rate.
(define-media-getter media-playback-rate js-media-playback-rate media-value->value)

;; media-set-playback-rate! : external? real? -> void?
;;   Set the playback rate.
(define-media-setter media-set-playback-rate! js-set-media-playback-rate! 'media-set-playback-rate! media-value->self)

;; media-controls? : external? -> boolean?
;;   Read the controls flag.
(define-media-getter media-controls? js-media-controls media-i32->boolean)

;; media-set-controls! : external? any/c -> void?
;;   Set the controls flag.
(define-media-setter media-set-controls! js-set-media-controls! 'media-set-controls! media-boolean->i32)

;; media-loop? : external? -> boolean?
;;   Read the loop flag.
(define-media-getter media-loop? js-media-loop media-i32->boolean)

;; media-set-loop! : external? any/c -> void?
;;   Set the loop flag.
(define-media-setter media-set-loop! js-set-media-loop! 'media-set-loop! media-boolean->i32)

;; media-ended? : external? -> boolean?
;;   Read whether playback has reached the end.
(define-media-getter media-ended? js-media-ended media-i32->boolean)

;; media-paused? : external? -> boolean?
;;   Read whether playback is paused.
(define-media-getter media-paused? js-media-paused media-i32->boolean)

;; media-seeking? : external? -> boolean?
;;   Read whether the media element is currently seeking.
(define-media-getter media-seeking? js-media-seeking media-i32->boolean)

;; media-preload : external? -> string?
;;   Read the preload hint.
(define-media-getter media-preload js-media-preload media-value->value)

;; media-set-preload! : external? (or/c string? symbol?) -> void?
;;   Set the preload hint.
(define-media-setter media-set-preload! js-set-media-preload! 'media-set-preload! media-stringish->string)

;; media-src : external? -> string?
;;   Read the media source URL.
(define-media-getter media-src js-media-src media-value->value)

;; media-set-src! : external? (or/c string? symbol?) -> void?
;;   Set the media source URL.
(define-media-setter media-set-src! js-set-media-src! 'media-set-src! media-stringish->string)

;; media-controls-list : external? -> dom-token-list?
;;   Read the browser controlsList policy object.
(define-media-wrapped-getter media-controls-list js-media-controls-list dom-token-list-wrap)

;; media-keys : external? -> (or/c #f media-keys-info?)
;;   Read the attached MediaKeys object.
(define-media-wrapped-getter media-keys js-media-media-keys media-keys-info-wrap)

;; media-set-media-keys! : external? (or/c media-keys-info? external/raw) -> external/raw
;;   Attach MediaKeys for encrypted media playback.
(define (media-set-media-keys! media keys)
  (js-media-set-media-keys! media (if (media-keys-info? keys)
                                      (media-keys-info-unwrap keys)
                                      keys)))

;; media-media-group : external? -> string?
;;   Read the media group identifier.
(define-media-getter media-media-group js-media-media-group media-value->value)

;; media-set-media-group! : external? (or/c string? symbol?) -> void?
;;   Set the media group identifier.
(define-media-setter media-set-media-group! js-set-media-media-group! 'media-set-media-group! media-stringish->string)

;; media-disable-remote-playback? : external? -> boolean?
;;   Read whether remote playback is disabled.
(define-media-getter media-disable-remote-playback? js-media-disable-remote-playback media-i32->boolean)

;; media-set-disable-remote-playback! : external? boolean? -> void?
;;   Enable or disable remote playback.
(define-media-setter media-set-disable-remote-playback! js-set-media-disable-remote-playback! 'media-set-disable-remote-playback! media-boolean->i32)

;; media-preserves-pitch? : external? -> boolean?
;;   Read whether pitch should be preserved during playback-rate changes.
(define-media-getter media-preserves-pitch? js-media-preserves-pitch media-i32->boolean)

;; media-set-preserves-pitch! : external? boolean? -> void?
;;   Enable or disable pitch preservation during playback-rate changes.
(define-media-setter media-set-preserves-pitch! js-set-media-preserves-pitch! 'media-set-preserves-pitch! media-boolean->i32)

;; media-ready-state-number : external? -> exact-nonnegative-integer?
;;   Read the browser readyState code.
(define-media-getter media-ready-state-number js-media-ready-state media-value->value)

;; media-ready-state : external? -> symbol?
;;   Read the browser readyState as a symbol.
(define (media-ready-state media)
  (case (media-ready-state-number media)
    [(0) 'have-nothing]
    [(1) 'have-metadata]
    [(2) 'have-current-data]
    [(3) 'have-future-data]
    [(4) 'have-enough-data]
    [else (string->symbol (format "ready-state-~a" (media-ready-state-number media)))]))

;; media-src-object : external? -> (or/c #f media-stream? media-source-info?)
;;   Read the current source object for the media element.
(define (media-src-object media)
  (define src-object (js-media-src-object media))
  (cond
    [(not src-object) #f]
    [(not (js-nullish? (js-ref/extern src-object "getTracks")))
      (media-stream-wrap src-object)]
    [else (media-source-info-wrap src-object)]))

;; media-set-src-object! : external? (or/c #f media-stream? media-source-info? external/raw) -> void?
;;   Set the source object for the media element.
(define (media-set-src-object! media src-object)
  (js-set-media-src-object!
   media
   (cond
     [(not src-object) #f]
     [(media-stream? src-object) (media-stream-unwrap src-object)]
     [(media-source-info? src-object) (media-source-info-unwrap src-object)]
     [else src-object]))
  (void))

;; media-add-text-track! : external? (or/c string? symbol?) [label #f] [language #f] -> text-track?
;;   Add a browser text track and return the wrapped track object. #f omits an optional argument.
(define (media-add-text-track! media kind [label #f] [language #f])
  (define kind* (media-stringish->string 'media-add-text-track! kind))
  (define label* (media-resolve-stringish-optional 'media-add-text-track! label))
  (define language* (media-resolve-stringish-optional 'media-add-text-track! language))
  (text-track-wrap (js-media-add-text-track! media kind* label* language*)))

;; media-audio-tracks : external? -> (or/c #f audio-track-list?)
;;   Read the browser audio track list.
(define (media-audio-tracks media)
  (audio-track-list-wrap (js-media-audio-tracks media)))

;; media-buffered : external? -> time-ranges?
;;   Read the buffered time ranges.
(define (media-buffered media)
  (time-ranges-wrap (js-media-buffered media)))

;; media-error : external? -> (or/c #f media-error-info?)
;;   Read the current media error, if any.
(define (media-error media)
  (media-error-info-wrap (js-media-error media)))

;; media-play : external? -> external/raw
;;   Start playback.
(define (media-play media)
  (js-media-play media))

;; media-pause : external? -> void?
;;   Pause playback.
(define (media-pause media)
  (js-media-pause media)
  (void))

;; media-load! : external? -> void?
;;   Reload the media resource.
(define (media-load! media)
  (js-media-load! media)
  (void))

;; media-fast-seek! : external? real? -> void?
;;   Jump to a position without full playback semantics.
(define (media-fast-seek! media t)
  (js-media-fast-seek! media t)
  (void))

;; media-can-play-type : external? string? -> string?
;;   Ask the browser which codecs it can play.
(define (media-can-play-type media type)
  (js-media-can-play-type media (media-stringish->string 'media-can-play-type type)))

;; media-played : external? -> time-ranges?
;;   Read the played time ranges.
(define (media-played media)
  (time-ranges-wrap (js-media-played media)))

;; media-seekable : external? -> time-ranges?
;;   Read the seekable time ranges.
(define (media-seekable media)
  (time-ranges-wrap (js-media-seekable media)))

;; media-text-tracks : external? -> (or/c #f text-track-list?)
;;   Read the browser text track list.
(define (media-text-tracks media)
  (text-track-list-wrap (js-media-text-tracks media)))

;; media-video-tracks : external? -> (or/c #f video-track-list?)
;;   Read the browser video track list.
(define (media-video-tracks media)
  (video-track-list-wrap (js-media-video-tracks media)))

;; media-capture-stream : external? [frame-rate #f] -> media-stream?
;;   Capture the media as a stream. #f omits the optional frame rate.
(define (media-capture-stream media [frame-rate #f])
  (media-stream-wrap (js-media-capture-stream media (media-resolve-optional frame-rate))))

;; media-network-state-number : external? -> exact-nonnegative-integer?
;;   Read the browser network state code.
(define-media-getter media-network-state-number js-media-network-state media-value->value)

;; media-network-state : external? -> symbol?
;;   Read the browser network state as a symbol.
(define (media-network-state media)
  (media-network-state->symbol (media-network-state-number media)))

;; media-sink-id : external? -> string?
;;   Read the current sink id.
(define-media-getter media-sink-id js-media-sink-id media-value->value)

;; media-set-sink-id! : external? (or/c string? symbol?) -> external/raw
;;   Choose an output sink if supported.
(define-media-setter media-set-sink-id! js-media-set-sink-id! 'media-set-sink-id! media-stringish->string)
