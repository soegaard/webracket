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

;; media-autoplay? : external? -> boolean?
;;   Read whether autoplay is enabled.
(define (media-autoplay? media)
  (media-i32->boolean (js-media-autoplay media)))

;; media-set-autoplay! : external? boolean? -> void?
;;   Enable or disable autoplay.
(define (media-set-autoplay! media autoplay?)
  (js-set-media-autoplay! media (if autoplay? 1 0))
  (void))

;; media-current-src : external? -> string?
;;   Read the resolved media source URL.
(define (media-current-src media)
  (js-media-current-src media))

;; media-cross-origin : external? -> (or/c #f string?)
;;   Read the CORS mode for media requests.
(define (media-cross-origin media)
  (js-media-cross-origin media))

;; media-set-cross-origin! : external? (or/c string? symbol?) -> void?
;;   Set the CORS mode for media requests.
(define (media-set-cross-origin! media cross-origin)
  (js-set-media-cross-origin! media (media-stringish->string 'media-set-cross-origin! cross-origin))
  (void))

;; media-current-time : external? -> real?
;;   Read the current playback time.
(define (media-current-time media)
  (js-media-current-time media))

;; media-set-current-time! : external? real? -> void?
;;   Seek to a playback time.
(define (media-set-current-time! media t)
  (js-set-media-current-time! media t)
  (void))

;; media-volume : external? -> real?
;;   Read the volume level.
(define (media-volume media)
  (js-media-volume media))

;; media-set-volume! : external? real? -> void?
;;   Set the volume level.
(define (media-set-volume! media volume)
  (js-set-media-volume! media volume)
  (void))

;; media-muted : external? -> boolean?
;;   Read the muted flag.
(define (media-muted media)
  (media-i32->boolean (js-media-muted media)))

;; media-set-muted! : external? any/c -> void?
;;   Set the muted flag.
(define (media-set-muted! media muted?)
  (js-set-media-muted! media (if muted? 1 0))
  (void))

;; media-default-muted : external? -> boolean?
;;   Read the default-muted flag.
(define (media-default-muted media)
  (media-i32->boolean (js-media-default-muted media)))

;; media-set-default-muted! : external? any/c -> void?
;;   Set the default-muted flag.
(define (media-set-default-muted! media muted?)
  (js-set-media-default-muted! media (if muted? 1 0))
  (void))

;; media-default-playback-rate : external? -> real?
;;   Read the default playback rate.
(define (media-default-playback-rate media)
  (js-media-default-playback-rate media))

;; media-set-default-playback-rate! : external? real? -> void?
;;   Set the default playback rate.
(define (media-set-default-playback-rate! media rate)
  (js-set-media-default-playback-rate! media rate)
  (void))

;; media-playback-rate : external? -> real?
;;   Read the playback rate.
(define (media-playback-rate media)
  (js-media-playback-rate media))

;; media-set-playback-rate! : external? real? -> void?
;;   Set the playback rate.
(define (media-set-playback-rate! media rate)
  (js-set-media-playback-rate! media rate)
  (void))

;; media-controls? : external? -> boolean?
;;   Read the controls flag.
(define (media-controls? media)
  (media-i32->boolean (js-media-controls media)))

;; media-set-controls! : external? any/c -> void?
;;   Set the controls flag.
(define (media-set-controls! media controls?)
  (js-set-media-controls! media (if controls? 1 0))
  (void))

;; media-loop? : external? -> boolean?
;;   Read the loop flag.
(define (media-loop? media)
  (media-i32->boolean (js-media-loop media)))

;; media-set-loop! : external? any/c -> void?
;;   Set the loop flag.
(define (media-set-loop! media loop?)
  (js-set-media-loop! media (if loop? 1 0))
  (void))

;; media-ended? : external? -> boolean?
;;   Read whether playback has reached the end.
(define (media-ended? media)
  (media-i32->boolean (js-media-ended media)))

;; media-paused? : external? -> boolean?
;;   Read whether playback is paused.
(define (media-paused? media)
  (media-i32->boolean (js-media-paused media)))

;; media-preload : external? -> string?
;;   Read the preload hint.
(define (media-preload media)
  (js-media-preload media))

;; media-set-preload! : external? (or/c string? symbol?) -> void?
;;   Set the preload hint.
(define (media-set-preload! media preload)
  (js-set-media-preload! media (media-stringish->string 'media-set-preload! preload))
  (void))

;; media-src : external? -> string?
;;   Read the media source URL.
(define (media-src media)
  (js-media-src media))

;; media-set-src! : external? (or/c string? symbol?) -> void?
;;   Set the media source URL.
(define (media-set-src! media src)
  (js-set-media-src! media (media-stringish->string 'media-set-src! src))
  (void))

;; media-controls-list : external? -> dom-token-list?
;;   Read the browser controlsList policy object.
(define (media-controls-list media)
  (dom-token-list-wrap (js-media-controls-list media)))

;; media-keys : external? -> (or/c #f media-keys-info?)
;;   Read the attached MediaKeys object.
(define (media-keys media)
  (media-keys-info-wrap (js-media-media-keys media)))

;; media-set-media-keys! : external? (or/c media-keys-info? external/raw) -> external/raw
;;   Attach MediaKeys for encrypted media playback.
(define (media-set-media-keys! media keys)
  (js-media-set-media-keys! media (if (media-keys-info? keys)
                                      (media-keys-info-unwrap keys)
                                      keys)))

;; media-media-group : external? -> string?
;;   Read the media group identifier.
(define (media-media-group media)
  (js-media-media-group media))

;; media-set-media-group! : external? (or/c string? symbol?) -> void?
;;   Set the media group identifier.
(define (media-set-media-group! media media-group)
  (js-set-media-media-group! media (media-stringish->string 'media-set-media-group! media-group))
  (void))

;; media-disable-remote-playback? : external? -> boolean?
;;   Read whether remote playback is disabled.
(define (media-disable-remote-playback? media)
  (media-i32->boolean (js-media-disable-remote-playback media)))

;; media-set-disable-remote-playback! : external? boolean? -> void?
;;   Enable or disable remote playback.
(define (media-set-disable-remote-playback! media disabled?)
  (js-set-media-disable-remote-playback! media (if disabled? 1 0))
  (void))

;; media-src-object : external? -> (or/c #f media-stream? media-source-info?)
;;   Read the current source object for the media element.
(define (media-src-object media)
  (define src-object (js-media-src-object media))
  (cond
    [(not src-object) #f]
    [(js-ref src-object "getTracks") (media-stream-wrap src-object)]
    [else (media-source-info-wrap src-object)]))

;; media-set-src-object! : external? (or/c #f media-stream? media-source-info? external/raw) -> void?
;;   Set the source object for the media element.
(define (media-set-src-object! media src-object)
  (js-set-media-src-object! media
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
(define (media-network-state-number media)
  (js-media-network-state media))

;; media-network-state : external? -> symbol?
;;   Read the browser network state as a symbol.
(define (media-network-state media)
  (media-network-state->symbol (media-network-state-number media)))

;; media-sink-id : external? -> string?
;;   Read the current sink id.
(define (media-sink-id media)
  (js-media-sink-id media))

;; media-set-sink-id! : external? (or/c string? symbol?) -> external/raw
;;   Choose an output sink if supported.
(define (media-set-sink-id! media sink-id)
  (js-media-set-sink-id! media (media-stringish->string 'media-set-sink-id! sink-id)))
