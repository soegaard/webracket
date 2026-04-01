#lang webracket

;;;
;;; Media wrappers
;;;

;; i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (i32->boolean v)
  (not (zero? v)))

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
  (i32->boolean (js-media-muted media)))

;; media-set-muted! : external? any/c -> void?
;;   Set the muted flag.
(define (media-set-muted! media muted?)
  (js-set-media-muted! media (if muted? 1 0))
  (void))

;; media-default-muted : external? -> boolean?
;;   Read the default-muted flag.
(define (media-default-muted media)
  (i32->boolean (js-media-default-muted media)))

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
  (i32->boolean (js-media-controls media)))

;; media-set-controls! : external? any/c -> void?
;;   Set the controls flag.
(define (media-set-controls! media controls?)
  (js-set-media-controls! media (if controls? 1 0))
  (void))

;; media-loop? : external? -> boolean?
;;   Read the loop flag.
(define (media-loop? media)
  (i32->boolean (js-media-loop media)))

;; media-set-loop! : external? any/c -> void?
;;   Set the loop flag.
(define (media-set-loop! media loop?)
  (js-set-media-loop! media (if loop? 1 0))
  (void))

;; media-preload : external? -> string?
;;   Read the preload hint.
(define (media-preload media)
  (js-media-preload media))

;; media-set-preload! : external? string? -> void?
;;   Set the preload hint.
(define (media-set-preload! media preload)
  (js-set-media-preload! media preload)
  (void))

;; media-src : external? -> string?
;;   Read the media source URL.
(define (media-src media)
  (js-media-src media))

;; media-set-src! : external? string? -> void?
;;   Set the media source URL.
(define (media-set-src! media src)
  (js-set-media-src! media src)
  (void))

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
  (js-media-can-play-type media type))

;; media-set-sink-id! : external? string? -> external/raw
;;   Choose an output sink if supported.
(define (media-set-sink-id! media sink-id)
  (js-media-set-sink-id! media sink-id))
