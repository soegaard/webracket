#lang racket/base

(require racket/contract/base)

(provide
  (struct-out media-keys-info)
  (struct-out media-source-info)
  (struct-out media-stream)
  (struct-out media-error-info)
  (struct-out audio-track-list)
  (struct-out audio-track)
  (struct-out text-track-list)
  (struct-out text-track)
  (struct-out video-track-list)
  (struct-out video-track)
  (struct-out time-ranges)
  media-current-time
  media-autoplay?
  media-set-autoplay!
  media-volume
  media-set-volume!
  media-muted
  media-set-muted!
  media-default-muted
  media-set-default-muted!
  media-default-playback-rate
  media-set-default-playback-rate!
  media-playback-rate
  media-set-playback-rate!
  media-current-src
  media-cross-origin
  media-set-cross-origin!
  media-controls?
  media-set-controls!
  media-loop?
  media-set-loop!
  media-preload
  media-set-preload!
  media-src
  media-set-src!
  media-controls-list
  media-keys
  media-set-media-keys!
  media-src-object
  media-set-src-object!
  media-ended?
  media-paused?
  media-media-group
  media-set-media-group!
  media-disable-remote-playback?
  media-set-disable-remote-playback!
  media-add-text-track!
  media-audio-tracks
  media-buffered
  media-error
  media-error-info-code
  media-error-info-message
  media-play
  media-pause
  media-load!
  media-fast-seek!
  media-can-play-type
  media-played
  media-seekable
  media-text-tracks
  media-video-tracks
  media-capture-stream
  media-network-state
  media-network-state-number
  media-sink-id
  audio-track-list-length
  audio-track-list-item
  audio-track-kind
  audio-track-label
  audio-track-language
  audio-track-id
  audio-track-enabled?
  audio-track-set-enabled!
  text-track-list-length
  text-track-list-item
  text-track-kind
  text-track-label
  text-track-language
  text-track-id
  text-track-mode
  text-track-set-mode!
  video-track-list-length
  video-track-list-item
  video-track-kind
  video-track-label
  video-track-language
  video-track-id
  video-track-selected?
  media-set-sink-id!
  (for-label (all-defined-out)))

(struct media-keys-info (raw) #:transparent)
(struct media-source-info (raw) #:transparent)
(struct media-stream (raw) #:transparent)
(struct media-error-info (raw) #:transparent)
(struct audio-track-list (raw) #:transparent)
(struct audio-track (raw) #:transparent)
(struct text-track-list (raw) #:transparent)
(struct text-track (raw) #:transparent)
(struct video-track-list (raw) #:transparent)
(struct video-track (raw) #:transparent)
(struct time-ranges (raw) #:transparent)

(define media-current-time any/c)
(define media-autoplay? any/c)
(define media-set-autoplay! any/c)
(define media-volume any/c)
(define media-set-volume! any/c)
(define media-muted any/c)
(define media-set-muted! any/c)
(define media-default-muted any/c)
(define media-set-default-muted! any/c)
(define media-default-playback-rate any/c)
(define media-set-default-playback-rate! any/c)
(define media-playback-rate any/c)
(define media-set-playback-rate! any/c)
(define media-current-src any/c)
(define media-cross-origin any/c)
(define media-set-cross-origin! any/c)
(define media-controls? any/c)
(define media-set-controls! any/c)
(define media-loop? any/c)
(define media-set-loop! any/c)
(define media-ended? any/c)
(define media-paused? any/c)
(define media-media-group any/c)
(define media-set-media-group! any/c)
(define media-disable-remote-playback? any/c)
(define media-set-disable-remote-playback! any/c)
(define media-preload any/c)
(define media-set-preload! any/c)
(define media-src any/c)
(define media-set-src! any/c)
(define media-controls-list any/c)
(define media-keys any/c)
(define media-set-media-keys! any/c)
(define media-src-object any/c)
(define media-set-src-object! any/c)
(define media-add-text-track! any/c)
(define media-audio-tracks any/c)
(define media-buffered any/c)
(define media-error any/c)
(define media-error-info-code any/c)
(define media-error-info-message any/c)
(define media-play any/c)
(define media-pause any/c)
(define media-load! any/c)
(define media-fast-seek! any/c)
(define media-can-play-type any/c)
(define media-played any/c)
(define media-seekable any/c)
(define media-text-tracks any/c)
(define media-video-tracks any/c)
(define media-capture-stream any/c)
(define media-network-state any/c)
(define media-network-state-number any/c)
(define media-sink-id any/c)
(define audio-track-list-length any/c)
(define audio-track-list-item any/c)
(define audio-track-kind any/c)
(define audio-track-label any/c)
(define audio-track-language any/c)
(define audio-track-id any/c)
(define audio-track-enabled? any/c)
(define audio-track-set-enabled! any/c)
(define text-track-list-length any/c)
(define text-track-list-item any/c)
(define text-track-kind any/c)
(define text-track-label any/c)
(define text-track-language any/c)
(define text-track-id any/c)
(define text-track-mode any/c)
(define text-track-set-mode! any/c)
(define video-track-list-length any/c)
(define video-track-list-item any/c)
(define video-track-kind any/c)
(define video-track-label any/c)
(define video-track-language any/c)
(define video-track-id any/c)
(define video-track-selected? any/c)
(define media-set-sink-id! any/c)
