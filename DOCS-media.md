# Reference: media.ffi

This document describes the browser HTMLMediaElement bindings exported
by `ffi/media.ffi` in WebRacket.

Use this family for playback state, audio/video control, and media
element configuration.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- playback state such as `media-current-time`, `media-volume`, and `media-muted`
- configuration helpers such as `media-preload`, `media-src`, and `media-controls?`
- playback controls such as `media-play`, `media-pause`, and `media-load!`
