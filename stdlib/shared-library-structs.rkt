#lang webracket

;;;
;;; Shared DOM structs.
;;;

;; element : external/raw -> element?
;;   Wrap a browser Element object.
(struct element (raw) #:transparent)

;; element-wrap : any/c -> any/c
;;   Wrap a raw browser Element object, leaving wrapped values alone.
(define (element-wrap value)
  (if (or (not value) (element? value))
      value
      (element value)))

;; element-unwrap : any/c -> any/c
;;   Unwrap an element struct to its raw browser object.
(define (element-unwrap value)
  (if (element? value)
      (element-raw value)
      (if (text-node? value)
          (text-node-raw value)
          value)))

;; text-node : external/raw -> text-node?
;;   Wrap a browser Text node.
(struct text-node (raw) #:transparent)

;; text-node-wrap : any/c -> any/c
;;   Wrap a raw browser Text node, leaving wrapped values alone.
(define (text-node-wrap value)
  (if (text-node? value)
      value
      (text-node value)))

;; text-node-unwrap : any/c -> any/c
;;   Unwrap a text-node struct to its raw browser object.
(define (text-node-unwrap value)
  (if (text-node? value)
      (text-node-raw value)
      value))

;; node : external/raw -> node?
;;   Wrap a browser Node object.
(struct node (raw) #:transparent)

;; node-wrap : any/c -> any/c
;;   Wrap a raw browser Node object, leaving wrapped values alone.
(define (node-wrap value)
  (if (or (not value) (node? value) (element? value) (text-node? value) (attr? value))
      value
      (node value)))

;; node-unwrap : any/c -> any/c
;;   Unwrap a node struct to its raw browser object.
(define (node-unwrap value)
  (cond
    [(node? value) (node-raw value)]
    [(element? value) (element-raw value)]
    [(text-node? value) (text-node-raw value)]
    [(attr? value) (attr-raw value)]
    [else value]))

;; attr : external/raw -> attr?
;;   Wrap a browser Attr node.
(struct attr (raw) #:transparent)

;; attr-wrap : any/c -> any/c
;;   Wrap a raw browser Attr node, leaving wrapped values alone.
(define (attr-wrap value)
  (if (or (not value) (attr? value))
      value
      (attr value)))

;; attr-unwrap : any/c -> any/c
;;   Unwrap an attr struct to its raw browser object.
(define (attr-unwrap value)
  (if (attr? value)
      (attr-raw value)
      value))

;; node-list : external/raw -> node-list?
;;   Wrap a browser NodeList object.
(struct node-list (raw) #:transparent)

;; node-list-wrap : any/c -> any/c
;;   Wrap a raw browser NodeList object, leaving wrapped values alone.
(define (node-list-wrap value)
  (if (or (not value) (node-list? value))
      value
      (node-list value)))

;; node-list-unwrap : any/c -> any/c
;;   Unwrap a node-list struct to its raw browser object.
(define (node-list-unwrap value)
  (if (node-list? value)
      (node-list-raw value)
      value))

;; html-collection : external/raw -> html-collection?
;;   Wrap a browser HTMLCollection object.
(struct html-collection (raw) #:transparent)

;; html-collection-wrap : any/c -> any/c
;;   Wrap a raw browser HTMLCollection object, leaving wrapped values alone.
(define (html-collection-wrap value)
  (if (or (not value) (html-collection? value))
      value
      (html-collection value)))

;; html-collection-unwrap : any/c -> any/c
;;   Unwrap an html-collection struct to its raw browser object.
(define (html-collection-unwrap value)
  (if (html-collection? value)
      (html-collection-raw value)
      value))

;; dom-token-list : external/raw -> dom-token-list?
;;   Wrap a browser DOMTokenList object.
(struct dom-token-list (raw) #:transparent)

;; dom-token-list-wrap : any/c -> any/c
;;   Wrap a raw browser DOMTokenList object, leaving wrapped values alone.
(define (dom-token-list-wrap value)
  (if (or (not value) (dom-token-list? value))
      value
      (dom-token-list value)))

;; dom-token-list-unwrap : any/c -> any/c
;;   Unwrap a DOMTokenList struct to its raw browser object.
(define (dom-token-list-unwrap value)
  (if (dom-token-list? value)
      (dom-token-list-raw value)
      value))

;; shadow-root : external/raw -> shadow-root?
;;   Wrap a browser ShadowRoot object.
(struct shadow-root (raw) #:transparent)

;; shadow-root-wrap : any/c -> any/c
;;   Wrap a raw browser ShadowRoot object, leaving wrapped values alone.
(define (shadow-root-wrap value)
  (if (or (not value) (shadow-root? value))
      value
      (shadow-root value)))

;; shadow-root-unwrap : any/c -> any/c
;;   Unwrap a shadow-root struct to its raw browser object.
(define (shadow-root-unwrap value)
  (if (shadow-root? value)
      (shadow-root-raw value)
      value))

;; animation : external/raw -> animation?
;;   Wrap a browser Animation object.
(struct animation (raw) #:transparent)

;; animation-wrap : any/c -> any/c
;;   Wrap a raw browser Animation object, leaving wrapped values alone.
(define (animation-wrap value)
  (if (or (not value) (animation? value))
      value
      (animation value)))

;; animation-unwrap : any/c -> any/c
;;   Unwrap an animation struct to its raw browser object.
(define (animation-unwrap value)
  (if (animation? value)
      (animation-raw value)
      value))

;; computed-style-map : external/raw -> computed-style-map?
;;   Wrap a browser ComputedStyleMap object.
(struct computed-style-map (raw) #:transparent)

;; computed-style-map-wrap : any/c -> any/c
;;   Wrap a raw browser ComputedStyleMap object, leaving wrapped values alone.
(define (computed-style-map-wrap value)
  (if (or (not value) (computed-style-map? value))
      value
      (computed-style-map value)))

;; computed-style-map-unwrap : any/c -> any/c
;;   Unwrap a computed-style-map struct to its raw browser object.
(define (computed-style-map-unwrap value)
  (if (computed-style-map? value)
      (computed-style-map-raw value)
      value))

;; audio-listener : external/raw -> audio-listener?
;;   Wrap a browser AudioListener object.
(struct audio-listener (raw) #:transparent)

;; audio-listener-wrap : any/c -> any/c
;;   Wrap a raw browser AudioListener object, leaving wrapped values alone.
(define (audio-listener-wrap value)
  (if (or (not value) (audio-listener? value))
      value
      (audio-listener value)))

;; audio-listener-unwrap : any/c -> any/c
;;   Unwrap an audio-listener struct to its raw browser object.
(define (audio-listener-unwrap value)
  (if (audio-listener? value)
      (audio-listener-raw value)
      value))

;; audio-periodic-wave : external/raw -> audio-periodic-wave?
;;   Wrap a browser PeriodicWave object.
(struct audio-periodic-wave (raw) #:transparent)

;; audio-periodic-wave-wrap : any/c -> any/c
;;   Wrap a raw browser PeriodicWave object, leaving wrapped values alone.
(define (audio-periodic-wave-wrap value)
  (if (or (not value) (audio-periodic-wave? value))
      value
      (audio-periodic-wave value)))

;; audio-periodic-wave-unwrap : any/c -> any/c
;;   Unwrap an audio-periodic-wave struct to its raw browser object.
(define (audio-periodic-wave-unwrap value)
  (if (audio-periodic-wave? value)
      (audio-periodic-wave-raw value)
      value))

;; array-like->vector : symbol? any/c (-> any/c any/c) -> vector?
;;   Convert a browser array-like value into a WebRacket vector.
(define (array-like->vector who value wrap-item)
  (define length (js-ref value "length"))
  (unless (exact-nonnegative-integer? length)
    (raise-argument-error who "array-like browser value" value))
  (let loop ([i 0] [acc '()])
    (if (= i length)
        (list->vector (reverse acc))
        (loop (add1 i)
              (cons (wrap-item (js-index value i)) acc)))))

;; dom-rect : external/raw -> dom-rect?
;;   Wrap a browser DOMRect object.
(struct dom-rect (raw) #:transparent)

;; dom-rect-wrap : any/c -> any/c
;;   Wrap a raw browser DOMRect object, leaving wrapped values alone.
(define (dom-rect-wrap value)
  (if (dom-rect? value)
      value
      (dom-rect value)))

;; dom-rect-unwrap : any/c -> any/c
;;   Unwrap a DOMRect struct to its raw browser object.
(define (dom-rect-unwrap value)
  (if (dom-rect? value)
      (dom-rect-raw value)
      value))

;; dom-rect-list : external/raw -> dom-rect-list?
;;   Wrap a browser DOMRectList object.
(struct dom-rect-list (raw) #:transparent)

;; dom-rect-list-wrap : any/c -> any/c
;;   Wrap a raw browser DOMRectList object, leaving wrapped values alone.
(define (dom-rect-list-wrap value)
  (if (or (not value) (dom-rect-list? value))
      value
      (dom-rect-list value)))

;; dom-rect-list-unwrap : any/c -> any/c
;;   Unwrap a dom-rect-list struct to its raw browser object.
(define (dom-rect-list-unwrap value)
  (if (dom-rect-list? value)
      (dom-rect-list-raw value)
      value))

;; selection : external/raw -> selection?
;;   Wrap a browser Selection object.
(struct selection (raw) #:transparent)

;; selection-wrap : any/c -> any/c
;;   Wrap a raw browser Selection object, leaving wrapped values alone.
(define (selection-wrap value)
  (if (or (not value) (selection? value))
      value
      (selection value)))

;; selection-unwrap : any/c -> any/c
;;   Unwrap a selection struct to its raw browser object.
(define (selection-unwrap value)
  (if (selection? value)
      (selection-raw value)
      value))

;; selection-range-count : selection? -> exact-nonnegative-integer?
;;   Read the number of ranges in the current selection.
(define (selection-range-count selection)
  (js-ref (selection-unwrap selection) "rangeCount"))

;; selection-is-collapsed? : selection? -> boolean?
;;   Report whether the current selection is collapsed.
(define (selection-is-collapsed? selection)
  (not (zero? (js-ref (selection-unwrap selection) "isCollapsed"))))

;; selection-anchor-node : selection? -> (or/c #f node?)
;;   Read the anchor node for the current selection.
(define (selection-anchor-node selection)
  (node-wrap (js-ref/extern (selection-unwrap selection) "anchorNode")))

;; selection-focus-node : selection? -> (or/c #f node?)
;;   Read the focus node for the current selection.
(define (selection-focus-node selection)
  (node-wrap (js-ref/extern (selection-unwrap selection) "focusNode")))

;; selection-to-string : selection? -> string?
;;   Convert the current selection to text.
(define (selection-to-string selection)
  (js-send/value (selection-unwrap selection) "toString" (vector)))

;; selection-remove-all-ranges! : selection? -> void?
;;   Clear all ranges from the current selection.
(define (selection-remove-all-ranges! selection)
  (js-send (selection-unwrap selection) "removeAllRanges" (vector))
  (void))

;; media-query-list : external/raw -> media-query-list?
;;   Wrap a browser MediaQueryList object.
(struct media-query-list (raw) #:transparent)

;; media-query-list-wrap : any/c -> any/c
;;   Wrap a raw browser MediaQueryList object, leaving wrapped values alone.
(define (media-query-list-wrap value)
  (if (or (not value) (media-query-list? value))
      value
      (media-query-list value)))

;; media-query-list-unwrap : any/c -> any/c
;;   Unwrap a media-query-list struct to its raw browser object.
(define (media-query-list-unwrap value)
  (if (media-query-list? value)
      (media-query-list-raw value)
      value))

;; media-query-list-media : media-query-list? -> string?
;;   Read the media query string.
(define (media-query-list-media media-query-list)
  (js-ref (media-query-list-unwrap media-query-list) "media"))

;; media-query-list-matches? : media-query-list? -> boolean?
;;   Report whether the media query currently matches.
(define (media-query-list-matches? media-query-list)
  (not (zero? (js-ref (media-query-list-unwrap media-query-list) "matches"))))

;; css-style-declaration : external/raw -> css-style-declaration?
;;   Wrap a browser CSSStyleDeclaration object.
(struct css-style-declaration (raw) #:transparent)

;; css-style-declaration-wrap : any/c -> any/c
;;   Wrap a raw browser CSSStyleDeclaration object, leaving wrapped values alone.
(define (css-style-declaration-wrap value)
  (if (or (not value) (css-style-declaration? value))
      value
      (css-style-declaration value)))

;; css-style-declaration-unwrap : any/c -> any/c
;;   Unwrap a css-style-declaration struct to its raw browser object.
(define (css-style-declaration-unwrap value)
  (if (css-style-declaration? value)
      (css-style-declaration-raw value)
      value))

;; css-style-declaration-css-text : css-style-declaration? -> string?
;;   Read the CSS text for a style declaration.
(define (css-style-declaration-css-text decl)
  (js-ref (css-style-declaration-unwrap decl) "cssText"))

;; css-style-declaration-set-css-text! : css-style-declaration? (or/c string? symbol?) -> void?
;;   Set the CSS text for a style declaration.
(define (css-style-declaration-set-css-text! decl text)
  (js-set! (css-style-declaration-unwrap decl)
           "cssText"
           (if (symbol? text) (symbol->string text) text))
  (void))

;; css-style-declaration-length : css-style-declaration? -> exact-nonnegative-integer?
;;   Read the number of style properties.
(define (css-style-declaration-length decl)
  (js-ref (css-style-declaration-unwrap decl) "length"))

;; css-style-declaration-item : css-style-declaration? exact-nonnegative-integer? -> (or/c #f string?)
;;   Read the style property name at an index.
(define (css-style-declaration-item decl index)
  (js-send/value (css-style-declaration-unwrap decl) "item" (vector index)))

;; css-style-declaration-get-property-value : css-style-declaration? (or/c string? symbol?) -> string?
;;   Read the value of a style property.
(define (css-style-declaration-get-property-value decl name)
  (js-send/value (css-style-declaration-unwrap decl)
                 "getPropertyValue"
                 (vector (if (symbol? name) (symbol->string name) name))))

;; css-style-declaration-set-property! : css-style-declaration? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Set the value of a style property.
(define (css-style-declaration-set-property! decl name value)
  (js-send (css-style-declaration-unwrap decl)
           "setProperty"
           (vector (if (symbol? name) (symbol->string name) name)
                   (if (symbol? value) (symbol->string value) value)))
  (void))

;; css-style-declaration-remove-property! : css-style-declaration? (or/c string? symbol?) -> string?
;;   Remove a style property and return its previous value.
(define (css-style-declaration-remove-property! decl name)
  (js-send/value (css-style-declaration-unwrap decl)
                 "removeProperty"
                 (vector (if (symbol? name) (symbol->string name) name))))

;; media-stream : external/raw -> media-stream?
;;   Wrap a browser MediaStream object.
(struct media-stream (raw) #:transparent)

;; media-stream-wrap : any/c -> any/c
;;   Wrap a raw browser MediaStream object, leaving wrapped values alone.
(define (media-stream-wrap value)
  (if (or (not value) (media-stream? value))
      value
      (media-stream value)))

;; media-stream-unwrap : any/c -> any/c
;;   Unwrap a media-stream struct to its raw browser object.
(define (media-stream-unwrap value)
  (if (media-stream? value)
      (media-stream-raw value)
      value))

;; media-source-info : external/raw -> media-source-info?
;;   Wrap a browser MediaSource object.
(struct media-source-info (raw) #:transparent)

;; media-source-info-wrap : any/c -> any/c
;;   Wrap a raw browser MediaSource object, leaving wrapped values alone.
(define (media-source-info-wrap value)
  (if (or (not value) (media-source-info? value))
      value
      (media-source-info value)))

;; media-source-info-unwrap : any/c -> any/c
;;   Unwrap a media-source-info struct to its raw browser object.
(define (media-source-info-unwrap value)
  (if (media-source-info? value)
      (media-source-info-raw value)
      value))

;; media-keys-info : external/raw -> media-keys-info?
;;   Wrap a browser MediaKeys object.
(struct media-keys-info (raw) #:transparent)

;; media-keys-info-wrap : any/c -> any/c
;;   Wrap a raw browser MediaKeys object, leaving wrapped values alone.
(define (media-keys-info-wrap value)
  (if (or (not value) (media-keys-info? value))
      value
      (media-keys-info value)))

;; media-keys-info-unwrap : any/c -> any/c
;;   Unwrap a media-keys-info struct to its raw browser object.
(define (media-keys-info-unwrap value)
  (if (media-keys-info? value)
      (media-keys-info-raw value)
      value))

;; audio-track : external/raw -> audio-track?
;;   Wrap a browser AudioTrack object.
(struct audio-track (raw) #:transparent)

;; audio-track-wrap : any/c -> any/c
;;   Wrap a raw browser AudioTrack object, leaving wrapped values alone.
(define (audio-track-wrap value)
  (if (or (not value) (audio-track? value))
      value
      (audio-track value)))

;; audio-track-unwrap : any/c -> any/c
;;   Unwrap an audio-track struct to its raw browser object.
(define (audio-track-unwrap value)
  (if (audio-track? value)
      (audio-track-raw value)
      value))

;; audio-track-kind : audio-track? -> string?
;;   Read the browser track kind.
(define (audio-track-kind track)
  (js-ref (audio-track-unwrap track) "kind"))

;; audio-track-label : audio-track? -> string?
;;   Read the browser track label.
(define (audio-track-label track)
  (js-ref (audio-track-unwrap track) "label"))

;; audio-track-language : audio-track? -> string?
;;   Read the browser track language.
(define (audio-track-language track)
  (js-ref (audio-track-unwrap track) "language"))

;; audio-track-id : audio-track? -> string?
;;   Read the browser track id.
(define (audio-track-id track)
  (js-ref (audio-track-unwrap track) "id"))

;; audio-track-enabled? : audio-track? -> boolean?
;;   Read whether the audio track is enabled.
(define (audio-track-enabled? track)
  (js-ref (audio-track-unwrap track) "enabled"))

;; audio-track-set-enabled! : audio-track? boolean? -> void?
;;   Enable or disable the audio track.
(define (audio-track-set-enabled! track enabled?)
  (js-set! (audio-track-unwrap track) "enabled" enabled?)
  (void))

;; text-track : external/raw -> text-track?
;;   Wrap a browser TextTrack object.
(struct text-track (raw) #:transparent)

;; text-track-wrap : any/c -> any/c
;;   Wrap a raw browser TextTrack object, leaving wrapped values alone.
(define (text-track-wrap value)
  (if (or (not value) (text-track? value))
      value
      (text-track value)))

;; text-track-unwrap : any/c -> any/c
;;   Unwrap a text-track struct to its raw browser object.
(define (text-track-unwrap value)
  (if (text-track? value)
      (text-track-raw value)
      value))

;; text-track-kind : text-track? -> string?
;;   Read the browser track kind.
(define (text-track-kind track)
  (js-ref (text-track-unwrap track) "kind"))

;; text-track-label : text-track? -> string?
;;   Read the browser track label.
(define (text-track-label track)
  (js-ref (text-track-unwrap track) "label"))

;; text-track-language : text-track? -> string?
;;   Read the browser track language.
(define (text-track-language track)
  (js-ref (text-track-unwrap track) "language"))

;; text-track-id : text-track? -> string?
;;   Read the browser track id.
(define (text-track-id track)
  (js-ref (text-track-unwrap track) "id"))

;; text-track-mode : text-track? -> string?
;;   Read the browser track mode.
(define (text-track-mode track)
  (js-ref (text-track-unwrap track) "mode"))

;; text-track-set-mode! : text-track? (or/c string? symbol?) -> void?
;;   Set the browser track mode.
(define (text-track-set-mode! track mode)
  (js-set! (text-track-unwrap track)
           "mode"
           (if (symbol? mode) (symbol->string mode) mode))
  (void))

;; video-track : external/raw -> video-track?
;;   Wrap a browser VideoTrack object.
(struct video-track (raw) #:transparent)

;; video-track-wrap : any/c -> any/c
;;   Wrap a raw browser VideoTrack object, leaving wrapped values alone.
(define (video-track-wrap value)
  (if (or (not value) (video-track? value))
      value
      (video-track value)))

;; video-track-unwrap : any/c -> any/c
;;   Unwrap a video-track struct to its raw browser object.
(define (video-track-unwrap value)
  (if (video-track? value)
      (video-track-raw value)
      value))

;; video-track-kind : video-track? -> string?
;;   Read the browser track kind.
(define (video-track-kind track)
  (js-ref (video-track-unwrap track) "kind"))

;; video-track-label : video-track? -> string?
;;   Read the browser track label.
(define (video-track-label track)
  (js-ref (video-track-unwrap track) "label"))

;; video-track-language : video-track? -> string?
;;   Read the browser track language.
(define (video-track-language track)
  (js-ref (video-track-unwrap track) "language"))

;; video-track-id : video-track? -> string?
;;   Read the browser track id.
(define (video-track-id track)
  (js-ref (video-track-unwrap track) "id"))

;; video-track-selected? : video-track? -> boolean?
;;   Read whether the video track is selected.
(define (video-track-selected? track)
  (js-ref (video-track-unwrap track) "selected"))

;; media-error-info : external/raw -> media-error-info?
;;   Wrap a browser MediaError object.
(struct media-error-info (raw) #:transparent)

;; media-error-info-wrap : any/c -> any/c
;;   Wrap a raw browser MediaError object, leaving wrapped values alone.
(define (media-error-info-wrap value)
  (if (or (not value) (media-error-info? value))
      value
      (media-error-info value)))

;; media-error-info-unwrap : any/c -> any/c
;;   Unwrap a media-error-info struct to its raw browser object.
(define (media-error-info-unwrap value)
  (if (media-error-info? value)
      (media-error-info-raw value)
      value))

;; media-error-info-code : media-error-info? -> exact-nonnegative-integer?
;;   Read the browser media error code.
(define (media-error-info-code media-error)
  (js-ref (media-error-info-unwrap media-error) "code"))

;; media-error-info-message : media-error-info? -> string?
;;   Read the browser media error message.
(define (media-error-info-message media-error)
  (js-ref (media-error-info-unwrap media-error) "message"))

;; audio-track-list : external/raw -> audio-track-list?
;;   Wrap a browser AudioTrackList object.
(struct audio-track-list (raw) #:transparent)

;; audio-track-list-wrap : any/c -> any/c
;;   Wrap a raw browser AudioTrackList object, leaving wrapped values alone.
(define (audio-track-list-wrap value)
  (if (or (not value) (audio-track-list? value))
      value
      (audio-track-list value)))

;; audio-track-list-unwrap : any/c -> any/c
;;   Unwrap an audio-track-list struct to its raw browser object.
(define (audio-track-list-unwrap value)
  (if (audio-track-list? value)
      (audio-track-list-raw value)
      value))

;; audio-track-list-length : audio-track-list? -> exact-nonnegative-integer?
;;   Read the number of audio tracks.
(define (audio-track-list-length track-list)
  (js-ref (audio-track-list-unwrap track-list) "length"))

;; audio-track-list-item : audio-track-list? exact-nonnegative-integer? -> (or/c #f audio-track?)
;;   Read an audio track by index.
(define (audio-track-list-item track-list index)
  (audio-track-wrap (js-send/extern/nullish (audio-track-list-unwrap track-list) "item" (vector index))))

;; text-track-list : external/raw -> text-track-list?
;;   Wrap a browser TextTrackList object.
(struct text-track-list (raw) #:transparent)

;; text-track-list-wrap : any/c -> any/c
;;   Wrap a raw browser TextTrackList object, leaving wrapped values alone.
(define (text-track-list-wrap value)
  (if (or (not value) (text-track-list? value))
      value
      (text-track-list value)))

;; text-track-list-unwrap : any/c -> any/c
;;   Unwrap a text-track-list struct to its raw browser object.
(define (text-track-list-unwrap value)
  (if (text-track-list? value)
      (text-track-list-raw value)
      value))

;; text-track-list-length : text-track-list? -> exact-nonnegative-integer?
;;   Read the number of text tracks.
(define (text-track-list-length track-list)
  (js-ref (text-track-list-unwrap track-list) "length"))

;; text-track-list-item : text-track-list? exact-nonnegative-integer? -> (or/c #f text-track?)
;;   Read a text track by index.
(define (text-track-list-item track-list index)
  (text-track-wrap (js-send/extern/nullish (text-track-list-unwrap track-list) "item" (vector index))))

;; video-track-list : external/raw -> video-track-list?
;;   Wrap a browser VideoTrackList object.
(struct video-track-list (raw) #:transparent)

;; video-track-list-wrap : any/c -> any/c
;;   Wrap a raw browser VideoTrackList object, leaving wrapped values alone.
(define (video-track-list-wrap value)
  (if (or (not value) (video-track-list? value))
      value
      (video-track-list value)))

;; video-track-list-unwrap : any/c -> any/c
;;   Unwrap a video-track-list struct to its raw browser object.
(define (video-track-list-unwrap value)
  (if (video-track-list? value)
      (video-track-list-raw value)
      value))

;; video-track-list-length : video-track-list? -> exact-nonnegative-integer?
;;   Read the number of video tracks.
(define (video-track-list-length track-list)
  (js-ref (video-track-list-unwrap track-list) "length"))

;; video-track-list-item : video-track-list? exact-nonnegative-integer? -> (or/c #f video-track?)
;;   Read a video track by index.
(define (video-track-list-item track-list index)
  (video-track-wrap (js-send/extern/nullish (video-track-list-unwrap track-list) "item" (vector index))))

;; time-ranges : external/raw -> time-ranges?
;;   Wrap a browser TimeRanges object.
(struct time-ranges (raw) #:transparent)

;; time-ranges-wrap : any/c -> any/c
;;   Wrap a raw browser TimeRanges object, leaving wrapped values alone.
(define (time-ranges-wrap value)
  (if (or (not value) (time-ranges? value))
      value
      (time-ranges value)))

;; time-ranges-unwrap : any/c -> any/c
;;   Unwrap a time-ranges struct to its raw browser object.
(define (time-ranges-unwrap value)
  (if (time-ranges? value)
      (time-ranges-raw value)
      value))

;; time-ranges-length : time-ranges? -> exact-nonnegative-integer?
;;   Read the number of time ranges.
(define (time-ranges-length time-ranges)
  (js-ref (time-ranges-unwrap time-ranges) "length"))

;; time-ranges-start : time-ranges? exact-nonnegative-integer? -> real?
;;   Read the start time for a range.
(define (time-ranges-start time-ranges index)
  (js-send/value (time-ranges-unwrap time-ranges) "start" (vector index)))

;; time-ranges-end : time-ranges? exact-nonnegative-integer? -> real?
;;   Read the end time for a range.
(define (time-ranges-end time-ranges index)
  (js-send/value (time-ranges-unwrap time-ranges) "end" (vector index)))
