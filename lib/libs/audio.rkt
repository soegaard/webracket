#lang webracket

;;;
;;; Web Audio API
;;;

;; This library brings the Web Audio API to WebRacket.

;; https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API

;; This library wraps the low-level `js-audio-*` bindings with checked
;; `audio-*` helpers for browser-side code loaded via `include-lib`.


;; TODO
;;  - Reorder functions into "chapters".
;;  - Add more chapter headers

;;; -------------------------------------------------------------------
;;; Callback Caching 
;;; -------------------------------------------------------------------

;; Callbacks are cached, so the same procedure always maps to the same external.

;; procedure->external-cache : hash?
;;   Cache JS callback wrappers so the same procedure maps to the same external.
(define procedure->external-cache (make-hasheq))

;; procedure->stable-external : procedure? -> external?
;;   Reuse a cached callback wrapper for a procedure when possible.
(define (procedure->stable-external proc)
  (define cached (hash-ref procedure->external-cache proc #f))
  (cond
    [cached cached]
    [else
     (define external (procedure->external proc))
     (hash-set! procedure->external-cache proc external)
     external]))

;;; -------------------------------------------------------------------
;;; Predicates
;;; -------------------------------------------------------------------

;; audio-constructor-present? : string? -> boolean?
;;   Check whether the host environment exposes a named Audio constructor.
(define (audio-constructor-present? name)
  #t)

;; audio-context? : any/c -> boolean?
;;   Check whether x is an AudioContext value.
(define (audio-context? x)
  (and (external? x)
       (audio-constructor-present? "AudioContext")
       (js-instanceof x (js-var "AudioContext"))))

;; audio-node? : any/c -> boolean?
;;   Check whether x is an AudioNode value.
(define (audio-node? x)
  (and (external? x)
       (audio-constructor-present? "AudioNode")
       (js-instanceof x (js-var "AudioNode"))))

;; audio-param? : any/c -> boolean?
;;   Check whether x is an AudioParam value.
(define (audio-param? x)
  (and (external? x)
       (audio-constructor-present? "AudioParam")
       (js-instanceof x (js-var "AudioParam"))))

;; audio-buffer? : any/c -> boolean?
;;   Check whether x is an AudioBuffer value.
(define (audio-buffer? x)
  (and (external? x)
       (audio-constructor-present? "AudioBuffer")
       (js-instanceof x (js-var "AudioBuffer"))))

;; audio-gain-node? : any/c -> boolean?
;;   Check whether x is a GainNode value.
(define (audio-gain-node? x)
  (and (external? x)
       (audio-constructor-present? "GainNode")
       (js-instanceof x (js-var "GainNode"))))

;; audio-oscillator-node? : any/c -> boolean?
;;   Check whether x is an OscillatorNode value.
(define (audio-oscillator-node? x)
  (and (external? x)
       (audio-constructor-present? "OscillatorNode")
       (js-instanceof x (js-var "OscillatorNode"))))

;; audio-buffer-source-node? : any/c -> boolean?
;;   Check whether x is an AudioBufferSourceNode value.
(define (audio-buffer-source-node? x)
  (and (external? x)
       (audio-constructor-present? "AudioBufferSourceNode")
       (js-instanceof x (js-var "AudioBufferSourceNode"))))

;; audio-analyser-node? : any/c -> boolean?
;;   Check whether x is an AnalyserNode value.
(define (audio-analyser-node? x)
  (and (external? x)
       (audio-constructor-present? "AnalyserNode")
       (js-instanceof x (js-var "AnalyserNode"))))

;; audio-biquad-filter-node? : any/c -> boolean?
;;   Check whether x is a BiquadFilterNode value.
(define (audio-biquad-filter-node? x)
  (and (external? x)
       (audio-constructor-present? "BiquadFilterNode")
       (js-instanceof x (js-var "BiquadFilterNode"))))

;; audio-constant-source-node? : any/c -> boolean?
;;   Check whether x is a ConstantSourceNode value.
(define (audio-constant-source-node? x)
  (and (external? x)
       (audio-constructor-present? "ConstantSourceNode")
       (js-instanceof x (js-var "ConstantSourceNode"))))

;; audio-dynamics-compressor-node? : any/c -> boolean?
;;   Check whether x is a DynamicsCompressorNode value.
(define (audio-dynamics-compressor-node? x)
  (and (external? x)
       (audio-constructor-present? "DynamicsCompressorNode")
       (js-instanceof x (js-var "DynamicsCompressorNode"))))

;; audio-panner-node? : any/c -> boolean?
;;   Check whether x is a PannerNode value.
(define (audio-panner-node? x)
  (and (external? x)
       (audio-constructor-present? "PannerNode")
       (js-instanceof x (js-var "PannerNode"))))

;; audio-stereo-panner-node? : any/c -> boolean?
;;   Check whether x is a StereoPannerNode value.
(define (audio-stereo-panner-node? x)
  (and (external? x)
       (audio-constructor-present? "StereoPannerNode")
       (js-instanceof x (js-var "StereoPannerNode"))))

;; audio-channel-splitter-node? : any/c -> boolean?
;;   Check whether x is a ChannelSplitterNode value.
(define (audio-channel-splitter-node? x)
  (and (external? x)
       (audio-constructor-present? "ChannelSplitterNode")
       (js-instanceof x (js-var "ChannelSplitterNode"))))

;; audio-channel-merger-node? : any/c -> boolean?
;;   Check whether x is a ChannelMergerNode value.
(define (audio-channel-merger-node? x)
  (and (external? x)
       (audio-constructor-present? "ChannelMergerNode")
       (js-instanceof x (js-var "ChannelMergerNode"))))

;;; -------------------------------------------------------------------
;;; Checkers
;;; -------------------------------------------------------------------

;; check-audio-context : symbol? any/c -> void?
;;   Ensure x is an AudioContext value.
(define (check-audio-context who x)
  (unless (audio-context? x)
    (raise-argument-error who "audio-context?" x)))

;; check-audio-node : symbol? any/c -> void?
;;   Ensure x is an AudioNode value.
(define (check-audio-node who x)
  (unless (audio-node? x)
    (raise-argument-error who "audio-node?" x)))

;; check-audio-gain-node : symbol? any/c -> void?
;;   Ensure x is a GainNode value.
(define (check-audio-gain-node who x)
  (unless (audio-gain-node? x)
    (raise-argument-error who "audio-gain-node?" x)))

;; check-audio-oscillator-node : symbol? any/c -> void?
;;   Ensure x is an OscillatorNode value.
(define (check-audio-oscillator-node who x)
  (unless (audio-oscillator-node? x)
    (raise-argument-error who "audio-oscillator-node?" x)))

;; check-audio-buffer-source-node : symbol? any/c -> void?
;;   Ensure x is an AudioBufferSourceNode value.
(define (check-audio-buffer-source-node who x)
  (unless (audio-buffer-source-node? x)
    (raise-argument-error who "audio-buffer-source-node?" x)))

;; check-audio-analyser-node : symbol? any/c -> void?
;;   Ensure x is an AnalyserNode value.
(define (check-audio-analyser-node who x)
  (unless (audio-analyser-node? x)
    (raise-argument-error who "audio-analyser-node?" x)))

;; check-audio-biquad-filter-node : symbol? any/c -> void?
;;   Ensure x is a BiquadFilterNode value.
(define (check-audio-biquad-filter-node who x)
  (unless (audio-biquad-filter-node? x)
    (raise-argument-error who "audio-biquad-filter-node?" x)))

;; check-audio-constant-source-node : symbol? any/c -> void?
;;   Ensure x is a ConstantSourceNode value.
(define (check-audio-constant-source-node who x)
  (unless (audio-constant-source-node? x)
    (raise-argument-error who "audio-constant-source-node?" x)))

;; check-audio-dynamics-compressor-node : symbol? any/c -> void?
;;   Ensure x is a DynamicsCompressorNode value.
(define (check-audio-dynamics-compressor-node who x)
  (unless (audio-dynamics-compressor-node? x)
    (raise-argument-error who "audio-dynamics-compressor-node?" x)))

;; check-audio-panner-node : symbol? any/c -> void?
;;   Ensure x is a PannerNode value.
(define (check-audio-panner-node who x)
  (unless (audio-panner-node? x)
    (raise-argument-error who "audio-panner-node?" x)))

;; check-audio-stereo-panner-node : symbol? any/c -> void?
;;   Ensure x is a StereoPannerNode value.
(define (check-audio-stereo-panner-node who x)
  (unless (audio-stereo-panner-node? x)
    (raise-argument-error who "audio-stereo-panner-node?" x)))

;; check-audio-channel-splitter-node : symbol? any/c -> void?
;;   Ensure x is a ChannelSplitterNode value.
(define (check-audio-channel-splitter-node who x)
  (unless (audio-channel-splitter-node? x)
    (raise-argument-error who "audio-channel-splitter-node?" x)))

;; check-audio-channel-merger-node : symbol? any/c -> void?
;;   Ensure x is a ChannelMergerNode value.
(define (check-audio-channel-merger-node who x)
  (unless (audio-channel-merger-node? x)
    (raise-argument-error who "audio-channel-merger-node?" x)))

;; check-audio-param : symbol? any/c -> void?
;;   Ensure x is an AudioParam value.
(define (check-audio-param who x)
  (unless (audio-param? x)
    (raise-argument-error who "audio-param?" x)))

;; check-audio-buffer : symbol? any/c -> void?
;;   Ensure x is an AudioBuffer value.
(define (check-audio-buffer who x)
  (unless (audio-buffer? x)
    (raise-argument-error who "audio-buffer?" x)))

;; check-audio-periodic-wave : symbol? any/c -> void?
;;   Ensure x is a PeriodicWave value.
(define (check-audio-periodic-wave who x)
  (unless (audio-periodic-wave? x)
    (raise-argument-error who "audio-periodic-wave?" x)))

;; check-audio-stringish : symbol? any/c -> void?
;;   Ensure v is a string or symbol.
(define (check-audio-stringish who v)
  (unless (or (string? v) (symbol? v))
    (raise-argument-error who "(or/c string? symbol?)" v)))

;; normalize-audio-stringish : symbol? any/c -> string?
;;   Convert a string-or-symbol value to a string.
(define (normalize-audio-stringish who v)
  (check-audio-stringish who v)
  (if (symbol? v) (symbol->string v) v))

;; check-audio-optional-time : symbol? any/c -> void?
;;   Ensure t is absent or a real number.
(define (check-audio-optional-time who t)
  (unless (or (eq? t #f) (real? t))
    (raise-argument-error who "(or/c #f real?)" t)))

;; check-audio-optional-number : symbol? any/c -> void?
;;   Ensure v is absent or a real number.
(define (check-audio-optional-number who v)
  (unless (or (eq? v #f) (real? v))
    (raise-argument-error who "(or/c #f real?)" v)))

;; check-audio-byte-buffer : symbol? any/c -> void?
;;   Ensure v is a bytes value or a mutable vector of exact integers.
(define (check-audio-byte-buffer who v)
  (unless (or (bytes? v)
              (vector? v))
    (raise-argument-error who "(or/c bytes? vector?)" v))
  (when (vector? v)
    (for-each (lambda (x)
                (unless (exact-integer? x)
                  (raise-argument-error who "exact-integer?" x)))
              (vector->list v))))

;; check-audio-float-vector : symbol? any/c -> void?
;;   Ensure v is a vector of real numbers.
(define (check-audio-float-vector who v)
  (unless (vector? v)
    (raise-argument-error who "vector?" v))
  (for-each (lambda (x)
              (unless (real? x)
                (raise-argument-error who "real?" x)))
            (vector->list v)))

;;; -------------------------------------------------------------------
;;; Constructors
;;; -------------------------------------------------------------------


;; audio-vector->js-float32-array : vector? -> external?
;;   Convert a Racket vector of reals to a browser Float32Array.
(define (audio-vector->js-float32-array values)
  (js-new (js-Float32Array)
          (vector (js-array/extern values))))

;; audio-make-js-uint8-array : exact-nonnegative-integer? -> external?
;;   Allocate a zero-filled browser Uint8Array of the requested length.
(define (audio-make-js-uint8-array length)
  (js-new (js-Uint8Array) (vector length)))

;; audio-make-js-float32-array : exact-nonnegative-integer? -> external?
;;   Allocate a zero-filled browser Float32Array of the requested length.
(define (audio-make-js-float32-array length)
  (js-new (js-Float32Array) (vector length)))

;; audio-copy-js-array-into-bytes! : symbol? external? bytes? -> void?
;;   Copy a browser typed array back into a Racket bytes value.
(define (audio-copy-js-array-into-bytes! who js-array dest)
  (define len (bytes-length dest))
  (for ([i (in-range len)])
    (bytes-set! dest i (js-index js-array i))))

;; audio-copy-js-array-into-vector! : symbol? external? vector? -> void?
;;   Copy a browser typed array back into a Racket vector.
(define (audio-copy-js-array-into-vector! who js-array dest)
  (define len (vector-length dest))
  (for ([i (in-range len)])
    (vector-set! dest i (js-index js-array i))))

;; check-audio-handler : symbol? any/c -> void?
;;   Ensure handler is a procedure, an external callback, or #f.
(define (check-audio-handler who handler)
  (unless (or (procedure? handler) (external? handler) (eq? handler #f))
    (raise-argument-error who "(or/c #f procedure? external?)" handler)))

;; check-audio-listener : symbol? any/c -> void?
;;   Ensure listener is a procedure or an external callback.
(define (check-audio-listener who listener)
  (unless (or (procedure? listener) (external? listener))
    (raise-argument-error who "(or/c procedure? external?)" listener)))

;; check-audio-listener-option : symbol? any/c -> void?
;;   Ensure an event-listener option is boolean or an external JS object.
(define (check-audio-listener-option who option)
  (unless (or (boolean? option) (external? option))
    (raise-argument-error who "(or/c boolean? external?)" option)))


;; audio-handler->external : symbol? any/c -> any/c
;;   Convert a handler to an external callback or JS null.
(define (audio-handler->external who handler)
  (check-audio-handler who handler)
  (cond
    [(eq? handler #f) (js-undefined)]
    [(external? handler) handler]
    [else (procedure->stable-external handler)]))

;; audio-listener->external : symbol? any/c -> external?
;;   Convert a listener procedure to a JS callback value.
(define (audio-listener->external who listener)
  (cond
    [(external? listener) listener]
    [(procedure? listener) (procedure->stable-external listener)]
    [else
     (raise-argument-error who "(or/c procedure? external?)" listener)]))

;; audio-set-handler! : symbol? external? string? any/c -> void?
;;   Install or clear an Audio event handler property.
(define (audio-set-handler! who target prop handler)
  (check-audio-node who target)
  (js-set! target prop (audio-handler->external who handler))
  (void))

;; audio-set-handler!/context : symbol? external? string? any/c -> void?
;;   Install or clear an AudioContext handler property.
(define (audio-set-handler!/context who target prop handler)
  (check-audio-context who target)
  (js-set! target prop (audio-handler->external who handler))
  (void))

;; audio-add-event-listener! : external? (or/c string? symbol?) (or/c procedure? external?) (or/c boolean? external?) ... -> external?
;;   Add an event listener and return the installed listener token.
(define (audio-add-event-listener! target event-name listener . options)
  (unless (or (audio-context? target) (audio-node? target))
    (raise-argument-error 'audio-add-event-listener! "audio-context? or audio-node?" target))
  (define event-name* (normalize-audio-stringish 'audio-add-event-listener! event-name))
  (define listener* (audio-listener->external 'audio-add-event-listener! listener))
  (for-each (lambda (option)
              (check-audio-listener-option 'audio-add-event-listener! option))
            options)
  (define args
    (if (null? options)
        (vector event-name* listener*)
        (list->vector (list* event-name* listener* options))))
  (js-send/extern/nullish target "addEventListener" args)
  listener*)

;; audio-remove-event-listener! : external? (or/c string? symbol?) (or/c procedure? external?) (or/c boolean? external?) ... -> void?
;;   Remove a previously registered event listener.
(define (audio-remove-event-listener! target event-name listener . options)
  (unless (or (audio-context? target) (audio-node? target))
    (raise-argument-error 'audio-remove-event-listener! "audio-context? or audio-node?" target))
  (define event-name* (normalize-audio-stringish 'audio-remove-event-listener! event-name))
  (define listener* (audio-listener->external 'audio-remove-event-listener! listener))
  (for-each (lambda (option)
              (check-audio-listener-option 'audio-remove-event-listener! option))
            options)
  (define args
    (if (null? options)
        (vector event-name* listener*)
        (list->vector (list* event-name* listener* options))))
  (js-send/extern/nullish target "removeEventListener" args)
  (void))

;;; -------------------------------------------------------------------
;;; Audio Context
;;; -------------------------------------------------------------------

;; audio-context-new : -> audio-context?
;;   Create a new AudioContext.
(define (audio-context-new)
  (js-audio-context-new))

;; audio-context-close : audio-context? -> external?
;;   Close an AudioContext and return the browser Promise.
(define (audio-context-close ctx)
  (check-audio-context 'audio-context-close ctx)
  (js-audio-context-close ctx))

;; audio-context-resume : audio-context? -> external?
;;   Resume an AudioContext and return the browser Promise.
(define (audio-context-resume ctx)
  (check-audio-context 'audio-context-resume ctx)
  (js-audio-context-resume ctx))

;; audio-context-suspend : audio-context? -> external?
;;   Suspend an AudioContext and return the browser Promise.
(define (audio-context-suspend ctx)
  (check-audio-context 'audio-context-suspend ctx)
  (js-audio-context-suspend ctx))

;; audio-context-state : audio-context? -> string?
;;   Read the AudioContext state string.
(define (audio-context-state ctx)
  (check-audio-context 'audio-context-state ctx)
  (js-audio-context-state ctx))

;; audio-context-current-time : audio-context? -> real?
;;   Read the current AudioContext time.
(define (audio-context-current-time ctx)
  (check-audio-context 'audio-context-current-time ctx)
  (js-audio-context-current-time ctx))

;; audio-context-sample-rate : audio-context? -> real?
;;   Read the AudioContext sample rate.
(define (audio-context-sample-rate ctx)
  (check-audio-context 'audio-context-sample-rate ctx)
  (js-audio-context-sample-rate ctx))

;; audio-context-base-latency : audio-context? -> real?
;;   Read the AudioContext base latency.
(define (audio-context-base-latency ctx)
  (check-audio-context 'audio-context-base-latency ctx)
  (js-audio-context-base-latency ctx))

;; audio-context-output-latency : audio-context? -> real?
;;   Read the AudioContext output latency.
(define (audio-context-output-latency ctx)
  (check-audio-context 'audio-context-output-latency ctx)
  (js-audio-context-output-latency ctx))

;; audio-context-destination : audio-context? -> external?
;;   Read the destination node for an AudioContext.
(define (audio-context-destination ctx)
  (check-audio-context 'audio-context-destination ctx)
  (js-audio-context-destination ctx))

;; audio-context-listener : audio-context? -> external?
;;   Read the listener for an AudioContext.
(define (audio-context-listener ctx)
  (check-audio-context 'audio-context-listener ctx)
  (audio-listener-wrap (js-audio-context-listener ctx)))

;; audio-context-create-gain : audio-context? -> audio-gain-node?
;;   Create a GainNode attached to a context.
(define (audio-context-create-gain ctx)
  (check-audio-context 'audio-context-create-gain ctx)
  (js-audio-context-create-gain ctx))

;; audio-context-create-oscillator : audio-context? -> audio-oscillator-node?
;;   Create an OscillatorNode attached to a context.
(define (audio-context-create-oscillator ctx)
  (check-audio-context 'audio-context-create-oscillator ctx)
  (js-audio-context-create-oscillator ctx))

;; audio-context-create-buffer-source : audio-context? -> audio-buffer-source-node?
;;   Create an AudioBufferSourceNode attached to a context.
(define (audio-context-create-buffer-source ctx)
  (check-audio-context 'audio-context-create-buffer-source ctx)
  (js-audio-context-create-buffer-source ctx))

;; audio-context-create-analyser : audio-context? -> audio-analyser-node?
;;   Create an AnalyserNode attached to a context.
(define (audio-context-create-analyser ctx)
  (check-audio-context 'audio-context-create-analyser ctx)
  (js-audio-context-create-analyser ctx))

;; audio-context-create-biquad-filter : audio-context? -> audio-biquad-filter-node?
;;   Create a BiquadFilterNode attached to a context.
(define (audio-context-create-biquad-filter ctx)
  (check-audio-context 'audio-context-create-biquad-filter ctx)
  (js-audio-context-create-biquad-filter ctx))

;; audio-context-create-constant-source : audio-context? -> audio-constant-source-node?
;;   Create a ConstantSourceNode attached to a context.
(define (audio-context-create-constant-source ctx)
  (check-audio-context 'audio-context-create-constant-source ctx)
  (js-audio-context-create-constant-source ctx))

;; audio-context-create-channel-splitter : audio-context? exact-integer? -> audio-channel-splitter-node?
;;   Create a ChannelSplitterNode attached to a context.
(define (audio-context-create-channel-splitter ctx channels)
  (check-audio-context 'audio-context-create-channel-splitter ctx)
  (unless (exact-integer? channels)
    (raise-argument-error 'audio-context-create-channel-splitter "exact-integer?" channels))
  (js-audio-context-create-channel-splitter ctx channels))

;; audio-context-create-channel-merger : audio-context? exact-integer? -> audio-channel-merger-node?
;;   Create a ChannelMergerNode attached to a context.
(define (audio-context-create-channel-merger ctx channels)
  (check-audio-context 'audio-context-create-channel-merger ctx)
  (unless (exact-integer? channels)
    (raise-argument-error 'audio-context-create-channel-merger "exact-integer?" channels))
  (js-audio-context-create-channel-merger ctx channels))

;; audio-context-create-dynamics-compressor : audio-context? -> audio-dynamics-compressor-node?
;;   Create a DynamicsCompressorNode attached to a context.
(define (audio-context-create-dynamics-compressor ctx)
  (check-audio-context 'audio-context-create-dynamics-compressor ctx)
  (js-audio-context-create-dynamics-compressor ctx))

;; audio-context-create-panner : audio-context? -> audio-panner-node?
;;   Create a PannerNode attached to a context.
(define (audio-context-create-panner ctx)
  (check-audio-context 'audio-context-create-panner ctx)
  (js-audio-context-create-panner ctx))

;; audio-context-create-stereo-panner : audio-context? -> audio-stereo-panner-node?
;;   Create a StereoPannerNode attached to a context.
(define (audio-context-create-stereo-panner ctx)
  (check-audio-context 'audio-context-create-stereo-panner ctx)
  (js-audio-context-create-stereo-panner ctx))

;; audio-context-create-buffer : audio-context? exact-integer? exact-integer? real? -> audio-buffer?
;;   Create an AudioBuffer with the requested channel count, length, and sample rate.
(define (audio-context-create-buffer ctx channels length sample-rate)
  (check-audio-context 'audio-context-create-buffer ctx)
  (unless (exact-integer? channels)
    (raise-argument-error 'audio-context-create-buffer "exact-integer?" channels))
  (unless (exact-integer? length)
    (raise-argument-error 'audio-context-create-buffer "exact-integer?" length))
  (unless (real? sample-rate)
    (raise-argument-error 'audio-context-create-buffer "real?" sample-rate))
  (js-audio-context-create-buffer ctx channels length sample-rate))

;; audio-context-create-periodic-wave : audio-context? vector? vector? -> audio-periodic-wave?
;;   Create a PeriodicWave from real and imaginary Fourier coefficients.
(define (audio-context-create-periodic-wave ctx real imag)
  (check-audio-context 'audio-context-create-periodic-wave ctx)
  (check-audio-float-vector 'audio-context-create-periodic-wave real)
  (check-audio-float-vector 'audio-context-create-periodic-wave imag)
  (audio-periodic-wave-wrap
   (js-audio-context-create-periodic-wave ctx
                                          (audio-vector->js-float32-array real)
                                          (audio-vector->js-float32-array imag))))

;; audio-context-decode-audio-data : audio-context? (or/c bytes? external?) -> external?
;;   Decode audio data and return the browser Promise.
(define (audio-context-decode-audio-data ctx data)
  (check-audio-context 'audio-context-decode-audio-data ctx)
  (unless (or (bytes? data) (external? data))
    (raise-argument-error 'audio-context-decode-audio-data "(or/c bytes? external?)" data))
  (js-audio-context-decode-audio-data ctx data))

;;; -------------------------------------------------------------------
;;; Audio Node
;;; -------------------------------------------------------------------


;; audio-node-context : audio-node? -> audio-context?
;;   Read the owning AudioContext for a node.
(define (audio-node-context node)
  (check-audio-node 'audio-node-context node)
  (js-audio-node-context node))

;; audio-node-connect : audio-node? (or/c audio-node? audio-param?) [exact-integer?] [exact-integer?] -> external?
;;   Connect one node to another node or param.
(define (audio-node-connect node destination [output-index #f] [input-index #f])
  (check-audio-node 'audio-node-connect node)
  (unless (or (audio-node? destination) (audio-param? destination))
    (raise-argument-error 'audio-node-connect "(or/c audio-node? audio-param?)" destination))
  (check-audio-optional-number 'audio-node-connect output-index)
  (check-audio-optional-number 'audio-node-connect input-index)
  (js-audio-node-connect node
                         destination
                         (if output-index output-index (void))
                         (if input-index input-index (void))))

;; audio-node-disconnect : audio-node? [or/c audio-node? audio-param?] [exact-integer?] [exact-integer?] -> void?
;;   Disconnect a node from the graph.
(define (audio-node-disconnect node [destination #f] [output-index #f] [input-index #f])
  (check-audio-node 'audio-node-disconnect node)
  (unless (or (eq? destination #f) (audio-node? destination) (audio-param? destination))
    (raise-argument-error 'audio-node-disconnect "(or/c #f audio-node? audio-param?)" destination))
  (check-audio-optional-number 'audio-node-disconnect output-index)
  (check-audio-optional-number 'audio-node-disconnect input-index)
  (js-audio-node-disconnect node
                            (if destination destination (void))
                            (if output-index output-index (void))
                            (if input-index input-index (void))))

;; audio-param-value : audio-param? -> real?
;;   Read the current value of an AudioParam.
(define (audio-param-value param)
  (check-audio-param 'audio-param-value param)
  (js-audio-param-value param))

;; audio-param-set-value! : audio-param? real? -> void?
;;   Set the current AudioParam value.
(define (audio-param-set-value! param value)
  (check-audio-param 'audio-param-set-value! param)
  (unless (real? value)
    (raise-argument-error 'audio-param-set-value! "real?" value))
  (js-audio-param-set-value! param value))

;; audio-param-set-value-at-time! : audio-param? real? real? -> void?
;;   Schedule a value change at a given time.
(define (audio-param-set-value-at-time! param value time)
  (check-audio-param 'audio-param-set-value-at-time! param)
  (unless (real? value)
    (raise-argument-error 'audio-param-set-value-at-time! "real?" value))
  (unless (real? time)
    (raise-argument-error 'audio-param-set-value-at-time! "real?" time))
  (js-audio-param-set-value-at-time! param value time))

;; audio-param-linear-ramp-to-value-at-time! : audio-param? real? real? -> void?
;;   Schedule a linear ramp.
(define (audio-param-linear-ramp-to-value-at-time! param value time)
  (check-audio-param 'audio-param-linear-ramp-to-value-at-time! param)
  (unless (real? value)
    (raise-argument-error 'audio-param-linear-ramp-to-value-at-time! "real?" value))
  (unless (real? time)
    (raise-argument-error 'audio-param-linear-ramp-to-value-at-time! "real?" time))
  (js-audio-param-linear-ramp-to-value-at-time! param value time))

;; audio-param-exponential-ramp-to-value-at-time! : audio-param? real? real? -> void?
;;   Schedule an exponential ramp.
(define (audio-param-exponential-ramp-to-value-at-time! param value time)
  (check-audio-param 'audio-param-exponential-ramp-to-value-at-time! param)
  (unless (real? value)
    (raise-argument-error 'audio-param-exponential-ramp-to-value-at-time! "real?" value))
  (unless (real? time)
    (raise-argument-error 'audio-param-exponential-ramp-to-value-at-time! "real?" time))
  (js-audio-param-exponential-ramp-to-value-at-time! param value time))

;; audio-param-set-target-at-time! : audio-param? real? real? real? -> void?
;;   Schedule a target value.
(define (audio-param-set-target-at-time! param value time time-constant)
  (check-audio-param 'audio-param-set-target-at-time! param)
  (for-each (lambda (x)
              (unless (real? x)
                (raise-argument-error 'audio-param-set-target-at-time! "real?" x)))
            (list value time time-constant))
  (js-audio-param-set-target-at-time! param value time time-constant))

;; audio-param-set-value-curve-at-time! : audio-param? vector? real? real? -> void?
;;   Schedule a value curve.
(define (audio-param-set-value-curve-at-time! param values start-time duration)
  (check-audio-param 'audio-param-set-value-curve-at-time! param)
  (check-audio-float-vector 'audio-param-set-value-curve-at-time! values)
  (for-each (lambda (x)
              (unless (real? x)
                (raise-argument-error 'audio-param-set-value-curve-at-time! "real?" x)))
            (list start-time duration))
  (js-audio-param-set-value-curve-at-time! param
                                           (audio-vector->js-float32-array values)
                                           start-time
                                           duration))

;; audio-param-cancel-scheduled-values! : audio-param? real? -> void?
;;   Cancel scheduled values from a time onward.
(define (audio-param-cancel-scheduled-values! param time)
  (check-audio-param 'audio-param-cancel-scheduled-values! param)
  (unless (real? time)
    (raise-argument-error 'audio-param-cancel-scheduled-values! "real?" time))
  (js-audio-param-cancel-scheduled-values! param time))

;; audio-param-cancel-and-hold-at-time! : audio-param? real? -> void?
;;   Cancel scheduled values and hold the current value.
(define (audio-param-cancel-and-hold-at-time! param time)
  (check-audio-param 'audio-param-cancel-and-hold-at-time! param)
  (unless (real? time)
    (raise-argument-error 'audio-param-cancel-and-hold-at-time! "real?" time))
  (js-audio-param-cancel-and-hold-at-time! param time))

;; audio-buffer-length : audio-buffer? -> exact-nonnegative-integer?
;;   Read the AudioBuffer length.
(define (audio-buffer-length buffer)
  (check-audio-buffer 'audio-buffer-length buffer)
  (js-audio-buffer-length buffer))

;; audio-buffer-duration : audio-buffer? -> real?
;;   Read the AudioBuffer duration.
(define (audio-buffer-duration buffer)
  (check-audio-buffer 'audio-buffer-duration buffer)
  (js-audio-buffer-duration buffer))

;; audio-buffer-sample-rate : audio-buffer? -> real?
;;   Read the AudioBuffer sample rate.
(define (audio-buffer-sample-rate buffer)
  (check-audio-buffer 'audio-buffer-sample-rate buffer)
  (js-audio-buffer-sample-rate buffer))

;; audio-buffer-number-of-channels : audio-buffer? -> exact-nonnegative-integer?
;;   Read the AudioBuffer channel count.
(define (audio-buffer-number-of-channels buffer)
  (check-audio-buffer 'audio-buffer-number-of-channels buffer)
  (js-audio-buffer-number-of-channels buffer))

;; audio-buffer-get-channel-data : audio-buffer? exact-integer? -> vector?
;;   Read a channel's sample data as a WebRacket vector of numbers.
(define (audio-buffer-get-channel-data buffer channel)
  (check-audio-buffer 'audio-buffer-get-channel-data buffer)
  (unless (exact-integer? channel)
    (raise-argument-error 'audio-buffer-get-channel-data "exact-integer?" channel))
  (array-like->vector 'audio-buffer-get-channel-data
                      (js-audio-buffer-get-channel-data buffer channel)
                      (lambda (value) value)))

;; audio-gain-node-gain : audio-gain-node? -> audio-param?
;;   Read the GainNode gain parameter.
(define (audio-gain-node-gain node)
  (check-audio-gain-node 'audio-gain-node-gain node)
  (js-audio-gain-node-gain node))

;; audio-oscillator-node-type : audio-oscillator-node? -> string?
;;   Read the OscillatorNode waveform type.
(define (audio-oscillator-node-type node)
  (check-audio-oscillator-node 'audio-oscillator-node-type node)
  (js-audio-oscillator-node-type node))

;; audio-oscillator-node-set-type! : audio-oscillator-node? (or/c string? symbol?) -> void?
;;   Set the OscillatorNode waveform type.
(define (audio-oscillator-node-set-type! node type)
  (check-audio-oscillator-node 'audio-oscillator-node-set-type! node)
  (define type* (normalize-audio-stringish 'audio-oscillator-node-set-type! type))
  (js-audio-oscillator-node-set-type! node type*))

;; audio-oscillator-node-frequency : audio-oscillator-node? -> audio-param?
;;   Read the OscillatorNode frequency parameter.
(define (audio-oscillator-node-frequency node)
  (check-audio-oscillator-node 'audio-oscillator-node-frequency node)
  (js-audio-oscillator-node-frequency node))

;; audio-oscillator-node-detune : audio-oscillator-node? -> audio-param?
;;   Read the OscillatorNode detune parameter.
(define (audio-oscillator-node-detune node)
  (check-audio-oscillator-node 'audio-oscillator-node-detune node)
  (js-audio-oscillator-node-detune node))

;; audio-oscillator-node-start! : audio-oscillator-node? [real?] -> void?
;;   Start the oscillator.
(define (audio-oscillator-node-start! node [when #f])
  (check-audio-oscillator-node 'audio-oscillator-node-start! node)
  (check-audio-optional-time 'audio-oscillator-node-start! when)
  (js-audio-oscillator-node-start! node (if when when (void))))

;; audio-oscillator-node-stop! : audio-oscillator-node? [real?] -> void?
;;   Stop the oscillator.
(define (audio-oscillator-node-stop! node [when #f])
  (check-audio-oscillator-node 'audio-oscillator-node-stop! node)
  (check-audio-optional-time 'audio-oscillator-node-stop! when)
  (js-audio-oscillator-node-stop! node (if when when (void))))

;; audio-oscillator-node-set-periodic-wave! : audio-oscillator-node? (or/c external? audio-periodic-wave?) -> void?
;;   Set a periodic wave on the oscillator.
(define (audio-oscillator-node-set-periodic-wave! node wave)
  (check-audio-oscillator-node 'audio-oscillator-node-set-periodic-wave! node)
  (unless (or (external? wave) (audio-periodic-wave? wave))
    (raise-argument-error 'audio-oscillator-node-set-periodic-wave! "(or/c external? audio-periodic-wave?)" wave))
  (js-audio-oscillator-node-set-periodic-wave! node
                                               (if (audio-periodic-wave? wave)
                                                   (audio-periodic-wave-raw wave)
                                                   wave)))

;; audio-buffer-source-node-buffer : audio-buffer-source-node? -> (or/c #f audio-buffer?)
;;   Read the current buffer.
(define (audio-buffer-source-node-buffer node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-buffer node)
  (js-audio-audio-buffer-source-node-buffer node))

;; audio-buffer-source-node-set-buffer! : audio-buffer-source-node? (or/c #f audio-buffer?) -> void?
;;   Set the current buffer.
(define (audio-buffer-source-node-set-buffer! node buffer)
  (check-audio-buffer-source-node 'audio-buffer-source-node-set-buffer! node)
  (unless (or (eq? buffer #f) (audio-buffer? buffer))
    (raise-argument-error 'audio-buffer-source-node-set-buffer! "(or/c #f audio-buffer?)" buffer))
  (js-audio-audio-buffer-source-node-set-buffer! node (if buffer buffer (js-null))))

;; audio-buffer-source-node-playback-rate : audio-buffer-source-node? -> audio-param?
;;   Read the playbackRate parameter.
(define (audio-buffer-source-node-playback-rate node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-playback-rate node)
  (js-audio-audio-buffer-source-node-playback-rate node))

;; audio-buffer-source-node-detune : audio-buffer-source-node? -> audio-param?
;;   Read the detune parameter.
(define (audio-buffer-source-node-detune node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-detune node)
  (js-audio-audio-buffer-source-node-detune node))

;; audio-buffer-source-node-loop : audio-buffer-source-node? -> boolean?
;;   Read the loop flag.
(define (audio-buffer-source-node-loop node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-loop node)
  (js-audio-audio-buffer-source-node-loop node))

;; audio-buffer-source-node-set-loop! : audio-buffer-source-node? boolean? -> void?
;;   Set the loop flag.
(define (audio-buffer-source-node-set-loop! node flag)
  (check-audio-buffer-source-node 'audio-buffer-source-node-set-loop! node)
  (unless (boolean? flag)
    (raise-argument-error 'audio-buffer-source-node-set-loop! "boolean?" flag))
  (js-audio-audio-buffer-source-node-set-loop! node flag))

;; audio-buffer-source-node-loop-start : audio-buffer-source-node? -> real?
;;   Read the loop start time.
(define (audio-buffer-source-node-loop-start node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-loop-start node)
  (js-audio-audio-buffer-source-node-loop-start node))

;; audio-buffer-source-node-set-loop-start! : audio-buffer-source-node? real? -> void?
;;   Set the loop start time.
(define (audio-buffer-source-node-set-loop-start! node value)
  (check-audio-buffer-source-node 'audio-buffer-source-node-set-loop-start! node)
  (unless (real? value)
    (raise-argument-error 'audio-buffer-source-node-set-loop-start! "real?" value))
  (js-audio-audio-buffer-source-node-set-loop-start! node value))

;; audio-buffer-source-node-loop-end : audio-buffer-source-node? -> real?
;;   Read the loop end time.
(define (audio-buffer-source-node-loop-end node)
  (check-audio-buffer-source-node 'audio-buffer-source-node-loop-end node)
  (js-audio-audio-buffer-source-node-loop-end node))

;; audio-buffer-source-node-set-loop-end! : audio-buffer-source-node? real? -> void?
;;   Set the loop end time.
(define (audio-buffer-source-node-set-loop-end! node value)
  (check-audio-buffer-source-node 'audio-buffer-source-node-set-loop-end! node)
  (unless (real? value)
    (raise-argument-error 'audio-buffer-source-node-set-loop-end! "real?" value))
  (js-audio-audio-buffer-source-node-set-loop-end! node value))

;; audio-buffer-source-node-start! : audio-buffer-source-node? [real?] [real?] [real?] -> void?
;;   Start the buffer source.
(define (audio-buffer-source-node-start! node [when #f] [offset #f] [duration #f])
  (check-audio-buffer-source-node 'audio-buffer-source-node-start! node)
  (check-audio-optional-time 'audio-buffer-source-node-start! when)
  (check-audio-optional-number 'audio-buffer-source-node-start! offset)
  (check-audio-optional-number 'audio-buffer-source-node-start! duration)
  (js-audio-audio-buffer-source-node-start! node
                                            (if when when (void))
                                            (if offset offset (void))
                                            (if duration duration (void))))

;; audio-buffer-source-node-stop! : audio-buffer-source-node? [real?] -> void?
;;   Stop the buffer source.
(define (audio-buffer-source-node-stop! node [when #f])
  (check-audio-buffer-source-node 'audio-buffer-source-node-stop! node)
  (check-audio-optional-time 'audio-buffer-source-node-stop! when)
  (js-audio-audio-buffer-source-node-stop! node (if when when (void))))

;; audio-analyser-node-fft-size : audio-analyser-node? -> exact-nonnegative-integer?
;;   Read the FFT size.
(define (audio-analyser-node-fft-size node)
  (check-audio-analyser-node 'audio-analyser-node-fft-size node)
  (js-audio-analyser-node-fft-size node))

;; audio-analyser-node-set-fft-size! : audio-analyser-node? exact-integer? -> void?
;;   Set the FFT size.
(define (audio-analyser-node-set-fft-size! node size)
  (check-audio-analyser-node 'audio-analyser-node-set-fft-size! node)
  (unless (exact-integer? size)
    (raise-argument-error 'audio-analyser-node-set-fft-size! "exact-integer?" size))
  (js-audio-analyser-node-set-fft-size! node size))

;; audio-analyser-node-frequency-bin-count : audio-analyser-node? -> exact-nonnegative-integer?
;;   Read the frequency bin count.
(define (audio-analyser-node-frequency-bin-count node)
  (check-audio-analyser-node 'audio-analyser-node-frequency-bin-count node)
  (js-audio-analyser-node-frequency-bin-count node))

;; audio-analyser-node-min-decibels : audio-analyser-node? -> real?
;;   Read the minimum decibels value.
(define (audio-analyser-node-min-decibels node)
  (check-audio-analyser-node 'audio-analyser-node-min-decibels node)
  (js-audio-analyser-node-min-decibels node))

;; audio-analyser-node-set-min-decibels! : audio-analyser-node? real? -> void?
;;   Set the minimum decibels value.
(define (audio-analyser-node-set-min-decibels! node value)
  (check-audio-analyser-node 'audio-analyser-node-set-min-decibels! node)
  (unless (real? value)
    (raise-argument-error 'audio-analyser-node-set-min-decibels! "real?" value))
  (js-audio-analyser-node-set-min-decibels! node value))

;; audio-analyser-node-max-decibels : audio-analyser-node? -> real?
;;   Read the maximum decibels value.
(define (audio-analyser-node-max-decibels node)
  (check-audio-analyser-node 'audio-analyser-node-max-decibels node)
  (js-audio-analyser-node-max-decibels node))

;; audio-analyser-node-set-max-decibels! : audio-analyser-node? real? -> void?
;;   Set the maximum decibels value.
(define (audio-analyser-node-set-max-decibels! node value)
  (check-audio-analyser-node 'audio-analyser-node-set-max-decibels! node)
  (unless (real? value)
    (raise-argument-error 'audio-analyser-node-set-max-decibels! "real?" value))
  (js-audio-analyser-node-set-max-decibels! node value))

;; audio-analyser-node-smoothing-time-constant : audio-analyser-node? -> real?
;;   Read the smoothing time constant.
(define (audio-analyser-node-smoothing-time-constant node)
  (check-audio-analyser-node 'audio-analyser-node-smoothing-time-constant node)
  (js-audio-analyser-node-smoothing-time-constant node))

;; audio-analyser-node-set-smoothing-time-constant! : audio-analyser-node? real? -> void?
;;   Set the smoothing time constant.
(define (audio-analyser-node-set-smoothing-time-constant! node value)
  (check-audio-analyser-node 'audio-analyser-node-set-smoothing-time-constant! node)
  (unless (real? value)
    (raise-argument-error 'audio-analyser-node-set-smoothing-time-constant! "real?" value))
  (js-audio-analyser-node-set-smoothing-time-constant! node value))

;; audio-analyser-node-get-byte-frequency-data! : audio-analyser-node? (or/c bytes? vector?) -> void?
;;   Fill a byte frequency buffer.
(define (audio-analyser-node-get-byte-frequency-data! node data)
  (check-audio-analyser-node 'audio-analyser-node-get-byte-frequency-data! node)
  (check-audio-byte-buffer 'audio-analyser-node-get-byte-frequency-data! data)
  (define js-data (audio-make-js-uint8-array (if (bytes? data)
                                                 (bytes-length data)
                                                 (vector-length data))))
  (js-audio-analyser-node-get-byte-frequency-data! node js-data)
  (cond
    [(bytes? data) (audio-copy-js-array-into-bytes! 'audio-analyser-node-get-byte-frequency-data! js-data data)]
    [else (audio-copy-js-array-into-vector! 'audio-analyser-node-get-byte-frequency-data! js-data data)]))

;; audio-analyser-node-get-byte-time-domain-data! : audio-analyser-node? (or/c bytes? vector?) -> void?
;;   Fill a byte time-domain buffer.
(define (audio-analyser-node-get-byte-time-domain-data! node data)
  (check-audio-analyser-node 'audio-analyser-node-get-byte-time-domain-data! node)
  (check-audio-byte-buffer 'audio-analyser-node-get-byte-time-domain-data! data)
  (define js-data (audio-make-js-uint8-array (if (bytes? data)
                                                 (bytes-length data)
                                                 (vector-length data))))
  (js-audio-analyser-node-get-byte-time-domain-data! node js-data)
  (cond
    [(bytes? data) (audio-copy-js-array-into-bytes! 'audio-analyser-node-get-byte-time-domain-data! js-data data)]
    [else (audio-copy-js-array-into-vector! 'audio-analyser-node-get-byte-time-domain-data! js-data data)]))

;; audio-analyser-node-get-float-frequency-data! : audio-analyser-node? vector? -> void?
;;   Fill a float frequency buffer.
(define (audio-analyser-node-get-float-frequency-data! node data)
  (check-audio-analyser-node 'audio-analyser-node-get-float-frequency-data! node)
  (check-audio-float-vector 'audio-analyser-node-get-float-frequency-data! data)
  (define js-data (audio-make-js-float32-array (vector-length data)))
  (js-audio-analyser-node-get-float-frequency-data! node js-data)
  (audio-copy-js-array-into-vector! 'audio-analyser-node-get-float-frequency-data! js-data data))

;; audio-analyser-node-get-float-time-domain-data! : audio-analyser-node? vector? -> void?
;;   Fill a float time-domain buffer.
(define (audio-analyser-node-get-float-time-domain-data! node data)
  (check-audio-analyser-node 'audio-analyser-node-get-float-time-domain-data! node)
  (check-audio-float-vector 'audio-analyser-node-get-float-time-domain-data! data)
  (define js-data (audio-make-js-float32-array (vector-length data)))
  (js-audio-analyser-node-get-float-time-domain-data! node js-data)
  (audio-copy-js-array-into-vector! 'audio-analyser-node-get-float-time-domain-data! js-data data))

;; audio-biquad-filter-node-type : audio-biquad-filter-node? -> string?
;;   Read the filter type string.
(define (audio-biquad-filter-node-type node)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-type node)
  (js-audio-biquad-filter-node-type node))

;; audio-biquad-filter-node-set-type! : audio-biquad-filter-node? (or/c string? symbol?) -> void?
;;   Set the filter type string.
(define (audio-biquad-filter-node-set-type! node type)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-set-type! node)
  (define type* (normalize-audio-stringish 'audio-biquad-filter-node-set-type! type))
  (js-audio-biquad-filter-node-set-type! node type*))

;; audio-biquad-filter-node-frequency : audio-biquad-filter-node? -> audio-param?
;;   Read the frequency parameter.
(define (audio-biquad-filter-node-frequency node)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-frequency node)
  (js-audio-biquad-filter-node-frequency node))

;; audio-biquad-filter-node-detune : audio-biquad-filter-node? -> audio-param?
;;   Read the detune parameter.
(define (audio-biquad-filter-node-detune node)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-detune node)
  (js-audio-biquad-filter-node-detune node))

;; audio-biquad-filter-node-q : audio-biquad-filter-node? -> audio-param?
;;   Read the Q parameter.
(define (audio-biquad-filter-node-q node)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-q node)
  (js-audio-biquad-filter-node-q node))

;; audio-biquad-filter-node-gain : audio-biquad-filter-node? -> audio-param?
;;   Read the gain parameter.
(define (audio-biquad-filter-node-gain node)
  (check-audio-biquad-filter-node 'audio-biquad-filter-node-gain node)
  (js-audio-biquad-filter-node-gain node))

;; audio-constant-source-node-offset : audio-constant-source-node? -> audio-param?
;;   Read the offset parameter.
(define (audio-constant-source-node-offset node)
  (check-audio-constant-source-node 'audio-constant-source-node-offset node)
  (js-audio-constant-source-node-offset node))

;; audio-constant-source-node-start! : audio-constant-source-node? [real?] -> void?
;;   Start the constant source node.
(define (audio-constant-source-node-start! node [when #f])
  (check-audio-constant-source-node 'audio-constant-source-node-start! node)
  (check-audio-optional-time 'audio-constant-source-node-start! when)
  (js-audio-constant-source-node-start! node (if when when (void))))

;; audio-constant-source-node-stop! : audio-constant-source-node? [real?] -> void?
;;   Stop the constant source node.
(define (audio-constant-source-node-stop! node [when #f])
  (check-audio-constant-source-node 'audio-constant-source-node-stop! node)
  (check-audio-optional-time 'audio-constant-source-node-stop! when)
  (js-audio-constant-source-node-stop! node (if when when (void))))

;; audio-dynamics-compressor-node-threshold : audio-dynamics-compressor-node? -> audio-param?
;;   Read the threshold parameter.
(define (audio-dynamics-compressor-node-threshold node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-threshold node)
  (js-audio-dynamics-compressor-node-threshold node))

;; audio-dynamics-compressor-node-knee : audio-dynamics-compressor-node? -> audio-param?
;;   Read the knee parameter.
(define (audio-dynamics-compressor-node-knee node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-knee node)
  (js-audio-dynamics-compressor-node-knee node))

;; audio-dynamics-compressor-node-ratio : audio-dynamics-compressor-node? -> audio-param?
;;   Read the ratio parameter.
(define (audio-dynamics-compressor-node-ratio node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-ratio node)
  (js-audio-dynamics-compressor-node-ratio node))

;; audio-dynamics-compressor-node-reduction : audio-dynamics-compressor-node? -> real?
;;   Read the reduction value.
(define (audio-dynamics-compressor-node-reduction node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-reduction node)
  (js-audio-dynamics-compressor-node-reduction node))

;; audio-dynamics-compressor-node-attack : audio-dynamics-compressor-node? -> audio-param?
;;   Read the attack parameter.
(define (audio-dynamics-compressor-node-attack node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-attack node)
  (js-audio-dynamics-compressor-node-attack node))

;; audio-dynamics-compressor-node-release : audio-dynamics-compressor-node? -> audio-param?
;;   Read the release parameter.
(define (audio-dynamics-compressor-node-release node)
  (check-audio-dynamics-compressor-node 'audio-dynamics-compressor-node-release node)
  (js-audio-dynamics-compressor-node-release node))

;; audio-panner-node-panning-model : audio-panner-node? -> string?
;;   Read the panning model.
(define (audio-panner-node-panning-model node)
  (check-audio-panner-node 'audio-panner-node-panning-model node)
  (js-audio-panner-node-panning-model node))

;; audio-panner-node-set-panning-model! : audio-panner-node? (or/c string? symbol?) -> void?
;;   Set the panning model.
(define (audio-panner-node-set-panning-model! node value)
  (check-audio-panner-node 'audio-panner-node-set-panning-model! node)
  (define value* (normalize-audio-stringish 'audio-panner-node-set-panning-model! value))
  (js-audio-panner-node-set-panning-model! node value*))

;; audio-panner-node-distance-model : audio-panner-node? -> string?
;;   Read the distance model.
(define (audio-panner-node-distance-model node)
  (check-audio-panner-node 'audio-panner-node-distance-model node)
  (js-audio-panner-node-distance-model node))

;; audio-panner-node-set-distance-model! : audio-panner-node? (or/c string? symbol?) -> void?
;;   Set the distance model.
(define (audio-panner-node-set-distance-model! node value)
  (check-audio-panner-node 'audio-panner-node-set-distance-model! node)
  (define value* (normalize-audio-stringish 'audio-panner-node-set-distance-model! value))
  (js-audio-panner-node-set-distance-model! node value*))

;; audio-panner-node-position-x : audio-panner-node? -> audio-param?
;;   Read the positionX parameter.
(define (audio-panner-node-position-x node)
  (check-audio-panner-node 'audio-panner-node-position-x node)
  (js-audio-panner-node-position-x node))

;; audio-panner-node-position-y : audio-panner-node? -> audio-param?
;;   Read the positionY parameter.
(define (audio-panner-node-position-y node)
  (check-audio-panner-node 'audio-panner-node-position-y node)
  (js-audio-panner-node-position-y node))

;; audio-panner-node-position-z : audio-panner-node? -> audio-param?
;;   Read the positionZ parameter.
(define (audio-panner-node-position-z node)
  (check-audio-panner-node 'audio-panner-node-position-z node)
  (js-audio-panner-node-position-z node))

;; audio-panner-node-orientation-x : audio-panner-node? -> audio-param?
;;   Read the orientationX parameter.
(define (audio-panner-node-orientation-x node)
  (check-audio-panner-node 'audio-panner-node-orientation-x node)
  (js-audio-panner-node-orientation-x node))

;; audio-panner-node-orientation-y : audio-panner-node? -> audio-param?
;;   Read the orientationY parameter.
(define (audio-panner-node-orientation-y node)
  (check-audio-panner-node 'audio-panner-node-orientation-y node)
  (js-audio-panner-node-orientation-y node))

;; audio-panner-node-orientation-z : audio-panner-node? -> audio-param?
;;   Read the orientationZ parameter.
(define (audio-panner-node-orientation-z node)
  (check-audio-panner-node 'audio-panner-node-orientation-z node)
  (js-audio-panner-node-orientation-z node))

;; audio-panner-node-ref-distance : audio-panner-node? -> real?
;;   Read the refDistance value.
(define (audio-panner-node-ref-distance node)
  (check-audio-panner-node 'audio-panner-node-ref-distance node)
  (js-audio-panner-node-ref-distance node))

;; audio-panner-node-set-ref-distance! : audio-panner-node? real? -> void?
;;   Set the refDistance value.
(define (audio-panner-node-set-ref-distance! node value)
  (check-audio-panner-node 'audio-panner-node-set-ref-distance! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-ref-distance! "real?" value))
  (js-audio-panner-node-set-ref-distance! node value))

;; audio-panner-node-max-distance : audio-panner-node? -> real?
;;   Read the maxDistance value.
(define (audio-panner-node-max-distance node)
  (check-audio-panner-node 'audio-panner-node-max-distance node)
  (js-audio-panner-node-max-distance node))

;; audio-panner-node-set-max-distance! : audio-panner-node? real? -> void?
;;   Set the maxDistance value.
(define (audio-panner-node-set-max-distance! node value)
  (check-audio-panner-node 'audio-panner-node-set-max-distance! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-max-distance! "real?" value))
  (js-audio-panner-node-set-max-distance! node value))

;; audio-panner-node-rolloff-factor : audio-panner-node? -> real?
;;   Read the rolloffFactor value.
(define (audio-panner-node-rolloff-factor node)
  (check-audio-panner-node 'audio-panner-node-rolloff-factor node)
  (js-audio-panner-node-rolloff-factor node))

;; audio-panner-node-set-rolloff-factor! : audio-panner-node? real? -> void?
;;   Set the rolloffFactor value.
(define (audio-panner-node-set-rolloff-factor! node value)
  (check-audio-panner-node 'audio-panner-node-set-rolloff-factor! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-rolloff-factor! "real?" value))
  (js-audio-panner-node-set-rolloff-factor! node value))

;; audio-panner-node-cone-inner-angle : audio-panner-node? -> real?
;;   Read the coneInnerAngle value.
(define (audio-panner-node-cone-inner-angle node)
  (check-audio-panner-node 'audio-panner-node-cone-inner-angle node)
  (js-audio-panner-node-cone-inner-angle node))

;; audio-panner-node-set-cone-inner-angle! : audio-panner-node? real? -> void?
;;   Set the coneInnerAngle value.
(define (audio-panner-node-set-cone-inner-angle! node value)
  (check-audio-panner-node 'audio-panner-node-set-cone-inner-angle! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-cone-inner-angle! "real?" value))
  (js-audio-panner-node-set-cone-inner-angle! node value))

;; audio-panner-node-cone-outer-angle : audio-panner-node? -> real?
;;   Read the coneOuterAngle value.
(define (audio-panner-node-cone-outer-angle node)
  (check-audio-panner-node 'audio-panner-node-cone-outer-angle node)
  (js-audio-panner-node-cone-outer-angle node))

;; audio-panner-node-set-cone-outer-angle! : audio-panner-node? real? -> void?
;;   Set the coneOuterAngle value.
(define (audio-panner-node-set-cone-outer-angle! node value)
  (check-audio-panner-node 'audio-panner-node-set-cone-outer-angle! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-cone-outer-angle! "real?" value))
  (js-audio-panner-node-set-cone-outer-angle! node value))

;; audio-panner-node-cone-outer-gain : audio-panner-node? -> real?
;;   Read the coneOuterGain value.
(define (audio-panner-node-cone-outer-gain node)
  (check-audio-panner-node 'audio-panner-node-cone-outer-gain node)
  (js-audio-panner-node-cone-outer-gain node))

;; audio-panner-node-set-cone-outer-gain! : audio-panner-node? real? -> void?
;;   Set the coneOuterGain value.
(define (audio-panner-node-set-cone-outer-gain! node value)
  (check-audio-panner-node 'audio-panner-node-set-cone-outer-gain! node)
  (unless (real? value)
    (raise-argument-error 'audio-panner-node-set-cone-outer-gain! "real?" value))
  (js-audio-panner-node-set-cone-outer-gain! node value))

;; audio-stereo-panner-node-pan : audio-stereo-panner-node? -> audio-param?
;;   Read the pan parameter.
(define (audio-stereo-panner-node-pan node)
  (check-audio-stereo-panner-node 'audio-stereo-panner-node-pan node)
  (js-audio-stereo-panner-node-pan node))

;; audio-context-onstatechange! : audio-context? (or/c #f procedure? external?) -> void?
;;   Install or clear the AudioContext onstatechange handler.
(define (audio-context-onstatechange! ctx handler)
  (audio-set-handler!/context 'audio-context-onstatechange! ctx "onstatechange" handler))

;; audio-oscillator-node-onended! : audio-oscillator-node? (or/c #f procedure? external?) -> void?
;;   Install or clear the oscillator onended handler.
(define (audio-oscillator-node-onended! node handler)
  (audio-set-handler! 'audio-oscillator-node-onended! node "onended" handler))

;; audio-buffer-source-node-onended! : audio-buffer-source-node? (or/c #f procedure? external?) -> void?
;;   Install or clear the buffer source onended handler.
(define (audio-buffer-source-node-onended! node handler)
  (audio-set-handler! 'audio-buffer-source-node-onended! node "onended" handler))

;; audio-constant-source-node-onended! : audio-constant-source-node? (or/c #f procedure? external?) -> void?
;;   Install or clear the constant source onended handler.
(define (audio-constant-source-node-onended! node handler)
  (audio-set-handler! 'audio-constant-source-node-onended! node "onended" handler))
