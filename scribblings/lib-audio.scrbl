#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/audio-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[audio]}
@declare-exporting[(lib "scribblings/audio-labels.rkt" "webracket")]

@(how-to-require include-lib audio (lib "libs/audio.rkt"))
@(compile-option-bar "Compile option: " "--ffi audio")

Web Audio lets you build sound graphs directly in the page. Instead of
only playing back files, you can create oscillator sources, route
signals through filters and gain nodes, and inspect the result with
analysers.

Use audio when you want to:

@itemlist[
  @item{synthesizers and tone generators}
  @item{audio visualizers}
  @item{music tools and loopers}
  @item{low-latency signal routing}
]

The @racket[audio] library is the checked, high-level API for Web Audio
programs in WebRacket. It sits on top of the lower-level
@tt{ffi/audio.ffi} bindings, but application code should normally use
the @racket[audio-*] functions documented on this page.

String-like audio arguments accept either strings or symbols, and the
wrapper converts symbols to their string names. Optional arguments use
@racket[#f] to mean that the argument is omitted.

The @racket[audio-context-listener] result is wrapped as an
@racket[audio-listener] value. The raw listener accessor is listed in
the @seclink["raw-accessors"]{Raw Accessors} appendix.

@section{Audio Quick Start}

To use the audio API, create a context, create a source node, connect it
to the destination, and start it.

@racketblock[
(code:comment "Include the audio library.")
(include-lib audio)

(code:comment "Create an audio context.")
(define ctx (audio-context-new))

(code:comment "Create an oscillator node.")
(define osc (audio-context-create-oscillator ctx))

(code:comment "Set the oscillator shape, connect it, and start it.")
(audio-oscillator-node-set-type! osc 'sine)
(audio-node-connect osc (audio-context-destination ctx))
(audio-oscillator-node-start! osc)
]

@section{Audio API Reference}

@subsection{Audio Validation}

@defproc[(audio-context? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AudioContext value.
}

@defproc[(audio-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AudioNode value.
}

@defproc[(audio-param? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AudioParam value.
}

@defproc[(audio-buffer? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AudioBuffer value.
}

@defproc[(audio-gain-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a GainNode value.
}

@defproc[(audio-oscillator-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an OscillatorNode value.
}

@defproc[(audio-buffer-source-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AudioBufferSourceNode value.
}

@defproc[(audio-analyser-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is an AnalyserNode value.
}

@defproc[(audio-biquad-filter-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a BiquadFilterNode value.
}

@defproc[(audio-constant-source-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a ConstantSourceNode value.
}

@defproc[(audio-dynamics-compressor-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DynamicsCompressorNode value.
}

@defproc[(audio-panner-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a PannerNode value.
}

@defproc[(audio-stereo-panner-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a StereoPannerNode value.
}

@defproc[(audio-channel-splitter-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a ChannelSplitterNode value.
}

@defproc[(audio-channel-merger-node? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a ChannelMergerNode value.
}

@subsection{Audio Context}

@defproc[(audio-context-new) audio-context?]{
@(mdn-bar "AudioContext() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/AudioContext")

Creates a new audio context with the browser defaults.
}

@defproc[(audio-context-close [ctx audio-context?]) external?]{
@(mdn-bar "AudioContext: close() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/close")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Closes the context and returns the browser promise.
}

@defproc[(audio-context-resume [ctx audio-context?]) external?]{
@(mdn-bar "AudioContext: resume() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/resume")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Resumes a suspended context and returns the browser promise.
}

@defproc[(audio-context-suspend [ctx audio-context?]) external?]{
@(mdn-bar "AudioContext: suspend() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/suspend")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Suspends the context and returns the browser promise.
}

@defproc[(audio-context-state [ctx audio-context?]) string?]{
@(mdn-bar "AudioContext: state property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/state")

Returns the current browser state string.
}

@defproc[(audio-context-current-time [ctx audio-context?]) real?]{
@(mdn-bar "AudioContext: currentTime property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/currentTime")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the current time of the context clock.
}

@defproc[(audio-context-sample-rate [ctx audio-context?]) real?]{
@(mdn-bar "AudioContext: sampleRate property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/sampleRate")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the sample rate used by the context.
}

@defproc[(audio-context-base-latency [ctx audio-context?]) real?]{
@(mdn-bar "AudioContext: baseLatency property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/baseLatency")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the estimated base latency of the audio pipeline.
}

@defproc[(audio-context-output-latency [ctx audio-context?]) real?]{
@(mdn-bar "AudioContext: outputLatency property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/outputLatency")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the estimated output latency of the audio pipeline.
}

@defproc[(audio-context-destination [ctx audio-context?]) audio-node?]{
@(mdn-bar "AudioContext: destination property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/destination")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the final destination node for the context.
}

@defstruct[audio-listener ([raw external/raw])]{
@racket[audio-context-listener] returns a wrapped browser
@racketid[AudioListener] object. The raw browser object is stored in
@racket[raw].
}

@defproc[(audio-context-listener [ctx audio-context?]) audio-listener?]{
@(mdn-bar "AudioContext: listener property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/listener")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Returns the associated audio listener object as a wrapped
@racket[audio-listener] value.
}

@subsection{Audio Nodes and Params}

@defproc[(audio-context-create-gain [ctx audio-context?]) audio-gain-node?]{
@(mdn-bar "AudioContext: createGain() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createGain")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a gain node attached to @racket[ctx].
}

@defproc[(audio-context-create-oscillator [ctx audio-context?]) audio-oscillator-node?]{
@(mdn-bar "AudioContext: createOscillator() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createOscillator")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates an oscillator node attached to @racket[ctx].
}

@defproc[(audio-context-create-buffer-source [ctx audio-context?]) audio-buffer-source-node?]{
@(mdn-bar "AudioContext: createBufferSource() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createBufferSource")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a buffer source node attached to @racket[ctx].
}

@defproc[(audio-context-create-analyser [ctx audio-context?]) audio-analyser-node?]{
@(mdn-bar "AudioContext: createAnalyser() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createAnalyser")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates an analyser node attached to @racket[ctx].
}

@defproc[(audio-context-create-biquad-filter [ctx audio-context?]) audio-biquad-filter-node?]{
@(mdn-bar "AudioContext: createBiquadFilter() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createBiquadFilter")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a biquad filter node attached to @racket[ctx].
}

@defproc[(audio-context-create-constant-source [ctx audio-context?]) audio-constant-source-node?]{
@(mdn-bar "AudioContext: createConstantSource() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createConstantSource")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a constant source node attached to @racket[ctx].
}

@defproc[(audio-context-create-channel-splitter [ctx audio-context?]
                                                [channels exact-integer?])
         audio-channel-splitter-node?]{
@(mdn-bar "AudioContext: createChannelSplitter() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createChannelSplitter")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a channel splitter node attached to @racket[ctx].
}

@defproc[(audio-context-create-channel-merger [ctx audio-context?]
                                              [channels exact-integer?])
         audio-channel-merger-node?]{
@(mdn-bar "AudioContext: createChannelMerger() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createChannelMerger")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a channel merger node attached to @racket[ctx].
}

@defproc[(audio-context-create-dynamics-compressor [ctx audio-context?]) audio-dynamics-compressor-node?]{
@(mdn-bar "AudioContext: createDynamicsCompressor() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createDynamicsCompressor")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a dynamics compressor node attached to @racket[ctx].
}

@defproc[(audio-context-create-panner [ctx audio-context?]) audio-panner-node?]{
@(mdn-bar "AudioContext: createPanner() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createPanner")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a panner node attached to @racket[ctx].
}

@defproc[(audio-context-create-stereo-panner [ctx audio-context?]) audio-stereo-panner-node?]{
@(mdn-bar "AudioContext: createStereoPanner() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createStereoPanner")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates a stereo panner node attached to @racket[ctx].
}

@defproc[(audio-context-create-buffer [ctx audio-context?]
                                      [channels exact-integer?]
                                      [length exact-integer?]
                                      [sample-rate real?])
         audio-buffer?]{
@(mdn-bar "AudioContext: createBuffer() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createBuffer")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value.

Creates an audio buffer with the requested size.
}

@defstruct[audio-periodic-wave ([raw external/raw])]{
@racket[audio-context-create-periodic-wave] returns a wrapped browser
@racketid[PeriodicWave] value. The raw browser object is stored in
@racket[raw].
}

@defproc[(audio-context-create-periodic-wave [ctx audio-context?]
                                             [real vector?]
                                             [imag vector?])
         audio-periodic-wave?]{
@(mdn-bar "AudioContext: createPeriodicWave() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/createPeriodicWave")

The raw @racket[ctx] argument should be a browser @racketid[AudioContext]
value. The @racket[real] and @racket[imag] arguments should be vectors of
real numbers that become browser @racketid[Float32Array] values.

Creates a wrapped browser @racketid[PeriodicWave] value.
}

@defproc[(audio-context-decode-audio-data [ctx audio-context?]
                                          [data (or/c bytes? external?)])
         external?]{
@(mdn-bar "AudioContext: decodeAudioData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/decodeAudioData")

The raw @racket[ctx] argument should be a browser
@racketid[AudioContext] value. The @racket[data] argument should be
either Racket @racket[bytes] or a browser @racketid[ArrayBuffer] or
typed-array value.

Decodes audio data and returns the browser promise.
}

@defproc[(audio-node-connect [node audio-node?]
                             [destination (or/c audio-node? audio-param?)]
                             [output-index (or/c #f real?) #f]
                             [input-index (or/c #f real?) #f])
         external?]{
@(mdn-bar "AudioNode: connect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/connect")

The raw @racket[node] argument should be a browser @racketid[AudioNode]
value. The raw @racket[destination] argument should be either a browser
@racketid[AudioNode] or @racketid[AudioParam] value.

Connects @racket[node] to another node or parameter.
}

@defproc[(audio-node-disconnect [node audio-node?]
                                [destination (or/c #f audio-node? audio-param?) #f]
                                [output-index (or/c #f real?) #f]
                                [input-index (or/c #f real?) #f])
         void?]{
@(mdn-bar "AudioNode: disconnect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/disconnect")

The raw @racket[node] argument should be a browser @racketid[AudioNode]
value. The raw @racket[destination] argument, when supplied, should be
either a browser @racketid[AudioNode] or @racketid[AudioParam] value.

Disconnects @racket[node] from the current audio graph.
}

@defproc[(audio-node-context [node audio-node?]) audio-context?]{
@(mdn-bar "AudioNode: context property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/context")

The raw @racket[node] argument should be a browser @racketid[AudioNode]
value.

Returns the audio context that owns the node.
}

@defproc[(audio-param-value [param audio-param?]) real?]{
@(mdn-bar "AudioParam: value property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value")

The raw @racket[param] argument should be a browser @racketid[AudioParam]
value.

Returns the current parameter value.
}

@defproc[(audio-gain-node-gain [node audio-gain-node?]) audio-param?]{
@(mdn-bar "GainNode: gain property"
          "https://developer.mozilla.org/en-US/docs/Web/API/GainNode/gain")

The raw @racket[node] argument should be a browser @racketid[GainNode]
value.

Returns the gain parameter.
}

@defproc[(audio-oscillator-node-type [node audio-oscillator-node?]) string?]{
@(mdn-bar "OscillatorNode: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/type")

Returns the oscillator type string.
}

@defproc[(audio-oscillator-node-set-type! [node audio-oscillator-node?]
                                          [type (or/c string? symbol?)])
         void?]{
@(mdn-bar "OscillatorNode: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/type")

Sets the oscillator type string.
}

@defproc[(audio-oscillator-node-frequency [node audio-oscillator-node?]) audio-param?]{
@(mdn-bar "OscillatorNode: frequency property"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/frequency")

Returns the frequency parameter.
}

@defproc[(audio-oscillator-node-detune [node audio-oscillator-node?]) audio-param?]{
@(mdn-bar "OscillatorNode: detune property"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/detune")

Returns the detune parameter.
}

@defproc[(audio-oscillator-node-set-periodic-wave! [node audio-oscillator-node?]
                                                   [wave (or/c external? audio-periodic-wave?)])
         void?]{
@(mdn-bar "OscillatorNode: setPeriodicWave() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/setPeriodicWave")

The raw @racket[wave] argument should be a browser
@racketid[PeriodicWave] value, or a wrapped
@racket[audio-periodic-wave] value.

Sets the oscillator's periodic wave.
}

@defproc[(audio-buffer-source-node-buffer [node audio-buffer-source-node?])
         (or/c #f audio-buffer?)]{
@(mdn-bar "AudioBufferSourceNode: buffer property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/buffer")

Returns the current buffer, or @racket[#f] when no buffer is set.
}

@defproc[(audio-buffer-source-node-set-buffer! [node audio-buffer-source-node?]
                                                     [buffer (or/c #f audio-buffer?)])
         void?]{
@(mdn-bar "AudioBufferSourceNode: buffer property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/buffer")

Sets the current buffer, or clears it when @racket[buffer] is @racket[#f].
}

@defproc[(audio-buffer-source-node-playback-rate [node audio-buffer-source-node?]) audio-param?]{
@(mdn-bar "AudioBufferSourceNode: playbackRate property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/playbackRate")

Returns the playback-rate parameter.
}

@defproc[(audio-buffer-source-node-detune [node audio-buffer-source-node?]) audio-param?]{
@(mdn-bar "AudioBufferSourceNode: detune property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/detune")

Returns the detune parameter.
}

@defproc[(audio-buffer-source-node-loop [node audio-buffer-source-node?]) boolean?]{
@(mdn-bar "AudioBufferSourceNode: loop property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loop")

Returns whether looping is enabled.
}

@defproc[(audio-buffer-source-node-set-loop! [node audio-buffer-source-node?]
                                                   [flag boolean?])
         void?]{
@(mdn-bar "AudioBufferSourceNode: loop property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loop")

Enables or disables looping.
}

@defproc[(audio-buffer-source-node-loop-start [node audio-buffer-source-node?]) real?]{
@(mdn-bar "AudioBufferSourceNode: loopStart property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loopStart")

Returns the loop start time.
}

@defproc[(audio-buffer-source-node-set-loop-start! [node audio-buffer-source-node?]
                                                         [value real?])
         void?]{
@(mdn-bar "AudioBufferSourceNode: loopStart property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loopStart")

Sets the loop start time.
}

@defproc[(audio-buffer-source-node-loop-end [node audio-buffer-source-node?]) real?]{
@(mdn-bar "AudioBufferSourceNode: loopEnd property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loopEnd")

Returns the loop end time.
}

@defproc[(audio-buffer-source-node-set-loop-end! [node audio-buffer-source-node?]
                                                       [value real?])
         void?]{
@(mdn-bar "AudioBufferSourceNode: loopEnd property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/loopEnd")

Sets the loop end time.
}

@defproc[(audio-param-set-value! [param audio-param?] [value real?]) void?]{
@(mdn-bar "AudioParam: value property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value")

Sets the current parameter value.
}

@defproc[(audio-param-set-value-at-time! [param audio-param?]
                                         [value real?]
                                         [time real?])
         void?]{
@(mdn-bar "AudioParam: setValueAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueAtTime")

Schedules a value change at the given time.
}

@defproc[(audio-param-linear-ramp-to-value-at-time! [param audio-param?]
                                                    [value real?]
                                                    [time real?])
         void?]{
@(mdn-bar "AudioParam: linearRampToValueAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/linearRampToValueAtTime")

Schedules a linear ramp.
}

@defproc[(audio-param-exponential-ramp-to-value-at-time! [param audio-param?]
                                                         [value real?]
                                                         [time real?])
         void?]{
@(mdn-bar "AudioParam: exponentialRampToValueAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/exponentialRampToValueAtTime")

Schedules an exponential ramp.
}

@defproc[(audio-param-set-target-at-time! [param audio-param?]
                                         [value real?]
                                         [time real?]
                                         [time-constant real?])
         void?]{
@(mdn-bar "AudioParam: setTargetAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setTargetAtTime")

Schedules a target value.
}

@defproc[(audio-param-set-value-curve-at-time! [param audio-param?]
                                               [values vector?]
                                               [start-time real?]
                                               [duration real?])
         void?]{
@(mdn-bar "AudioParam: setValueCurveAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime")

The @racket[values] argument should be a Racket vector of real
numbers.

Schedules a value curve.
}

@defproc[(audio-param-cancel-scheduled-values! [param audio-param?]
                                               [time real?])
         void?]{
@(mdn-bar "AudioParam: cancelScheduledValues() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues")

Cancels scheduled values from a time onward.
}

@defproc[(audio-param-cancel-and-hold-at-time! [param audio-param?]
                                               [time real?])
         void?]{
@(mdn-bar "AudioParam: cancelAndHoldAtTime() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelAndHoldAtTime")

Cancels scheduled values and holds the current value.
}

@defproc[(audio-buffer-length [buffer audio-buffer?]) exact-nonnegative-integer?]{
@(mdn-bar "AudioBuffer: length property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer/length")

Returns the number of sample-frames in the buffer.
}

@defproc[(audio-buffer-duration [buffer audio-buffer?]) real?]{
@(mdn-bar "AudioBuffer: duration property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer/duration")

Returns the duration of the buffer in seconds.
}

@defproc[(audio-buffer-sample-rate [buffer audio-buffer?]) real?]{
@(mdn-bar "AudioBuffer: sampleRate property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer/sampleRate")

Returns the sample rate of the buffer.
}

@defproc[(audio-buffer-number-of-channels [buffer audio-buffer?]) exact-nonnegative-integer?]{
@(mdn-bar "AudioBuffer: numberOfChannels property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer/numberOfChannels")

Returns the number of channels in the buffer.
}

@defproc[(audio-buffer-get-channel-data [buffer audio-buffer?]
                                       [channel exact-integer?])
         vector?]{
@(mdn-bar "AudioBuffer: getChannelData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer/getChannelData")

Returns the channel data for @racket[channel] as a WebRacket vector of
sample values.
}

@subsection{Synthesis And Playback}

@defproc[(audio-oscillator-node-start! [node audio-oscillator-node?]
                                       [when (or/c #f real?) #f])
         void?]{
@(mdn-bar "OscillatorNode: start() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/start")

Starts the oscillator. If @racket[when] is omitted, the browser default
is used.
}

@defproc[(audio-oscillator-node-stop! [node audio-oscillator-node?]
                                      [when (or/c #f real?) #f])
         void?]{
@(mdn-bar "OscillatorNode: stop() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/stop")

Stops the oscillator. If @racket[when] is omitted, the browser default
is used.
}

@defproc[(audio-buffer-source-node-start! [node audio-buffer-source-node?]
                                                [when (or/c #f real?) #f]
                                                [offset (or/c #f real?) #f]
                                                [duration (or/c #f real?) #f])
         void?]{
@(mdn-bar "AudioBufferSourceNode: start() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/start")

Starts the buffer source. Omitted trailing values use the browser defaults.
}

@defproc[(audio-buffer-source-node-stop! [node audio-buffer-source-node?]
                                               [when (or/c #f real?) #f])
         void?]{
@(mdn-bar "AudioBufferSourceNode: stop() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/stop")

Stops the buffer source. If @racket[when] is omitted, the browser default
is used.
}

@defproc[(audio-analyser-node-fft-size [node audio-analyser-node?]) exact-nonnegative-integer?]{
@(mdn-bar "AnalyserNode: fftSize property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/fftSize")

Returns the FFT size.
}

@defproc[(audio-analyser-node-set-fft-size! [node audio-analyser-node?]
                                            [size exact-integer?])
         void?]{
@(mdn-bar "AnalyserNode: fftSize property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/fftSize")

Sets the FFT size.
}

@defproc[(audio-analyser-node-frequency-bin-count [node audio-analyser-node?]) exact-nonnegative-integer?]{
@(mdn-bar "AnalyserNode: frequencyBinCount property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/frequencyBinCount")

Returns the frequency-bin count.
}

@defproc[(audio-analyser-node-min-decibels [node audio-analyser-node?]) real?]{
@(mdn-bar "AnalyserNode: minDecibels property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/minDecibels")

Returns the minimum decibels value.
}

@defproc[(audio-analyser-node-set-min-decibels! [node audio-analyser-node?]
                                                [value real?])
         void?]{
@(mdn-bar "AnalyserNode: minDecibels property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/minDecibels")

Sets the minimum decibels value.
}

@defproc[(audio-analyser-node-max-decibels [node audio-analyser-node?]) real?]{
@(mdn-bar "AnalyserNode: maxDecibels property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/maxDecibels")

Returns the maximum decibels value.
}

@defproc[(audio-analyser-node-set-max-decibels! [node audio-analyser-node?]
                                                [value real?])
         void?]{
@(mdn-bar "AnalyserNode: maxDecibels property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/maxDecibels")

Sets the maximum decibels value.
}

@defproc[(audio-analyser-node-smoothing-time-constant [node audio-analyser-node?]) real?]{
@(mdn-bar "AnalyserNode: smoothingTimeConstant property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/smoothingTimeConstant")

Returns the smoothing time constant.
}

@defproc[(audio-analyser-node-set-smoothing-time-constant! [node audio-analyser-node?]
                                                           [value real?])
         void?]{
@(mdn-bar "AnalyserNode: smoothingTimeConstant property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/smoothingTimeConstant")

Sets the smoothing time constant.
}

@defproc[(audio-analyser-node-get-byte-frequency-data! [node audio-analyser-node?]
                                                       [data (or/c bytes? vector?)])
         void?]{
@(mdn-bar "AnalyserNode: getByteFrequencyData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteFrequencyData")

The @racket[data] argument should be either Racket @racket[bytes] or
a Racket vector of exact integers.

Fills a byte frequency buffer.
}

@defproc[(audio-analyser-node-get-byte-time-domain-data! [node audio-analyser-node?]
                                                         [data (or/c bytes? vector?)])
         void?]{
@(mdn-bar "AnalyserNode: getByteTimeDomainData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteTimeDomainData")

The @racket[data] argument should be either Racket @racket[bytes] or
a Racket vector of exact integers.

Fills a byte time-domain buffer.
}

@defproc[(audio-analyser-node-get-float-frequency-data! [node audio-analyser-node?]
                                                        [data vector?])
         void?]{
@(mdn-bar "AnalyserNode: getFloatFrequencyData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatFrequencyData")

The @racket[data] argument should be a Racket vector of real numbers.

Fills a float frequency buffer.
}

@defproc[(audio-analyser-node-get-float-time-domain-data! [node audio-analyser-node?]
                                                          [data vector?])
         void?]{
@(mdn-bar "AnalyserNode: getFloatTimeDomainData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatTimeDomainData")

The @racket[data] argument should be a Racket vector of real numbers.

Fills a float time-domain buffer.
}

@defproc[(audio-biquad-filter-node-type [node audio-biquad-filter-node?]) string?]{
@(mdn-bar "BiquadFilterNode: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/type")

Returns the filter type string.
}

@defproc[(audio-biquad-filter-node-set-type! [node audio-biquad-filter-node?]
                                             [type (or/c string? symbol?)])
         void?]{
@(mdn-bar "BiquadFilterNode: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/type")

Sets the filter type string.
}

@defproc[(audio-biquad-filter-node-frequency [node audio-biquad-filter-node?]) audio-param?]{
@(mdn-bar "BiquadFilterNode: frequency property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/frequency")

Returns the frequency parameter.
}

@defproc[(audio-biquad-filter-node-detune [node audio-biquad-filter-node?]) audio-param?]{
@(mdn-bar "BiquadFilterNode: detune property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/detune")

Returns the detune parameter.
}

@defproc[(audio-biquad-filter-node-q [node audio-biquad-filter-node?]) audio-param?]{
@(mdn-bar "BiquadFilterNode: Q property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/Q")

Returns the Q parameter.
}

@defproc[(audio-biquad-filter-node-gain [node audio-biquad-filter-node?]) audio-param?]{
@(mdn-bar "BiquadFilterNode: gain property"
          "https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/gain")

Returns the gain parameter.
}

@defproc[(audio-constant-source-node-offset [node audio-constant-source-node?]) audio-param?]{
@(mdn-bar "ConstantSourceNode: offset property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode/offset")

Returns the offset parameter.
}

@defproc[(audio-constant-source-node-start! [node audio-constant-source-node?]
                                           [when (or/c #f real?) #f])
         void?]{
@(mdn-bar "ConstantSourceNode: start() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode/start")

Starts the constant source. If @racket[when] is omitted, the browser
default is used.
}

@defproc[(audio-constant-source-node-stop! [node audio-constant-source-node?]
                                          [when (or/c #f real?) #f])
         void?]{
@(mdn-bar "ConstantSourceNode: stop() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode/stop")

Stops the constant source. If @racket[when] is omitted, the browser
default is used.
}

@subsection{Dynamics, Routing, And Spatialization}

@defproc[(audio-dynamics-compressor-node-threshold [node audio-dynamics-compressor-node?]) audio-param?]{
@(mdn-bar "DynamicsCompressorNode: threshold property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/threshold")

Returns the threshold parameter.
}

@defproc[(audio-dynamics-compressor-node-knee [node audio-dynamics-compressor-node?]) audio-param?]{
@(mdn-bar "DynamicsCompressorNode: knee property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/knee")

Returns the knee parameter.
}

@defproc[(audio-dynamics-compressor-node-ratio [node audio-dynamics-compressor-node?]) audio-param?]{
@(mdn-bar "DynamicsCompressorNode: ratio property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/ratio")

Returns the ratio parameter.
}

@defproc[(audio-dynamics-compressor-node-reduction [node audio-dynamics-compressor-node?]) real?]{
@(mdn-bar "DynamicsCompressorNode: reduction property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/reduction")

Returns the reduction value.
}

@defproc[(audio-dynamics-compressor-node-attack [node audio-dynamics-compressor-node?]) audio-param?]{
@(mdn-bar "DynamicsCompressorNode: attack property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/attack")

Returns the attack parameter.
}

@defproc[(audio-dynamics-compressor-node-release [node audio-dynamics-compressor-node?]) audio-param?]{
@(mdn-bar "DynamicsCompressorNode: release property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode/release")

Returns the release parameter.
}

@defproc[(audio-panner-node-panning-model [node audio-panner-node?]) string?]{
@(mdn-bar "PannerNode: panningModel property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/panningModel")

Returns the panning model string.
}

@defproc[(audio-panner-node-set-panning-model! [node audio-panner-node?]
                                               [value (or/c string? symbol?)])
         void?]{
@(mdn-bar "PannerNode: panningModel property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/panningModel")

Sets the panning model string.
}

@defproc[(audio-panner-node-distance-model [node audio-panner-node?]) string?]{
@(mdn-bar "PannerNode: distanceModel property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/distanceModel")

Returns the distance model string.
}

@defproc[(audio-panner-node-set-distance-model! [node audio-panner-node?]
                                               [value (or/c string? symbol?)])
         void?]{
@(mdn-bar "PannerNode: distanceModel property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/distanceModel")

Sets the distance model string.
}

@defproc[(audio-panner-node-position-x [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: positionX property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/positionX")

Returns the positionX parameter.
}

@defproc[(audio-panner-node-position-y [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: positionY property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/positionY")

Returns the positionY parameter.
}

@defproc[(audio-panner-node-position-z [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: positionZ property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/positionZ")

Returns the positionZ parameter.
}

@defproc[(audio-panner-node-orientation-x [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: orientationX property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/orientationX")

Returns the orientationX parameter.
}

@defproc[(audio-panner-node-orientation-y [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: orientationY property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/orientationY")

Returns the orientationY parameter.
}

@defproc[(audio-panner-node-orientation-z [node audio-panner-node?]) audio-param?]{
@(mdn-bar "PannerNode: orientationZ property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/orientationZ")

Returns the orientationZ parameter.
}

@defproc[(audio-panner-node-ref-distance [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: refDistance property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/refDistance")

Returns the reference distance.
}

@defproc[(audio-panner-node-set-ref-distance! [node audio-panner-node?]
                                              [value real?])
         void?]{
@(mdn-bar "PannerNode: refDistance property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/refDistance")

Sets the reference distance.
}

@defproc[(audio-panner-node-max-distance [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: maxDistance property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/maxDistance")

Returns the maximum distance.
}

@defproc[(audio-panner-node-set-max-distance! [node audio-panner-node?]
                                              [value real?])
         void?]{
@(mdn-bar "PannerNode: maxDistance property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/maxDistance")

Sets the maximum distance.
}

@defproc[(audio-panner-node-rolloff-factor [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: rolloffFactor property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/rolloffFactor")

Returns the rolloff factor.
}

@defproc[(audio-panner-node-set-rolloff-factor! [node audio-panner-node?]
                                                [value real?])
         void?]{
@(mdn-bar "PannerNode: rolloffFactor property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/rolloffFactor")

Sets the rolloff factor.
}

@defproc[(audio-panner-node-cone-inner-angle [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: coneInnerAngle property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneInnerAngle")

Returns the inner cone angle.
}

@defproc[(audio-panner-node-set-cone-inner-angle! [node audio-panner-node?]
                                                  [value real?])
         void?]{
@(mdn-bar "PannerNode: coneInnerAngle property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneInnerAngle")

Sets the inner cone angle.
}

@defproc[(audio-panner-node-cone-outer-angle [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: coneOuterAngle property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneOuterAngle")

Returns the outer cone angle.
}

@defproc[(audio-panner-node-set-cone-outer-angle! [node audio-panner-node?]
                                                  [value real?])
         void?]{
@(mdn-bar "PannerNode: coneOuterAngle property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneOuterAngle")

Sets the outer cone angle.
}

@defproc[(audio-panner-node-cone-outer-gain [node audio-panner-node?]) real?]{
@(mdn-bar "PannerNode: coneOuterGain property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneOuterGain")

Returns the outer cone gain.
}

@defproc[(audio-panner-node-set-cone-outer-gain! [node audio-panner-node?]
                                                 [value real?])
         void?]{
@(mdn-bar "PannerNode: coneOuterGain property"
          "https://developer.mozilla.org/en-US/docs/Web/API/PannerNode/coneOuterGain")

Sets the outer cone gain.
}

@defproc[(audio-stereo-panner-node-pan [node audio-stereo-panner-node?]) audio-param?]{
@(mdn-bar "StereoPannerNode: pan property"
          "https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode/pan")

Returns the pan parameter.
}

@subsection{Audio Event Handlers}

@defproc[(audio-context-onstatechange! [ctx audio-context?]
                                       [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "AudioContext: onstatechange property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioContext/onstatechange")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function value that receives a browser
@racketid[Event] value.

Installs or clears the context state-change handler.
}

@defproc[(audio-oscillator-node-onended! [node audio-oscillator-node?]
                                         [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "OscillatorNode: onended property"
          "https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/onended")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function value that receives a browser
@racketid[Event] value.

Installs or clears the oscillator ended handler.
}

@defproc[(audio-buffer-source-node-onended! [node audio-buffer-source-node?]
                                                  [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "AudioBufferSourceNode: onended property"
          "https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/onended")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function value that receives a browser
@racketid[Event] value.

Installs or clears the buffer-source ended handler.
}

@defproc[(audio-constant-source-node-onended! [node audio-constant-source-node?]
                                              [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "ConstantSourceNode: onended property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode/onended")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function value.

Installs or clears the constant-source ended handler.
}

@defproc[(audio-add-event-listener! [target (or/c audio-context? audio-node?)]
                                    [event-name (or/c string? symbol?)]
                                    [listener (or/c procedure? external?)]
                                    [option (or/c boolean? external?) ...])
         external?]{
@(mdn-bar "EventTarget: addEventListener() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener")

The raw @racket[target] argument should be a browser @racketid[AudioContext]
or @racketid[AudioNode] value. The raw @racket[listener] argument, when
supplied as an external, should be a browser event-listener function
value. The optional @racket[option] values are the browser's listener
options, such as booleans or an @racketid[AddEventListenerOptions]
dictionary.

Adds an event listener and returns the installed callback token.
}

@defproc[(audio-remove-event-listener! [target (or/c audio-context? audio-node?)]
                                       [event-name (or/c string? symbol?)]
                                       [listener (or/c procedure? external?)]
                                       [option (or/c boolean? external?) ...])
         void?]{
@(mdn-bar "EventTarget: removeEventListener() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/removeEventListener")

The raw @racket[target] argument should be a browser @racketid[AudioContext]
or @racketid[AudioNode] value. The raw @racket[listener] argument, when
supplied as an external, should be a browser event-listener function
value. The optional @racket[option] values are the browser's listener
options, such as booleans or an @racketid[AddEventListenerOptions]
dictionary.

Removes a previously registered event listener.
}
