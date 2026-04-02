;;;
;;; audio.ffi
;;;

;; Focused tests for `audio.ffi` and the `audio` wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/audio.ffi -r test-audio.rkt

(include-lib audio)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-false got label)
  (when got
    (error 'check-false label)))

(define (check-number= got want label)
  (unless (= got want)
    (error 'check-number= label)))

(define (expect-contract-error thunk)
  (with-handlers ([exn:fail:contract? (lambda (_e) #t)])
    (thunk)
    #f))

(define (install!)
  (js-eval
   "globalThis.AudioParam = class AudioParam {
      constructor(value = 0) {
        this.value = value;
        this.calls = [];
      }
      setValueAtTime(value, time) {
        this.calls.push(['setValueAtTime', value, time]);
        this.value = value;
      }
    };

    globalThis.AudioNode = class AudioNode {
      constructor(context) {
        this.context = context;
        this.connections = [];
        this.disconnections = [];
      }
      connect(destination, outputIndex, inputIndex) {
        this.connections.push([destination, outputIndex, inputIndex]);
        return destination;
      }
      disconnect(outputIndex, inputIndex) {
        this.disconnections.push([outputIndex, inputIndex]);
      }
      addEventListener(eventName, listener, options) {
        this.lastAdd = [eventName, listener, options];
      }
      removeEventListener(eventName, listener, options) {
        this.lastRemove = [eventName, listener, options];
      }
    };

    globalThis.AudioBuffer = class AudioBuffer {
      constructor(numberOfChannels, length, sampleRate) {
        this.numberOfChannels = numberOfChannels;
        this.length = length;
        this.sampleRate = sampleRate;
        this.duration = length / sampleRate;
        this.channelData = Array.from({length: numberOfChannels}, (_, i) => new Float32Array([i, i + 1]));
      }
      getChannelData(channel) {
        return this.channelData[channel];
      }
    };

    globalThis.GainNode = class GainNode extends AudioNode {
      constructor(context) {
        super(context);
        this.gain = new AudioParam(1);
      }
    };

    globalThis.OscillatorNode = class OscillatorNode extends AudioNode {
      constructor(context) {
        super(context);
        this.type = 'sine';
        this.frequency = new AudioParam(440);
        this.detune = new AudioParam(0);
        this.started = [];
        this.stopped = [];
      }
      start(when) {
        this.started.push(when);
      }
      stop(when) {
        this.stopped.push(when);
      }
      setPeriodicWave(wave) {
        this.periodicWave = wave;
      }
    };

    globalThis.AudioBufferSourceNode = class AudioBufferSourceNode extends AudioNode {
      constructor(context) {
        super(context);
        this.buffer = null;
        this.playbackRate = new AudioParam(1);
        this.detune = new AudioParam(0);
        this.loop = false;
        this.loopStart = 0;
        this.loopEnd = 0;
        this.started = [];
        this.stopped = [];
      }
      start(when, offset, duration) {
        this.started.push([when, offset, duration]);
      }
      stop(when) {
        this.stopped.push(when);
      }
    };

    globalThis.ConstantSourceNode = class ConstantSourceNode extends AudioNode {
      constructor(context) {
        super(context);
        this.offset = new AudioParam(1);
        this.started = [];
        this.stopped = [];
      }
      start(when) {
        this.started.push(when);
      }
      stop(when) {
        this.stopped.push(when);
      }
    };

    globalThis.ChannelSplitterNode = class ChannelSplitterNode extends AudioNode {
      constructor(context, channels = 6) {
        super(context);
        this.channels = channels;
      }
    };

    globalThis.ChannelMergerNode = class ChannelMergerNode extends AudioNode {
      constructor(context, channels = 6) {
        super(context);
        this.channels = channels;
      }
    };

    globalThis.DynamicsCompressorNode = class DynamicsCompressorNode extends AudioNode {
      constructor(context) {
        super(context);
        this.threshold = new AudioParam(-24);
        this.knee = new AudioParam(30);
        this.ratio = new AudioParam(12);
        this.reduction = -12;
        this.attack = new AudioParam(0.003);
        this.release = new AudioParam(0.25);
      }
    };

    globalThis.PannerNode = class PannerNode extends AudioNode {
      constructor(context) {
        super(context);
        this.panningModel = 'HRTF';
        this.distanceModel = 'inverse';
        this.positionX = new AudioParam(0);
        this.positionY = new AudioParam(0);
        this.positionZ = new AudioParam(0);
        this.orientationX = new AudioParam(1);
        this.orientationY = new AudioParam(0);
        this.orientationZ = new AudioParam(0);
        this.refDistance = 1;
        this.maxDistance = 10000;
        this.rolloffFactor = 1;
        this.coneInnerAngle = 360;
        this.coneOuterAngle = 360;
        this.coneOuterGain = 0;
      }
    };

    globalThis.StereoPannerNode = class StereoPannerNode extends AudioNode {
      constructor(context) {
        super(context);
        this.pan = new AudioParam(0);
      }
    };

    globalThis.AnalyserNode = class AnalyserNode extends AudioNode {
      constructor(context) {
        super(context);
        this.fftSize = 32;
        this.frequencyBinCount = 16;
        this.minDecibels = -100;
        this.maxDecibels = -30;
        this.smoothingTimeConstant = 0.5;
      }
      getByteFrequencyData(data) {
        this.lastByteFrequencyData = data;
        if (data.length > 0) {
          data[0] = 11;
        }
      }
      getByteTimeDomainData(data) {
        this.lastByteTimeDomainData = data;
        if (data.length > 0) {
          data[0] = 22;
        }
      }
      getFloatFrequencyData(data) {
        this.lastFloatFrequencyData = data;
        if (data.length > 0) {
          data[0] = 33.5;
        }
      }
      getFloatTimeDomainData(data) {
        this.lastFloatTimeDomainData = data;
        if (data.length > 0) {
          data[0] = 44.5;
        }
      }
    };

    globalThis.BiquadFilterNode = class BiquadFilterNode extends AudioNode {
      constructor(context) {
        super(context);
        this.type = 'lowpass';
        this.frequency = new AudioParam(350);
        this.detune = new AudioParam(0);
        this.q = new AudioParam(1);
        this.gain = new AudioParam(0);
      }
    };

    globalThis.AudioContext = class AudioContext {
      constructor() {
        this.state = 'running';
        this.currentTime = 1.25;
        this.sampleRate = 48000;
        this.baseLatency = 0.125;
        this.outputLatency = 0.25;
        this.destination = new AudioNode(this);
        this.listener = {kind: 'listener'};
        this.calls = [];
      }
      close() {
        this.calls.push(['close']);
        return {kind: 'close-promise'};
      }
      resume() {
        this.calls.push(['resume']);
        return {kind: 'resume-promise'};
      }
      suspend() {
        this.calls.push(['suspend']);
        return {kind: 'suspend-promise'};
      }
      createGain() {
        this.calls.push(['createGain']);
        return new GainNode(this);
      }
      createOscillator() {
        this.calls.push(['createOscillator']);
        return new OscillatorNode(this);
      }
      createBufferSource() {
        this.calls.push(['createBufferSource']);
        return new AudioBufferSourceNode(this);
      }
      createConstantSource() {
        this.calls.push(['createConstantSource']);
        return new ConstantSourceNode(this);
      }
      createChannelSplitter(channels) {
        this.calls.push(['createChannelSplitter', channels]);
        return new ChannelSplitterNode(this, channels);
      }
      createChannelMerger(channels) {
        this.calls.push(['createChannelMerger', channels]);
        return new ChannelMergerNode(this, channels);
      }
      createDynamicsCompressor() {
        this.calls.push(['createDynamicsCompressor']);
        return new DynamicsCompressorNode(this);
      }
      createPanner() {
        this.calls.push(['createPanner']);
        return new PannerNode(this);
      }
      createStereoPanner() {
        this.calls.push(['createStereoPanner']);
        return new StereoPannerNode(this);
      }
      createAnalyser() {
        this.calls.push(['createAnalyser']);
        return new AnalyserNode(this);
      }
      createBiquadFilter() {
        this.calls.push(['createBiquadFilter']);
        return new BiquadFilterNode(this);
      }
      createBuffer(numberOfChannels, length, sampleRate) {
        this.calls.push(['createBuffer', numberOfChannels, length, sampleRate]);
        return new AudioBuffer(numberOfChannels, length, sampleRate);
      }
      addEventListener(eventName, listener, options) {
        this.lastAdd = [eventName, listener, options];
      }
      removeEventListener(eventName, listener, options) {
        this.lastRemove = [eventName, listener, options];
      }
    };"))

(list
 (list "Audio raw bridge"
       (let* ([_    (install!)]
              [ctx  (js-audio-context-new)])
         (check-true
          (and (equal? (js-audio-context-state ctx) "running")
               (equal? (js-audio-context-current-time ctx) 1.25))
          "raw bridge")
         #t))
  (list "Audio wrappers"
       (let* ([_   (install!)]
              [ctx (audio-context-new)]
              [gain (audio-context-create-gain ctx)]
              [osc (audio-context-create-oscillator ctx)]
              [buffer (audio-context-create-buffer ctx 2 4 48000)])
         (check-true (audio-context? ctx) "wrapper context predicate")
         (check-equal (audio-context-state ctx) "running" "wrapper state")
         (check-number= (audio-context-current-time ctx) 1.25 "wrapper currentTime")
         (check-number= (audio-context-sample-rate ctx) 48000 "wrapper sampleRate")
         (check-number= (audio-context-base-latency ctx) 0.125 "wrapper baseLatency")
         (check-number= (audio-context-output-latency ctx) 0.25 "wrapper outputLatency")
         (define audio-listener-value (audio-context-listener ctx))
         (check-true (audio-listener? audio-listener-value) "wrapper listener predicate")
         (check-equal (js-ref (audio-listener-raw audio-listener-value) "kind")
                      "listener"
                      "wrapper listener raw")
         (check-true (audio-gain-node? gain) "wrapper gain predicate")
         (check-number= (audio-param-value (audio-gain-node-gain gain)) 1 "wrapper gain default")
         (audio-param-set-value! (audio-gain-node-gain gain) 0.25)
         (check-number= (audio-param-value (audio-gain-node-gain gain)) 0.25 "wrapper gain update")
         (check-true (audio-oscillator-node? osc) "wrapper oscillator predicate")
         (check-equal (audio-oscillator-node-type osc) "sine" "wrapper oscillator type")
         (audio-oscillator-node-set-type! osc 'triangle)
         (check-equal (audio-oscillator-node-type osc) "triangle" "wrapper oscillator type update")
         (check-number= (audio-param-value (audio-oscillator-node-frequency osc)) 440 "wrapper oscillator frequency")
         (audio-param-set-value! (audio-oscillator-node-frequency osc) 523.25)
         (check-number= (audio-param-value (audio-oscillator-node-frequency osc)) 523.25 "wrapper oscillator frequency update")
         (check-true (audio-buffer? buffer) "wrapper buffer predicate")
         (check-number= (audio-buffer-length buffer) 4 "wrapper buffer length")
         (check-number= (audio-buffer-duration buffer) (/ 4 48000) "wrapper buffer duration")
         (check-number= (audio-buffer-sample-rate buffer) 48000 "wrapper buffer sample rate")
         (check-number= (audio-buffer-number-of-channels buffer) 2 "wrapper buffer channels")
         (check-true (external? (audio-buffer-get-channel-data buffer 0))
                     "wrapper buffer channel data")

         (define buffer-source (audio-context-create-buffer-source ctx))
         (check-true (audio-buffer-source-node? buffer-source)
                     "wrapper buffer source predicate")
         (check-true (js-null? (audio-buffer-source-node-buffer buffer-source))
                     "wrapper buffer source default buffer")
         (audio-buffer-source-node-set-buffer! buffer-source buffer)
         (check-true (audio-buffer? (audio-buffer-source-node-buffer buffer-source))
                     "wrapper buffer source buffer update")
         (check-number= (audio-param-value (audio-buffer-source-node-playback-rate buffer-source))
                        1
                        "wrapper buffer source playback rate")
         (audio-param-set-value! (audio-buffer-source-node-playback-rate buffer-source) 1.5)
         (check-number= (audio-param-value (audio-buffer-source-node-playback-rate buffer-source))
                        1.5
                        "wrapper buffer source playback rate update")
         (check-number= (audio-param-value (audio-buffer-source-node-detune buffer-source))
                        0
                        "wrapper buffer source detune")
         (check-false (audio-buffer-source-node-loop buffer-source)
                     "wrapper buffer source loop default")
         (audio-buffer-source-node-set-loop! buffer-source #t)
         (check-true (audio-buffer-source-node-loop buffer-source)
                     "wrapper buffer source loop update")
         (check-number= (audio-buffer-source-node-loop-start buffer-source)
                        0
                        "wrapper buffer source loop start")
         (audio-buffer-source-node-set-loop-start! buffer-source 0.25)
         (check-number= (audio-buffer-source-node-loop-start buffer-source)
                        0.25
                        "wrapper buffer source loop start update")
         (check-number= (audio-buffer-source-node-loop-end buffer-source)
                        0
                        "wrapper buffer source loop end")
         (audio-buffer-source-node-set-loop-end! buffer-source 1.5)
         (check-number= (audio-buffer-source-node-loop-end buffer-source)
                        1.5
                        "wrapper buffer source loop end update")
         (audio-buffer-source-node-start! buffer-source 0.5 0.25 1.0)
         (define buffer-source-start (vector-ref (js-ref buffer-source "started") 0))
         (check-number= (vector-ref buffer-source-start 0) 0.5
                        "wrapper buffer source start when")
         (check-number= (vector-ref buffer-source-start 1) 0.25
                        "wrapper buffer source start offset")
         (check-number= (vector-ref buffer-source-start 2) 1.0
                        "wrapper buffer source start duration")
         (audio-buffer-source-node-stop! buffer-source 2.5)
         (check-equal (vector-ref (js-ref buffer-source "stopped") 0)
                      2.5
                      "wrapper buffer source stop")
         (audio-buffer-source-node-onended! buffer-source (lambda (_evt) (void)))
         (check-true (external? (js-ref buffer-source "onended"))
                     "wrapper buffer source handler install")

         (define constant-source (audio-context-create-constant-source ctx))
         (check-true (audio-constant-source-node? constant-source)
                     "wrapper constant source predicate")
         (check-number= (audio-param-value (audio-constant-source-node-offset constant-source))
                        1
                        "wrapper constant source offset")
         (audio-param-set-value! (audio-constant-source-node-offset constant-source) 0.75)
         (check-number= (audio-param-value (audio-constant-source-node-offset constant-source))
                        0.75
                        "wrapper constant source offset update")
         (audio-constant-source-node-start! constant-source 0.25)
         (check-number= (vector-ref (js-ref constant-source "started") 0)
                        0.25
                        "wrapper constant source start")
         (audio-constant-source-node-stop! constant-source 1.25)
         (check-number= (vector-ref (js-ref constant-source "stopped") 0)
                        1.25
                        "wrapper constant source stop")
         (audio-constant-source-node-onended! constant-source (lambda (_evt) (void)))
         (check-true (external? (js-ref constant-source "onended"))
                     "wrapper constant source handler install")

         (audio-node-connect osc gain)
         (audio-node-connect gain (audio-context-destination ctx))
         (audio-node-connect constant-source gain)
         (define osc-connections (js-ref osc "connections"))
         (check-number= (vector-length osc-connections) 1
                        "wrapper oscillator connection count")
         (define osc-connection (vector-ref osc-connections 0))
         (check-true (audio-gain-node? (vector-ref osc-connection 0))
                     "wrapper oscillator connection destination")
         (define gain-connections (js-ref gain "connections"))
         (check-number= (vector-length gain-connections) 1
                        "wrapper gain connection count")
         (define gain-connection (vector-ref gain-connections 0))
         (check-true (audio-node? (vector-ref gain-connection 0))
                     "wrapper gain connection destination")
         (define constant-source-connections (js-ref constant-source "connections"))
         (check-number= (vector-length constant-source-connections) 1
                        "wrapper constant source connection count")
         (define constant-source-connection (vector-ref constant-source-connections 0))
         (check-true (audio-gain-node? (vector-ref constant-source-connection 0))
                     "wrapper constant source connection destination")
         (audio-node-disconnect osc gain)
         (audio-node-disconnect gain (audio-context-destination ctx))
         (define osc-disconnections (js-ref osc "disconnections"))
         (check-number= (vector-length osc-disconnections) 1
                        "wrapper oscillator disconnect count")
         (define gain-disconnections (js-ref gain "disconnections"))
         (check-number= (vector-length gain-disconnections) 1
                        "wrapper gain disconnect count")

         (define analyser (audio-context-create-analyser ctx))
         (check-true (audio-analyser-node? analyser)
                     "wrapper analyser predicate")
         (check-number= (audio-analyser-node-fft-size analyser)
                        32
                        "wrapper analyser fft size")
         (audio-analyser-node-set-fft-size! analyser 64)
         (check-number= (audio-analyser-node-fft-size analyser)
                        64
                        "wrapper analyser fft size update")
         (check-number= (audio-analyser-node-frequency-bin-count analyser)
                        16
                        "wrapper analyser frequency bin count")
         (check-number= (audio-analyser-node-min-decibels analyser)
                        -100
                        "wrapper analyser min decibels")
         (audio-analyser-node-set-min-decibels! analyser -120)
         (check-number= (audio-analyser-node-min-decibels analyser)
                        -120
                        "wrapper analyser min decibels update")
         (check-number= (audio-analyser-node-max-decibels analyser)
                        -30
                        "wrapper analyser max decibels")
         (audio-analyser-node-set-max-decibels! analyser -12)
         (check-number= (audio-analyser-node-max-decibels analyser)
                        -12
                        "wrapper analyser max decibels update")
         (check-number= (audio-analyser-node-smoothing-time-constant analyser)
                        0.5
                        "wrapper analyser smoothing constant")
         (audio-analyser-node-set-smoothing-time-constant! analyser 0.75)
         (check-number= (audio-analyser-node-smoothing-time-constant analyser)
                        0.75
                        "wrapper analyser smoothing constant update")
         (define analyser-byte-frequency (make-bytes 8))
         (audio-analyser-node-get-byte-frequency-data! analyser analyser-byte-frequency)
         (check-true #t "wrapper analyser byte frequency call")
         (define analyser-byte-time-domain (make-bytes 8))
         (audio-analyser-node-get-byte-time-domain-data! analyser analyser-byte-time-domain)
         (check-true #t "wrapper analyser byte time-domain call")
         (define analyser-float-frequency (js-eval "new Float32Array(8)"))
         (audio-analyser-node-get-float-frequency-data! analyser analyser-float-frequency)
         (check-true #t "wrapper analyser float frequency call")
         (define analyser-float-time-domain (js-eval "new Float32Array(8)"))
         (audio-analyser-node-get-float-time-domain-data! analyser analyser-float-time-domain)
         (check-true #t "wrapper analyser float time-domain call")

         (define biquad (audio-context-create-biquad-filter ctx))
         (check-true (audio-biquad-filter-node? biquad)
                     "wrapper biquad predicate")
         (check-equal (audio-biquad-filter-node-type biquad)
                      "lowpass"
                      "wrapper biquad type")
         (audio-biquad-filter-node-set-type! biquad 'highpass)
         (check-equal (audio-biquad-filter-node-type biquad)
                      "highpass"
                      "wrapper biquad type update")
         (check-number= (audio-param-value (audio-biquad-filter-node-frequency biquad))
                        350
                        "wrapper biquad frequency")
         (audio-param-set-value! (audio-biquad-filter-node-frequency biquad) 880)
         (check-number= (audio-param-value (audio-biquad-filter-node-frequency biquad))
                        880
                        "wrapper biquad frequency update")
         (check-number= (audio-param-value (audio-biquad-filter-node-detune biquad))
                        0
                        "wrapper biquad detune")
         (audio-param-set-value! (audio-biquad-filter-node-detune biquad) 12)
         (check-number= (audio-param-value (audio-biquad-filter-node-detune biquad))
                        12
                        "wrapper biquad detune update")
         (check-number= (audio-param-value (audio-biquad-filter-node-q biquad))
                        1
                        "wrapper biquad q")
         (audio-param-set-value! (audio-biquad-filter-node-q biquad) 0.707)
         (check-number= (audio-param-value (audio-biquad-filter-node-q biquad))
                        0.707
                        "wrapper biquad q update")
         (check-number= (audio-param-value (audio-biquad-filter-node-gain biquad))
                        0
                        "wrapper biquad gain")
         (audio-param-set-value! (audio-biquad-filter-node-gain biquad) 4.5)
         (check-number= (audio-param-value (audio-biquad-filter-node-gain biquad))
                        4.5
                        "wrapper biquad gain update")

         (define splitter (audio-context-create-channel-splitter ctx 4))
         (check-true (audio-channel-splitter-node? splitter)
                     "wrapper splitter predicate")
         (check-number= (js-ref splitter "channels") 4
                        "wrapper splitter channels")

         (define merger (audio-context-create-channel-merger ctx 4))
         (check-true (audio-channel-merger-node? merger)
                     "wrapper merger predicate")
         (check-number= (js-ref merger "channels") 4
                        "wrapper merger channels")

         (define compressor (audio-context-create-dynamics-compressor ctx))
         (check-true (audio-dynamics-compressor-node? compressor)
                     "wrapper compressor predicate")
         (check-number= (audio-param-value (audio-dynamics-compressor-node-threshold compressor))
                        -24
                        "wrapper compressor threshold")
         (audio-param-set-value! (audio-dynamics-compressor-node-threshold compressor) -30)
         (check-number= (audio-param-value (audio-dynamics-compressor-node-threshold compressor))
                        -30
                        "wrapper compressor threshold update")
         (check-number= (audio-param-value (audio-dynamics-compressor-node-knee compressor))
                        30
                        "wrapper compressor knee")
         (check-number= (audio-param-value (audio-dynamics-compressor-node-ratio compressor))
                        12
                        "wrapper compressor ratio")
         (check-number= (audio-dynamics-compressor-node-reduction compressor)
                        -12
                        "wrapper compressor reduction")
         (check-number= (audio-param-value (audio-dynamics-compressor-node-attack compressor))
                        0.003
                        "wrapper compressor attack")
         (check-number= (audio-param-value (audio-dynamics-compressor-node-release compressor))
                        0.25
                        "wrapper compressor release")

         (define panner (audio-context-create-panner ctx))
         (check-true (audio-panner-node? panner)
                     "wrapper panner predicate")
         (check-equal (audio-panner-node-panning-model panner)
                      "HRTF"
                      "wrapper panner model")
         (audio-panner-node-set-panning-model! panner 'equalpower)
         (check-equal (audio-panner-node-panning-model panner)
                      "equalpower"
                      "wrapper panner model update")
         (check-equal (audio-panner-node-distance-model panner)
                      "inverse"
                      "wrapper panner distance model")
         (audio-panner-node-set-distance-model! panner 'linear)
         (check-equal (audio-panner-node-distance-model panner)
                      "linear"
                      "wrapper panner distance model update")
         (check-number= (audio-param-value (audio-panner-node-position-x panner))
                        0
                        "wrapper panner position x")
         (audio-param-set-value! (audio-panner-node-position-x panner) 1)
         (check-number= (audio-param-value (audio-panner-node-position-x panner))
                        1
                        "wrapper panner position x update")
         (check-number= (audio-param-value (audio-panner-node-orientation-x panner))
                        1
                        "wrapper panner orientation x")
         (check-number= (audio-panner-node-ref-distance panner)
                        1
                        "wrapper panner refDistance")
         (audio-panner-node-set-ref-distance! panner 2)
         (check-number= (audio-panner-node-ref-distance panner)
                        2
                        "wrapper panner refDistance update")
         (check-number= (audio-panner-node-max-distance panner)
                        10000
                        "wrapper panner maxDistance")
         (audio-panner-node-set-max-distance! panner 12000)
         (check-number= (audio-panner-node-max-distance panner)
                        12000
                        "wrapper panner maxDistance update")
         (check-number= (audio-panner-node-rolloff-factor panner)
                        1
                        "wrapper panner rolloff")
         (audio-panner-node-set-rolloff-factor! panner 0.5)
         (check-number= (audio-panner-node-rolloff-factor panner)
                        0.5
                        "wrapper panner rolloff update")
         (check-number= (audio-panner-node-cone-inner-angle panner)
                        360
                        "wrapper panner cone inner angle")
         (audio-panner-node-set-cone-inner-angle! panner 180)
         (check-number= (audio-panner-node-cone-inner-angle panner)
                        180
                        "wrapper panner cone inner angle update")
         (check-number= (audio-panner-node-cone-outer-angle panner)
                        360
                        "wrapper panner cone outer angle")
         (audio-panner-node-set-cone-outer-angle! panner 270)
         (check-number= (audio-panner-node-cone-outer-angle panner)
                        270
                        "wrapper panner cone outer angle update")
         (check-number= (audio-panner-node-cone-outer-gain panner)
                        0
                        "wrapper panner cone outer gain")
         (audio-panner-node-set-cone-outer-gain! panner 0.25)
         (check-number= (audio-panner-node-cone-outer-gain panner)
                        0.25
                        "wrapper panner cone outer gain update")

         (define stereo-panner (audio-context-create-stereo-panner ctx))
         (check-true (audio-stereo-panner-node? stereo-panner)
                     "wrapper stereo panner predicate")
         (check-number= (audio-param-value (audio-stereo-panner-node-pan stereo-panner))
                        0
                        "wrapper stereo panner pan")
         (audio-param-set-value! (audio-stereo-panner-node-pan stereo-panner) 0.5)
         (check-number= (audio-param-value (audio-stereo-panner-node-pan stereo-panner))
                        0.5
                        "wrapper stereo panner pan update")

         (audio-context-onstatechange! ctx (lambda (_evt) (void)))
         (check-true #t "wrapper context handler install")
         (audio-context-onstatechange! ctx #f)
         (check-true #t "wrapper context handler clear")

         (audio-oscillator-node-onended! osc (lambda (_evt) (void)))
         (check-true #t "wrapper oscillator handler install")
         (audio-oscillator-node-onended! osc #f)
         (check-true #t "wrapper oscillator handler clear")

         (define listener (lambda (_evt) (void)))
         (audio-add-event-listener! osc "ended" listener #t)
         (check-equal (vector-ref (js-ref osc "lastAdd") 0) "ended"
                      "wrapper add listener name")
         (check-true (external? (vector-ref (js-ref osc "lastAdd") 1))
                     "wrapper add listener callback")
         (check-true (equal? (vector-ref (js-ref osc "lastAdd") 2) #t)
                     "wrapper add listener options")
         (audio-remove-event-listener! osc "ended" listener #f)
         (check-equal (vector-ref (js-ref osc "lastRemove") 0) "ended"
                      "wrapper remove listener name")
         (check-true (external? (vector-ref (js-ref osc "lastRemove") 1))
                     "wrapper remove listener callback")
         (check-true (equal? (vector-ref (js-ref osc "lastRemove") 2) #f)
                     "wrapper remove listener options")

         (check-true (expect-contract-error (lambda () (audio-context-create-gain #f)))
                     "wrapper constructor validation")
         #t)))
