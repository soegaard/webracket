---
title: Parser Hooks & Terminal Sequences
category: Guides
---

The following guide gives a short overview on how to extend xterm.js' functionality by using parser hooks. With these you can either modify the default behavior for certain terminal sequences or build custom sequences with your own functionality.

### Contents
- [Getting started with Parser Hooks](#getting-started-with-parser-hooks)
  * [What is hookable?](#what-is-hookable)
  * [Lifecycle / Execution Context of Parser Hooks](#lifecycle--execution-context-of-parser-hooks)
  * [Simple Hook Example](#simple-hook-example)
  * [Return Value and Execution Order](#return-value-and-execution-order)
- [Custom Terminal Sequences](#custom-terminal-sequences)
- [Async Actions in Parser Hooks](#async-actions-in-parser-hooks)
- [Limitations of Parser Hooks](#limitations-of-parser-hooks)
- [Background - What are Terminal Sequences?](#background---what-are-terminal-sequences)

## Getting started with Parser Hooks

### What is hookable?

xterm.js currently exposes parser hooks for these terminal sequences types:

- `ESC` type via `parser.registerEscHandler`
- `CSI` type via `parser.registerCsiHandler`
- `DCS` type via `parser.registerDcsHandler`
- `OSC` type via `parser.registerOscHandler`

See the [list of supported sequences]({{site.baseurl}}/docs/api/vtfeatures/) to get an idea, which functionality can be
intercepted or altered by parser hooks. Hooks for single byte control functions (C0/C1) and `PRINT` are currently not exposed. `PM`, `SOS` and `APC`, though being recognized on parser level, are not supported.


### Lifecycle / Execution Context of Parser Hooks

To work with parser hooks correctly it is important to understand, how and when they are executed. xterm.js maintains roughly 4 time slices of execution contexts:
1. Terminal input  
  Whenever a `term.write(data, callback)` is done, the data gets added synchronously to an input write buffer and will return immediately. This is the only action that is guaranteed to be synchronous to your calling context.
2. Input processing  
  The input processing is triggered asynchronously by a `setTimeout` call whenever there is pending write buffer data. Here the transformation of the input data into the terminal state happens, roughly:

    - The data gets decoded to UTF32 codepoints, either from `string` or `Uint8Array` (UTF8 byte sequence).
    - The parser walks the codepoints to identify terminal sequences. For every finished sequence the parser stops parsing and calls the attached sequence handlers (in-band processing). The sequence handlers are hookable ("parser hooks").
    - For every sequence type the parser maintains a list of attached handlers. They are probed in reverse registering order until one returned `true` or the list was exhausted.
    - A sequence handler does the state transformation on the terminal object (mainly involves terminal buffer content changes and cursor updates).

    When the parser has finished processing a single chunk, the callback given to the `write` call gets triggered to indicate that this particular chunk was finally processed and the data gets eventually discarded. So far no screen updates have happened.

    **Note:** The whole parsing and state manipulation process is synchronous to a position in a chunk of data and should not be interupted. With some limitations it is possible to use async code in parser hooks (see [below](#async-actions-in-parser-hooks)). Also with async hooks the execution contexts are more complicated than illustrated here, yet the idea regarding the chunk position remains the same.

3. Screen updates  
  Step 2. is only allowed to run for a certain time. After that time it gets halted at a chunk border and the screen update happens. The renderer code will evaluate the terminal state and decide, what has to be re-drawn. It is not possible to conclude, whether or when a certain chunk of data will finally appear on the screen. Note that there is a chance under heavy data input, that a chunk will never make it to the screen, just because the terminal state has already progressed further before any screen redraw happened (slow scroll mode is not supported by xterm.js).

4. Event processing  
  Any browser event (e.g. mouse movements, keystrokes) will be processed between the other 3 steps.

**Takeaway**:
  - Parser hooks should only contain synchronous code.
  - Parser hooks should return whether a sequence was successfully handled and no further processing shall happen.
  - Parser hooks are blocking regarding the input data, the terminal state will not progress before a hook was finished.
  - Parser hooks should finish quickly to not slow down input processing too much.


### Simple Hook Example

Let's walk the steps to create a custom parser hook by a simple example. Given you want to be informed whenever the cursor gets repositioned absolutely. First we search the corresponding sequence in the list of [supported sequences]({{site.baseurl}}/docs/api/vtfeatures/). It turns out that `CUP` is what we are looking for and is defined as:

```
CSI Ps ; Ps H
```

This tells us 3 important things:
- sequences type is CSI
- the sequence is expected to have up to 2 numerical params
- final byte is `H`, no prefix or intermediate bytes (see below to understand the sequence formats)

With this information creating a hook for `CUP` is straight forward:

```typescript
// a simple custom hook indicating when the cursor got moved by CUP
const myHook = term.parser.registerCsiHandler({final: 'H'}, params => {
  console.log('cursor got repositioned absolutely by CUP');
  return false;   // also probe for other handlers
});
...
// get rid of the custom hook later on
myHook.dispose();
```

Dealing with numerical parameters is abit more tricky, as they are always treated as optional by the parser. This means, that the parser would recognize any of these as a valid `CUP` sequence:
- no params - `CSI H`
- one explicit param - `CSI 10 H`
- two default params - `CSI ; H`
- two explicit params - `CSI 10 ; 20 H`
- more explicit params - `CSI 1 ; 2 ; 3 ; 4 ; 5 H`

If no parameters are given to a CSI sequence, the parser would always append one default parameter as 0. Also any other not explicitly given parameter will be treated as a default value of 0 (zero default mode - ZDM). After these parameter extensions done by the parser, `params` will finally contain the following:
- `CSI H` --> `[0]`
- `CSI 10 H` --> `[10]`
- `CSI ; H` --> `[0, 0]`
- `CSI 10 ; 20 H` --> `[10, 20]`
- `CSI 1 ; 2 ; 3 ; 4 ; 5 H` --> `[1, 2, 3, 4, 5]`

Obviously some of them do not fit into the format definition given for `CUP`. To figure out how a terminal would deal with these edge cases, it is important to know more about handling of default values by a certain sequence function and error recovery in general. We cannot cover this in detail here, so lets just point out, how `CUP` is typically implemented:
- a parameter of 0 defaults to 1
- missing parameters are set to 1
- excessive parameters are ignored

Since the parser does not know anything about these sequence specific rules, we have to apply them manually and can finally work with those values:

```typescript
// custom hook indicating new cursor position set by CUP
const myHook = term.parser.registerCsiHandler({final: 'H'}, params => {
  // 0 defaults to 1
  params = params.map(p => p || 1);
  // fill up to 2 params with default value
  while (params.length < 2) params.push(1);
  // ignore excessive params
  params = params.slice(0, 2);
  // do some work
  console.log({row: params[0], col: params[1]});
  return false;   // also probe for other handlers
});
...
// get rid of the custom hook later on
myHook.dispose();
```

which should lead to this output:
- `CSI H` --> `{row: 1, col: 1}`
- `CSI 10 H` --> `{row: 10, col: 1}`
- `CSI ; H` --> `{row: 1, col: 1}`
- `CSI 10 ; 20 H` --> `{row: 10, col: 20}`
- `CSI 1 ; 2 ; 3 ; 4 ; 5 H` --> `{row: 1, col: 2}`

Congratulations, you have build your first working terminal sequence function by hooking into the parser. For other sequence types it works conceptually similar with slightly different callback interfaces. As you might have figured already by reading the example, creating a properly working terminal sequence hook needs some knowledge about sequence formats and their default behavior. For a general introduction into terminal sequences, see below. For a more detailed description of certain terminal functions, see [ECMA-48](https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf), [DEC STD 070](https://ia801903.us.archive.org/16/items/bitsavers_decstandar0VideoSystemsReferenceManualDec91_74264381/EL-SM070-00_DEC_STD_070_Video_Systems_Reference_Manual_Dec91.pdf) or the [VT520 Programmer Information](https://vt100.net/docs/vt520-rm/ek-vt520-rm.pdf) on [VT100.net](https://vt100.net/). 


### Return Value and Execution Order

The return value of a hook handler determines, whether another handler should be probed. To make sense of this, it is important to know, that hook handlers are registered like event handlers, but in reverse order (last one added always to be probed first by the parser):

```typescript
const term = new Terminal();

// after startup we have one default CUP handler
term.write(`\x1b[H`);  // write CUP(1, 1):   --> default

// register second CUP handler
const customCUP = term.parser.registerCsiHandler({final: 'H'}, params => {...});
term.write(`\x1b[H`);  // write CUP(1, 1):   --> customCUP --> default

// register third CUP handler
const anotherCUP = term.parser.registerCsiHandler({final: 'H'}, params => {...});
term.write(`\x1b[H`);  // write CUP(1, 1):   --> anotherCUP --> customCUP --> default

// register CUP handler, that stops further processing
const stopCUP = term.parser.registerCsiHandler({final: 'H'}, params => { ...; return true; });
term.write(`\x1b[H`);  // write CUP(1, 1):   --> stopCUP
```

This makes it possible to intercept terminal sequences in complex ways. Please note, for built-in sequence support you should never skip the default handler execution by returning `true` from your handler, unless you know what you are doing.


## Custom Terminal Sequences

With the parser hooks system it is quite easy to define your own custom sequences, which can be used to extend xterm.js' functionality even further.

Before doing this, it is important to note, that a custom terminal sequence will most likely break support with other terminals, thus this step should only be considered as a last resort. If interoperability with other terminals is no issue, read on.

Given you want to be able to send some arbitrary data over to xterm.js and handle it in a certain way, these are the steps to achieve that:

- Find a suitable sequence type to carry your data.  
  In general CSI sequences are only useful to carry positive integer parameters up to a certain size and length. In xterm.js they are limited to 32 32-bit parameters (up to 2³¹ - 1). OSC and DCS sequences are defined as ASCII string types, thus allow to carry any ASCII printable characters as payload, which makes them useful for BASE64 encoded binary data transport. Main difference between OCS and DCS is the fact, that DCS additionally supports numercial parameters in the same fashion as CSI sequences do. **Caveat** If you use OSC/DCS sequences to transport arbitrary or string data, always treat the data as untrusted on emulator side that might need further sanitizing before doing any further processing.
- Consult [ECMA-48](https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf) and [DEC STD 070](https://ia801903.us.archive.org/16/items/bitsavers_decstandar0VideoSystemsReferenceManualDec91_74264381/EL-SM070-00_DEC_STD_070_Video_Systems_Reference_Manual_Dec91.pdf) to find a free function identifier.  
  This is important to not clash with any other existing predefined function. Sadly there is no good unified ressource about pre-taken function identifiers across common terminal emulators. If in doubt, also consult their documentation. As a rule of thumb - resort to private area for CSI (see ECMA-48, watch out for DEC privates) or take a rather high number (>1000) for an OSC function.
- Create a hook to digest the data.

Let's illustrate this further by a quick example. Given you want to be able send MIDI data over to xterm.js to be output by the browser. Furthermore you want to be able to alter the pitch before outputting it, thus you have basically this abstract function declaration:

```typescript
function outputMidi(pitch: number, payload: MidiData);
```

For payload we either need a sequence type of OSC or DCS, as CSI cannot transport it. Now DCS can express positive integer values as parameters directly, it might be good enough for us. If `pitch` shall be `float` or allowed to be negative, this might not work anymore and we would have to put `pitch` into the payload as well.

If a positive integer is enough, we can get away with this DCS sequence definition:

```
DCS ? Ps a Pt ST  - play BASE64 encoded midi data in Pt pitched by Ps
```

which defines a DCS sequence with the identifier `{prefix: '?', final: 'a'}` to contain one numerical parameter `Ps` and payload in `Pt`. As noted above, selecting a free function identifier is tricky and involves searching through official documentation. To our knowledge a combination of '?' as prefix byte and 'a' as the final byte is not yet taken in DCS. Further note that the allowed bytes are also limited (see API documentation for `IFunctionIdentifier`). Those limits are tested by xterm.js and will throw an error if not respected.

Now on xterm.js side this can be set up as follows:

```typescript
const midiHandler = term.registerDcsHandler({prefix: '?', final: 'a'}, (params, data) => {
  // since params are optional, we have to provide some sane default
  // default: pitch to 440 Hz
  const pitch = params[0] || 440;
  // convert BASE64 string back to your midi format
  const midiData = convertB64ToMidi(data);
  // sanity checks (never skip that step, as the data might be malicious)
  if (isValidData(midiData)) {
    // some midi player you wrote before
    midiPlayer.play(pitch, atob(data));
  }  
});
```

Now a program running in xterm.js can directly play midi with a certain pitch:

```typescript
process.stdout.write(`\x1bP?${pitch}a${binaryMidiData.toString('base64')}\x1b\\`);
process.stdout.write(`\x1bP?a${binaryMidiData.toString('base64')}\x1b\\`);  // default pitch of 440 Hz
```

Hopefully this small example illustrates the power of the hooks system. It makes xterm.js a great playground for experimenting with new terminal features by utilizing the npm ecosystem, which might help to get terminals out of the dusty corner of hardly noticed, yet very helpful everyday utilities.

For production usage always keep in mind, that extending xterm.js this way will make your program incompatible with other terminal emulators. Still a well thought-out extension might find its way into other emulators, if it is seen as a useful extension to the terminal interface in general.


## Async Actions in Parser Hooks

Normally a custom parser handler should only contain synchronous code and finish rather quickly. All default handlers shipped with xterm.js are build that way to provide very fast input processing.

Still there are circumstances where the invocation of an async interface is needed before the input processing should continue.
For these cases a handler may also return a promise which resolves to a `boolean`:

```typescript
const myCustomHandler = term.registerCsiHandler(..., params => {
  if (canBeSyncCode) {
    // fast sync path
    ...
    return false;
  }
  // slow async path
  const someWaitingCondition: Promise<any> = asyncInterface(...);
  return someWaitingCondition
    .then(more)
    .then(steps)
    .then(() => false)
});
```
Or with `async`/`await` syntax:

```typescript
const myCustomHandler = term.registerCsiHandler(..., async params => {
  const someWaitingCondition: Promise<any> = asyncInterface(...);
  await steps(await more(await someWaitingCondition));
  return false;
});
```

Whenever a handler returns a promise, the parser stops input processing at the current band position
and returns control to the top level (stack unwinding). Input processing will not resume until
that promise got fulfilled (resolved or rejected). With this in-band blocking the parser can
guarantee the right execution order of sequence handlers, the terminal state will not progress by later
input data before the active async handler finally finished its work.

Note that the second example should only be used, if all control paths have to go through async code.
As soon as your handler has an opportunity for a sync code path, the first example should be preferred
to benefit from the parser's sync code optimization.


Caveats regarding async parser handlers:
- **general slowdown of input processing**  
  Due to needed stack unwinding async handlers come to a rather high price of poor parsing throughput.
  They should only be used if a synchronous implementation is not applicable.
- **can block terminal input forever**  
  Make sure that a returned promise will always be fulfilled in a timely fashion to avoid
  poor user experience. With `logLevel.WARN` or higher, the terminal will warn about an async handler
  taking too long (after 5s), which might be helpful during development. Note that a dangling promise,
  that never gets fulfilled, will render the terminal unusable (partially recoverable by calling `Terminal.reset`).
- **partially mutating terminal state**  
  Although the terminal state will not progress by data input between several `Thenables` of an active async handler,
  it still will change shape from other user actions like resizing or calling `Terminal.reset`. Always re-check
  your state assumptions at the beginning of a `Thenable` before applying your changes.
- **proper insert/error strategy**  
  If your handler has to rely on multiple event loop invocations (multiple `Thenables`) try to avoid
  altering the terminal state piece-by-piece. Instead collect your own state information and insert it late
  in one action. This way chances are low to leave garbage on the terminal due to some late error in your handler.
  Note that the terminal tries to keep working on async errors and logs the error to `console`.
  Still expected errors should be properly guarded as usual for promised code (skipped in examples above for better
  readability).

_Given all those limitations, what are indicators to actually use an async handler?_
- any functionality depending on async browser interfaces to overcome their "viral" nature
- handlers with perceivable negative impact on mainthread performance
  (high computation needs or busy waiting conditions)
- a slow consuming handler to get proper flow control with backpressure
- exceptional rare sequences with almost no impact on normal input data flow


## Limitations of Parser Hooks

- **Filtering of Parameters**  
  Some CSI sequences like `SGR` support parameter stacking, where these calls:

  - `SGR 0`
  - `SGR 1`
  - `SGR 2`

  whould be equivalent to this single call:
  - `SGR 0 ; 1 ; 2`

  In the first case the parser would call 3 times into an `SGR` hook with one parameter, while the latter would create one call with all 3 paramters applied at once. While in the first case a hook can deliberately stop the call propagation for a certain parameter value, it is not possible to do that in the latter case without stopping the other parameters as well. If you run into this edge case, please file an issue to discuss, whether we should implement parameter filtering.

- **Reduced Terminal State**  
  xterm.js exposes only a reduced set of the terminal state via its public API. This is mainly to keep the API as stable and useful as possible without cluttering it too much. If you have an urgent need for some missing bits, please file an issue to discuss this further.

- **Payload limits for OSC/DCS**  
  The OSC and DCS API have a hardcoded payload limit of 10MB to avoid running into out-of-memory issues.



## Background - What are Terminal Sequences?

Terminal sequences are instructions targeted at a terminal (emulator) to alter certain aspects of data processing and data presentation. They are directly embedded in the data stream (in-band) and can be seen as a kind of markup system for the terminal.

**Note:** Other than most modern document based markup languages, many terminal sequences do not enforce enclosing data (as seen in XML, e.g. &lt;command&gt;data&lt;/command&gt;), instead they operate in a more stream friendly manner by manipulating a terminal state immediately that stays active until another sequence might alter it again.

Terminal sequences have a long history and originate in the need to control certain aspects of mechanical input/output devices like real typewriters, well known examples:
- **LF** (line feed `'\n'`)  
  instructs the typewriter to advance the paper by a line
- **CR** (carriage return `'\r'`)  
  instructs the typewriter to move the carriage back to the home position (beginning of the line)

In the 60s these basic control functions were specified as single byte control codes taking the lower 32 positions (**C0** control codes) and were later extended by 32 more codes (**C1** control codes). Many control codes were widely adopted and survived as fundamental design principles in data processing until now, others were more specific to how machines interacted in the 60s and are less likely to be seen these days.

Still in common use:
- **LF** (line feed `'\n'`)  
Moves the printing position one line down (sometimes also to the left). Used as end-of-line marker (EOL) in Unix and derived operating systems.
- **CR** (carriage return `'\r'`)  
Moves the printing position to the start of the line, allowing overprinting. Used in combination with line feed CRLF as end-of-line marker (EOL) in Windows and several protocols like HTTP, FTP and SMTP.
- **HT** (horizontal tab `'\t'`)  
Moves the printing position right to the next tab stop. Understood by most editors.
- **NUL** (null `'\0'`)  
Used to mark the end of a string by many programming languages known as "null termination".
- **ESC** (escape)  
Introducer of escape sequences. Also used by some editors to switch into a command mode (e.g. vim).
- **SUB** (substitute, Ctrl-Z)  
Used for job control in many POSIX shells to signal SIGSTP to a foreground process, end-of-file marker (EOF) under Windows.
- **ETX** (end of text, Ctrl-C)  
Used to interupt a running process from a controlling terminal.
- **EOT** (end of transmission, Ctrl-D)  
Used as end-of-file marker (EOF) under POSIX (exiting a foreground process).


With the upcoming of the more capable video terminals in the early 70s there was the need to handle new features that could not be controlled by a single byte anymore, which eventually led to the specification of longer sequences spanning multiple bytes (ECMA-48, first release in 1976, with several revisions up to 1991):
- **ESC - ESCAPE**
  - common form: `ESC <optional intermediate bytes> <final byte>`
  - usage: simple control functions without further params (similar to C0/C1, further subspecified by ECMA-35)
- **CSI - CONTROL SEQUENCE INTRODUCER**
  - common form: `CSI <optional prefix byte> P1 ; P2 ; ... <optional intermediate bytes> <final byte>`
  - usage: control functions with numerical params (P1, P2, ...)
- **OSC - OPERATING SYSTEM COMMAND**
  - common form: `OSC <identifier> ; <payload> ST`
  - usage: control functions meant to deal with certain OS aspects, with string payload (printables)
  - note: The identifier is not part of any official specification, still using a numercial function identifier became a de-facto standard these days.
- **DCS - DEVICE CONTROL STRING**
  - common form: `DCS <optional prefix byte> P1 ; P2 ; ... <optional intermediate bytes> <final byte> <payload> ST`
  - control functions meant to deal with the device (the terminal itself), with string payload (printables)
  - note: The form listed here is adopted from DEC, while ECMA-48 does not specify the inner structure of DCS commands.
- **APC/SOS/PM** (skipped here, since mostly unsupported by common emulators)

These sequences are known as "ANSI escape sequences" (they all start with the escape control code `ESC` in 7-bit mode and were adopted by ANSI in 1979). They build the foundation of most control functions of a terminal along with the older C0/C1 control codes.

In the late 70s and early 80s, many vendors came up with different terminal types and tons of control functions (most notably DEC with its [VT series](https://vt100.net/)). The number of partly incompatible devices was a major burden for software developers, which led to the idea to collect their features (`termcap`/`terminfo`) and to level out the differences by higher level TUI libraries (`curses`/`ncurses` under Unix). This made it possible to support several terminal types without the need to write terminal specific sequences yourself. Most advanced command-line applications on POSIX systems use those libraries or alternatives today.

With the upcoming of affordable, more capable graphical hardware in the 90s and the wider adoption of GUIs in general, hardware terminals became quickly obsolete. Still POSIX derived operating systems kept their strong affiliation to the command-line interface in the form of terminal emulators running in the GUI.

Prominent early emulators of the 80s are `DECTerm` and `xterm`, which both aimed primarily for DEC VT series compatibility. While `DECTerm` was seen as superior in the beginning in terms of emulated capabilities, `xterm` soon closed the gap, was widely available as an open source emulator and eventually became the de-facto standard of VT compatible emulation these days. Despite its age, `xterm` is still under very active development by Thomas E. Dickey ([changelog](https://invisible-island.net/xterm/xterm.log.html)).
