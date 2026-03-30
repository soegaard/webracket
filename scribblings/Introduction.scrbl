#lang scribble/manual

@title{Introduction}

The WebRacket project is an attempt to bring Racket programs to the web.

WebRacket compiles programs to @hyperlink["https://webassembly.org/"]{WebAssembly}.
WebAssembly can be thought of as "portable machine code". A host is needed
to run a WebAssembly program.
We support two host types: @hyperlink["https://nodejs.org/"]{Node.js} and browsers.

The @hyperlink["https://nodejs.org/"]{Node.js} host can be used to run WebRacket programs in the terminal.
This is useful for testing the parts of a program that do not involve
browser-specific functionality.

The browser host allows you to run a WebRacket program in
most modern browsers including @hyperlink["https://www.google.com/chrome/"]{Chrome},
@hyperlink["https://www.mozilla.org/firefox/"]{Firefox}, and
@hyperlink["https://www.apple.com/safari/"]{Safari}.

There are direct bindings for several browser APIs such as
DOM, Canvas, MathJax, XtermJS, and JSXGraph.
If you need other browser APIs, you can use the dynamic FFI.

WebRacket does not support all features of Racket - but the subset supported
is large enough to write proper webapps.

The idea of WebRacket is not to replace the current Racket compiler.
Rather the idea is that WebRacket can be used to prototype ideas
that can influence a future Racket WebAssembly backend.


@bold{Warning: This documentation is a work in progress. Details can change.}
