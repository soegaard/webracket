# WebRacket

The WebRacket language is a subset of Racket that compiles to WebAssembly (wasm).

The goal is eventually to have a compiler for full Racket. 
However, to quote Piet Hein, "Things take time".
The subset supported by the WebRacket compiler is however large enough,
to enable programmers to build practical programs for the web.

The generated WebAssembly can be run either in the terminal (via Node) or in the browser.
The browser is the main focus.
WebAssembly is somewhat of a moving target.
The compiler only uses widely supported features of WebAssembly. 
Expect the generated code to work in Chrome, Firefox and Safari.

A JavaScript FFI makes it possible to use standard JavaScript functions as well as browser specific APIs. 
Included are bindings for the DOM, Canvas, MathJax, XTermJS and JSXGraph.

The hope is that this project allows the Racket community to experiment with WebAssembly.
The ideal outcome is that the experience can be used to extend the normal Racket compiler
with a WebAssembly backend. In the mean time, we can have fun writing Racket programs
that runs in the browser.


# Overview of the supported language subset

The WebRacket compiler accepts a file containing a top-level form as input.
The `module` form is not supported. However, `include` is available.

For supported data types most functions in `racket/base` and `racket` 
are available as primitives. Most are implemented directly in WebAssembly,
some are reimplemented in WebRacket.


## Datatypes
Most basic data types are implemented, some have restrictions.

### Numbers
The numerical tower only contains only flonums and fixnums.
Complex numbers and bignums are missing.

### Hash Tables
Mutable hash tables of all for varities (eq?, eqv?, equal?, always?) are supported.
Immutable hash tables are not yet supported.
The values of a hash table are strongly held (i.e. no weak hash tables yet).

### Regular Expressions
No direct support for regular expressions at the moment.
These will materialize when the support for linklets and modules improve.
[The `expander` source contains an implementation of regular expressions.]

### Ports
Since the main target is the browser, only string (and byte string) ports are supported.
If there is interest for file ports (for the terminal), let me know.

### Structures
Most structure related bells and whistles are implemented including super structures, 
structure properties and applicable structures. 
Most notable feature missing is prefab structures.



## Syntactic Forms

The WebRacket compiler uses the standard Racket expander, so a large number
of syntactic forms are supported. This includes `for` and `match`.

Most notable omissions are the forms `module`, `module*` and `with-continuation-mark`.

## Control Flow

Tail calls are supported.
Multiple values are supported.
Upward flowing exceptions are supported.

Continuations and continuation marks are not supported.
In particular, there is no support for `call/cc`  (sorry Shriram).

Other omissions: promises, breaks, exit and black box.

## Concurrency and Parallelism

Single threaded for now.


## Foreign Function Interface

The JavaScript foreign function interface is used to access browser functionality.
The hope is that the community will help write bindings for commonly used libraries.
To some degree the generation of foreign function interfaces can be automated
with the help of an LLM.

Included are bindings for the DOM, Canvas, MathJax, XTermJS and JSXGraph.


# The Road Ahead

After the initial release, the focus is to fix bugs found by early adopters.

Then the top priority is to support modules.
Work on implementing linklets (needed to support modules) have already started.

Due to my interests, complex numbers and bignums are likely to appear sooner rather than later.

Impersonators and chaperones are needed to support contracts.

Unlocking modules (and linklets) will also unlock the full implementation of 
regular expressions present in the source of the Racket expander.

Support for continuations and continuation marks, although high on the wish list,
is something that is trickier to implement given the nature of the target.
Last resort is to add a CPS pass to the compiler.


# Installation

The WebRacket compiler depends on two external programs: `wasm-tools` and `node`.

The `wasm-tools` project by Bytecode Alliance consists of a suite of WebAssembly tools.
WebRacket uses the `wasm-tools` to compile WebAssembly source files (.wat)
(in S-expression format) to bytecode (.wasm).

Node (or Node.js) is a JavaScript runtime environment used to run JavaScript programs in the terminal. 
WebRacket uses Node to run programs directly in the terminal. 
This is useful for testing programs that do not depend on browser functionality.
A relatively new version is needed. 

When the target is the browser, the WebRacket compiler can optionally produce
an html file that loads the generated WebAssembly program. In order to test
locally, the package `raco-static-web` by Sam Philips is very convenient.


## wasm-tools

1. Download the lates release from:

       https://github.com/bytecodealliance/wasm-tools/releases

2. Unpack the tar-ball:

       tar -xvf wasm-tools-1.243.0-aarch64-macos.tar.gz

3. Open `README.md` in the browser to see further instructions.

4. Make sure to place `wasm-tools` in somewhere in your PATH.
   On macOS, you can do it with:
   
       sudo mv wasm-tools /usr/local/bin/

5. Test that `wasm-tools` works:

       wasm-tools

Depending on security settings, you might get a dialog on macOS.
If so, open the system prefences and find the "Privacy and Security" tab.
Then allow `wasm-tools` to run


## Node.js

1. Go to https://nodejs.org/en/download and follow the instructions.

2. Test that `node` works in the terminal (and that it is in you path).

       node

WebRacket needs support for `exnref` so your Node version needs
to accept the `--experimental-wasm-exnref` flag.


Here is what I see, when Node is started:

    % node --experimental-wasm-exnref --expose-gc
    Welcome to Node.js v24.9.0.
    Type ".help" for more information.


## Racket 

The compiler needs Racket 9 or newer.

## raco-static-web

1. Install the web-server using `raco`.

       raco pkg install raco-static-web

2. Test it works. Go to a folder that holds an html file.
   Then start the web-server with:
   
       raco static-web

   Follow the printing instructions.



## Short Compiler Overview

The WebRacket compiler is a direct-style compiler.
This choice has made it easier to relate the generated code to the source program.
In the future we will probably need to add a CPS-pass in order to support
continuations and continuation marks.

The frontend of the WebRacket compiler uses `read-syntax` to read 
a WebRacket program from a file. The resulting syntax object is fed
into the normal Racket expander to produce a program in fully expanded form.

The middle end of the compiler consists of several passes implemented
using the NanoPass framework.

The passes are as follows:

    unexpand
    parse
    flatten-topbegin
    infer-names
    convert-quotations
    explicit-begin
    explict-case-lambda
    α-rename
    assignment-conversion
    categorize-applications
    anormalize
    closure-conversion
    flatten-begin
    (classify-variables)
    generate-code

See the comments in "compiler.rkt" for an explanation of each pass.

The code generator generates WebAssembly in the form of S-expressions
in the "folded" format.

This code generator is inspired by "Destination-driven Code Generation"
by Dybvig, Hieb and Butler. There are som differences however. The code
generator in the paper generates "flat" code (assembler) where as we
generate nested Web Assembly instructions.

Finally, the external tool `wasm-tools parse` converts the S-expression
representation into bytecode format.

The main part of the compiler is in "compiler.rkt".
The WebAssembly runtime is in "runtime-wasm.rkt".
The standard library (implemented in WebRacket) is found in `stdlib/`.
FFI bindings for popular libraries are in `ffi/`.




