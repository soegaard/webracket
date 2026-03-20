#lang scribble/manual

@(require scribble/manual
          scribble/example
          scribble-tools
          racket/string
          "../structs.rkt"
          "ffi-doc.rkt")

@(define (mdn path [label #f])
   (hyperlink (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" path)
              (or label path)))

@(define dom-ffi-index (load-ffi-doc-index "ffi/dom.ffi"))
@(define dom-ffi-docs (load-ffi-docs "ffi/dom.ffi"))
@(define dom-doc-specs
   (for/list ([fd (in-list dom-ffi-docs)])
     (define name (foreign-racket-name (foreign-doc-foreign fd)))
     (define arg-contracts (ffi-doc-argument-contracts dom-ffi-index name))
     (define args
       (for/list ([c (in-list arg-contracts)]
                  [i (in-naturals 1)])
         (list (string->symbol (format "arg~a" i)) c)))
     (define result (ffi-doc-result-contract dom-ffi-index name))
     (list name args result)))
@(define dom-documented-bindings (map car dom-doc-specs))
@(define (dom-desc name)
   (or (ffi-doc-description dom-ffi-index name)
       (ffi-doc-default-description dom-ffi-index name)))
@(define (dom-sig name)
   (ffi-doc-signature-line dom-ffi-index name "dom.ffi"))
@(define (dom-return-line name)
   (ffi-doc-return-note dom-ffi-index name))
@(define (mdn-label path)
   (define parts (string-split path "/"))
   (if (= (length parts) 2)
       (string-append (car parts) "." (cadr parts))
       path))
@(define (dom-mdn-link name)
   (define path (ffi-doc-mdn-path/default dom-ffi-index name))
   (mdn path (mdn-label path)))
@(define scribble-ns (variable-reference->namespace (#%variable-reference)))
@(define (render-dom-defproc spec)
   (define name   (list-ref spec 0))
   (define args   (list-ref spec 1))
   (define result (list-ref spec 2))
   (eval
    `(defproc* [[(,name ,@args) ,result]]
       (para (dom-desc ',name))
       (if (dom-return-line ',name)
           (para (dom-return-line ',name))
           (list))
       (para (list (bold "MDN:") " " (dom-mdn-link ',name)))
       (para (tt (dom-sig ',name))))
    scribble-ns))
@title{WebRacket Manual}
@table-of-contents[]


@section{Introduction}

The WebRacket project is an attempt to bring Racket programs to the web.

WebRacket compiles programs to WebAssembly (Wasm-GC) and provides a
@tt{#lang webracket} language for writing source programs.

@bold{Warning: This documentation is a work in progress and is not to be
trusted as an authoritative or complete specification.}

This manual currently documents:

@itemlist[
  @item{Installation prerequisites and setup steps.}
  @item{The command-line compiler driver.}
  @item{Library forms that are implemented as syntax-layer helpers.}
]


@section{WebRacket at a Glance}

A high-level overview of what WebRacket is, what it targets, and how the main
pieces fit together.

@subsection{What WebRacket Is}

WebRacket is a @bold{Racket-to-WebAssembly compiler} designed to run practical
Racket programs directly in modern web browsers.

It compiles a substantial subset of Racket to WebAssembly, which you can think
of as portable machine code for the browser.

WebAssembly runs in a virtual machine that is separate from the JavaScript
engine. Because of this separation, a foreign-function interface (FFI) is
required when WebRacket code needs browser features such as DOM operations,
timers, and graphics APIs.

In practice, WebRacket:

@itemlist[
  @item{Targets modern browsers such as Chrome, Firefox, and Safari.}
  @item{Uses a custom runtime designed for WebAssembly.}
  @item{Keeps the boundary to JavaScript explicit via the FFI.}
]

@subsection{What WebRacket Is Not}

WebRacket is not a drop-in replacement for Racket on the Racket VM.
Some features are missing or differ because of browser and WebAssembly
constraints.

WebRacket aims for a useful and predictable subset, with differences reduced
over time.

@subsection{Execution Model}

WebRacket programs execute as single-threaded WebAssembly modules embedded in
a browser environment. This model drives how computation, memory management,
and host interaction are structured.

@itemlist[
  @item{@bold{Single-threaded execution}: no parallel threads or shared-memory
        concurrency.}
  @item{@bold{Custom runtime}: implements core Racket value representations and
        primitive operations.}
  @item{@bold{WebAssembly GC types}: uses Wasm GC for heap-allocated values
        where available.}
]

@subsection{Language Coverage}

WebRacket focuses on practical web programs. It supports many core forms and
data structures, while some areas are still in progress.

Supported areas include:

@itemlist[
  @item{Fixnums and flonums.}
  @item{Strings and byte strings.}
  @item{Symbols and keywords.}
  @item{Pairs, vectors, boxes, and mutable hash tables.}
  @item{The standard expander, including @racket[for] and @racket[match].}
  @item{Tail calls, multiple values, and upward exceptions.}
  @item{Structures with super-structures and properties.}
  @item{Large parts of @tt{racket/base}.}
]

Current limitations include:

@itemlist[
  @item{Single-threaded execution only.}
  @item{Module support is still in progress.}
  @item{No support for continuations and continuation marks.}
  @item{Some numeric and control features are not yet implemented.}
  @item{Keyword-argument behavior is evolving; see the Libraries chapter for
        current @racket[define/key] and @racket[call/key] support.}
]

@subsection{JavaScript FFI}

WebRacket integrates with the browser through an explicit JavaScript FFI.
This is how programs use the DOM, Canvas, and libraries such as MathJax,
Xterm.js, and JSXGraph.

The FFI keeps conversions and effects explicit so boundary behavior stays
predictable and debuggable.

@subsection{Very Brief Compiler Overview}

WebRacket uses a direct-style compiler that translates expanded Racket programs
to WebAssembly through a sequence of code transformations. This structure
makes it easier to relate generated code back to source code.

@section{Installation}

Set up WebRacket for both terminal and browser use.

@subsection{Prerequisites}

You need:

@itemlist[
  @item{@tt{wasm-tools} (Bytecode Alliance) v1.243.0 or newer}
  @item{@tt{node} with support for @tt{--experimental-wasm-exnref}}
  @item{Racket 9.0 or newer}
  @item{@tt{raco-static-web}}
  @item{A local clone of the WebRacket repository}
]

@subsection{Installation Steps}

@bold{1. Install wasm-tools}

Download the latest release:
@url{https://github.com/bytecodealliance/wasm-tools/releases}

Extract and put it on your @tt{PATH}, then verify:

@shellblock{
tar -xvf wasm-tools-1.243.0-aarch64-macos.tar.gz
sudo mv wasm-tools /usr/local/bin/
wasm-tools
}

On macOS, if quarantine blocks execution:

@shellblock{
sudo xattr -d com.apple.quarantine /usr/local/bin/wasm-tools
}

@bold{2. Install Node.js}

Download:
@url{https://nodejs.org/en/download}

Verify Node and required flags:

@shellblock{
node
node --experimental-wasm-exnref --expose-gc
}

@bold{3. Install Racket}

Install Racket 9.0 or newer:
@url{https://download.racket-lang.org/}

@bold{4. Install raco-static-web}

@shellblock{
raco pkg install raco-static-web
raco static-web
}

@bold{5. Install nanopass}

@shellblock{
raco pkg install nanopass
}

@bold{6. Clone WebRacket}

@shellblock{
git clone https://github.com/soegaard/webracket.git
}

@bold{7. Install the local checkout as package @tt{webracket}}

Run in the root of the cloned @tt{webracket} repository:

@shellblock{
raco pkg install --auto --name webracket .
}

@bold{8. Quick test}

Serve examples and open one in your browser:

@shellblock{
cd examples
raco static-web
}

Then open:
@url{http://localhost:8000/}

@section{Command-Line Tool}

This chapter is the current command-line reference for WebRacket. For
installation and first-run walkthroughs, see the website
Quick Start and Installation pages.

The entry point is @tt{webracket.rkt}. It accepts either:

@itemlist[
  @item{One source file plus optional flags (compile mode).}
  @item{No source file together with @tt{--list-primitives} (listing mode).}
]

@subsection{Overview}

The overall goal is to compile a @tt{#lang webracket} source file to a
WebAssembly module.

The generated @tt{.wasm} file is not used alone. It is paired with a generated
host support file:

@itemlist[
  @item{Node mode: a @tt{.js} host file that loads and runs the @tt{.wasm}
        module in the terminal.}
  @item{Browser mode: a @tt{.html} host file that loads and runs the @tt{.wasm}
        module in the browser.}
]

So the normal output is a small set of files that work together: compile
artifact(s), metadata, and host-side runtime support.

@subsection{Canonical Invocation}

@shellblock{
racket webracket.rkt [option ...] <filename>
}

Primitive listing mode:

@shellblock{
racket webracket.rkt --list-primitives [--ffi <file> ...]
}

@subsection{Quick Start (CLI)}

Minimal compile:

@shellblock{
racket webracket.rkt program.rkt
}

Compile and run immediately in Node mode:

@shellblock{
racket webracket.rkt --node --run program.rkt
}

@subsection{Common Tasks}

Compile with default output names:

@shellblock{
racket webracket.rkt program.rkt
}

Choose explicit output files:

@shellblock{
racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt
}

Target browser host generation:

@shellblock{
racket webracket.rkt --browser program.rkt
}

Compile without the standard library:

@shellblock{
racket webracket.rkt --no-stdlib program.rkt
}

Dump compiler passes (limit to 25 dumps):

@shellblock{
racket webracket.rkt --dump-passes tmp/passes --dump-passes-limit 25 program.rkt
}

Print timing information:

@shellblock{
racket webracket.rkt --timings program.rkt
}

Write pretty-formatted WAT (default is non-pretty):

@shellblock{
racket webracket.rkt --pretty-wat program.rkt
}

Include one or more @tt{.ffi} files:

@shellblock{
racket webracket.rkt --ffi dom --ffi standard program.rkt
}

Print all known primitives:

@shellblock{
racket webracket.rkt --list-primitives
}

Print primitives including names from selected @tt{.ffi} files:

@shellblock{
racket webracket.rkt --list-primitives --ffi dom --ffi standard
}

@subsection{Operational Constraints}

@itemlist[
  @item{@tt{--run} cannot be used together with @tt{--browser}.}
  @item{Compile mode requires exactly one source filename.}
  @item{Listing mode (@tt{--list-primitives}) requires no source filename.}
  @item{When using @tt{--run} in Node mode, a working @tt{node} executable is required.}
  @item{A working @tt{wasm-tools} installation is required for WAT-to-WASM assembly.}
  @item{Output paths must be distinct (WAT, WASM, map sidecar, and host output cannot collide).}
  @item{Output locations must be writable.}
  @item{@tt{--dump-passes-limit <n>} requires an exact nonnegative integer.}
]

@subsection{Output Files And Defaults}

For input @tt{program.rkt}, default outputs are:

@itemlist[
  @item{@tt{program.wat}: Text-form WebAssembly produced by the compiler. Useful
        for inspection and debugging.}
  @item{@tt{program.wasm}: Binary WebAssembly module produced from @tt{.wat}.
        This is the executable artifact.}
  @item{@tt{program.wasm.map.sexp}: Label map sidecar metadata used to map
        generated code labels back to source-oriented information.}
  @item{@tt{program.js} (Node mode default): Host support/runtime file that
        loads the @tt{.wasm} module and provides the environment needed to run
        in Node.js.}
  @item{@tt{program.html} (browser mode default): Host support/runtime file that
        loads the @tt{.wasm} module and provides the environment needed to run
        in a browser.}
]

@subsection{Complete Option Reference}

Run behavior:

@itemlist[
  @item{@tt{-r} / @tt{--run}: Run the compiled program after generation (Node mode only).}
]

Target selection (mutually exclusive):

@itemlist[
  @item{@tt{-n} / @tt{--node}: Generate Node host output (default).}
  @item{@tt{-b} / @tt{--browser}: Generate browser host output.}
]

Output path options:

@itemlist[
  @item{@tt{--wat-file <path>}: Explicit WAT output path.}
  @item{@tt{--wasm-file <path>}: Explicit WASM output path.}
  @item{@tt{--host-file <path>}: Explicit host output path.}
]

Debug and inspection options:

@itemlist[
  @item{@tt{-v} / @tt{--verbose}: Verbose compiler messages.}
  @item{@tt{--list-primitives}: Print sorted primitive names and exit.}
  @item{@tt{--label-map-forms}: Include source form payloads in @tt{.wasm.map.sexp} (default).}
  @item{@tt{--no-label-map-forms}: Omit source form payloads in @tt{.wasm.map.sexp}.}
  @item{@tt{--dump-passes <dir>}: Write per-pass dumps to @tt{<dir>}.}
  @item{@tt{--dump-passes-limit <n>}: Limit pass dumps; @tt{0} means no dumps.}
  @item{@tt{--timings}: Print timing breakdown for compilation stages.}
  @item{@tt{--no-pretty-wat}: Write @tt{.wat} without pretty formatting (default).}
  @item{@tt{--pretty-wat}: Write @tt{.wat} with pretty formatting.}
]

Standard library inclusion:

@itemlist[
  @item{@tt{--stdlib}: Include standard library code (default).}
  @item{@tt{--no-stdlib}: Do not include standard library code.}
]

FFI and linker flags:

@itemlist[
  @item{@tt{--ffi <file>}: Add an @tt{.ffi} input file; may be repeated.}
  @item{@tt{-l} / @tt{--link-flags <flag>}: Accepted but currently ignored by the driver.}
]

@subsection{Examples}

@itemlist[
  @item{@shell-code{racket webracket.rkt program.rkt}}
  @item{@shell-code{racket webracket.rkt --list-primitives}}
  @item{@shell-code{racket webracket.rkt --list-primitives --ffi dom --ffi standard}}
  @item{@shell-code{racket webracket.rkt --node --run program.rkt}}
  @item{@shell-code{racket webracket.rkt --browser program.rkt}}
  @item{@shell-code{racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt}}
  @item{@shell-code{racket webracket.rkt --no-stdlib program.rkt}}
  @item{@shell-code{racket webracket.rkt --dump-passes tmp/passes --dump-passes-limit 25 program.rkt}}
  @item{@shell-code{racket webracket.rkt --timings program.rkt}}
  @item{@shell-code{racket webracket.rkt --pretty-wat program.rkt}}
  @item{@shell-code{racket webracket.rkt --ffi dom --ffi standard program.rkt}}
]


@section{Browser API}

Bindings are listed here in reference-manual style.

@subsection{FFI Return Types}

FFI signatures in @tt{.ffi} files use a small set of return-type tags.
In this manual, entries present those tags with user-facing wording:

@itemlist[
  @item{@tt{extern/raw}: shown as @racket[external]}
  @item{@tt{extern}: shown as @racket[(or/c #f external)]}
  @item{@tt{extern/undefined}: shown as @racket[(or/c undefined external)]}
  @item{@tt{extern/nullish}: shown as @racket[(or/c #f undefined external)]}
  @item{@tt{value}: generic WebRacket value}
  @item{@tt{string}: string result}
  @item{@tt{boolean}: boolean result}
  @item{@tt{i32}: 32-bit signed integer}
  @item{@tt{u32}: 32-bit unsigned integer}
  @item{@tt{f64}: 64-bit floating-point number}
  @item{@tt{()}: no result (void)}
]

@subsection{DOM}

These entries come from @tt{ffi/dom.ffi}.

@(for/list ([spec (in-list dom-doc-specs)])
   (render-dom-defproc spec))

@;-------------------------------------------------------------------

@section{Libraries}

@defform[(include-lib lib-id)]{

Includes a WebRacket library by identifier.

The form can be used only at the module top level. A repeated include of the
same library in the same module has no additional effect.

Currently available libraries include:

@itemlist[
  @item{@racket[(require-lib define)] to make @racket[define/key] and
        @racket[call/key] available.}
]
}

@;-------------------------------------------------------------------

@include-section["web-easy.scrbl"]

@;-------------------------------------------------------------------

@section{Keyword arguments}

@defform*/subs[[(define/key (head args) body ...+)]
               ([head id
                      (head args)]
                [args (code:line arg ...)
                      (code:line arg ... @#,racketparenfont{.} rest-id)]
                     [arg arg-id
                     [arg-id default-expr]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id default-expr])])]{

This form is provided by @racket[(require-lib define)].

The form creates a transformer binding for @racket[id].
The binding behaves as follows:

@itemlist[
  @item{In identifier position, @racket[id] expands to a procedure value.}
  @item{In application position, @racket[(id ...)] is rewritten to a call of
        that procedure, where keyword tokens are converted to keyword-value
        expressions.}
]

The generated procedure accepts required positional arguments, optional
positional arguments, an optional rest argument, required keyword arguments,
and optional keyword arguments.

In @tt{#lang webracket}, procedure application rewrites keyword tokens to
keyword values. This means keyword call syntax also works through aliases and
higher-order positions, not only in direct calls to the macro-bound name.

For direct calls to the macro-bound identifier, malformed keyword use is
reported during expansion (for example unknown or duplicate keywords).
Runtime calls raise exceptions for positional arity mismatches and missing
required keyword arguments.

@racketblock[
(require-lib define)

(define/key (f x #:k k)
  (+ x k))

(f 3 #:k 4)

(define/key (g x [y 10] . r)
  (list x y r))

(g 1)
(g 1 2 3 4)

(define h f)
(h 3 #:k 4)
]
}

@defform[(call/key f arg ...)]{

This form is provided by @racket[(require-lib define)].

Calls @racket[f] while allowing keyword-call surface syntax in argument
position. Each keyword token in @racket[arg ...] is rewritten as a keyword
value expression before the call.

This form is still available as an explicit keyword-call rewrite form.
In current @tt{#lang webracket}, direct applications already rewrite keyword
tokens, so @racket[call/key] is usually optional.

@racketblock[
(require-lib define)

(define/key (greet who #:style style)
  (string-append who ":" style))

(define g greet)

(call/key g "Ada" #:style "formal")
]
}
