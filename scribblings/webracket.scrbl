#lang scribble/manual

@(require scribble/manual
          scribble/example)

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
  @item{The command-line compiler driver.}
  @item{Library forms that are implemented as syntax-layer helpers.}
]

@section{Command-Line Tool}

This chapter is the current command-line reference for WebRacket. For
installation and first-run walkthroughs, see the website
Quick Start and Installation pages.

The entry point is @tt{webracket.rkt}. It accepts one source file and optional
flags.

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

@verbatim{
racket webracket.rkt [option ...] <filename>
}

@subsection{Quick Start (CLI)}

Minimal compile:

@verbatim{
racket webracket.rkt program.rkt
}

Compile and run immediately in Node mode:

@verbatim{
racket webracket.rkt --node --run program.rkt
}

@subsection{Common Tasks}

Compile with default output names:

@verbatim{
racket webracket.rkt program.rkt
}

Choose explicit output files:

@verbatim{
racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt
}

Target browser host generation:

@verbatim{
racket webracket.rkt --browser program.rkt
}

Compile without the standard library:

@verbatim{
racket webracket.rkt --no-stdlib program.rkt
}

Dump compiler passes (limit to 25 dumps):

@verbatim{
racket webracket.rkt --dump-passes tmp/passes --dump-passes-limit 25 program.rkt
}

Print timing information:

@verbatim{
racket webracket.rkt --timings program.rkt
}

Include one or more @tt{.ffi} files:

@verbatim{
racket webracket.rkt --ffi dom --ffi standard program.rkt
}

@subsection{Operational Constraints}

@itemlist[
  @item{@tt{--run} cannot be used together with @tt{--browser}.}
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
  @item{@tt{--label-map-forms}: Include source form payloads in @tt{.wasm.map.sexp} (default).}
  @item{@tt{--no-label-map-forms}: Omit source form payloads in @tt{.wasm.map.sexp}.}
  @item{@tt{--dump-passes <dir>}: Write per-pass dumps to @tt{<dir>}.}
  @item{@tt{--dump-passes-limit <n>}: Limit pass dumps; @tt{0} means no dumps.}
  @item{@tt{--timings}: Print timing breakdown for compilation stages.}
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
  @item{@tt{racket webracket.rkt program.rkt}}
  @item{@tt{racket webracket.rkt --node --run program.rkt}}
  @item{@tt{racket webracket.rkt --browser program.rkt}}
  @item{@tt{racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt}}
  @item{@tt{racket webracket.rkt --no-stdlib program.rkt}}
  @item{@tt{racket webracket.rkt --dump-passes tmp/passes --dump-passes-limit 25 program.rkt}}
  @item{@tt{racket webracket.rkt --timings program.rkt}}
  @item{@tt{racket webracket.rkt --ffi dom --ffi standard program.rkt}}
]

@section{Libraries}

@defform[(include-lib lib-id)]{

Includes a WebRacket library by identifier.

The form can be used only at the module top level. A repeated include of the
same library in the same module has no additional effect.

Currently available libraries include:

@itemlist[
  @item{@racket[(include-lib define)] to make @racket[define/key] and
        @racket[call/key] available.}
]
}

@subsection{Keyword arguments}

@defform*/subs[[(define/key (head args) body ...+)]
               ([head id
                      (head args)]
                [args (code:line arg ...)
                      (code:line arg ... @#,racketparenfont{.} rest-id)]
                     [arg arg-id
                     [arg-id default-expr]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id default-expr])])]{

This form is provided by @racket[(include-lib define)].

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
(include-lib define)

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

This form is provided by @racket[(include-lib define)].

Calls @racket[f] while allowing keyword-call surface syntax in argument
position. Each keyword token in @racket[arg ...] is rewritten as a keyword
value expression before the call.

This form is still available as an explicit keyword-call rewrite form.
In current @tt{#lang webracket}, direct applications already rewrite keyword
tokens, so @racket[call/key] is usually optional.

@racketblock[
(include-lib define)

(define/key (greet who #:style style)
  (string-append who ":" style))

(define g greet)

(call/key g "Ada" #:style "formal")
]
}
