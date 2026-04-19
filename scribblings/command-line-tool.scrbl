#lang scribble/manual

@(require scribble-tools)

@title{Command-Line Tool}

This chapter is the current command-line reference for WebRacket. For
installation and first-run walkthroughs, see the website
Quick Start and Installation pages.

The entry point is @tt{webracket.rkt}. It accepts either:

@itemlist[
  @item{One source file plus optional flags (compile mode).}
  @item{No source file together with @tt{--list-primitives} (listing mode).}
]

@section{Overview}

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

@section{Canonical Invocation}

@shellblock{
racket webracket.rkt [option ...] <filename>
}

Primitive listing mode:

@shellblock{
racket webracket.rkt --list-primitives [--ffi <file> ...]
}

@section{Quick Start (CLI)}

Minimal compile:

@shellblock{
racket webracket.rkt program.rkt
}

Compile and run immediately in Node mode:

@shellblock{
racket webracket.rkt --node --run program.rkt
}

@section{Common Tasks}

Compile with default output names:

@shellblock{
racket webracket.rkt program.rkt
}

Choose explicit output files:

@shellblock{
racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt
}

Choose a destination directory for the default outputs:

@shellblock{
racket webracket.rkt --dest out program.rkt
}

Explicit `--wat-file`, `--wasm-file`, and `--host-file` options still take
precedence over `--dest`.

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

@section{Operational Constraints}

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

@section{Output Files And Defaults}

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

@section{Complete Option Reference}

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
  @item{@tt{--dest <dir>}: Write the default outputs under @tt{<dir>}.}
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

@section{Command-Line Examples}

@itemlist[
  @item{@shell-code{racket webracket.rkt program.rkt}}
  @item{@shell-code{racket webracket.rkt --list-primitives}}
  @item{@shell-code{racket webracket.rkt --list-primitives --ffi dom --ffi standard}}
  @item{@shell-code{racket webracket.rkt --node --run program.rkt}}
  @item{@shell-code{racket webracket.rkt --browser program.rkt}}
  @item{@shell-code{racket webracket.rkt --wat-file out/program.wat --wasm-file out/program.wasm --host-file out/program.js program.rkt}}
  @item{@shell-code{racket webracket.rkt --dest out program.rkt}}
  @item{@shell-code{racket webracket.rkt --no-stdlib program.rkt}}
  @item{@shell-code{racket webracket.rkt --dump-passes tmp/passes --dump-passes-limit 25 program.rkt}}
  @item{@shell-code{racket webracket.rkt --timings program.rkt}}
  @item{@shell-code{racket webracket.rkt --pretty-wat program.rkt}}
  @item{@shell-code{racket webracket.rkt --ffi dom --ffi standard program.rkt}}
]
