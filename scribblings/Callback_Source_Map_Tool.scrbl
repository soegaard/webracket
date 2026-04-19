#lang scribble/manual

@(require scribble-tools)

@title{Callback Source-Map Tool}

This chapter documents the helper script
@tt{tools/find-callback-source.rkt}.

The tool is meant for callback debugging. When WebRacket reports a callback
error, the message may include a debug label such as @tt{label.4054}. That
label is emitted by the compiler and also recorded in the generated
@tt{.wasm.map.sexp} file.

The tool lets you go from:

@itemlist[
  @item{a callback error message containing @tt{debug-id: label.N}}
  @item{to the corresponding source file, line, and column}
]

@section{When To Use It}

Use this tool when:

@itemlist[
  @item{a browser or Node callback error names a failing callback with a
        @tt{debug-id}}
  @item{you want to find the source location for the generated callback label}
  @item{you want to inspect the source form recorded in the label map}
]

Typical workflow:

@itemlist[
  @item{Reproduce the callback error.}
  @item{Copy the @tt{debug-id} label from the error message.}
  @item{Run @tt{tools/find-callback-source.rkt} on the corresponding
        @tt{.wasm.map.sexp} file.}
]

@section{Basic Usage}

The command takes two required arguments:

@itemlist[
  @item{the path to a @tt{.wasm.map.sexp} file}
  @item{the label to look up, such as @tt{label.4054}}
]

@bold{Example}

@shellblock{
racket tools/find-callback-source.rkt test/test-ffi.wasm.map.sexp label.5792
}

The output prints:

@itemlist[
  @item{the label}
  @item{the source path}
  @item{the source line and column}
  @item{the recorded span}
  @item{the @tt{same-as} target when present}
]

Example output shape:

@verbatim[#:indent 2]{
label: label.5792
source: /path/to/file.rkt:667:32
span: 3482
}

@section{Showing The Recorded Form}

When the label map was generated with form payloads enabled, the tool can also
print the recorded source form.

Use @tt{--show-form}:

@shellblock{
racket tools/find-callback-source.rkt --show-form web-site-new/src/test-toc.wasm.map.sexp label.4054
}

Example output shape:

@verbatim[#:indent 2]{
label: label.4054
source: /path/to/file.rkt:107:2
span: 135
form: "(let-values ...)"
}

@section{Relationship To Callback Errors}

Callback errors can now include a machine-oriented debug identifier:

@verbatim[#:indent 2]{
WebRacket callback error
  kind: arity-mismatch
  name: on-click
  id: 0
  debug-id: label.4054
  expected: 0
  argc: 1
  detail: callback arity mismatch
}

In that case, use the label from the @tt{debug-id} line directly:

@shellblock{
racket tools/find-callback-source.rkt path/to/program.wasm.map.sexp label.4054
}

@section{Requirements}

The tool expects the generated source map sidecar file produced by the normal
WebRacket compiler flow:

@itemlist[
  @item{@tt{program.wasm.map.sexp}}
]

The source-form output depends on the label map including form payloads. That
is the default compiler behavior. If the map was generated with
@tt{--no-label-map-forms}, source locations still work, but @tt{--show-form}
may print nothing for the form.

@section{Notes}

@itemlist[
  @item{The tool is read-only. It does not modify the source map.}
  @item{The label only needs to be stable within one build artifact set.
        Use the @tt{.wasm.map.sexp} file that was produced together with the
        failing program.}
  @item{Some entries are aliases recorded with @tt{same-as}. The tool prints
        that alias target when present.}
]
