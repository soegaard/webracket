#lang scribble/manual

@(require scribble-tools)

@title[#:tag "console-bridge"]{The Browser Console Bridge}

The browser console bridge lets a generated browser page expose selected
WebRacket bindings to the browser's JavaScript console.

In practice, it gives you a small helper named @tt{WR} in the page.
You can then use the browser console to:

@itemlist[
  @item{look up a top-level WebRacket variable}
  @item{call a top-level WebRacket function}
  @item{inspect which names are available}
  @item{debug a running page without editing the source file first}]

This chapter explains what the bridge is, what it is for, how to enable
it, and how to use it from the browser console.

@section{What It Is}

Normally, a WebRacket program runs inside WebAssembly, and the browser
console runs JavaScript.

Those are two different worlds:

@itemlist[
  @item{your WebRacket definitions live inside the compiled WebAssembly program}
  @item{the browser console can only see ordinary JavaScript values}]

The console bridge is a small browser-side helper that connects those
two worlds.

When the bridge is enabled, WebRacket installs a function named
@tt{globalThis.WR}. In the browser console, that means you can write:

@verbatim|{
WR("x")
WR("add1*", 41)
WR.call("current-counter")
WR.search("display")
WR.help()
WR.names()
}|

and the page asks the running WebRacket program to look up or call the
corresponding binding.

@section{What It Is For}

The console bridge is mainly a debugging and exploration tool.

It is useful when you want to:

@itemlist[
  @item{check the current value of a top-level variable}
  @item{call a helper function in a running page}
  @item{see whether a name from your program or the standard library is available}
  @item{experiment from the console while developing browser code}]

It is @bold{not} a general-purpose evaluator for arbitrary Racket
source text.

For example, this works:

@verbatim|{
WR("counter")
WR("increment!", 1)
}|

But this does @bold{not} mean “evaluate a Racket expression from a
string”:

@verbatim|{
WR("(+ 1 2)")
}|

That string is treated as a binding name, not as source code to read and
evaluate.

@section{How To Enable It}

The bridge is off by default.

To enable it, compile your browser program with
@tt{--console-bridge}:

@shellblock{
racket webracket.rkt --browser --console-bridge counter.rkt
}

This generates the usual browser output files, such as:

@itemlist[
  @item{@tt{counter.html}}
  @item{@tt{counter.wasm}}]

When you open @tt{counter.html} in a browser, the page installs
@tt{WR} in the browser console.

@section{Complete Example}

Here is a small complete example:

@filebox["counter.rkt"
@verbatim|{
(define counter 0)

(define (increment! amount)
  (set! counter (+ counter amount))
  counter)

(define (current-counter)
  counter)

(display "counter page ready")
}|]

There is also a checked-in example in the repository:

@itemlist[
  @item{@filepath{examples/console-bridge/console-bridge.rkt}}]

Compile it:

@shellblock{
racket webracket.rkt --browser --console-bridge counter.rkt
}

Serve the generated files from the directory where they were written:

@shellblock{
raco static-web
}

Then open the URL printed by @tt{raco static-web} in your browser and
visit @tt{counter.html}.

Open Developer Tools and switch to the @tt{Console} tab.

At that point, the page is running and the console bridge is available.

@section{Opening The Browser Console}

If you have not used the browser console before, the general pattern is:

@itemlist[
  @item{open the generated page in your browser}
  @item{open Developer Tools}
  @item{choose the @tt{Console} tab}
  @item{type calls such as @tt{WR("counter")} and press Enter}]

In Chrome and other Chromium-based browsers, common shortcuts are:

@itemlist[
  @item{@tt{F12}}
  @item{@tt{Ctrl+Shift+I} on Windows and Linux}
  @item{@tt{Cmd+Option+I} on macOS}]

If the page is open but @tt{WR} is missing, the usual cause is that the
program was compiled without @tt{--console-bridge}.

@section{Console Session}

Here is an example of what a browser console session can look like.

Look up a variable:

@verbatim|{
> WR("counter")
0
}|

Call a function:

@verbatim|{
> WR("increment!", 5)
5
}|

Read the variable again:

@verbatim|{
> WR("counter")
5
}|

Call a zero-argument function explicitly:

@verbatim|{
> WR.call("current-counter")
5
}|

Ask for a quick reminder:

@verbatim|{
> WR.help()
WebRacket console bridge
  WR(name, ...args)        Look up or call a binding and print the WebRacket result.
  WR.value(name, ...args)  Return the converted JavaScript value without printing.
  ...
}|

List the exposed names:

@verbatim|{
> WR.names()
["counter", "current-counter", "display", "increment!", ...]
}|

Search the exposed names:

@verbatim|{
> WR.search("count")
["counter", "current-counter"]
}|

Show richer binding metadata:

@verbatim|{
> WR.namesDetailed()
[
  { name: "counter", origin: "program", mutable: true, kind: "top-level", source: "/.../counter.rkt" },
  { name: "display", origin: "stdlib", mutable: false, kind: "top-level", source: "/.../stdlib/..." },
  ...
]
}|

Read a value without printing it first:

@verbatim|{
> WR.value("counter")
5
}|

Print the WebRacket representation explicitly:

@verbatim|{
> WR.write("counter")
5
}|

Inspect the converted JavaScript value directly:

@verbatim|{
> WR.print("counter")
5
}|

Inspect the full result object:

@verbatim|{
> WR.raw("counter")
{
  ok: true,
  kind: "value",
  value: 5,
  printed: "5",
  error: "",
  message: "",
  operation: "ref",
  name: "counter",
  args: []
}
}|

Try a missing name:

@verbatim|{
> WR.raw("does-not-exist")
{
  ok: false,
  kind: "missing-binding",
  value: false,
  printed: "",
  error: "...",
  message: "...",
  operation: "ref",
  name: "does-not-exist",
  args: []
}
}|

Format a JavaScript value with the WebRacket formatter:

@verbatim|{
> WR.format(["bridge", 7])
"(bridge 7)"
}|

The exact formatting of objects in the console depends on the browser,
but the overall shape is the same.

@section{Console Bridge Operations}

When the bridge is enabled, the browser page installs several console
bridge entry points:

@subsection{@tt{WR(name, ...args)}}

This is the main convenience form.

If you give it just a name, it looks up that binding, prints the result
to the console in WebRacket style, and returns a JavaScript value.

If you give it a name plus extra arguments, it treats the named binding
as a function, calls it, prints the result, and returns the converted
JavaScript value.

Examples:

@verbatim|{
WR("counter")
WR("increment!", 1)
}|

@subsection{@tt{WR.value(name, ...args)}}

This is the same lookup or call, but without the automatic console
printing.

Use it when you only want the returned JavaScript value.

Example:

@verbatim|{
const n = WR.value("counter");
}|

@subsection{@tt{WR.call(name, ...args)}}

This always performs a function call, even when you do not supply any
arguments.

That is useful for a zero-argument function, because:

@itemlist[
  @item{@tt{WR("current-counter")} looks up the binding}
  @item{@tt{WR.call("current-counter")} invokes the function}]

Example:

@verbatim|{
WR.call("current-counter")
}|

@subsection{@tt{WR.raw(name, ...args)}}

This returns a richer result object that shows whether the operation
succeeded.

It is useful for debugging failures or distinguishing a missing binding
from a successful call.

Typical fields are:

@itemlist[
  @item{@tt{ok}: whether the operation succeeded}
  @item{@tt{kind}: a short result tag such as @tt{\"value\"} or @tt{\"missing-binding\"}}
  @item{@tt{value}: the JavaScript value, when conversion succeeded}
  @item{@tt{printed}: the WebRacket-style printed form}
  @item{@tt{error}: error information when the operation failed}
  @item{@tt{message}: the same failure in a form suitable for throwing as a JavaScript @tt{Error}}
  @item{@tt{operation}: @tt{\"ref\"} for lookup or @tt{\"call\"} for a function call}
  @item{@tt{name} and @tt{args}: the original bridge request}]

@subsection{@tt{WR.names()}}

This returns a JavaScript array of the names that the bridge exposes for
the current page.

Use it when you are not sure which bindings are available.

@subsection{@tt{WR.search(query)}}

This searches the exposed binding names using a case-insensitive
substring match.

Use it when:

@itemlist[
  @item{you remember part of a name but not the whole name}
  @item{@tt{WR.names()} returns too many bindings to scan comfortably}]

Example:

@verbatim|{
WR.search("display")
WR.search("ADD1")
}|

@subsection{@tt{WR.namesDetailed()}}

This returns a JavaScript array of objects with a little more context
for each exposed binding.

Typical fields are:

@itemlist[
  @item{@tt{name}: the binding name}
  @item{@tt{origin}: where the binding came from, such as @tt{\"program\"} or @tt{\"stdlib\"}}
  @item{@tt{mutable}: whether the top-level binding is mutable}
  @item{@tt{kind}: currently @tt{\"top-level\"}}
  @item{@tt{source}: the source path when available}]

Use it when @tt{WR.names()} is too flat and you want to understand why a
binding is present.

@subsection{@tt{WR.write(name, ...args)}}

This behaves like @tt{WR(name, ...args)}, but it makes the intent
explicit: print the WebRacket representation and return the converted
JavaScript value.

@subsection{@tt{WR.print(name, ...args)}}

This also looks up or calls a binding, but it uses JavaScript console
inspection for the converted value instead of the WebRacket printed
representation.

It is most useful for vectors, arrays, and structured data that you want
to expand directly in DevTools.

@subsection{@tt{WR.help()}}

This prints a short usage summary in the console and returns that same
text as a JavaScript string.

@subsection{@tt{WR.format(value)}}

This sends an ordinary JavaScript value through the bridge and asks
WebRacket to format it.

It is handy when you want to compare:

@itemlist[
  @item{the JavaScript console view of a value}
  @item{the WebRacket printed view of the same value}]

@section{How Values Cross The Boundary}

The console bridge sits between:

@itemlist[
  @item{WebRacket values inside WebAssembly}
  @item{JavaScript values in the browser console}]

When you call @tt{WR}, WebRacket converts the result to a form that the
browser host can understand, and then the browser host converts that to
an ordinary JavaScript value.

So, for example:

@itemlist[
  @item{a WebRacket fixnum becomes a JavaScript number}
  @item{a WebRacket string becomes a JavaScript string}
  @item{a WebRacket vector becomes a JavaScript array}
  @item{a missing binding becomes a structured error result}]

The bridge also keeps a printed WebRacket-style representation so the
console output is easier to read while debugging.

@section{What Names Are Exposed}

The bridge exposes top-level bindings from the compiled browser program.

In practice, that means names such as:

@itemlist[
  @item{definitions from your source file}
  @item{top-level bindings brought in by the standard library that are part of the compiled program}]

Local variables are not exposed.

So if you write:

@filebox["example.rkt"
@verbatim|{
(define counter 0)

(define (increment! amount)
  (let ([temporary (* amount 2)])
    (set! counter (+ counter temporary))
    counter))
}|]

then @tt{counter} and @tt{increment!} may be available through
@tt{WR}, but @tt{temporary} is not.

@section{When To Use It}

The console bridge is a good fit when:

@itemlist[
  @item{you are developing a browser page and want quick inspection from DevTools}
  @item{you want to test a helper without recompiling for each tiny change}
  @item{you want to understand the state of a running WebRacket page}]

It is less appropriate when:

@itemlist[
  @item{you need a general REPL for arbitrary Racket expressions}
  @item{you want to expose an application API to ordinary page JavaScript in production}
  @item{you do not want the page to publish debugging hooks globally}]

Because of that last point, the bridge is opt-in and disabled by default.

@section{Troubleshooting}

If @tt{WR} is missing in the browser console, check these common causes:

@itemlist[
  @item{The page was compiled without @tt{--console-bridge}.}
  @item{The page has not finished loading yet. Wait for the page to start, then try again.}
  @item{You are serving an older generated @tt{.html} or @tt{.wasm} file than the one you just built.}
  @item{The browser page failed to load the generated @tt{.wasm} file, so the runtime never finished installing the bridge.}]

When the bridge is enabled successfully, the browser console prints a
short banner telling you to try @tt{WR.help()} or @tt{WR.names()}.

If a lookup fails, start with:

@verbatim|{
WR.raw("name-you-expect")
WR.names()
WR.namesDetailed()
}|

That usually tells you whether:

@itemlist[
  @item{the name is missing entirely}
  @item{the binding exists under a different spelling}
  @item{the binding came from your program or from the standard library}]
