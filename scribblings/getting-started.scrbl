#lang scribble/manual
@(require scribble-tools
          (only-in (lib "core.rkt" "webracket") include-lib))

@title{Getting Started}

@section{What is WebRacket?}

WebRacket is a compiler and runtime that lets you write programs in
Racket and run them in the browser by compiling them to WebAssembly.

In practice, it gives you:

@itemlist[
  @item{a subset of Racket}
  @item{compiled to fast, portable WebAssembly}
  @item{with direct access to browser APIs and JavaScript}]


@subsection{How it works (at a glance)}

@itemlist[
  @item{You write a Racket program (@tt{.rkt})}
  @item{WebRacket compiles it to:
    @itemlist[
      @item{a @tt{.wasm} file (your program)}
      @item{a small JavaScript loader}]}
  @item{The browser runs the result}]


@section{What you can build}

WebRacket is aimed at building interactive programs that run in the browser.

@subsection{Web applications}

You can build dynamic web pages and small applications with user interaction,
event handling, and DOM manipulation.

@subsection{Graphics and visualizations}

Using the canvas API, you can create drawings, animations, and interactive
visualizations.

@subsection{Multimedia applications}

WebRacket supports audio APIs, making it possible to build sound-based tools,
synthesizers, and interactive audio applications.

@subsection{Browser-integrated tools}

You can access standard browser features such as:
@itemlist[
  @item{the DOM}
  @item{event handling}
  @item{local storage and IndexedDB}
  @item{network communication via fetch and WebSocket}]



@section{Your First Program}

This section shows a minimal WebRacket program that renders a simple
Hello World page in the browser.

@subsection{Step 1: Write a program}

Create a file named @tt{hello.rkt} with the following contents:

@filebox["hello.rkt"
@verbatim|{
(include-lib web-easy)

(define hello-world-app
  (window
    (container
      (h1 "Hello World")
      (text "Have a nice day."))))

(mount-renderer!
 (render hello-world-app))
}|]


@subsection{Step 2: Compile the program}

Compile the program for the browser with the FFI libraries needed by
@tt{web-easy}:

@shellblock{
webracket --browser hello.rkt
}

This generates the files needed to run the program in a browser.
The files are @tt{hello.html} and @tt{hello.wasm}.

@subsection{Step 3: Start a local web server}

To run the program in a browser, start a local web server in the
directory containing the generated files:

@margin-note{If the default port 8000 is already in use, add an @shell-code{-p 8001} to the command.}
@shellblock{
raco static-web
}

By default, this starts a local server and prints the address to use
in your browser.

@subsection{Step 4: Open the page in your browser}

Open the address shown by @shell-code{raco static-web}, then load
@tt{hello.html}.

You should see a page containing the heading @tt{Hello World} and the
text @tt{Have a nice day.}

If you want to inspect or call selected WebRacket bindings from the
browser's JavaScript console while a page is running, see
@secref["console-bridge"].

@subsection{What this program does}

The calls to @racket[window], @racket[container], @racket[h1], and
@racket[text] construct a description of the user interface.
The call to @racket[render] produces a renderer for the interface.
The call to @racket[mount-renderer!] inserts it into the browser page.



@section{Mental Model}

This section gives a high-level picture of how a WebRacket program runs.

@subsection{From Racket to the browser}

A WebRacket program goes through the following steps:

@itemlist[
  @item{You write a program in Racket.}
  @item{The WebRacket compiler translates it to WebAssembly.}
  @item{A small JavaScript loader starts the program in the browser.}
  @item{The browser executes the result.}]

In short:

@centered{@tt{Racket → WebRacket compiler → WebAssembly + JavaScript → Browser}}

@subsection{What runs where}

A WebRacket program involves three parts:

@itemlist[
  @item{@bold{WebAssembly:} Your compiled program runs here.}
  @item{@bold{JavaScript:} A small loader connects the program to the browser.}
  @item{@bold{The browser:} Provides the DOM and other APIs.}]

Your Racket code does not run directly in JavaScript. Instead, it runs
inside WebAssembly and interacts with the browser through the provided
libraries and FFI.

@subsection{Producing output}

Unlike a traditional Racket program, output is not shown in a terminal.

To display something in the browser, your program must construct a user
interface and insert it into the page. For example:

@itemlist[
  @item{Using @racket[web-easy] to build and display UI elements.}
  @item{Using DOM libraries to create and modify elements.}]

Calling @racket[render] creates a representation of the interface, and
@racket[mount-renderer!] inserts it into the page so it becomes visible.

@subsection{A different kind of program}

WebRacket programs are typically:

@itemlist[
  @item{interactive}
  @item{event-driven}
  @item{running continuously in the browser}]

Instead of computing a result and exiting, a program usually reacts to
user input such as clicks, key presses, or network events.

@subsection{Summary}

A WebRacket program:

@itemlist[
  @item{is written in Racket}
  @item{compiled to WebAssembly}
  @item{runs inside the browser}
  @item{interacts with the page through libraries and FFI}]
