#lang scribble/manual

@(require scribble-tools
          (for-label (lib "scribblings/primitives-labels.rkt" "webracket")))

@(define (long-option name)
   (litchar (string-append "--" name)))

@title[#:tag "files-and-vfs"]{Files and the Virtual Filesystem}

WebRacket programs can use many familiar Racket file, path, and port
operations. Since a WebRacket program may run in a browser, it cannot assume
that the host has a normal operating-system filesystem available to the
compiled program. Instead, WebRacket provides a @deftech{virtual filesystem},
or @deftech{VFS}.

The VFS is an in-memory filesystem seen by the WebRacket program. Host files,
URLs, inline data, directories, and tar archives can be loaded into VFS paths.
After that, the program works with those VFS paths using ordinary Racket-style
operations such as @racket[file->string], @racket[open-input-file],
@racket[directory-list], and @racket[copy-file].

@section{The Basic Idea}

The goal is for WebRacket programs to use files in the usual Racket way. A
program should be able to open @tt{/app/config.txt}, read bytes from it, list
a directory, or write a result file without caring whether it is running under
Node or in a browser.

The browser is the reason a VFS is needed. Browser programs do not have direct
access to an operating-system filesystem. A browser page cannot simply open an
arbitrary local file path such as @tt{/Users/alice/config.txt}. Instead, the
host page can fetch URLs, receive embedded data, or use data that was bundled
into the generated output.

So WebRacket gives the compiled program a virtual filesystem. The next
question is how that filesystem gets its initial files. WebRacket populates
the VFS by associating VFS paths with contents before the Racket program
starts. Those contents can come from host files, fetched URLs, inline text,
inline bytes, directories, or tar archives.

That means there are two names to keep separate:

@itemlist[
  @item{@bold{The source} names where the contents come from outside the
        WebRacket program, such as a Node host path @tt{data/config.txt}, a
        browser URL @tt{./config.txt}, inline text, or an archive
        @tt{./assets.tgz}.}
  @item{@bold{The VFS path} names where those contents appear inside the
        WebRacket program, such as @tt{/app/config.txt} or
        @tt{/assets/logo.txt}.}
]

The command line creates these associations:

@verbatim|{
source contents
          |
          v
   WebRacket VFS path
          |
          v
 Racket file and port operations
}|

For example, this command preloads an inline text file:

@shellblock{
racket webracket.rkt --node --vfs-text /app/message.txt=hello program.rkt
}

Then @tt{program.rkt} can read the file by its VFS path and print its
contents:

@racketblock{
(displayln (file->string "/app/message.txt"))
}

Running the program prints:

@verbatim|{
hello
}|

The rest of this chapter follows that order:

@itemlist[
  @item{First, choose how to populate the VFS before the program starts.}
  @item{Then, use ordinary Racket file, path, and port operations on the VFS
        paths.}
  @item{Finally, keep track of which paths are mutable memory files and which
        paths are read-only mounts.}
]

@section{VFS Path Rules}

VFS paths use Unix-style slash syntax. Absolute VFS paths start with
@tt{/}. Command-line preload and mount targets must be absolute VFS paths.

@itemlist[
  @item{@tt{/app/data.txt} is an absolute VFS path.}
  @item{@tt{app/data.txt} is not accepted as a command-line VFS target.}
  @item{The root directory is @tt{/}.}
  @item{Relative paths used inside a program are resolved relative to the
        current VFS directory.}
]

@section{Populating the VFS}

The command-line VFS options decide which contents are available when the
WebRacket program starts:

@tabular[
 #:sep @hspace[2]
 #:style 'boxed
 (list
  (list @bold{Option} @bold{Node} @bold{Browser} @bold{Result} @bold{Use case})
  (list @long-option["vfs-file"] "yes" "no" "mutable file"
        "read one host file into the VFS")
  (list @long-option["vfs-url"] "yes" "yes" "mutable file"
        "fetch one URL into the VFS")
  (list @long-option["vfs-text"] "yes" "yes" "mutable file"
        "embed small text directly")
  (list @long-option["vfs-base64"] "yes" "yes" "mutable file"
        "embed small binary data directly")
  (list @long-option["vfs-mkdir"] "yes" "yes" "mutable directory"
        "create an initially empty directory")
  (list @long-option["vfs-dir"] "yes" "no" "mutable directory tree"
        "read a host directory tree")
  (list @long-option["vfs-tar-file"] "yes" "no" "read-only mount"
        "mount a host tar archive")
  (list @long-option["vfs-tar-url"] "yes" "yes" "read-only mount"
        "fetch and mount a tar archive"))]

The @long-option["vfs-file"], @long-option["vfs-dir"], and
@long-option["vfs-tar-file"] options are Node-oriented because they read host
paths. In browser mode, use URL, inline, or archive URL options instead.

@section{Preloading Individual Files}

Preloading creates ordinary files or directories in the mutable VFS memory
backend before the program starts.

@subsection{Host Files}

Use @long-option["vfs-file"] in Node mode to read one host file and preload its
contents into one VFS file:

@shellblock{
racket webracket.rkt --node --vfs-file /app/config.txt=config.txt program.rkt
}

Inside the program:

@racketblock{
(displayln (file->string "/app/config.txt"))
}

If @tt{config.txt} contains @tt{debug=true}, the program prints:

@verbatim|{
debug=true
}|

Host file preloading reads from the filesystem available to the generated Node
host module. It is not for browser pages, since browser code cannot freely read
local host paths. In browser mode, use @long-option["vfs-url"] for resources that the
generated page can fetch.

@subsection{URLs}

Use @long-option["vfs-url"] to fetch a URL into one VFS file. This is primarily useful
in browser mode:

@shellblock{
racket webracket.rkt --browser --vfs-url /app/config.txt=./config.txt program.rkt
}

The argument has two paths with different meanings. The left side,
@tt{/app/config.txt}, is the absolute VFS path that the WebRacket program will
see. The right side, @tt{./config.txt}, is a URL source used by the generated
host code.

The source URL can be absolute, such as @tt{https://example.com/config.txt},
or relative, such as @tt{./config.txt}. A relative URL is not relative to the
Racket source file. It is resolved when the generated host code runs. In
browser mode, @tt{./config.txt} is fetched relative to the generated
@tt{.html} file, so the asset should be served beside that page:

@verbatim|{
  out.html
  out.wasm
  config.txt
}|

With those files in the same served directory, @tt{./config.txt} names the
browser URL next to @tt{out.html}, while @tt{/app/config.txt} names the VFS
file that the Racket program reads.

In Node mode, @long-option["vfs-url"] also uses @tt{fetch}, so it can load URLs that
Node can fetch, such as @tt{https://example.com/config.txt}. Local Node files
should normally be loaded with @long-option["vfs-file"] instead.

@subsection{Inline Text}

Use @long-option["vfs-text"] for small text fixtures and configuration strings:

@shellblock{
racket webracket.rkt --node --vfs-text /app/mode.txt=test program.rkt
}

The text is embedded directly in the generated host support file.
For example:

@racketblock{
(displayln (file->string "/app/mode.txt"))
}

prints:

@verbatim|{
test
}|

@subsection{Inline Bytes}

Use @long-option["vfs-base64"] for small binary values:

@shellblock{
racket webracket.rkt --node --vfs-base64 /app/blob.dat=aGVsbG8= program.rkt
}

This option is intended for small data. Large binary assets should normally be
loaded from files, URLs, or archives instead.

The example base64 string is @tt{hello}. Inside the program:

@racketblock{
(displayln (bytes->string/utf-8 (file->bytes "/app/blob.dat")))
}

prints:

@verbatim|{
hello
}|

@subsection{Empty Directories}

Use @long-option["vfs-mkdir"] to create an empty VFS directory before the program runs:

@shellblock{
racket webracket.rkt --node --vfs-mkdir /app/cache program.rkt
}

Inside the program:

@racketblock{
(displayln (directory-exists? "/app/cache"))
}

prints:

@verbatim|{
#t
}|

@section{Preloading Directories}

Use @long-option["vfs-dir"] to copy a host directory tree into the VFS:

@shellblock{
racket webracket.rkt --node --vfs-dir /assets=assets program.rkt
}

The program can then list and read files under @tt{/assets}:

@racketblock{
(displayln (map path->string (directory-list "/assets")))
(displayln (file->string "/assets/readme.txt"))
}

If the host directory contains @tt{readme.txt} whose contents are
@tt{hello assets}, the program prints:

@verbatim|{
(readme.txt)
hello assets
}|

Directory preloading currently loads the directory contents eagerly. In Node
mode, the host directory is read by the generated Node host support file. In
browser mode, prefer URL or tar archive preloads, since a browser page cannot
freely read arbitrary local directories.

@section{Mounting Tar Archives}

A tar archive stores many files in one archive. WebRacket can mount a tar file
as a read-only subtree in the VFS.

Use @long-option["vfs-tar-file"] for a host tar file in Node mode:

@shellblock{
racket webracket.rkt --node --vfs-tar-file /assets=assets.tar program.rkt
}

Use @long-option["vfs-tar-url"] for a tar archive fetched by URL:

@shellblock{
racket webracket.rkt --browser --vfs-tar-url /assets=./assets.tar program.rkt
}

Inside the program, files in the archive appear under the mount path:

@racketblock{
(displayln (file->string "/assets/hello.txt"))
(displayln (map path->string (directory-list "/assets")))
}

If the archive contains @tt{hello.txt} whose contents are @tt{hello archive},
the program prints:

@verbatim|{
hello archive
(hello.txt)
}|

Tar mounts are read-only. You can read files from them, list directories, and
copy files out of them. To change data, copy it into a mutable VFS directory
or file and write there.

@section{Mutable Files and Read-Only Mounts}

Preloaded individual files, inline text, inline bytes, empty directories, and
preloaded directory trees live in the mutable memory backend. A program can
write them, replace them, delete them, and create new files next to them.

Tar archives are different. A tar archive is mounted as a read-only backend.
The program can read from a tar mount, list its directories, and copy files
out of it, but it cannot modify the archive in place.

To modify data from a tar archive, copy it to a mutable path first:

@racketblock{
(copy-file "/assets/template.txt" "/app/template.txt")
(delete-file "/app/template.txt")

(call-with-output-file "/app/template.txt"
  (lambda (out)
    (display "updated" out)))

(displayln (file->string "/app/template.txt"))
}

prints:

@verbatim|{
updated
}|

The source path @tt{/assets/template.txt} can be in a tar mount. The
destination path @tt{/app/template.txt} must be in a mutable VFS directory.

@subsection{Compressed Tar Archives}

The same tar options also support gzip-compressed archives. Sources ending in
@tt{.tar.gz} or @tt{.tgz} are decompressed automatically:

@shellblock{
racket webracket.rkt --node --vfs-tar-file /assets=assets.tgz program.rkt
}

@shellblock{
racket webracket.rkt --browser --vfs-tar-url /assets=./assets.tar.gz program.rkt
}

WebRacket also recognizes gzip data by its magic bytes after loading. This
means a downloaded gzip archive can still be handled when its URL does not end
in @tt{.tar.gz} or @tt{.tgz}.

In browsers, gzip decompression uses @tt{DecompressionStream("gzip")} when the
browser provides it. In Node, gzip decompression uses @tt{node:zlib}.
Decompression failures are reported as WebRacket VFS gzip errors.

@section{Program Runs and Persistence}

The VFS is initialized when the generated host program starts. Each run begins
with the preloads and mounts described on the command line.

Writes made by the WebRacket program go into the in-memory VFS for that run.
They are visible to later file operations in the same run, but they are not
written back to the original host file, URL, or tar archive.

For example, if a program writes @tt{/app/out.txt}, that file exists inside
the VFS while the program is running. It is not automatically saved as a host
file beside the generated JavaScript or HTML file.

@section{Reading Files}

Whole-file helpers work on VFS paths:

@racketblock{
(displayln (file->string "/app/config.txt"))
(write (file->bytes "/app/blob.dat"))
(newline)
(write (file->lines "/app/lines.txt"))
(newline)
(write (file->bytes-lines "/app/lines.dat"))
(newline)
}

For files containing @tt{debug=true}, @tt{hello}, and two lines
@tt{alpha} and @tt{beta}, respectively, this prints:

@verbatim|{
debug=true
#"hello"
("alpha" "beta")
(#"alpha" #"beta")
}|

Input ports work too:

@racketblock{
(define in (open-input-file "/app/config.txt"))
(displayln (read-line in))
(displayln (read-string 5 in))
(displayln (read-byte in))
}

For a file containing @tt{alpha} followed by a newline and then @tt{beta},
this prints:

@verbatim|{
alpha
beta
#<eof>
}|

Use @racket[port-count-lines!] when you want line, column, and position
tracking on an input port:

@racketblock{
(define in (open-input-file "/app/source.txt"))
(port-count-lines! in)
(displayln (read-line in))
(write (call-with-values (lambda () (port-next-location in)) list))
(newline)
}

For a file whose first line is @tt{first}, this prints a location after the
first line. The exact column convention follows Racket's port location
tracking:

@verbatim|{
first
(2 0 7)
}|

Location tracking matters for parsers and readers that report source
positions. WebRacket tracks positions on supported input ports, including
file-backed input ports.

@section{Writing Files}

The mutable VFS memory backend supports writing through output ports and file
helpers:

@racketblock{
(call-with-output-file "/app/out.txt"
  (lambda (out)
    (display "hello" out)))

(displayln (file->string "/app/out.txt"))
}

prints:

@verbatim|{
hello
}|

Common output operations include @racket[open-output-file],
@racket[call-with-output-file], @racket[with-output-to-file],
@racket[display-to-file], and byte/string output procedures such as
@racket[write-bytes] and @racket[write-string].

If a path is inside a read-only backend, such as a tar mount, write operations
fail. Write to a mutable VFS path instead.

@section{Directories, Copying, Renaming, and Deleting}

The VFS supports common filesystem operations on mutable paths:

@racketblock{
(make-directory "/app/generated")
(copy-file "/assets/template.txt" "/app/generated/template.txt")
(rename-file-or-directory "/app/generated/template.txt"
                          "/app/generated/result.txt")
(displayln (file-exists? "/app/generated/result.txt"))
(delete-file "/app/generated/result.txt")
(displayln (file-exists? "/app/generated/result.txt"))
}

prints:

@verbatim|{
#t
#f
}|

Copying from a tar mount to a mutable VFS path is allowed because the data is
read from the tar backend and written into the memory backend. Renaming or
deleting files inside a tar mount is not allowed.

Useful predicates and inspection functions include:

@racketblock{
(displayln (file-exists? "/app/config.txt"))
(displayln (directory-exists? "/app"))
(displayln (file-size "/app/config.txt"))
(displayln (map path->string (directory-list "/app")))
}

For an @tt{/app} directory containing @tt{config.txt}, this prints:

@verbatim|{
#t
#t
10
(config.txt)
}|

@section{API Support Snapshot}

The VFS is meant to support the file and port operations that WebRacket
programs commonly need. This table is a practical guide, not a replacement
for the implemented primitive list.

@tabular[
 #:sep @hspace[2]
 #:style 'boxed
 (list
  (list @bold{Area} @bold{Examples} @bold{Status})
  (list "Whole-file reads"
        @elem{@racket[file->string], @racket[file->bytes],
              @racket[file->lines], @racket[file->bytes-lines]}
        "supported")
  (list "Input ports"
        @elem{@racket[open-input-file], @racket[call-with-input-file],
              @racket[with-input-from-file], @racket[read-byte],
              @racket[peek-byte], @racket[read-line], @racket[read-string]}
        "supported")
  (list "Output ports"
        @elem{@racket[open-output-file], @racket[call-with-output-file],
              @racket[with-output-to-file], @racket[display-to-file],
              @racket[write-bytes], @racket[write-string],
              @racket[flush-output]}
        "supported on mutable paths")
  (list "Port location tracking"
        @elem{@racket[port-count-lines!], @racket[port-next-location]}
        "supported for file and string input ports")
  (list "Path and file predicates"
        @elem{@racket[file-exists?], @racket[directory-exists?],
              @racket[file-size]}
        "supported")
  (list "Directory operations"
        @elem{@racket[directory-list], @racket[make-directory],
              @racket[make-directory*]}
        "supported on mutable paths")
  (list "File mutation"
        @elem{@racket[copy-file], @racket[rename-file-or-directory],
              @racket[delete-file], @racket[delete-directory]}
        "supported on mutable paths; tar mounts are read-only"))]

@section{Backends}

Most users do not need to think about backends, but the idea explains why some
paths are mutable and others are read-only.

@itemlist[
  @item{@bold{Memory backend:} The ordinary mutable VFS storage. Preloaded
        text, base64 data, individual files, created directories, and files
        written by the program live here.}
  @item{@bold{Tar backend:} A read-only view of a tar archive. It indexes the
        archive and serves files from it without turning the archive into
        mutable files.}
  @item{@bold{Synthetic mount directories:} Directory entries that connect a
        mounted backend into the rest of the VFS tree. For example, mounting a
        tar archive at @tt{/assets} creates the path @tt{/assets} so that the
        archive contents can be reached below it.}
]

This backend model is visible mainly through mutation rules. Files in the
memory backend can be changed. Files in a tar backend cannot.

@section{Limitations}

The VFS is designed to make Racket file and port operations useful in
WebRacket, but it is not a promise of full operating-system filesystem access.

@itemlist[
  @item{Browser programs cannot read arbitrary local files or directories
        unless those resources are supplied by the host page, fetched by URL,
        or embedded in the generated output.}
  @item{Tar mounts are read-only.}
  @item{Large preloads and archives use memory in the JavaScript host and in
        the WebAssembly runtime. Prefer external files or archives for large
        assets instead of inline text or base64 data.}
  @item{Not every Racket filesystem procedure is implemented. Consult the
        implemented primitive list for the current set.}
]

@section{Common Mistakes}

@itemlist[
  @item{Confusing a VFS path with a host path. In
        @tt{/app/config.txt=./config.txt}, the left side is the path that the
        WebRacket program sees. The right side is where the host gets the
        contents.}
  @item{Expecting a browser build to read a local host path. Browser pages
        cannot freely open @tt{./data.txt} as a filesystem path. Use
        @long-option["vfs-url"] for browser assets that are served with the
        generated page.}
  @item{Expecting a relative URL to be relative to the Racket source file. A
        relative URL such as @tt{./config.txt} is resolved by the generated
        host code when it runs. In browser mode, that means it is relative to
        the generated @tt{.html} file.}
  @item{Writing into a tar mount. Tar mounts are read-only. Copy the file to a
        mutable VFS path before changing it.}
  @item{Expecting VFS writes to update the original host file or archive. VFS
        writes are visible inside the running WebRacket program, but they are
        not automatically written back to the host filesystem or URL source.}
  @item{Leaving the compilation target implicit in examples. Use
        @long-option["node"] or @long-option["browser"] so the reader knows
        which host environment the command targets.}
]

@section{Complete Examples}

@subsection{Node: Read a Host File}

@filebox["read-config.rkt"
@racketblock{
(displayln (file->string "/app/config.txt"))
}]

Compile and run:

@shellblock{
racket webracket.rkt --node --run --vfs-file /app/config.txt=config.txt read-config.rkt
}

If @tt{config.txt} contains @tt{debug=true}, the program prints:

@verbatim|{
debug=true
}|

@subsection{Browser: Fetch a Text Asset}

@filebox["read-message.rkt"
@racketblock{
(displayln (file->string "/app/message.txt"))
}]

Compile for the browser:

@shellblock{
racket webracket.rkt --browser --vfs-url /app/message.txt=./message.txt read-message.rkt
}

Serve the generated @tt{.html}, @tt{.wasm}, and @tt{message.txt} from the
same directory. If @tt{message.txt} contains @tt{hello browser}, the program
prints:

@verbatim|{
hello browser
}|

@subsection{Browser: Mount a Compressed Tar Archive}

@filebox["read-assets.rkt"
@racketblock{
(displayln (file->string "/assets/hello.txt"))
}]

Compile for the browser:

@shellblock{
racket webracket.rkt --browser --vfs-tar-url /assets=./assets.tgz read-assets.rkt
}

The archive @tt{assets.tgz} should contain a file named @tt{hello.txt}. The
program reads it as @tt{/assets/hello.txt}. If that file contains
@tt{hello archive}, the program prints:

@verbatim|{
hello archive
}|
