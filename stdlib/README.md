Standard Library
----------------

This folder contains the part of the standard library that is implemented in WebRacket.
The majority of primitives are implemented directly in WebAssembly.

The files in `stdlib` uses `#lang webracket`. 
This makes it easier to test and debug the implemented functions when
editing a single file.

Since the WebRacket compiler currently has no support for modules,
we have used the moral equivalent of `include` to combine the
files in this folder. This happens in `stdlib.rkt` which starts:

    #lang webracket
    (include/reader "shared-library-structs.rkt" read-syntax/skip-first-line)
    (include/reader "qq-and-or.rkt"   read-syntax/skip-first-line)
    (include/reader "parameters.rkt"  read-syntax/skip-first-line)
    (include/reader "exceptions.rkt"  read-syntax/skip-first-line)
    ...

The file `shared-library-structs.rkt` is included first so shared
wrapper structs like `element` and `dom-rect` are available to later
stdlib files and to the public DOM wrapper libraries without any
compiler changes. It is a special shared-helper file rather than a
normal stdlib primitive file.

When a docs page needs to link to a shared appendix or section, give
that target an explicit Scribble tag with `#:tag`. This keeps the
cross references stable and avoids undefined-tag warnings in the
rendered manual.
    
Note that `stdlib-for-browser.rkt` includes `stdlib.rkt` and `browser.rkt`.
The file `browser.rkt` is only used when the target is the browser.
When the target is `node`, `browser.rkt` is not included.

Currently, `browser.rkt` contains `sxml-dom` that turns an sxml expression
into a dom value.
