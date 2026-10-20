#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-clipboard-labels.rkt" "webracket-doc")
                     (lib "scribblings/lib-js-promise-labels.rkt" "webracket-doc"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[clipboard]}
@declare-exporting[(lib "scribblings/lib-clipboard-labels.rkt" "webracket-doc")]

@defmodule[clipboard]

The @racketmodname[clipboard] module gives browser code access to
plain-text and MIME clipboard values through @tt{navigator.clipboard}, and
to the synchronous @tt{DataTransfer} object supplied by clipboard and drag
events.

@(mdn-bar "Clipboard API"
          "https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API")

Use @racket[clipboard] when you want to:

@itemlist[
  @item{check whether browser clipboard text support is available}
  @item{read the current @tt{navigator.clipboard} object as a wrapped value}
  @item{start asynchronous plain-text clipboard reads}
  @item{start asynchronous plain-text clipboard writes}
]

@bold{Important browser constraints}

@itemlist[
  @item{Most browsers expose @tt{navigator.clipboard} only in a secure context, typically HTTPS or localhost.}
  @item{Clipboard reads and writes commonly require a user activation such as a button click.}
  @item{Clipboard reads and writes are asynchronous and return @racket[js-promise?] values.}
]

String-like text arguments accept either strings or symbols. Symbols are
converted with @racket[symbol->string] before the browser call.

@section{Clipboard Quick Start}

@racketblock[
(require browser/dom
         clipboard
         js/promise)

(define clip (clipboard))

(when clip
  (js-promise-then
   (clipboard-write-text! clip "Hello from WebRacket")
   (lambda (_ignored)
     (js-log "clipboard write finished")
     (void))))
]

@section{Clipboard API}

@defproc[(clipboard-supported?) boolean?]{
@(mdn-bar "Navigator: clipboard property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Navigator/clipboard")
Returns @racket[#t] when the current browser exposes
@tt{navigator.clipboard} with the plain-text @tt{readText} and
@tt{writeText} methods.
}

@defproc[(clipboard) (or/c #f clipboard?)]{
@(mdn-bar "Navigator: clipboard property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Navigator/clipboard")
Returns the current browser Clipboard object wrapped in a checked value, or
@racket[#f] when the Clipboard API is not available in this environment.
}

@defproc[(clipboard? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped Clipboard value with
plain-text read and write support.
}

@defproc[(clipboard-raw [value clipboard?]) external/raw]{
Returns the wrapped raw browser Clipboard object.
}

@defproc[(clipboard-read-text [value clipboard?]) js-promise?]{
@(mdn-bar "Clipboard: readText() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText")
Starts an asynchronous clipboard text read and returns a wrapped JavaScript
Promise.
}

@defproc[(clipboard-write-text! [value clipboard?]
                                [text (or/c string? symbol?)])
         js-promise?]{
@(mdn-bar "Clipboard: writeText() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText")
Starts an asynchronous clipboard text write and returns a wrapped JavaScript
Promise.
}

@section{Synchronous event payloads}

Use these functions only while the browser event that supplied the
@tt{DataTransfer} remains active. They neither retain nor recreate that
event-scoped object.

@defproc[(data-transfer-wrap [value any/c]) (or/c #f data-transfer?)]{
@(mdn-bar "DataTransfer"
          "https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer")
Wraps a non-null native @tt{DataTransfer}, or returns @racket[#f].
}

@defproc[(data-transfer? [value any/c]) boolean?]{
Reports whether @racket[value] wraps a DataTransfer with text get and set methods.
}

@defproc[(data-transfer-raw [value data-transfer?]) external/raw]{
Returns the wrapped raw DataTransfer object.
}

@defproc[(data-transfer-get-data [transfer data-transfer?]
                                 [mime-type (or/c string? symbol?)])
         string?]{
@(mdn-bar "DataTransfer: getData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer/getData")
Reads the synchronous payload for the exact MIME type; an absent payload is an empty string.
}

@defproc[(data-transfer-set-data! [transfer data-transfer?]
                                  [mime-type (or/c string? symbol?)]
                                  [text (or/c string? symbol?)])
         void?]{
@(mdn-bar "DataTransfer: setData() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer/setData")
Sets a synchronous event payload and raises if the browser disallows the operation.
}

@defproc[(data-transfer-types [transfer data-transfer?]) vector?]{
@(mdn-bar "DataTransfer: types property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer/types")
Returns MIME type strings in browser-provided order.
}

@section{Blobs and ClipboardItems}

@defproc[(blob-supported?) boolean?]{
@(mdn-bar "Blob"
          "https://developer.mozilla.org/en-US/docs/Web/API/Blob")
Reports whether the browser provides a constructible @tt{Blob}.
}

@defproc[(make-text-blob [text (or/c string? symbol?)]
                         [mime-type (or/c #f string? symbol?) #f])
         blob?]{
@(mdn-bar "Blob: Blob() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/Blob/Blob")
Creates a text Blob; omitting @racket[mime-type] uses the browser's empty MIME type.
}

@defproc[(blob-size [value blob?]) exact-nonnegative-integer?]{
@(mdn-bar "Blob: size property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Blob/size")
Returns the Blob byte size.
}

@defproc[(blob-type [value blob?]) string?]{
@(mdn-bar "Blob: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Blob/type")
Returns the Blob MIME type string.
}

@defproc[(blob-text [value blob?]) js-promise?]{
@(mdn-bar "Blob: text() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Blob/text")
Starts an asynchronous UTF-8 text read.
}

@defproc[(clipboard-item-supported?) boolean?]{
@(mdn-bar "ClipboardItem"
          "https://developer.mozilla.org/en-US/docs/Web/API/ClipboardItem")
Reports whether the browser provides a constructible @tt{ClipboardItem}.
}

@defproc[(make-clipboard-item [mime-type (or/c string? symbol?)] [value blob?])
         clipboard-item?]{
@(mdn-bar "ClipboardItem: ClipboardItem() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/ClipboardItem/ClipboardItem")
Creates one MIME ClipboardItem containing @racket[value].
}

@defproc[(clipboard-item-types [item clipboard-item?]) vector?]{
@(mdn-bar "ClipboardItem: types property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ClipboardItem/types")
Returns the item's MIME type strings in native order.
}

@defproc[(clipboard-item-get-type [item clipboard-item?]
                                  [mime-type (or/c string? symbol?)])
         js-promise?]{
@(mdn-bar "ClipboardItem: getType() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/ClipboardItem/getType")
Starts an asynchronous MIME lookup whose fulfillment value is a native Blob.
}

@section{Native MIME reads and writes}

@defproc[(clipboard-read [value clipboard?]) js-promise?]{
@(mdn-bar "Clipboard: read() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/read")
Starts asynchronous ClipboardItem enumeration; permission and activation failures reject the Promise.
}

@defproc[(clipboard-write! [value clipboard?] [items vector?]) js-promise?]{
@(mdn-bar "Clipboard: write() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/write")
Writes a vector of ClipboardItems; browsers commonly require a trusted user activation and may reject it.
}
