#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-query-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-element-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-event-labels.rkt" "webracket"))
          )

@title{Library: @racketid[query]}
@declare-exporting[(lib "scribblings/lib-query-labels.rkt" "webracket")]

@(how-to-require include-lib query (lib "libs/query.rkt"))

The @racket[query] library provides a small set of tools for finding
DOM elements and working with the resulting selections.

@margin-note{The @racket[query] library is still experimental. Its API
may change as the selection helpers settle.}

Use @racket[query] when you want to:

@itemlist[
  @item{select elements with CSS selectors}
  @item{inspect the size or contents of a selection}
  @item{map or iterate over selected elements}
  @item{chain a selection through @racket[$chain] and dot-prefixed helper calls}
]

Currently, selectors accept strings only.
Helper functions such as @racket[element-set-text-content!]
accept the same values as documented in the underlying DOM libraries.

@section{QuickStart}

Start by including the library, building a small page sample,
selecting a heading, and reading its text.

The page sample is equivalent to @tt{<div id="query-root"><h1 id="hw">Hello
World!</h1></div>}.

@racketblock[
(include-lib document)
(include-lib sxml)
(include-lib query)

(code:comment "Build a small page sample in the current document.")
(define at (string->symbol "@"))
(define page
  (sxml->dom
   `(div (,at (id "query-root"))
         (h1 (,at (id "hw"))
             "Hello World!"))))

(element-append! (document-body) page)

(code:comment "Select the heading by id.")
(define sel ($ "#hw"))

(code:comment "Read the text from the first selected element.")
(.text sel)
(code:comment "=> \"Hello World!\"")
]

The @racket[$chain] macro gives you a convenient left-to-right style for
selection workflows:

@racketblock[
($chain ($ "#hw")
        .text)
(code:comment "=> \"Hello World!\"")
]

@section{Examples}

These examples show the most common selection operations.

@subsection{Selecting Elements}

@racketblock[
(define sel ($ ".card"))
]

@racketblock[
($selection? sel)
(code:comment "=> #t")
]

@racketblock[
($length sel)
(code:comment "=> 0")
]

@subsection{Inspecting a Selection}

@racketblock[
(define first-card ($first ($ ".card")))
]

When the selection is empty, @racket[$first] returns @racket[#f].

@racketblock[
(define nodes ($selection->vector ($ ".card")))
]

@subsection{Attributes}

Use the attribute helpers to read state from the first selected element.

@racketblock[
(define hw ($ "#hw"))
($attr hw "id")
(code:comment "=> \"hw\"")
(.attr hw 'id)
(code:comment "=> \"hw\"")
($has-attr? hw "id")
(code:comment "=> #t")
]

@subsection{Classes}

Use the class helpers to inspect and update the first selected element.

@racketblock[
(define card ($ ".card"))
($has-class? card "card")
(code:comment "=> #t")
($add-class! card "is-highlighted")
($remove-class! card "card")
]

@subsection{Text, Values, and Data}

Use the text, value, and data helpers to update common UI state.

@racketblock[
(define note ($ "#text-host"))
($text! note "Updated text")

(define field ($ "#value-host"))
($val field)
(code:comment "=> \"initial\"")
($val! field "updated")

(define meta ($ "#data-host"))
($data meta "state")
(code:comment "=> \"cold\"")
($data! meta "state" "warm")
]

@subsection{Finding Descendants}

Use @racket[$find] to search within each selected element.

@racketblock[
(define cards ($ ".card"))
($length ($find cards ".name"))
(code:comment "=> 2")
($chain cards
        .find
        ".tag"
        .map
        element-text-content)
(code:comment "=> a selection of tag labels")
]

@subsection{Children and Parents}

Use the child and parent helpers to move around the DOM tree.

@racketblock[
(define cards ($ ".card"))
($length ($children cards))
(code:comment "=> 4")
($length ($parent ($find cards ".name")))
(code:comment "=> 2")
]

@subsection{Closest Ancestors}

Use @racket[$closest] to walk upward to the nearest matching ancestor.

@racketblock[
(define leaves ($ ".leaf"))
($length ($closest leaves "section"))
(code:comment "=> 3")
($chain leaves
        .closest
        "section"
        .map
        element-id)
(code:comment "=> a selection of section ids")
]

@subsection{Sibling Navigation}

Use @racket[$next] and @racket[$prev] to walk sideways through sibling
elements.

@racketblock[
(define leaves ($ ".leaf"))
($length ($next leaves))
(code:comment "=> 1")
($length ($prev ($next leaves)))
(code:comment "=> 1")
(define first-leaf ($first leaves))
($length ($siblings (list->$selection (list first-leaf))))
(code:comment "=> 1")
]

@subsection{Iterating and Mapping}

@racketblock[
($chain ($ ".card")
        .for-each
        (lambda (node)
          (element-set-attribute! node 'data-seen 'yes)))
]

@racketblock[
($chain ($ ".card")
        .map
        (lambda (node)
          (element-text-content node)))
]

@racketblock[
($chain ($ ".card")
        .filter
        (lambda (node)
          (equal? (element-id node) "card-2"))
        .map
        element-text-content)
(code:comment "=> a selection containing the second card text")
]

@racketblock[
($chain ($ ".card")
        .where
        (lambda (node)
          (equal? (element-id node) "card-2"))
        .map
        element-text-content)
(code:comment "=> a selection containing the second card text")
]

@racketblock[
($chain ($ ".card")
        .not
        (lambda (node)
          (equal? (element-id node) "card-2"))
        .map
        element-id)
(code:comment "=> a selection containing the first card id")
]

@subsection{Event Handlers}

Use @racket[.on] to attach DOM event listeners to each selected element.

@racketblock[
(include-lib event)
(define clicks 0)

($chain ($ "#hw")
        .on
        "click"
        (lambda (evt)
          (set! clicks (add1 clicks))))

clicks
(code:comment "=> 0")
]

Event handlers receive the raw browser event value, so helpers such as
@racket[event-type] and @racket[prevent-default!] are available after
@racket[(include-lib event)].

Use @racket[.once] when you want a listener to fire only one time.

@racketblock[
(define once-count 0)

($chain ($ "#hw")
        .once
        "click"
        (lambda (_evt)
          (set! once-count (add1 once-count))))

once-count
(code:comment "=> 0")
]

After the first event, the listener removes itself automatically.

Use @racket[.trigger] when you want to dispatch an event from code.

@racketblock[
(define trigger-count 0)

($chain ($ "#hw")
        .on
        "click"
        (lambda (_evt)
          (set! trigger-count (add1 trigger-count))))

($chain ($ "#hw")
        .trigger
        "click")

trigger-count
(code:comment "=> 1")
]

Use @racket[.on-delegate] when you want one listener on a parent
element to react to matching descendants. The delegated listener
receives the matched descendant element and the event.

@racketblock[
(define delegate-count 0)

($chain ($ "#delegation-host")
        .on-delegate
        "click"
        "button"
        (lambda (matched evt)
          (set! delegate-count (add1 delegate-count))))

delegate-count
(code:comment "=> 0")
]

Because the delegated callback gets the matching element explicitly,
you can keep the handler logic focused on the item that was matched
rather than the event target that bubbled up.

@subsection{Text Content}

@racketblock[
(define title ($ "#hw"))
(.text title)
(code:comment "=> \"Hello World!\"")
]

The @racket[.text] helper reads the text content of the first selected
element and returns @racket[""] when the selection is empty.

@subsection{Chaining}

@racketblock[
($chain ($ "#hw") .text)
(code:comment "=> \"Hello World!\"")
]

The last step in a @racket[$chain] form may return a non-selection
value. That makes it convenient to end a pipeline with a query result,
a string, or any other final answer.

@section{API Reference}

@defstruct[$selection ([elements external/raw])]{
Wraps the current query result.
}

@defproc[($ [selector string?]) $selection?]{
Selects elements from the current document using a CSS selector.
}

@defproc[($select [selector string?]) $selection?]{
Alias for @racket[$].
}

@defproc[($length [sel $selection?]) exact-nonnegative-integer?]{
Returns the number of elements in @racket[sel].
}

@defproc[($ref [sel $selection?]
              [i exact-nonnegative-integer?]) (or/c #f any/c)]{
Returns the element at index @racket[i], or @racket[#f] when the index
is out of range.
}

@defproc[($first [sel $selection?]) (or/c #f any/c)]{
Returns the first element in @racket[sel], or @racket[#f] when the
selection is empty.
}

@defproc[($attr [sel $selection?]
                [name (or/c string? symbol?)]) (or/c #f string?)]{
Returns the named attribute on the first selected element, or
@racket[#f] when the selection is empty or the attribute is absent.
}

@defproc[($has-attr? [sel $selection?]
                     [name (or/c string? symbol?)]) boolean?]{
Returns @racket[#t] when the first selected element has the named
attribute.
}

@defproc[($class-list [sel $selection?]) (or/c #f dom-token-list?)]{
Returns the class list for the first selected element, or @racket[#f]
when the selection is empty.
}

@defproc[($has-class? [sel $selection?]
                      [class-name (or/c string? symbol?)]) boolean?]{
Returns @racket[#t] when the first selected element has the named
class.
}

@defproc[($add-class! [sel $selection?]
                      [class-name (or/c string? symbol?)])
         $selection?]{
Adds one or more classes to the first selected element and returns
@racket[sel].
}

@defproc[($remove-class! [sel $selection?]
                         [class-name (or/c string? symbol?)])
         $selection?]{
Removes one or more classes from the first selected element and returns
@racket[sel].
}

@defproc[($toggle-class! [sel $selection?]
                         [class-name (or/c string? symbol?)])
         $selection?]{
Toggles a class token on each selected element and returns @racket[sel].
}

@defproc[($text! [sel $selection?]
                 [text (or/c string? symbol?)])
         $selection?]{
Replaces the text content of each selected element and returns
@racket[sel].
}

@defproc[($val [sel $selection?]) (or/c #f string?)]{
Returns the value of the first selected element, or @racket[#f] when
the selection is empty.
}

@defproc[($val! [sel $selection?]
                [value (or/c string? symbol?)])
         $selection?]{
Sets the value of each selected element and returns @racket[sel].
}

@defproc[($data [sel $selection?]
                [name (or/c string? symbol?)]) (or/c #f string?)]{
Returns the named @tt{data-*} attribute on the first selected
element, or @racket[#f] when the selection is empty or the attribute is
absent.
}

@defproc[($data! [sel $selection?]
                 [name (or/c string? symbol?)]
                 [value (or/c string? symbol?)])
         $selection?]{
Sets the named @tt{data-*} attribute on each selected element and
returns @racket[sel].
}

@defproc[($find [sel $selection?]
                [selector string?]) $selection?]{
Finds descendant matches within each selected element and returns a
new selection.
}

@defproc[($children [sel $selection?]) $selection?]{
Returns a selection containing the child elements of each selected
element.
}

@defproc[($parent [sel $selection?]) $selection?]{
Returns a selection containing the parent elements of each selected
element.
}

@defproc[($closest [sel $selection?]
                   [selector string?]) $selection?]{
Returns a selection containing the nearest matching ancestor for each
selected element.
}

@defproc[($next [sel $selection?]) $selection?]{
Returns a selection containing the next element sibling of each
selected element.
}

@defproc[($prev [sel $selection?]) $selection?]{
Returns a selection containing the previous element sibling of each
selected element.
}

@defproc[($siblings [sel $selection?]) $selection?]{
Returns a selection containing the sibling elements surrounding each
selected element.
}

@defproc[($last [sel $selection?]) (or/c #f any/c)]{
Returns the last element in @racket[sel], or @racket[#f] when the
selection is empty.
}

@defproc[($empty? [sel $selection?]) boolean?]{
Returns @racket[#t] when @racket[sel] contains no elements.
}

@defproc[($selection->vector [sel $selection?]) vector?]{
Converts the selection into a Racket vector of wrapped elements.
}

@defproc[($vector [sel $selection?]) vector?]{
Alias for @racket[$selection->vector].
}

@defproc[($list [sel $selection?]) list?]{
Converts the selection into a Racket list of wrapped elements.
}

@defproc[($map [f (any/c -> any/c)]
              [sel $selection?]) $selection?]{
Applies @racket[f] to each selected element and returns a new
selection.
}

@defproc[($for-each [f (any/c -> any/c)]
                   [sel $selection?]) $selection?]{
Applies @racket[f] to each selected element and returns the original
selection.
}

@defproc[($filter [f (any/c -> any/c)]
                  [sel $selection?]) $selection?]{
Keeps only the selected elements for which @racket[f] returns true.
}

@defproc[(.map [sel $selection?]
               [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$map].
}

@defproc[(.for-each [sel $selection?]
                    [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$for-each].
}

@defproc[(.filter [sel $selection?]
                  [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$filter].
}

@defproc[($where [f (any/c -> any/c)]
                 [sel $selection?]) $selection?]{
Alias for @racket[$filter] with a selector-style name.
}

@defproc[(.where [sel $selection?]
                 [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$where].
}

@defproc[($not [f (any/c -> any/c)]
               [sel $selection?]) $selection?]{
Keeps the selected elements for which @racket[f] returns false.
}

@defproc[($on [event-name (or/c string? symbol?)]
              [listener (or/c procedure? external?)]
              [sel $selection?]) $selection?]{
Attaches a DOM event listener to each element in @racket[sel].
}

@defproc[($off [event-name (or/c string? symbol?)]
               [listener (or/c procedure? external?)]
               [sel $selection?]) $selection?]{
Removes a DOM event listener from each element in @racket[sel].
}

@defproc[($once [event-name (or/c string? symbol?)]
                [listener procedure?]
                [sel $selection?]) $selection?]{
Attaches a DOM event listener to each element in @racket[sel] that
removes itself after the first event.
}

@defproc[($trigger [event-name (or/c string? symbol?)]
                   [sel $selection?]) $selection?]{
Dispatches a bubbling DOM event on each selected element.
}

@defproc[($on-delegate [event-name (or/c string? symbol?)]
                       [selector string?]
                       [listener procedure?]
                       [sel $selection?]) $selection?]{
Attaches a delegated DOM event listener to each element in @racket[sel].
The listener is called with the matched descendant and the event.
}

@defproc[($off-delegate [event-name (or/c string? symbol?)]
                        [selector string?]
                        [listener procedure?]
                        [sel $selection?]) $selection?]{
Removes a delegated DOM event listener from each element in @racket[sel].
}

@defproc[(.not [sel $selection?]
               [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$not].
}

@defproc[(.on [sel $selection?]
              [event-name (or/c string? symbol?)]
              [listener (or/c procedure? external?)]) $selection?]{
Chainable alias for @racket[$on].
}

@defproc[(.off [sel $selection?]
               [event-name (or/c string? symbol?)]
               [listener (or/c procedure? external?)]) $selection?]{
Chainable alias for @racket[$off].
}

@defproc[(.once [sel $selection?]
                [event-name (or/c string? symbol?)]
                [listener procedure?]) $selection?]{
Chainable alias for @racket[$once].
}

@defproc[(.trigger [sel $selection?]
                   [event-name (or/c string? symbol?)]) $selection?]{
Chainable alias for @racket[$trigger].
}

@defproc[(.on-delegate [sel $selection?]
                       [event-name (or/c string? symbol?)]
                       [selector string?]
                       [listener procedure?]) $selection?]{
Chainable alias for @racket[$on-delegate].
}

@defproc[(.off-delegate [sel $selection?]
                        [event-name (or/c string? symbol?)]
                        [selector string?]
                        [listener procedure?]) $selection?]{
Chainable alias for @racket[$off-delegate].
}

@defproc[(.first [sel $selection?]) (or/c #f any/c)]{
Chainable alias for @racket[$first].
}

@defproc[(.attr [sel $selection?]
                [name (or/c string? symbol?)]) (or/c #f string?)]{
Chainable alias for @racket[$attr].
}

@defproc[(.has-attr? [sel $selection?]
                     [name (or/c string? symbol?)]) boolean?]{
Chainable alias for @racket[$has-attr?].
}

@defproc[(.class-list [sel $selection?]) (or/c #f dom-token-list?)]{
Chainable alias for @racket[$class-list].
}

@defproc[(.has-class? [sel $selection?]
                      [class-name (or/c string? symbol?)]) boolean?]{
Chainable alias for @racket[$has-class?].
}

@defproc[(.add-class! [sel $selection?]
                      [class-name (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$add-class!].
}

@defproc[(.remove-class! [sel $selection?]
                         [class-name (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$remove-class!].
}

@defproc[(.toggle-class! [sel $selection?]
                         [class-name (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$toggle-class!].
}

@defproc[(.text! [sel $selection?]
                 [text (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$text!].
}

@defproc[(.val [sel $selection?]) (or/c #f string?)]{
Chainable alias for @racket[$val].
}

@defproc[(.val! [sel $selection?]
                [value (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$val!].
}

@defproc[(.data [sel $selection?]
                [name (or/c string? symbol?)]) (or/c #f string?)]{
Chainable alias for @racket[$data].
}

@defproc[(.data! [sel $selection?]
                 [name (or/c string? symbol?)]
                 [value (or/c string? symbol?)])
         $selection?]{
Chainable alias for @racket[$data!].
}

@defproc[(.find [sel $selection?]
                [selector string?]) $selection?]{
Chainable alias for @racket[$find].
}

@defproc[(.children [sel $selection?]) $selection?]{
Chainable alias for @racket[$children].
}

@defproc[(.parent [sel $selection?]) $selection?]{
Chainable alias for @racket[$parent].
}

@defproc[(.closest [sel $selection?]
                   [selector string?]) $selection?]{
Chainable alias for @racket[$closest].
}

@defproc[(.next [sel $selection?]) $selection?]{
Chainable alias for @racket[$next].
}

@defproc[(.prev [sel $selection?]) $selection?]{
Chainable alias for @racket[$prev].
}

@defproc[(.siblings [sel $selection?]) $selection?]{
Chainable alias for @racket[$siblings].
}

@defproc[(.last [sel $selection?]) (or/c #f any/c)]{
Chainable alias for @racket[$last].
}

@defproc[(.empty? [sel $selection?]) boolean?]{
Chainable alias for @racket[$empty?].
}

@defproc[(.vector [sel $selection?]) vector?]{
Chainable alias for @racket[$vector].
}

@defproc[(.list [sel $selection?]) list?]{
Chainable alias for @racket[$list].
}

@defproc[(.text [sel $selection?]) string?]{
Returns the text content of the first selected element.
}

@defform[($chain seed .method arg ...)]{
Threads @racket[seed] through one or more dot-prefixed helper calls.
Each step receives the current selection as its first argument.
The final step may return any value, including a non-selection result.
}
