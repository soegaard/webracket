#lang scribble/manual
@(require scribble-tools
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[web-easy]}

@section{Introduction}

The @tt{web-easy} library is a declarative UI library for WebRacket.

The library is inspired by @tt{gui-easy}, whose goal is to simplify
the construction of user interfaces in Racket. The approach is to
wrap the existing imperative GUI API in a functional shell and
use observables to manage state.

This library follows the same approach, but the underlying
GUI toolkit is provided by the browser.
Due to the rich design possibilities on the web,
the set of available components is larger.


@section{Quickstart}

The following program fragments focus on the how view and observables
are used to create user interfaces. To see how the fragments are
turned into a runnable program, look at the @tt{examples/}
subfolder of the @racketid[web-easy] library. Each program in
this Quickstart has its own folder. 

TODO:insert-link-to-examples-at-github.

The examples are heavily inspired by the @racket[gui-easy] Quickstart.

@subsection{Hello World}

@margin-wiki["History of Hello World"]{"Hello,_World!"_program}
We begin with a Hello World program. 

The simplest version is:

@racketblock[
(define hello-world-app-1
  (window
   (h1 "Hello World")
   (text "Have a nice day.")))]

The components @racketid[window], @racketid[h1] and @racketid[text]
build a description of how the view should look like. The view
is rendered to the browser DOM with the help of @racketid[render].

When rendered, the contents of @racketid[window] will fill the entire
browser window. Thus the header and text have no side margins.

To restrict the width, we can put the contents in a container.

@racketblock[
(define hello-world-app-2
  (window
   (container
    (h1 "Hello World")
    (text "Have a nice day."))))]

The @racketid[container] component uses these settings as default:

@cssblock{
.we-container{
  width:     min(1200px, calc(100vw - 28px));
  max-width: 1200px;
  margin:    0 auto;
}}

The rules imply that the width on large screens is 1200px.
On small screens the size is the viewport minus 28px,
thus giving a margin of 14px on both sides. The @css-code{margin: 0 auto@";"}
centers the block horizontally.

The default rules can be overruled by inline styles or by using
an extra style sheet. Let's see how the @racket[#:attrs] keyword argument
can be used to add an inline style. Here we are changing the color
of the text.

@margin-note{Currently our poor-man's keyword arguments need to be
last in a call.}

@racketblock[
(define hello-world-app-3
  (window
   (container ; #:attrs '()  ; `((style ,my-container-style))
     (h1 "Hello World")
     (text "Have a nice day."
           #:attrs '((style "color: red;"))))))]

Similarly, we can change the default width for a container.

@racketblock[
(define my-container-style
  "  width: min(600px, calc(100vw - 28px));")
             
(define hello-world-app-4
  (window
   (container 
      (h1 "Hello World")
      (text "Have a nice day.")
    #:attrs `((style ,my-container-style)))))]


@subsection{A Single Counter}

The Hello World programs have a static view, nothing changes.
Usually programs have state that changes - and as a result
the user interface needs to update reflecting these changes.

In @racketid[web-easy] state is represented via @em{observables}.

@racketblock[
(define |@count| (|@| 0))

(define a-single-counter-app
  (window
   (container
    (h1 "A Single Counter")
    (hpanel
     (button "-" (λ () (|@count| . <~ . sub1)))
     (text (|@count| . ~> . number->string))
     (button "+" (λ () (|@count| . <~ . add1)))))))
]

The counter state is stored in a new observable @racket[(|@| 0)].
By convention names for observables like @racketid[|@count|]
begins with an @racketid[|@|].

The view @racket[(text (|@count| . ~> . number->string))] depends
on the observable value. If the value changes, the text is recomputed. 

The minus button has the on-click callback @racket[(|@count| . <~ . sub1)].
This updates the observable value. 

When an observable is updated, any dependencies are recomputed.
Here the text is recomputed.

To sum up, @racket[~>] is used to read (and map) the value of an observable,
and @racket[<~] is used to update. As an alternative to these
short names, one can use @racket[obs-map] and @racket[obs-update!].

@racketblock[
(define |@count| (|@| 0))

(define a-single-counter-app-2
  (window
   (container
    (h1 "A Single Counter - 2")
    (hpanel
     (button "-" (λ () (obs-update! |@count| sub1)))
     (text (obs-map |@count| number->string))
     (button "+" (λ () (obs-update! |@count| add1)))))))
]

@subsection{Multiple Counters}

Converting a single counter into a reusable counter component
is straight forward.

@racketblock[
(define (counter |@count| action)
  (hpanel
   (button "-" (λ () (action sub1)))
   (text (|@count| . ~> . number->string))
   (button "+" (λ () (action add1)))))
 
(define |@counter-1| (|@| 0))
(define |@counter-2| (|@| 0))

(define multiple-counters-app
  (window
   (container
    (h1 "Two Counters")
    (vpanel
     (counter |@counter-1| (λ (proc) (|@counter-1| . <~ . proc)))
     (counter |@counter-2| (λ (proc) (|@counter-2| . <~ . proc)))))))
]

@subsection{Dynamic Counters}


@racketblock[
(define |@counters| (|@| '((0 . 0))))

(define (append-counter counts)
  (define next-id (add1 (apply max -1 (map car counts))))
  (append counts `((,next-id . 0))))

(define (update-count counts k proc)
  (for/list ([entry (in-list counts)])
    (if (equal? (car entry) k)
        (cons k (proc (cdr entry)))
        entry))) 

(define (counter |@count| action)
  (hpanel ; #:stretch '(#t #f)
   (button "-" (λ () (action sub1)))
   (text (|@count| . ~> . number->string))
   (button "+" (λ () (action add1)))))

(define dynamic-counters-app
  (window ; #:size '(#f 200)
   (container
    (h1 " Counters")
    (vpanel
     (hpanel ; #:alignment '(center top) ; #:stretch '(#t #f)
      (button "Add counter"
              (λ () (|@counters| . <~ . append-counter))))    
     (list-view |@counters|
                (λ (k |@entry|)
                  (counter (|@entry| . ~> . cdr)
                           (λ (proc)
                             (|@counters| . <~ . (λ (counts)
                                                   (update-count counts k proc))))))
                car))))) ; key-of
]



@section{Common Keywords}

Most concrete component constructors support:

@itemlist[
  @item{@racket[#:id]    to set the root DOM id}
  @item{@racket[#:class] to add one or more CSS classes}
  @item{@racket[#:attrs] to add root DOM attributes}
]

Composition forms such as @racket[window], @racket[vpanel], and
@racket[hpanel] keep positional-only contracts.

@section{Layout Components}

@defproc[(window [child any/c] ...) any/c]{
Build a root window view.
                                           
Positional arguments: each @racket[child] is a view shown in the window.
}

@defproc[(vpanel [child any/c] ...) any/c]{
Build a vertical container.
Positional arguments: each @racket[child] is stacked top-to-bottom.
}

@defproc[(hpanel [child any/c] ...) any/c]{
Build a horizontal container.

Positional arguments: each @racket[child] is laid out left-to-right.
}

@defproc[(container [#:id    id    (or/c #f string? symbol?) #f]
                    [#:class class any/c                     #f]
                    [#:attrs attrs list?                     null]
                    [child any/c] ...)
         any/c]{
Build a centered container with width constraints.

Positional arguments: each @racket[child] is content inside the container.
}

@defproc[(grid [columns any/c]
               [#:id    id    (or/c #f string? symbol?) #f]
               [#:class class any/c                     #f]
               [#:attrs attrs list?                     null]
               [child any/c] ...)
         any/c]{
Build a grid container. The first child may be a gap value (number or CSS length string).

Positional arguments: @racket[columns] sets the column template/count; @racket[child] values are grid items.
}

@defproc[(stack [#:id    id    (or/c #f string? symbol?) #f]
                [#:class class any/c                     #f]
                [#:attrs attrs list?                     null]
                [child any/c] ...)
         any/c]{
Build a vertical stack container.

Positional arguments: each @racket[child] is a stacked item.
}

@defproc[(inline [#:id    id    (or/c #f string? symbol?) #f]
                 [#:class class any/c                     #f]
                 [#:attrs attrs list?                     null]
                 [child any/c] ...)
         any/c]{
Build a horizontal inline container.

Positional arguments: each @racket[child] is an inline item.
}

@defproc[(spacer [grow (or/c number? string?) 1]
                 [#:id    id    (or/c #f string? symbol?) #f]
                 [#:class class any/c                     #f]
                 [#:attrs attrs list?                     null])
         any/c]{
Build a flexible spacer for layout.

Positional arguments: @racket[grow] is the growth factor/size basis.
}

@defproc[(divider [orientation symbol? 'horizontal]
                  [#:id    id    (or/c #f string? symbol?) #f]
                  [#:class class any/c                     #f]
                  [#:attrs attrs list?                     null])
         any/c]{
Build a horizontal or vertical divider.

Positional arguments: @racket[orientation] is @racket['horizontal] or @racket['vertical].
}

@defproc[(group [label any/c]
                [#:id    id    (or/c #f string? symbol?) #f]
                [#:class class any/c                     #f]
                [#:attrs attrs list?                     null]
                [child any/c] ...)
         any/c]{
Build a labeled container (fieldset-like grouping).

Positional arguments:
@racket[label] is the legend/title;
each @racket[child] is group content.
}

@section{Text and Content Components}

@defproc[(text [value any/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         any/c]{
Build a text node view.

Positional arguments: @racket[value] is the displayed text value.
}

@defproc[(heading [level (integer-in 1 6)]
                  [content any/c]
                  [align symbol? 'left]
                  [spacing symbol? 'normal]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a semantic heading.

Positional arguments:
@racket[level] selects heading level 1..6,
@racket[content] is text/view content,
@racket[align] is heading alignment, and
@racket[spacing] controls heading spacing style.
}

@defproc[(h1 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-1 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h2 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-2 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h3 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-3 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h4 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-4 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h5 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-5 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h6 [content any/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         any/c]{
Build a level-6 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-heading [level (integer-in 1 6)]
                          [content any/c]
                          [#:id id (or/c #f string? symbol?) #f]
                          [#:class class any/c #f]
                          [#:attrs attrs list? null])
         any/c]{
Build a display-style heading.

Positional arguments:
@racket[level] selects display level 1..6 and
@racket[content] is heading text/content.
}

@defproc[(display-1 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-1 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-2 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-2 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-3 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-3 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-4 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-4 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-5 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-5 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-6 [content any/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a level-6 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(heading-with-subtitle [level (integer-in 1 6)]
                                [title any/c]
                                [subtitle any/c]
                                [#:id id (or/c #f string? symbol?) #f]
                                [#:class class any/c #f]
                                [#:attrs attrs list? null])
         any/c]{
Build a heading with subtitle text.

Positional arguments:
@racket[level] selects heading level;
@racket[title] is main text;
@racket[subtitle] is secondary text.
}

@defproc[(display-heading-with-subtitle [level (integer-in 1 6)]
                                        [title any/c]
                                        [subtitle any/c]
                                        [#:id id (or/c #f string? symbol?) #f]
                                        [#:class class any/c #f]
                                        [#:attrs attrs list? null])
         any/c]{
Build a display heading with subtitle text.

Positional arguments:
@racket[level] selects display level;
@racket[title] is main text;
@racket[subtitle] is secondary text.
}

@defproc[(lead [content any/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         any/c]{
Build lead paragraph text.

Positional arguments: @racket[content] is lead text/content.
}

@defproc[(blockquote [quote any/c]
                     [attrib any/c #f]
                     [align symbol? 'left]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         any/c]{
Build a blockquote with optional attribution and alignment.

Positional arguments:
@racket[quote] is quote text/content;
@racket[attrib] is optional attribution text;
@racket[align] is quote alignment.
}

@defproc[(image [src any/c]
                [alt any/c ""]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build an image view.

Positional arguments:
@racket[src] is the image URL/source and @racket[alt] is alt text.
}

@defproc[(link [label any/c]
               [href any/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         any/c]{
Build a hyperlink view.

Positional arguments:
@racket[label] is link text/content and
@racket[href] is the target URL.
}

@section{Action and Input Components}

@defproc[(button [label any/c]
                 [action procedure?]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         any/c]{
Build a clickable button.

Positional arguments:
@racket[label] is the button caption and
@racket[action] is called on click.
}

@defproc[(close-button [action procedure?]
                       [aria-label any/c "Close"]
                       [#:id id (or/c #f string? symbol?) #f]
                       [#:class class any/c #f]
                       [#:attrs attrs list? null])
         any/c]{
Build a standardized close button.

Positional arguments:
@racket[action] is called when pressed and
@racket[aria-label] sets accessible label text.
}

@defproc[(button-group [child any/c] ...
                       [#:id id (or/c #f string? symbol?) #f]
                       [#:class class any/c #f]
                       [#:attrs attrs list? null])
         any/c]{
Build a grouped button container.

Positional arguments:
each @racket[child] is typically a button view in the group.
}

@defproc[(toggle-button-group [items list?]
                              [selected any/c]
                              [on-change procedure?]
                              [#:multiple? multiple? any/c #f]
                              [#:id id (or/c #f string? symbol?) #f]
                              [#:class class any/c #f]
                              [#:attrs attrs list? null])
         any/c]{
Build a toggle button group (single or multiple select).

Positional arguments:
@racket[items] is the toggle option list;
@racket[selected] is current selection;
@racket[on-change] receives new selection values.
}

@defproc[(button-toolbar [group any/c] ...
                         [#:id id (or/c #f string? symbol?) #f]
                         [#:class class any/c #f]
                         [#:attrs attrs list? null])
         any/c]{
Build a toolbar composed of button groups.

Positional arguments:
each @racket[group] is a toolbar group or button-group view.
}

@defproc[(toolbar [child any/c] ...
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a generic horizontal toolbar.

Positional arguments:
each @racket[child] is a toolbar item view.
}

@defproc[(toolbar-group [child any/c] ...
                        [#:id id (or/c #f string? symbol?) #f]
                        [#:class class any/c #f]
                        [#:attrs attrs list? null])
         any/c]{
Build a grouped toolbar section.

Positional arguments: each @racket[child] is an item in one toolbar group.
}

@defproc[(input [value any/c]
                [on-change procedure?]
                [#:on-enter on-enter (or/c #f procedure?) #f]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build a text input.

Positional arguments: @racket[value] is current input value and @racket[on-change] receives updates.
}

@defproc[(textarea [value any/c]
                   [on-change procedure?]
                   [#:rows rows (or/c #f exact-positive-integer?) #f]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a textarea input.

Positional arguments: @racket[value] is current text and @racket[on-change] receives updates.
}

@defproc[(checkbox [checked any/c]
                   [on-toggle procedure?]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a checkbox input.

Positional arguments: @racket[checked] is current checked state and @racket[on-toggle] receives updates.
}

@defproc[(radios [options list?]
                 [selected any/c]
                 [on-select procedure?]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         any/c]{
Build a radio-choice input.

Positional arguments:
@racket[options] is radio option data;
@racket[selected] is current value;
@racket[on-select] receives selected value.
}

@defproc[(choice [options list?]
                 [selected any/c]
                 [on-select procedure?]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         any/c]{
Build a single-select dropdown.

Positional arguments:
@racket[options] is selectable options;
@racket[selected] is current value;
@racket[on-select] receives selected value.
}

@defproc[(slider [value any/c]
                 [on-change procedure?]
                 [min number? 0]
                 [max number? 100]
                 [#:step step (or/c #f number?) #f]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         any/c]{
Build a range slider.

Positional arguments:
@racket[value] is current numeric value;
@racket[on-change] receives updates;
@racket[min] and @racket[max] set range bounds.
}

@defproc[(pagination [items list?]
                     [current any/c]
                     [on-select procedure?]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         any/c]{
Build a pagination control.

Positional arguments:
@racket[items] is page item data;
@racket[current] is current page value;
@racket[on-select] receives chosen page.
}

@defproc[(breadcrumb [items list?]
                     [current any/c]
                     [on-select procedure?]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         any/c]{
Build breadcrumb navigation.

Positional arguments:
@racket[items] is breadcrumb path data;
@racket[current] is current crumb value;
@racket[on-select] handles navigation clicks.
}

@section{Feedback Components}

@defproc[(alert [value any/c]
                [level any/c 'info]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build an inline alert.

Positional arguments:
@racket[value] is alert text/content and
@racket[level] is severity/style level.
}

@defproc[(alert-rich [body any/c]
                     [title any/c #f]
                     [link-text any/c #f]
                     [link-href any/c #f]
                     [level any/c 'info]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         any/c]{
Build a richer alert with title and optional link parts.

Positional arguments:
@racket[body] is main message;
@racket[title] is optional heading;
@racket[link-text] and @racket[link-href] define optional link;
@racket[level] sets severity/style.
}

@defproc[(toast [open any/c]
                [on-close procedure?]
                [value any/c]
                [level any/c 'info]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build a toast notification.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close;
@racket[value] is message content;
@racket[level] selects tone.
}

@defproc[(badge [value any/c]
                [level any/c 'info]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build a badge label.

Positional arguments:
@racket[value] is badge content and @racket[level] sets tone/style.
}

@defproc[(spinner [label any/c "Loading..."]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a spinner indicator.
                
Positional arguments:
@racket[label] is optional accessible/loading text.
}

@defproc[(progress [value any/c]
                   [max-value any/c 100]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a progress indicator.

Positional arguments:
@racket[value] is current progress and @racket[max-value] is the full-scale maximum.
}

@defproc[(placeholder [shape any/c 'text]
                      [width any/c #f]
                      [#:id id (or/c #f string? symbol?) #f]
                      [#:class class any/c #f]
                      [#:attrs attrs list? null])
         any/c]{
Build a placeholder/skeleton block.

Positional arguments:
@racket[shape] selects placeholder style and @racket[width] optionally sets width.
}

@section{Navigation and Composite Components}

@defproc[(list-group [items list?]
                     [current any/c]
                     [on-select procedure?]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         any/c]{
Build a selectable list group.

Positional arguments: @racket[items] is item data;
@racket[current] is selected item value;
@racket[on-select] receives selected value.
}

@defproc[(tab-panel [tabs list?]
                    [current any/c]
                    [render-panel procedure?]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a tab panel.

Positional arguments:
@racket[tabs] is tab metadata;
@racket[current] is selected tab key;
@racket[render-panel] renders content for selected tab.
}

@defproc[(dropdown [label any/c]
                   [items list?]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a dropdown component.

Positional arguments:
@racket[label] is dropdown trigger label and @racket[items] is menu item data.
}

@defproc[(menu-bar [menu any/c] ...) any/c]{
Build a horizontal menu bar.

Positional arguments: each @racket[menu] is a @racket[menu] view.
}

@defproc[(menu [label any/c] [item any/c] ...) any/c]{
Build a menu in a menu bar.

Positional arguments:
@racket[label] is menu title and each @racket[item] is usually a @racket[menu-item] or divider.
}

@defproc[(menu-item [label any/c] [action procedure?]) any/c]{
Build a menu item.

Positional arguments: @racket[label] is item text and @racket[action] is called on activation.
}

@defproc[(navigation-bar [item any/c] ...
                         [#:id id (or/c #f string? symbol?) #f]
                         [#:class class any/c #f]
                         [#:attrs attrs list? null])
         any/c]{
Build a navigation bar.

Positional arguments: each @racket[item] is navbar content (brand, links, controls, menus, etc.).
}

@defproc[(top-bar [item any/c] ...
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a top bar container.

Positional arguments: each @racket[item] is top-bar content.
}

@section{Data and Structure Components}

@defproc[(table [columns list?]
                [rows list?]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build a table view.

Positional arguments: @racket[columns] defines table columns and @racket[rows] is row data.
}

@defproc[(list-view [items any/c]
                    [render-item procedure?]
                    [key-of procedure?])
         any/c]{
Build a keyed dynamic list view.

Positional arguments:
@racket[items] is the source collection/observable;
@racket[render-item] builds per-item views;
@racket[key-of] extracts stable item keys.
}

@defproc[(card [child any/c] ...
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         any/c]{
Build a card container.

Positional arguments: each @racket[child] is card content.
}

@defproc[(accordion [sections list?]
                    [current any/c]
                    [on-select procedure?]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build an accordion with one open section.

Positional arguments:
@racket[sections] is section data;
@racket[current] is open section key;
@racket[on-select] receives new open key.
}

@defproc[(carousel [slides list?]
                   [current any/c]
                   [on-select procedure?]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a carousel.

Positional arguments:
@racket[slides] is slide data;
@racket[current] is active slide key/index;
@racket[on-select] receives new active slide.
}

@defproc[(scrollspy [items list?]
                    [current any/c]
                    [on-select procedure?]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build a scrollspy navigation component.

Positional arguments:
@racket[items] is tracked section data;
@racket[current] is active section key;
@racket[on-select] receives selected section.
}

@section{Overlays}

@defproc[(dialog [open any/c]
                 [on-close procedure?]
                 [child any/c] ...
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         any/c]{
Build a dialog.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is dialog content.
}

@defproc[(modal [open any/c]
                [on-close procedure?]
                [child any/c] ...
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         any/c]{
Build a modal overlay.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is modal content.
}

@defproc[(offcanvas [open any/c]
                    [on-close procedure?]
                    [child any/c] ...
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         any/c]{
Build an offcanvas panel.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is panel content.
}

@defproc[(tooltip [anchor any/c]
                  [content any/c]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a tooltip wrapper.

Positional arguments:
@racket[anchor] is trigger/anchor content and @racket[content] is tooltip content.
}

@defproc[(popover [anchor any/c]
                  [content any/c]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         any/c]{
Build a popover wrapper.

Positional arguments:
@racket[anchor] is trigger/anchor content and @racket[content] is popover content.
}

@section{Reactive and Branching Components}

@defproc[(observable-view [obs any/c] [render procedure?]) any/c]{
Build a dynamic view from an observable value.

Positional arguments:
@racket[obs] is the observable source and @racket[render] maps each observed value to a view.
}

@defproc[(if-view [test any/c] [then-view any/c] [else-view any/c]) any/c]{
Build a two-way conditional view.

Positional arguments: @racket[test] is condition value/observable;
@racket[then-view] is true branch;
@racket[else-view] is false branch.
}

@defproc[(cond-view [clause pair?] ...) any/c]{
Build a multi-branch conditional view.

Positional arguments: each @racket[clause] is a condition/view branch pair.
}

@defproc[(case-view [target any/c] [clause pair?] ...) any/c]{
Build an equality-based branch view.

Positional arguments:
@racket[target] is matched value/observable;
each @racket[clause] maps match keys to a branch view.
}

@defproc[(collapse [open any/c]
                   [child any/c]
                   [#:id    id    (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         any/c]{
Build a collapsible region.

Positional arguments:
@racket[open] controls expanded state and @racket[child] is collapsible content.
}
