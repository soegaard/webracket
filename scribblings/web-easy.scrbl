#lang scribble/manual
@(require scribble-tools
          "webracket-scribble-utils.rkt"
          (for-label (only-in (lib "core.rkt" "webracket")
                              define define-values lambda λ
                              if cond case and or when unless
                              begin begin0 set!
                              let let* letrec local
                              let-values let*-values letrec-values
                              case-lambda with-handlers
                              for for*
                              for/list for*/list
                              for/vector for*/vector
                              for/sum for*/sum
                              for/fold for*/fold
                              for/or for*/or
                              for/and for*/and
                              for/first for*/first
                              quasiquote unquote unquote-splicing))
          (for-label (only-in (prefix-in racket: racket/base)
                              racket:define racket:define-values racket:lambda racket:for/list))
          (for-label (lib "scribblings/primitives-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-event-labels.rkt" "webracket"))
          (for-label (lib "scribblings/web-easy-labels.rkt" "webracket"))
          racket/base
          racket/file
          racket/list
          racket/runtime-path
          racket/string)

@(define-runtime-path web-easy-attrs-spec-path "../lib/web-easy/spec/html-element-attributes.sexp")

@(define (load-html-element-attrs-table)
   (define datum (call-with-input-file web-easy-attrs-spec-path read))
   (unless (and (list? datum)
                (pair? datum)
                (eq? (car datum) 'html-element-attributes))
     (error 'web-easy-docs "malformed html-element-attributes spec"))
   (define table-entry (assq 'table (cdr datum)))
   (unless (and table-entry (list? table-entry))
     (error 'web-easy-docs "missing table in html-element-attributes spec"))
   (define ht (make-hash))
   (for ([row (in-list (cdr table-entry))])
     (when (and (list? row)
                (= (length row) 2)
                (string? (car row))
                (list? (cadr row)))
       (hash-set! ht (car row) (filter string? (cadr row)))))
   ht)

@(define web-easy-html-element-attrs-table
   (load-html-element-attrs-table))

@(define (attrs-for-tags tags)
   (sort (remove-duplicates
          (append-map (lambda (tag)
                        (hash-ref web-easy-html-element-attrs-table tag '()))
                      tags))
         string<?))

@(define (attrs->keyword-string attrs)
   (string-join (map (lambda (a) (string-append "#:" a)) attrs)
                ", "
                #:before-last ", and "))

@title{Library: @racketid[web-easy]}
@declare-exporting[(lib "scribblings/web-easy-labels.rkt" "webracket")
                   (lib "scribblings/lib-event-labels.rkt" "webracket")]

@(how-to-require include-lib web-easy (lib "core.rkt" "webracket"))
@(compile-option-bar "Compile option: " "--ffi --browser --ffi dom --ffi standard --ffi js")



@section{About web-easy}

The @tt{web-easy} library is a declarative UI library for WebRacket.

The library is inspired by @tt{gui-easy}, whose goal is to simplify
the construction of user interfaces in Racket. The approach is to
wrap the existing imperative GUI API in a functional shell and
use observables to manage state.

This library follows the same approach, but the underlying
GUI toolkit is provided by the browser.
Due to the rich design possibilities on the web,
the set of available components is larger.
The intended core-vs-library split is described in @tt{lib/web-easy/API-LAYERS.md}.


@section{Quickstart}

The following program fragments focus on the how view and observables
are used to create user interfaces. To see how the fragments are
turned into a runnable program, look at the @tt{examples/}
subfolder of the @racketid[web-easy] library. Each program in
this Quickstart has its own folder. 

@url{https://github.com/soegaard/webracket/tree/main/lib/web-easy/examples}

The examples are heavily inspired by the @racket[gui-easy] Quickstart.

@subsection{JSXGraph Examples}

The @tt{examples/jsx-graph-gallery/} folder contains the JSXGraph
gallery used to exercise the wrapper classes in separate boards and
stress the browser bridge.

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


@section{Observables}

Observables are the state layer in @racketid[web-easy].
An observable holds a current value, can be updated, and can notify
dependent views or callbacks when the value changes.

The Quickstart examples use the short operator forms:

@itemlist[
 @item{@racket[(|@| v)] creates an observable}
 @item{@racket[(o . ~> . f)] reads/maps the current value through @racket[f]}
 @item{@racket[(o . <~ . f)] updates the current value with @racket[f]}
 @item{@racket[(:= o v)] replaces the current value with @racket[v]}
]

The corresponding procedure forms are documented below.

@defproc[(obs? [v any/c]) boolean?]{
Check whether @racket[v] is an observable.
}

@defproc[(obs [v any/c]
               [name symbol? 'anon]
               [derived? boolean? #f])
         observable?]{
Create an observable holding @racket[v].

The optional @racket[name] is used for debugging and error reporting.
If @racket[derived?] is true, the observable is derived/read-only and cannot be
updated with @racket[obs-update!] or @racket[obs-set!].
}

@defproc[(obs-name [o observable?]) symbol?]{
Return the name attached to @racket[o].
}

@defproc[(obs-peek [o observable?]) any/c]{
Return the current value of @racket[o].
}

@defproc[(obs-observe! [o observable?]
                       [f procedure?])
         void?]{
Register @racket[f] to be called whenever @racket[o] changes.

The callback receives the new value of @racket[o].
}

@defproc[(obs-unobserve! [o observable?]
                         [f procedure?])
         void?]{
Remove the observer @racket[f] from @racket[o].
}

@defproc[(obs-update! [o observable?]
                      [f procedure?])
         any/c]{
Update @racket[o] by applying @racket[f] to its current value.
The result becomes the new current value and is returned.
}

@defproc[(obs-set! [o observable?]
                   [v any/c])
         void?]{
Replace the current value of @racket[o] with @racket[v].
}

@defproc[(obs-map [o observable?]
                  [f procedure?])
         observable?]{
Create a derived observable whose value is @racket[(f (obs-peek o))].
}

@defproc[(obs-combine [f procedure?]
                      [o observable?] ...)
         observable?]{
Create a derived observable by applying @racket[f] to the current values
of one or more observables.

For example, this produces an observable whose value is always a list of
the current values of three source observables:

@racketblock[
(obs-combine list |@circles| |@selected-circle| |@diam|)]
}

@defproc[(obs-filter [o observable?]
                     [pred procedure?]
                     [default any/c #f])
         observable?]{
Create a derived observable that only updates when @racket[pred] accepts
the new value.

If the initial value does not satisfy @racket[pred], the derived observable
starts with @racket[default].
}

@defform[(obs-watch! o ... f)]{
Observe several observables at once and call @racket[f] with their current values.

@racket[obs-watch!] is shorthand for combining the source observables with
@racket[list] and then observing the combined result. The callback @racket[f]
is called with one argument per observable, in the same order as the
observable arguments.

Example:

@racketblock[
(obs-watch! |@circles| |@selected-circle| |@diam|
  (λ (circles selected-circle diam)
    (redraw-canvas! circles)))]

This is equivalent to:

@racketblock[
(obs-observe! (obs-combine list |@circles| |@selected-circle| |@diam|)
  (λ (state)
    (match state
      [(list circles selected-circle diam)
       (redraw-canvas! circles)])))]
}


@section{Common Keywords and Contracts}

Most concrete component constructors support:

@itemlist[
  @item{@racket[#:id]    to set the root DOM id}
  @item{@racket[#:class] to add one or more CSS classes}
  @item{@racket[#:attrs] to add root DOM attributes}
]

Composition forms such as @racket[window], @racket[vpanel], and
@racket[hpanel] keep positional-only contracts.

Primitive HTML element constructors also support a Phase 1 set of
generic bubbling DOM event keywords:

@itemlist[
  @item{@racket[#:on-click], @racket[#:on-doubleclick], @racket[#:on-contextmenu]}
  @item{@racket[#:on-copy], @racket[#:on-cut], @racket[#:on-paste]}
  @item{@racket[#:on-compositionstart], @racket[#:on-compositionupdate], @racket[#:on-compositionend]}
  @item{@racket[#:on-keydown], @racket[#:on-keyup]}
  @item{@racket[#:on-focus], @racket[#:on-blur], @racket[#:on-focusin], @racket[#:on-focusout]}
  @item{@racket[#:on-input], @racket[#:on-change], @racket[#:on-beforeinput], @racket[#:on-submit], @racket[#:on-reset], @racket[#:on-invalid]}
  @item{@racket[#:on-wheel], @racket[#:on-scroll]}
  @item{@racket[#:on-drag], @racket[#:on-dragstart], @racket[#:on-dragend], @racket[#:on-dragenter], @racket[#:on-dragleave], @racket[#:on-dragover], @racket[#:on-drop]}
  @item{@racket[#:on-touchstart], @racket[#:on-touchmove], @racket[#:on-touchend], @racket[#:on-touchcancel]}
  @item{@racket[#:on-load], @racket[#:on-error], @racket[#:on-abort], @racket[#:on-loadeddata], @racket[#:on-loadedmetadata]}
  @item{@racket[#:on-canplay], @racket[#:on-canplaythrough], @racket[#:on-play], @racket[#:on-playing], @racket[#:on-pause], @racket[#:on-ended], @racket[#:on-timeupdate], @racket[#:on-volumechange]}
  @item{@racket[#:on-animationstart], @racket[#:on-animationend], @racket[#:on-animationiteration], @racket[#:on-transitionend]}
  @item{@racket[#:on-mousedown], @racket[#:on-mousemove], @racket[#:on-mouseup]}
  @item{@racket[#:on-mouseenter], @racket[#:on-mouseleave], @racket[#:on-mouseover], @racket[#:on-mouseout]}
  @item{@racket[#:on-pointerdown], @racket[#:on-pointermove], @racket[#:on-pointerup]}
  @item{@racket[#:on-pointerenter], @racket[#:on-pointerleave], @racket[#:on-pointerover], @racket[#:on-pointerout], @racket[#:on-pointercancel]}
  @item{@racket[#:on-gotpointercapture], @racket[#:on-lostpointercapture]}
]

These keywords are part of the core primitive surface.
Each callback receives the raw browser event object, so handlers can read
fields such as @tt{offsetX}, @tt{offsetY}, @tt{clientX}, and @tt{clientY}.

Example:

@racketblock[
(Input #:on-keydown
        (lambda (evt)
          (define key (js-ref evt "key"))
          (js-log key)))
]

@defthing[html-attr-entry/c
          (or/c (cons/c symbol? any/c)
                (list/c symbol? any/c))]{
Contract alias for one attribute entry used in @racket[#:attrs].

An entry is either:

@itemlist[
  @item{a pair such as @racket[(cons 'id "hero")]}
  @item{a 2-element list such as @racket['(id "hero")]}
]
}

@defthing[html-attrs/c
          (or/c #f
                (listof html-attr-entry/c))]{
Contract alias for attribute lists accepted by @racket[#:attrs].
}

@defthing[content/c
          (or/c string?
                view?
                observable?)]{
Public-facing contract alias for primitive element content that may be:

@itemlist[
  @item{a string}
  @item{a child view}
  @item{an observable whose current value is a string or a view}
]
}

@defthing[text-content/c any/c]{
Predicate for values accepted by text-bearing constructors:
strings, symbols, numbers, booleans, characters, or observables whose
current value is one of those.
}

@section{Event Handling}

The following thin convenience wrappers are available on top of the raw
event object passed to primitive @racket[#:on-*] callbacks:

Example:

@racketblock[
(Canvas #:on-contextmenu
        (lambda (evt)
          (prevent-default! evt)
          (define x (mouse-event-offset-x evt))
          (define y (mouse-event-offset-y evt))
          (js-log (format "menu at (~a, ~a)" x y))))
]

@itemlist[
  @item{@racket[#:ref] on primitive elements}
  @item{@racket[event?], @racket[mouse-event?], @racket[keyboard-event?]}
  @item{@racket[pointer-event?], @racket[focus-event?], @racket[input-event?],
        @racket[submit-event?], @racket[touch-event?], @racket[wheel-event?]}
  @item{@racket[touch-list?], @racket[touch?]}
  @item{@racket[event-type], @racket[event-target], @racket[event-current-target]}
  @item{@racket[prevent-default!], @racket[stop-propagation!], @racket[stop-immediate-propagation!]}
  @item{@racket[mouse-event-offset-x], @racket[mouse-event-offset-y]}
  @item{@racket[mouse-event-client-x], @racket[mouse-event-client-y]}
  @item{@racket[mouse-event-page-x], @racket[mouse-event-page-y]}
  @item{@racket[mouse-event-screen-x], @racket[mouse-event-screen-y]}
  @item{@racket[mouse-event-button], @racket[mouse-event-buttons]}
  @item{@racket[mouse-event-alt-key?], @racket[mouse-event-ctrl-key?],
        @racket[mouse-event-meta-key?], @racket[mouse-event-shift-key?]}
  @item{@racket[keyboard-event-key], @racket[keyboard-event-code]}
  @item{@racket[keyboard-event-alt-key?], @racket[keyboard-event-ctrl-key?],
        @racket[keyboard-event-meta-key?], @racket[keyboard-event-shift-key?],
        @racket[keyboard-event-repeat?]}
  @item{@racket[touch-event-touches], @racket[touch-event-target-touches],
        @racket[touch-event-changed-touches]}
  @item{@racket[touch-list-length], @racket[touch-list-ref]}
  @item{@racket[touch-identifier], @racket[touch-client-x], @racket[touch-client-y]}
  @item{@racket[touch-page-x], @racket[touch-page-y],
        @racket[touch-screen-x], @racket[touch-screen-y]}
]

@subsection{Ref}

Primitive elements also accept a @racket[#:ref] keyword argument.
The value of @racket[#:ref] must be an observable.

A ref observable holds:

@itemlist[
  @item{@racket[#f] before the element is mounted}
  @item{the current DOM node while the element is mounted}
  @item{@racket[#f] again after the renderer is destroyed}
]

This gives @tt{web-easy} an imperative escape hatch without introducing a
separate ref type. A ref is simply an observable used to track the
current node.

Refs are most useful when you need to reach out from the declarative
view layer and perform a small imperative DOM action. Typical cases
include:

@itemlist[
  @item{moving focus to a particular element}
  @item{reading or adjusting browser-managed selection or playback state}
  @item{calling DOM or library APIs that operate on a concrete node}
]

Prefer ordinary views, callbacks, and observables when the behavior can
already be expressed declaratively.

@racketblock[
(define |@input| (|@| #f))

(vpanel
 (Input #:ref |@input|)
 (Button "Focus input"
         (lambda ()
           (define node (obs-peek |@input|))
           (when node
             (js-send node "focus" (vector))))))
]

@subsection{Autofocus}

Primitive elements accept a @racket[#:autofocus] keyword argument.
Components can also use @racket[#:autofocus] when they forward root
attributes to a primitive root element.

When @racket[#:autofocus] is true, @tt{web-easy} will focus the mounted
root DOM node after it appears in the browser. This makes
@racket[#:autofocus] useful not only for initial page load, but also for
dynamically mounted UI such as editors, dialogs, and popovers.

Use @racket[#:autofocus] for the common declarative case where a newly
mounted element should receive focus automatically. Use @racket[#:ref]
when you need lower-level control over exactly when or how focus is moved.

Components that forward root attributes can also forward @racket[#:ref]
and generic bubbling DOM event keywords such as @racket[#:on-keydown].

@racketblock[
(Input #:autofocus #t)

(Div #:tabindex 0
     #:autofocus #t
     "Keyboard focus starts here")
]

@defproc*[
([(event? [evt external])
  boolean?]
 [(mouse-event? [evt external])
  boolean?]
 [(keyboard-event? [evt external])
  boolean?]
 [(pointer-event? [evt external])
  boolean?]
 [(focus-event? [evt external])
  boolean?]
 [(input-event? [evt external])
  boolean?]
 [(submit-event? [evt external])
  boolean?]
 [(touch-event? [evt external])
  boolean?]
 [(wheel-event? [evt external])
  boolean?])]{
Check whether an @racket[external] value contains a DOM event object of a particular kind.

The argument @racket[evt] is an @racket[external] value. These predicates
check whether it contains a JavaScript
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Event"]{Event},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent"]{KeyboardEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent"]{PointerEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent"]{FocusEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/InputEvent"]{InputEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/SubmitEvent"]{SubmitEvent},
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent"]{TouchEvent},
or @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent"]{WheelEvent}
object, respectively.
}

@defproc*[
([(touch-list? [v any/c])
  boolean?]
 [(touch? [v any/c])
  boolean?])]{
Check whether a value is a browser touch collection or touch object.

@racket[touch-list?] reports whether @racket[v] is an @racket[external]
containing a JavaScript
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TouchList"]{TouchList}
object.

@racket[touch?] reports whether @racket[v] is an @racket[external]
containing a JavaScript
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Touch"]{Touch}
object.
}

@defproc*[
([(event-type [evt external])
  string?]
 [(event-target [evt external])
  (or/c #f external)]
 [(event-current-target [evt external])
  (or/c #f external)])]{
Read common properties from a raw browser event object.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Event"]{Event}
object.

@racket[event-type] returns the DOM event type, such as @racket["click"]
or @racket["contextmenu"].

@racket[event-target] returns the event target object, if any.

@racket[event-current-target] returns the object whose listener is
currently running, if any.
}

@defproc*[
([(prevent-default! [evt external])
  void?]
 [(stop-propagation! [evt external])
  void?]
 [(stop-immediate-propagation! [evt external])
  void?])]{
Control propagation and default browser behavior for a raw browser event object.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Event"]{Event}
object.

@racket[prevent-default!] requests that the browser skip the event's
default action.

@racket[stop-propagation!] stops the event from propagating further.

@racket[stop-immediate-propagation!] stops immediate propagation, so
later listeners for the same event are not run.
}

@defproc*[
([(mouse-event-offset-x [evt external])
  number?]
 [(mouse-event-offset-y [evt external])
  number?])]{
Read mouse coordinates relative to the event target.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

@racket[mouse-event-offset-x] returns the horizontal offset from the
left edge of the target.

@racket[mouse-event-offset-y] returns the vertical offset from the top
edge of the target.
}

@defproc*[
([(mouse-event-client-x [evt external])
  number?]
 [(mouse-event-client-y [evt external])
  number?])]{
Read mouse coordinates in viewport pixels.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

@racket[mouse-event-client-x] returns the horizontal client coordinate.

@racket[mouse-event-client-y] returns the vertical client coordinate.
}

@defproc*[
([(mouse-event-page-x [evt external])
  number?]
 [(mouse-event-page-y [evt external])
  number?])]{
Read mouse coordinates in document pixels.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

@racket[mouse-event-page-x] returns the horizontal page coordinate.

@racket[mouse-event-page-y] returns the vertical page coordinate.
}

@defproc*[
([(mouse-event-screen-x [evt external])
  number?]
 [(mouse-event-screen-y [evt external])
  number?])]{
Read mouse coordinates in screen pixels.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

@racket[mouse-event-screen-x] returns the horizontal screen coordinate.

@racket[mouse-event-screen-y] returns the vertical screen coordinate.
}

@defproc*[
([(mouse-event-button [evt external])
  exact-integer?]
 [(mouse-event-buttons [evt external])
  exact-integer?])]{
Read mouse button state from a mouse event.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

@racket[mouse-event-button] returns the button code for the event.

@racket[mouse-event-buttons] returns the current pressed-buttons bitmask.
}

@defproc*[
([(mouse-event-alt-key? [evt external])
  boolean?]
 [(mouse-event-ctrl-key? [evt external])
  boolean?]
 [(mouse-event-meta-key? [evt external])
  boolean?]
 [(mouse-event-shift-key? [evt external])
  boolean?])]{
Read modifier-key state from a mouse event.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent"]{MouseEvent}
object.

Each helper reports whether the corresponding modifier key was active.
}

@defproc*[
([(keyboard-event-key [evt external])
  string?]
 [(keyboard-event-code [evt external])
  string?])]{
Read the key identity from a keyboard event.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent"]{KeyboardEvent}
object.

@racket[keyboard-event-key] returns the key value reported by the browser.

@racket[keyboard-event-code] returns the physical key code.
}

@defproc*[
([(keyboard-event-alt-key? [evt external])
  boolean?]
 [(keyboard-event-ctrl-key? [evt external])
  boolean?]
 [(keyboard-event-meta-key? [evt external])
  boolean?]
 [(keyboard-event-shift-key? [evt external])
  boolean?]
 [(keyboard-event-repeat? [evt external])
  boolean?])]{
Read modifier-key and repeat status from a keyboard event.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent"]{KeyboardEvent}
object.

Each modifier helper reports whether the corresponding modifier key was active.

@racket[keyboard-event-repeat?] reports whether the event is auto-repeating.
}

@defproc*[
([(touch-event-touches [evt external])
  external]
 [(touch-event-target-touches [evt external])
  external]
 [(touch-event-changed-touches [evt external])
  external])]{
Read touch collections from a touch event.

The argument @racket[evt] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent"]{TouchEvent}
object.

Each function returns an @racket[external] containing a JavaScript
@hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TouchList"]{TouchList}
object.
}

@defproc*[
([(touch-list-length [xs external])
  exact-nonnegative-integer?]
 [(touch-list-ref [xs external]
                  [i exact-nonnegative-integer?])
  (or/c #f external)])]{
Inspect a touch list.

The argument @racket[xs] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TouchList"]{TouchList}
object.

@racket[touch-list-ref] returns @racket[#f] when @racket[i] is out of range.
}

@defproc*[
([(touch-identifier [t external])
  exact-integer?]
 [(touch-client-x [t external])
  number?]
 [(touch-client-y [t external])
  number?]
 [(touch-page-x [t external])
  number?]
 [(touch-page-y [t external])
  number?]
 [(touch-screen-x [t external])
  number?]
 [(touch-screen-y [t external])
  number?])]{
Read properties from a touch object.

The argument @racket[t] is an @racket[external] value expected to
contain a JavaScript @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Touch"]{Touch}
object.
}

@section{Theme}

@tt{web-easy} also provides a small stylesheet-based theme API.

This API is intentionally concrete:

@itemlist[
  @item{a @racket[theme] value describes the root html class and CSS files to use}
  @item{@racket[install-theme-manager!] installs the stylesheet link nodes}
  @item{@racket[set-theme!] switches the active theme}
  @item{@racket[observe-theme!] keeps a theme manager in sync with an observable}
]

The stylesheet load order is:

@itemlist[
  @item{core CSS}
  @item{theme CSS}
  @item{optional extra/page CSS}
]

The starter theme stylesheets use a small set of token tiers:

@itemlist[
  @item{neutral/page tokens such as background, foreground, subtle surfaces, and separators}
  @item{control and popup tokens for shared widget chrome}
  @item{semantic base tones such as @tt{--we-primary} and @tt{--we-success}}
  @item{semantic companion tokens for soft surfaces and readable accents:
        @tt{--we-*-subtle}, @tt{--we-*-border}, @tt{--we-*-emphasis}, and @tt{--we-*-on}}
]

This keeps the starter themes approachable: most customization can happen by
editing token values before changing component-specific selectors.

@defproc[(theme [id any/c]
                 [class-name string?]
                 [core-css string?]
                 [theme-css string?]
                 [extra-css (or/c #f string?)])
         theme?]{
Create a browser theme descriptor.

The resulting value records:

@itemlist[
  @item{@racket[id], an application-chosen theme identifier}
  @item{@racket[class-name], the class applied to the root @tt{<html>} element}
  @item{@racket[core-css], the shared core stylesheet path}
  @item{@racket[theme-css], the main theme stylesheet path}
  @item{@racket[extra-css], an optional extra/page stylesheet path}
]
}

@defproc[(theme? [v any/c])
         boolean?]{
Report whether @racket[v] is a theme descriptor.
}

@defproc[(theme-manager? [v any/c])
         boolean?]{
Report whether @racket[v] is a browser theme manager created by
@racket[install-theme-manager!].
}

@defproc[(install-theme-manager! [initial-theme theme?])
         theme-manager?]{
Install or reuse the managed stylesheet links in @tt{<head>} and apply
@racket[initial-theme] immediately.

The managed link ids are:

@itemlist[
  @item{@tt{we-theme-core-css}}
  @item{@tt{we-theme-css}}
  @item{@tt{we-theme-extra-css}}
]
}

@defproc[(set-theme! [manager theme-manager?]
                     [next-theme theme?])
         void?]{
Switch @racket[manager] to @racket[next-theme].

This updates the root html theme class and the managed stylesheet @tt{href}
attributes.
}

@defproc[(observe-theme! [manager theme-manager?]
                         [@theme obs?]
                         [theme->descriptor procedure?])
         void?]{
Synchronize @racket[manager] with the observable @racket[@theme].

The procedure @racket[theme->descriptor] is called on the current observable
value and on each later update. It must return a @racket[theme] value.
}

A common pattern is:

@itemlist[
  @item{define one or more @racket[theme] values}
  @item{install a theme manager with the initial theme}
  @item{optionally switch themes directly with @racket[set-theme!]}
  @item{or keep the manager in sync with an observable using @racket[observe-theme!]}
]

@subsection{Theme Example}

@racketblock[
(code:comment "Define two named themes that share the same core CSS.")
(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define dark-theme
  (theme 'dark
         "we-theme-dark"
         "web-easy-core.css"
         "theme-dark.css"
         #f))

(code:comment "Install the stylesheet links and apply the initial theme.")
(define tm
  (install-theme-manager! light-theme))

(code:comment "set-theme! uses the existing manager to switch to another theme.")
(set-theme! tm dark-theme)

(code:comment "Or controlled reactively from an observable.")
(define |@theme|
  (obs 'light))

(observe-theme! tm
                |@theme|
                (lambda (id)
                  (case id
                    [(light) light-theme]
                    [(dark)  dark-theme]
                    [else    light-theme])))
]

@section{Core Primitives}

The uppercase HTML-like constructors and low-level HTML helpers form the core primitive API.
They aim to mirror HTML semantics closely and provide the building blocks for higher-level
component libraries.

@subsection{Low-Level HTML Constructors}

The constructors below are low-level building blocks.
Most users should prefer the uppercase primitive constructors
(@racket[H1], @racket[P], @racket[Span], @racket[Div], and so on).

@defproc[(html-element [tag (or/c symbol? observable?)]
                       [content text-content/c]
                       [#:id id (or/c #f string? symbol?) #f]
                       [#:class class any/c #f]
                       [#:attrs attrs html-attrs/c null])
         view?]{
Build a low-level primitive HTML leaf element from a tag and content.

Positional arguments:
@racket[tag] is the element tag (for example @racket['h1], @racket['p], @racket['span]),
and @racket[content] is text-like content.
}

@defproc[(html-element-children [tag (or/c symbol? observable?)]
                                [#:attrs attrs html-attrs/c null]
                                [child view?] ...)
         view?]{
Build a low-level primitive HTML container element from a tag and child views.

Positional arguments:
@racket[tag] is the element tag (for example @racket['div], @racket['section]),
and each @racket[child] is rendered as a child view.
}

Internal note:
the related internal constructor @racket[observable-element-children]
supports an internal keyword @racket[#:after-render] used by some
compound components for backend bridging after child rebuilds.
This hook is internal and not part of the public stability contract.

For dynamic tag use with @racket[html-element] and @racket[html-element-children],
tag changes are handled with remount semantics: when the tag changes, the underlying DOM
node is replaced (similar to React/Angular element-type changes), rather than mutated in place.
This means DOM identity can change across tag updates.

For internal component authoring in @tt{web-easy}, @racket[define/component]
supports variadic children with @racket[#:rest], including together with
@racket[#:root-attrs].

When a component uses @racket[#:root-attrs], the forwarded root keyword
surface can include ordinary root attributes, @racket[#:ref],
@racket[#:autofocus], and generic bubbling DOM event keywords named
@racket[#:on-<dom-event-name>].

Internal status note:
built-in semantic/compound constructors are implemented with
@racket[define/component]; only @racket[html-element],
@racket[html-element-children], and internal
@racket[observable-element-children] remain on @racket[define/key].

@racketblock[
(define/component Example
  #:root-tag 'div
  #:rest children
  #:root-attrs attrs/final
  (define attrs/final
    (list (cons 'class "example")))
  (apply Div
         (append children
                 (list #:attrs attrs/final))))
]

@subsection{Primitive HTML Elements}

The uppercase constructors below map directly to primitive HTML elements.
They accept:

@itemlist[
  @item{@racket[content] as text/observable content}
  @item{@racket[#:attrs] as an optional attribute list matching @racket[html-attrs/c]}
  @item{keyword attributes corresponding to HTML attributes for the specific element
        (plus global attributes and @tt{data-*}/@tt{aria-*})}
  @item{generic bubbling DOM event keywords named @racket[#:on-<dom-event-name>]}
]

Attribute-list entries in @racket[#:attrs] must match @racket[html-attr-entry/c].

Example:

@racketblock[
(H1 "Title"
    #:attrs '((id "hero")
              (title "Welcome")
              (class "big")))
]

Even though the signatures below list @racket[#:attrs], direct keyword
attributes are also available for all allowed HTML attributes (global +
element-specific + @tt{data-*}/@tt{aria-*}).

Example:

@racketblock[
(H1 "Title"
    #:id "hero"
    #:lang "en"
    #:title "Welcome"
    #:data-testid "hero-1"
    #:aria-label "Main heading")
]

Unknown keyword attributes are rejected, and observable attribute values are supported.
Primitive DOM event callbacks receive the raw browser event object.

Example with direct keywords and @racket[#:attrs]:

@racketblock[
(window
 (container
  (H1 "Profile"
      #:id "profile-title"
      #:lang "en"
      #:attrs '((data-testid "title-1")))
  (P "Welcome to the profile page."
     #:class "lead")
  (Span "beta"
        #:attrs '((aria-label "beta label")
                  (style "font-style: italic;")))))
]

In this section, @em{leaf} means the constructor takes one content value
and produces a single HTML node (as opposed to container-style constructors
that are built from child view arguments).

Within each subsection below, constructors are listed alphabetically.

@subsection{Text and Inline Elements}

@defproc[(A [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<a>} element.

This is an anchor/link element, typically used for navigation.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("a")))}.
}

@defproc[(Abbr [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<abbr>} element.

This marks an abbreviation or acronym.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(B [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<b>} element.

This offsets text stylistically without implying extra semantic importance.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Cite [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<cite>} element.

This marks the title of a cited creative work.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Code [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<code>} element.

This marks inline code or code-like text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Data [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<data>} element.

This associates human-readable text with a machine-readable value.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("data")))}.
}

@defproc[(Del [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<del>} element.

This marks content that has been removed.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("del")))}.
}

@defproc[(Dfn [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<dfn>} element.

This marks a term being defined.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Em [content content/c] ...
             [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<em>} element.

This marks emphasized content (typically stress emphasis).

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc*[
([(H1 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?]
 [(H2 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?]
 [(H3 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?]
 [(H4 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?]
 [(H5 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?]
 [(H6 [content content/c] ...
      [#:align align any/c #f]
      [#:attrs attrs html-attrs/c null])
  view?])]{Build primitive @tt{<h1>} ... @tt{<h6>} elements.

These elements represent headings of different levels.
@racket[H1] is the most important heading level, and @racket[H6] is the least important.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

For @racket[H1] through @racket[H6], @racket[#:align] maps to the HTML
@tt{align} attribute. Typical values are @racket["left"], @racket["center"],
@racket["right"], and @racket["justify"].
@racketid[web-easy] currently passes the value through to the attribute
without validating it.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("h1" "h2" "h3" "h4" "h5" "h6")))}.
}

@defproc[(I [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<i>} element.

This offsets text in an alternate voice or mood (often rendered in italics).

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Ins [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<ins>} element.

This marks content that has been inserted.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("ins")))}.
}

@defproc[(Kbd [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<kbd>} element.

This marks user input text, such as keyboard shortcuts.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Mark [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<mark>} element.

This marks text that should be highlighted for reference.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(P [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<p>} element.

This is a paragraph element, typically used for ordinary text content.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("p")))}.
}

@defproc[(Pre [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<pre>} element.

This preserves whitespace and line breaks for preformatted text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("pre")))}.
}

@defproc[(Q [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<q>} element.

This marks an inline quotation.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("q")))}.
}

@defproc[(S [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<s>} element.

This marks text that is no longer accurate or relevant.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Samp [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<samp>} element.

This marks sample output text from a program or system.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Small [content content/c] ...
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<small>} element.

This marks side comments or fine-print style text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Span [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<span>} element.

This is an inline text container, typically used to style or annotate part of a line.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Strong [content content/c] ...
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<strong>} element.

This marks content with strong importance (typically bold emphasis).

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Sub [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<sub>} element.

This marks subscript text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Sup [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<sup>} element.

This marks superscript text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Time [content content/c] ...
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<time>} element.

This marks a date/time value in text.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("time")))}.
}

@defproc[(U [content content/c] ...
            [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<u>} element.

This marks non-textual annotation text (often rendered underlined).

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Var [content content/c] ...
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<var>} element.

This marks a variable name or placeholder.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@subsection{Form and Interactive Elements}

@defproc[(Button [child content/c] ...
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<button>} element.

This is a native button element for actions and form submission.

A single content item preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("button")))}.
}

@defproc[(Datalist [#:attrs attrs html-attrs/c null]
                   [child view?] ...)
         view?]{
Build a primitive @tt{<datalist>} element with children.

This provides predefined options for compatible form controls.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Details [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<details>} element with children.

This is a disclosure container that can be toggled open/closed.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("details")))}.
}

@defproc[(Dialog [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<dialog>} element with children.

This is a dialog/modal container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("dialog")))}.
}

@defproc[(Fieldset [#:attrs attrs html-attrs/c null]
                   [child view?] ...)
         view?]{
Build a primitive @tt{<fieldset>} element with children.

This groups related form controls and labels.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("fieldset")))}.
}

@defproc[(Form [#:attrs attrs html-attrs/c null]
               [child view?] ...)
         view?]{
Build a primitive @tt{<form>} element with children.

This is a native form container for controls and submission behavior.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("form")))}.
}

@defproc[(Input [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<input>} element.

This is a leaf/void form input element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("input")))}.
}

@defproc[(Label [content content/c] ...
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<label>} element.

This is a native label element for naming form controls.

A single text-like positional value preserves the historical text-bearing form.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("label")))}.
}

Example:

@racketblock[
(Form
 #:action "/submit"
 #:method "post"
 (Label "Name" #:for "name-input")
 (input "" (lambda (_v) (void)) #:id "name-input")
 (Button "Save" #:type "submit"))
]

@defproc[(Legend [content content/c] ...
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<legend>} element.

This is a caption/title for a @tt{<fieldset>}.

A single text-like positional value is accepted directly.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("legend")))}.
}

@defproc[(Meter [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<meter>} element with children.

This represents a scalar measurement within a known range.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("meter")))}.
}

@defproc[(Option [content content/c] ...
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<option>} element.

This is an option item used inside @tt{<select>}.

A single text-like positional value is accepted directly.
One or more content items are rendered in order; text-like items become raw text children and views remain child views.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("option")))}.
}

@defproc[(Optgroup [#:attrs attrs html-attrs/c null]
                   [child view?] ...)
         view?]{
Build a primitive @tt{<optgroup>} element with children.

This groups related @tt{<option>} elements inside @tt{<select>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("optgroup")))}.
}

@defproc[(Output [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<output>} element with children.

This represents the result of a calculation or user action.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("output")))}.
}

@defproc[(Progress [#:attrs attrs html-attrs/c null]
                   [child view?] ...)
         view?]{
Build a primitive @tt{<progress>} element with children.

This represents task completion progress.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("progress")))}.
}

@defproc[(Select [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<select>} element with children.

This is a form selection control, typically containing @tt{<option>} children.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("select")))}.
}

@defproc[(Summary [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<summary>} element with children.

This is the summary/label element for a @tt{<details>} container.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Textarea [content text-content/c]
                   [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<textarea>} element.

This is a multi-line text form control.

This primitive remains text/value-oriented and does not accept child views.
Use observables for its text/value content and keyword attributes instead of nested child elements.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("textarea")))}.
}

@subsection{Layout and Sectioning Elements}

@defproc[(Address [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<address>} element with children.

This marks contact information for an article or page.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Article [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<article>} element with children.

This is a self-contained composition container (for example, a blog post card).

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Aside [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<aside>} element with children.

This is a complementary/sidebar landmark/container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Blockquote [#:attrs attrs html-attrs/c null]
                     [child view?] ...)
         view?]{
Build a primitive @tt{<blockquote>} element with children.

This is the primitive block quotation container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("blockquote")))}.
}

@defproc[(Div [#:attrs attrs html-attrs/c null]
              [child view?] ...)
         view?]{
Build a primitive @tt{<div>} element with children.

This is a generic block-level container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("div")))}.
}

@defproc[(Figure [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<figure>} element with children.

This groups self-contained media/content, often with a caption.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Figcaption [#:attrs attrs html-attrs/c null]
                     [child view?] ...)
         view?]{
Build a primitive @tt{<figcaption>} element with children.

This is a caption/legend element for a @tt{<figure>}.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Footer [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<footer>} element with children.

This is a footer landmark/container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Header [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<header>} element with children.

This is a header landmark/container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Hgroup [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<hgroup>} element with children.

This groups one or more heading elements and optional subtitle content.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Main [#:attrs attrs html-attrs/c null]
               [child view?] ...)
         view?]{
Build a primitive @tt{<main>} element with children.

This is the main content landmark/container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Nav [#:attrs attrs html-attrs/c null]
              [child view?] ...)
         view?]{
Build a primitive @tt{<nav>} element with children.

This is a navigation landmark/container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Section [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<section>} element with children.

This is a thematic/semantic section container element.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@subsection{Lists and Table Elements}

@defproc[(Caption [#:attrs attrs html-attrs/c null]
                  [child view?] ...)
         view?]{
Build a primitive @tt{<caption>} element with children.

This is a caption/title element for a @tt{<table>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("caption")))}.
}

@defproc[(Col [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<col>} element.

This is a leaf/void table column formatting element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("col")))}.
}

@defproc[(Colgroup [#:attrs attrs html-attrs/c null]
                   [child view?] ...)
         view?]{
Build a primitive @tt{<colgroup>} element with children.

This groups @tt{<col>} elements for shared table column formatting.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("colgroup")))}.
}

@defproc[(Dd [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<dd>} element with children.

This is a description/value element used inside @tt{<dl>}.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Dl [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<dl>} element with children.

This is a description list container for term/description pairs.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("dl")))}.
}

@defproc[(Dt [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<dt>} element with children.

This is a term/name element used inside @tt{<dl>}.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Li [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<li>} element with children.

This is a list-item element used inside @tt{<ul>} or @tt{<ol>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("li")))}.
}






@defproc[(Menu [#:attrs attrs html-attrs/c null]
               [child view?] ...)
         view?]{
Build a primitive @tt{<menu>} element with children.

This is a menu/list container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("menu")))}.
}

@defproc[(Ol [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<ol>} element with children.

This is an ordered list container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("ol")))}.
}

@defproc[(Table [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<table>} element with children.

This is a table container element for tabular data.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("table")))}.
}

@defproc[(Tbody [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<tbody>} element with children.

This groups body rows in a @tt{<table>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("tbody")))}.
}

@defproc[(Td [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<td>} element with children.

This is a data cell element in a table row.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("td")))}.
}

@defproc[(Tfoot [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<tfoot>} element with children.

This groups footer rows in a @tt{<table>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("tfoot")))}.
}

@defproc[(Th [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<th>} element with children.

This is a header cell element in a table row.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("th")))}.
}

@defproc[(Thead [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<thead>} element with children.

This groups header rows in a @tt{<table>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("thead")))}.
}

@defproc[(Tr [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<tr>} element with children.

This is a row element used inside table section containers.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("tr")))}.
}

@defproc[(Ul [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<ul>} element with children.

This is an unordered list container element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("ul")))}.
}

@subsection{Media and Embedded Elements}

@defproc[(Audio [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<audio>} element with children.

This embeds sound content and can include nested @tt{<source>} and @tt{<track>} elements.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("audio")))}.
}

@defproc[(Canvas [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<canvas>} element with children.

This embeds script-driven drawing content and may include fallback child content.

Primitive event callbacks work directly on @racket[Canvas].
For example:

@racketblock[
(Canvas
 #:width 400
 #:height 300
 #:on-mouseup
 (lambda (evt)
   (define x (js-ref/extern evt "offsetX"))
   (define y (js-ref/extern evt "offsetY"))
   (js-log (format "mouse up at (~a, ~a)" x y)))
 (Span "Canvas fallback"))
]

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("canvas")))}.
}

@defproc[(Embed [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<embed>} element.

This is a leaf/void embedded-content element for external resources.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("embed")))}.
}

@defproc[(Iframe [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<iframe>} element with children.

This embeds another page/document and may include fallback child content.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("iframe")))}.
}

@defproc[(Img [#:src src any/c]
              [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<img>} element.

@racket[#:src] is required.
Use this for images and decorative/illustrative media.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("img")))}.
}

@defproc[(Object [#:attrs attrs html-attrs/c null]
                 [child view?] ...)
         view?]{
Build a primitive @tt{<object>} element with children.

This embeds external resources and may include fallback child content.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("object")))}.
}

@defproc[(Source [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<source>} element.

This is a leaf/void media source element used inside media containers.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("source")))}.
}

@defproc[(Track [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<track>} element.

This is a leaf/void timed-text track element used for captions/subtitles.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("track")))}.
}

@defproc[(Video [#:attrs attrs html-attrs/c null]
                [child view?] ...)
         view?]{
Build a primitive @tt{<video>} element with children.

This embeds video content and can include nested @tt{<source>} and @tt{<track>} elements.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("video")))}.
}

@subsection{Metadata and Document Elements}

@defproc[(Base [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<base>} element.

This is a leaf/void base-URL/target element for relative links.
At least one of @racket[#:href] or @racket[#:target] must be provided.
web-easy also enforces a conservative ordering check: among direct @racket[window]
children, @tt{<base>} must appear before URL-bearing primitive elements.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("base")))}.
}

@defproc[(Link [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<link>} element.

This is a leaf/void external resource link element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("link")))}.
}

@defproc[(Meta [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<meta>} element.

This is a leaf/void metadata element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("meta")))}.
}

@defproc[(Script [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<script>} element.

This is a leaf/void script resource element.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("script")))}.
}

@defproc[(Style [content text-content/c]
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<style>} element.

This defines inline style-sheet text.

This primitive remains text-only and does not accept child views.
It accepts plain strings and observables.
If an observable update becomes a non-string and non-@racket[#f] value, the update is ignored.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("style")))}.
}

@defproc[(Title [content text-content/c]
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<title>} element.

This sets the document title text.

This primitive remains text-only and does not accept child views.
It accepts plain strings and observables.
If an observable update becomes a non-string and non-@racket[#f] value, the update is ignored.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@subsection{Specialized Elements}

@defproc[(Area [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<area>} element.

This is a leaf/void clickable region element used inside @tt{<map>}.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("area")))}.
}

@defproc[(Bdi [#:attrs attrs html-attrs/c null]
              [child view?] ...)
         view?]{
Build a primitive @tt{<bdi>} element with children.

This isolates bidirectional text runs from surrounding text direction.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Bdo [#:attrs attrs html-attrs/c null]
              [child view?] ...)
         view?]{
Build a primitive @tt{<bdo>} element with children.

This overrides bidirectional text direction for its content.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Map [#:attrs attrs html-attrs/c null]
              [child view?] ...)
         view?]{
Build a primitive @tt{<map>} element with children.

This defines an image map container for clickable regions.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("map")))}.
}

@defproc[(Rp [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<rp>} element with children.

This provides fallback punctuation for ruby annotations.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Rt [#:attrs attrs html-attrs/c null]
             [child view?] ...)
         view?]{
Build a primitive @tt{<rt>} element with children.

This marks ruby annotation text associated with base text.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Ruby [#:attrs attrs html-attrs/c null]
               [child view?] ...)
         view?]{
Build a primitive @tt{<ruby>} element with children.

This marks ruby annotation text runs.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@defproc[(Slot [#:attrs attrs html-attrs/c null]
               [child view?] ...)
         view?]{
Build a primitive @tt{<slot>} element with children.

This defines a Web Components slot insertion point and may include fallback content.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("slot")))}.
}

@defproc[(Wbr [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<wbr>} element.

This inserts an optional line-break opportunity. It is a leaf/void element and takes no content argument.

Element-specific keyword attributes: none beyond global attributes
(plus @tt{data-*}/@tt{aria-*}).
}

@subsection{Void and Utility Elements}

@defproc[(Br [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<br>} element.

This inserts a line break. It is a leaf/void element and takes no content argument.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("br")))}.
}

@defproc[(Hr [#:attrs attrs html-attrs/c null])
         view?]{
Build a primitive @tt{<hr>} element.

This inserts a thematic break (horizontal rule). It is a leaf/void element and takes no content argument.

Element-specific keyword attributes: @tt{@(attrs->keyword-string (attrs-for-tags '("hr")))}.
}

@section{Library Components}

The lowercase constructors below are higher-level convenience components built on top of the
primitive layer. They may add classes, widget conventions, event bridging, and more opinionated
calling forms.

@subsection{Layout Components}

@defproc[(window [child view?] ...) any/c]{
Build a root window view.
                                           
Positional arguments: each @racket[child] is a view shown in the window.
}

@defproc[(vpanel [child view?] ...) any/c]{
Build a vertical container.
Positional arguments: each @racket[child] is stacked top-to-bottom.
}

@defproc[(hpanel [child view?] ...) any/c]{
Build a horizontal container.

Positional arguments: each @racket[child] is laid out left-to-right.
}

@defproc[(container [#:id    id    (or/c #f string? symbol?) #f]
                    [#:class class any/c                     #f]
                    [#:attrs attrs list?                     null]
                    [child view?] ...)
         view?]{
Build a centered container with width constraints.

Positional arguments: each @racket[child] is content inside the container.
}

@defproc[(grid [columns any/c]
               [#:id    id    (or/c #f string? symbol?) #f]
               [#:class class any/c                     #f]
               [#:attrs attrs list?                     null]
               [child view?] ...)
         view?]{
Build a grid container. The first child may be a gap value (number or CSS length string).

Positional arguments: @racket[columns] sets the column template/count; @racket[child] values are grid items.
}

@defproc[(stack [#:id    id    (or/c #f string? symbol?) #f]
                [#:class class any/c                     #f]
                [#:attrs attrs list?                     null]
                [child view?] ...)
         view?]{
Build a vertical stack container.

Positional arguments: each @racket[child] is a stacked item.
}

@defproc[(inline [#:id    id    (or/c #f string? symbol?) #f]
                 [#:class class any/c                     #f]
                 [#:attrs attrs list?                     null]
                 [child view?] ...)
         view?]{
Build a horizontal inline container.

Positional arguments: each @racket[child] is an inline item.
}

@defproc[(Fragment [child view?] ...)
         view?]{
Build a zero-wrapper composition view.

This groups child views without introducing an extra DOM node.
It is useful when composing compound components while preserving parent/child DOM shape.
}

@defproc[(spacer [grow (or/c number? string?) 1]
                 [#:id    id    (or/c #f string? symbol?) #f]
                 [#:class class any/c                     #f]
                 [#:attrs attrs list?                     null])
         view?]{
Build a flexible spacer for layout.

Positional arguments: @racket[grow] is the growth factor/size basis.
}

@defproc[(divider [orientation symbol? 'horizontal]
                  [#:id    id    (or/c #f string? symbol?) #f]
                  [#:class class any/c                     #f]
                  [#:attrs attrs list?                     null])
         view?]{
Build a horizontal or vertical divider.

Positional arguments: @racket[orientation] is @racket['horizontal] or @racket['vertical].
}

@defproc[(group [label any/c]
                [#:id    id    (or/c #f string? symbol?) #f]
                [#:class class any/c                     #f]
                [#:attrs attrs list?                     null]
                [child view?] ...)
         view?]{
Build a labeled container (fieldset-like grouping).

Positional arguments:
@racket[label] is the legend/title;
each @racket[child] is group content.
}

@subsection{Text and Content Components}

@defproc[(text [value text-content/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         view?]{
Build a text node view.

Positional arguments: @racket[value] is the displayed text value.
}

@defproc[(heading [level (integer-in 1 6)]
                  [content text-content/c]
                  [align symbol? 'left]
                  [spacing symbol? 'normal]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         view?]{
Build a semantic heading.

Positional arguments:
@racket[level] selects heading level 1..6,
@racket[content] is text/view content,
@racket[align] is heading alignment, and
@racket[spacing] controls heading spacing style.
}

@defproc[(h1 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-1 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h2 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-2 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h3 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-3 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h4 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-4 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h5 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-5 heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(h6 [content text-content/c]
             [#:id id (or/c #f string? symbol?) #f]
             [#:class class any/c #f]
             [#:attrs attrs list? null])
         view?]{
Build a level-6 heading.

Positional arguments: @racket[content] is heading text/content.
}


@defproc[(display-heading [level (integer-in 1 6)]
                          [content text-content/c]
                          [#:id id (or/c #f string? symbol?) #f]
                          [#:class class any/c #f]
                          [#:attrs attrs list? null])
         view?]{
Build a display-style heading.

Positional arguments:
@racket[level] selects display level 1..6 and
@racket[content] is heading text/content.
}

@defproc[(display-1 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-1 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-2 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-2 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-3 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-3 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-4 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-4 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-5 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-5 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(display-6 [content text-content/c]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs list? null])
         view?]{
Build a level-6 display heading.

Positional arguments: @racket[content] is heading text/content.
}

@defproc[(heading-with-subtitle [level (integer-in 1 6)]
                                [title text-content/c]
                                [subtitle text-content/c]
                                [#:id id (or/c #f string? symbol?) #f]
                                [#:class class any/c #f]
                                [#:attrs attrs list? null])
         view?]{
Build a heading with subtitle text.

Positional arguments:
@racket[level] selects heading level;
@racket[title] is main text;
@racket[subtitle] is secondary text.
}

@defproc[(display-heading-with-subtitle [level (integer-in 1 6)]
                                        [title text-content/c]
                                        [subtitle text-content/c]
                                        [#:id id (or/c #f string? symbol?) #f]
                                        [#:class class any/c #f]
                                        [#:attrs attrs list? null])
         view?]{
Build a display heading with subtitle text.

Positional arguments:
@racket[level] selects display level;
@racket[title] is main text;
@racket[subtitle] is secondary text.
}

@defproc[(lead [content text-content/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         view?]{
Build lead paragraph text.

Positional arguments: @racket[content] is lead text/content.
}

@defproc[(blockquote [content text-content/c]
                     [attribution (or/c #f text-content/c) #f]
                     [#:align align symbol? 'left]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs html-attrs/c null])
         view?]{
Build a blockquote with optional attribution and alignment.

Positional arguments:
@racket[content] is quote text/content;
@racket[attribution] is optional attribution text;
@racket[#:align] is quote alignment.
This constructor also accepts global HTML keyword attributes for its root
@tt{<figure>} element.
}

@defproc[(image [src any/c]
                [width any/c #f]
                [height any/c #f]
                [#:width width-kw any/c #f]
                [#:height height-kw any/c #f]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         view?]{
Build an image view.

Positional arguments:
@racket[src] is the image URL/source and @racket[width]/@racket[height]
are optional dimensions. Keyword @racket[#:width] and @racket[#:height]
override the optional positional width/height when provided.
This constructor also accepts global and @tt{<img>}-specific HTML keyword
attributes for the root element.
}

@defproc[(link [label any/c]
               [href any/c]
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs list? null])
         view?]{
Build a hyperlink view.

Positional arguments:
@racket[label] is link text/content and
@racket[href] is the target URL.
This constructor also accepts global HTML keyword attributes for its root
@tt{<a>} element.
}

@subsection{Action and Input Components}

@defproc[(button [label content/c]
                 [action procedure?]
                 [leading-icon (or/c #f content/c) #f]
                 [trailing-icon (or/c #f content/c) #f]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a clickable button.

Positional arguments:
@racket[label] is the button caption and
@racket[action] is called on click;
@racket[leading-icon] and @racket[trailing-icon] are optional icon/content slots.
The label and optional icon arguments may be plain text or child views,
so this component can nest richer content inside the rendered @tt{<button>}.
It is a higher-level convenience wrapper over the primitive @racket[Button].
This constructor also accepts global HTML keyword attributes for its root
@tt{<button>} element.
}

@defproc[(close-button [action procedure?]
                       [aria-label text-content/c "Close"]
                       [#:id id (or/c #f string? symbol?) #f]
                       [#:class class any/c #f]
                       [#:attrs attrs html-attrs/c null])
         view?]{
Build a standardized close button.

Positional arguments:
@racket[action] is called when pressed and
@racket[aria-label] sets accessible label text.
This constructor also accepts global HTML keyword attributes for its root
@tt{<button>} element.
}

@defproc[(button-group [child view?] ...
                       [#:id id (or/c #f string? symbol?) #f]
                       [#:class class any/c #f]
                       [#:attrs attrs html-attrs/c null])
         view?]{
Build a grouped button container.

Positional arguments:
each @racket[child] is typically a button view in the group.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(toggle-button-group [items list?]
                              [selected any/c]
                              [on-change procedure?]
                              [#:multiple? multiple? any/c #f]
                              [#:id id (or/c #f string? symbol?) #f]
                              [#:class class any/c #f]
                              [#:attrs attrs list? null])
         view?]{
Build a toggle button group (single or multiple select).

Positional arguments:
@racket[items] is the toggle option list;
@racket[selected] is current selection;
@racket[on-change] receives new selection values.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(button-toolbar [group view?] ...
                         [#:id id (or/c #f string? symbol?) #f]
                         [#:class class any/c #f]
                         [#:attrs attrs html-attrs/c null])
         view?]{
Build a toolbar composed of button groups.

Positional arguments:
each @racket[group] is a toolbar group or button-group view.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(toolbar [child view?] ...
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs html-attrs/c null])
         view?]{
Build a generic horizontal toolbar.

Positional arguments:
each @racket[child] is a toolbar item view.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(toolbar-group [child view?] ...
                        [#:id id (or/c #f string? symbol?) #f]
                        [#:class class any/c #f]
                        [#:attrs attrs html-attrs/c null])
         view?]{
Build a grouped toolbar section.

Positional arguments: each @racket[child] is an item in one toolbar group.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(input [value any/c]
                [on-change procedure?]
                [#:on-enter on-enter (or/c #f procedure?) #f]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         view?]{
Build a text input.

Positional arguments: @racket[value] is current input value and @racket[on-change] receives updates.
This constructor also accepts global and @tt{<input>}-specific HTML keyword
attributes for the root element.
}

@defproc[(textarea [value any/c]
                   [on-change procedure?]
                   [#:rows rows (or/c #f exact-positive-integer?) #f]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         view?]{
Build a textarea input.

Positional arguments: @racket[value] is current text and @racket[on-change] receives updates.
This constructor also accepts global and @tt{<textarea>}-specific HTML keyword
attributes for the root element.
}

@defproc[(checkbox [checked any/c]
                   [on-toggle procedure?]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         view?]{
Build a checkbox input.

Positional arguments: @racket[checked] is current checked state and @racket[on-toggle] receives updates.
This constructor also accepts global and @tt{<label>}-specific HTML keyword
attributes for the root element.
}

@defproc[(radios [options list?]
                 [selected any/c]
                 [on-select procedure?]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         view?]{
Build a radio-choice input.

Positional arguments:
@racket[options] is radio option data;
@racket[selected] is current value;
@racket[on-select] receives selected value.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(choice [options list?]
                 [selected any/c]
                 [on-select procedure?]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         view?]{
Build a single-select dropdown.

Positional arguments:
@racket[options] is selectable options;
@racket[selected] is current value;
@racket[on-select] receives selected value.
This constructor also accepts global HTML keyword attributes for its root
@tt{<label>} element.
}

@defproc[(slider [value any/c]
                 [on-change procedure?]
                 [min number? 0]
                 [max number? 100]
                 [#:step step (or/c #f number?) #f]
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs list? null])
         view?]{
Build a range slider.

Positional arguments:
@racket[value] is current numeric value;
@racket[on-change] receives updates;
@racket[min] and @racket[max] set range bounds.
This constructor also accepts global HTML keyword attributes for its root
@tt{<label>} element.
}

@defproc[(pagination [items list?]
                     [current any/c]
                     [on-select procedure?]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         view?]{
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
         view?]{
Build breadcrumb navigation.

Positional arguments:
@racket[items] is breadcrumb path data;
@racket[current] is current crumb value;
@racket[on-select] handles navigation clicks.
}

@subsection{Feedback Components}

@defproc[(alert [value any/c]
                [level any/c 'info]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs list? null])
         view?]{
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
         view?]{
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
         view?]{
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
         view?]{
Build a badge label.

Positional arguments:
@racket[value] is badge content and @racket[level] sets tone/style.
}

@defproc[(spinner [label any/c "Loading..."]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs list? null])
         view?]{
Build a spinner indicator.
                
Positional arguments:
@racket[label] is optional accessible/loading text.
}

@defproc[(progress [value any/c]
                   [max-value any/c 100]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         view?]{
Build a progress indicator.

Positional arguments:
@racket[value] is current progress and @racket[max-value] is the full-scale maximum.
}

@defproc[(placeholder [shape any/c 'text]
                      [width any/c #f]
                      [#:id id (or/c #f string? symbol?) #f]
                      [#:class class any/c #f]
                      [#:attrs attrs list? null])
         view?]{
Build a placeholder/skeleton block.

Positional arguments:
@racket[shape] selects placeholder style and @racket[width] optionally sets width.
}

@subsection{Navigation and Composite Components}

@defproc[(list-group [items list?]
                     [current any/c]
                     [on-select procedure?]
                     [#:id id (or/c #f string? symbol?) #f]
                     [#:class class any/c #f]
                     [#:attrs attrs list? null])
         view?]{
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
         view?]{
Build a tab panel.

Positional arguments:
@racket[tabs] is tab metadata;
@racket[current] is selected tab key;
@racket[render-panel] renders content for selected tab.
}

@defproc[(dropdown [label any/c]
                   [entries list?]
                   [action procedure?]
                   [placement symbol? 'down]
                   [#:placement placement-kw (or/c #f symbol?) #f]
                   [#:id id (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs list? null])
         view?]{
Build a dropdown component.

Positional arguments:
@racket[label] is dropdown trigger label;
@racket[entries] is menu item data;
@racket[action] receives the selected item key;
@racket[placement] is optional placement.
Keyword @racket[#:placement] overrides positional placement when provided.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(menu-bar [menu view?] ...) view?]{
Build a horizontal menu bar.

Positional arguments: each @racket[menu] is a @racket[menu] view.
This constructor also accepts global HTML keyword attributes for its root
@tt{<menu-bar>} element.
}

@defproc[(menu [label text-content/c] [item view?] ...) view?]{
Build a menu in a menu bar.

Positional arguments:
@racket[label] is menu title and each @racket[item] is usually a @racket[menu-item] or divider.
This constructor also accepts global HTML keyword attributes for its root
@tt{<menu>} element.
}

@defproc[(menu-popup [item view?] ...
                     [#:left left (or/c #f real? string?) #f]
                     [#:top top (or/c #f real? string?) #f]
                     [#:position position (or/c #f symbol? string?) #f]
                     [#:z-index z-index (or/c #f real? string?) #f])
         view?]{
Build a standalone popup menu container.

Positional arguments:
each @racket[item] is usually a @racket[menu-item] or divider.
This constructor also accepts global HTML keyword attributes for its root
@tt{<vpanel>} element.

Optional positioning keywords:
@racket[#:left] and @racket[#:top] place the popup explicitly;
numeric values are rendered in pixels.
@racket[#:position] overrides the CSS positioning mode and defaults to
@racket['absolute] when any explicit positioning keyword is used.
@racket[#:z-index] sets popup stacking order.

When @racket[#:position] is @racket['absolute] (the default for explicitly
positioned popups), @racket[#:left] and @racket[#:top] are interpreted in the
same way as CSS absolute positioning: they are relative to the popup's
containing block, which is usually the nearest positioned ancestor. In
practice, this means standalone context menus are often rendered inside a
wrapper with @tt{position: relative}, and the popup offsets are measured from
that wrapper's top-left corner.

Example:

@racketblock[
(Div #:style "position: relative;"
     (Canvas #:id "circle-canvas")
     (menu-popup
      #:left 120
      #:top 80
      #:z-index 1000
      (menu-item "Delete" (lambda () (void)))))
]

The browser backend gives standalone popup menus menu-style keyboard behavior:

@tt{ArrowDown} and @tt{Home} enter the popup at the first item.
@tt{ArrowUp} and @tt{End} enter the popup at the last item.
Typing a letter performs type-ahead focus.
Closing the popup returns focus to the element that was focused before the popup opened.
}

@defproc[(menu-item [label text-content/c]
                    [action procedure?]
                    [leading-icon (or/c #f content/c) #f]
                    [trailing-icon (or/c #f content/c) #f]
                    [#:disabled disabled boolean? #f])
         view?]{
Build a menu item.

Positional arguments:
@racket[label] is item text;
@racket[action] is called on activation;
@racket[leading-icon] and @racket[trailing-icon] are optional icon/content slots.
When @racket[#:disabled] is true, the item is rendered with disabled styling,
is skipped by popup keyboard navigation and type-ahead, and ignores activation.
This constructor also accepts global HTML keyword attributes for its root
@tt{<menu-item>} element.
}

@defproc[(navigation-bar [item view?] ...
                         [#:id id (or/c #f string? symbol?) #f]
                         [#:class class any/c #f]
                         [#:attrs attrs html-attrs/c null])
         view?]{
Build a navigation bar.

Positional arguments: each @racket[item] is navbar content (brand, links, controls, menus, etc.).
}

@defproc[(top-bar [item view?] ...
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs html-attrs/c null])
         view?]{
Build a top bar container.

Positional arguments: each @racket[item] is top-bar content.
}

@subsection{Data and Structure Components}

@defproc[(table [columns list?]
                [rows list?]
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a table view.

Positional arguments: @racket[columns] defines table columns and @racket[rows] is row data.
}

@defproc[(list-view [items (or/c list? observable?)]
                    [render-item procedure?]
                    [key-of procedure?])
         view?]{
Build a keyed dynamic list view.

Positional arguments:
@racket[items] is the source collection/observable;
@racket[render-item] builds per-item views;
@racket[key-of] extracts stable item keys.
}

@defproc[(card [child view?] ...
               [#:id id (or/c #f string? symbol?) #f]
               [#:class class any/c #f]
               [#:attrs attrs html-attrs/c null])
         view?]{
Build a card container.

Positional arguments: each @racket[child] is card content.
}

@defproc[(accordion [sections list?]
                    [current any/c]
                    [on-select procedure?]
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs html-attrs/c null])
         view?]{
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
                   [#:attrs attrs html-attrs/c null])
         view?]{
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
                    [#:attrs attrs html-attrs/c null])
         view?]{
Build a scrollspy navigation component.

Positional arguments:
@racket[items] is tracked section data;
@racket[current] is active section key;
@racket[on-select] receives selected section.
}

@subsection{Overlays}

@defproc[(dialog [open (or/c boolean? observable?)]
                 [on-close procedure?]
                 [#:size size (or/c #f symbol?) #f]
                 [#:title title text-content/c #f]
                 [#:description description text-content/c #f]
                 [#:footer footer (or/c #f content/c) #f]
                 [#:show-close? show-close? any/c #f]
                 [#:close-label close-label text-content/c "Close dialog"]
                 [#:tone tone any/c #f]
                 [#:tone-style tone-style any/c #f]
                 [child view?] ...
                 [#:id id (or/c #f string? symbol?) #f]
                 [#:class class any/c #f]
                 [#:attrs attrs html-attrs/c null])
         view?]{
Build a dialog.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is dialog content.
This constructor also accepts global HTML keyword attributes for its root
@tt{<dialog>} element.
}

@defproc[(modal [open (or/c boolean? observable?)]
                [on-close procedure?]
                [#:size size (or/c #f symbol?) #f]
                [#:title title text-content/c #f]
                [#:description description text-content/c #f]
                [#:footer footer (or/c #f content/c) #f]
                [#:show-close? show-close? any/c #f]
                [#:close-label close-label text-content/c "Close modal"]
                [#:tone tone any/c #f]
                [#:tone-style tone-style any/c #f]
                [child view?] ...
                [#:id id (or/c #f string? symbol?) #f]
                [#:class class any/c #f]
                [#:attrs attrs html-attrs/c null])
         view?]{
Build a modal overlay.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is modal content.
This constructor also accepts global HTML keyword attributes for its root
@tt{<dialog>} element.
}

@defproc[(offcanvas [open (or/c boolean? observable?)]
                    [on-close procedure?]
                    [#:side side (or/c #f symbol?) #f]
                    [child view?] ...
                    [#:id id (or/c #f string? symbol?) #f]
                    [#:class class any/c #f]
                    [#:attrs attrs html-attrs/c null])
         view?]{
Build an offcanvas panel.

Positional arguments:
@racket[open] controls visibility;
@racket[on-close] handles close requests;
each @racket[child] is panel content.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(tooltip [message text-content/c]
                  [child view?]
                  [#:placement placement (or/c #f symbol?) #f]
                  [#:title title (or/c #f text-content/c) #f]
                  [#:footer footer (or/c #f text-content/c) #f]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs html-attrs/c null])
         view?]{
Build a tooltip wrapper.

Positional arguments:
@racket[message] is tooltip text/content and @racket[child] is the trigger/anchor view.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@defproc[(popover [label text-content/c]
                  [child view?] ...
                  [#:placement placement (or/c #f symbol?) #f]
                  [#:title title (or/c #f text-content/c) #f]
                  [#:footer footer (or/c #f text-content/c) #f]
                  [#:id id (or/c #f string? symbol?) #f]
                  [#:class class any/c #f]
                  [#:attrs attrs html-attrs/c null])
         view?]{
Build a popover wrapper.

Positional arguments:
@racket[label] is trigger label/content and each @racket[child] is popover body content.
This constructor also accepts global HTML keyword attributes for its root
@tt{<div>} element.
}

@subsection{Reactive and Branching Components}

@defproc[(observable-view [obs any/c] [render procedure?]) view?]{
Build a dynamic view from an observable value.

Positional arguments:
@racket[obs] is the observable source and @racket[render] maps each observed value to a view.
}

@defproc[(if-view [test any/c] [then-view view?] [else-view view?]) view?]{
Build a two-way conditional view.

Positional arguments: @racket[test] is condition value/observable;
@racket[then-view] is true branch;
@racket[else-view] is false branch.
}

@defproc[(cond-view [clause pair?] ...) view?]{
Build a multi-branch conditional view.

Positional arguments: each @racket[clause] is a condition/view branch pair.
}

@defproc[(case-view [target any/c] [clause pair?] ...) view?]{
Build an equality-based branch view.

Positional arguments:
@racket[target] is matched value/observable;
each @racket[clause] maps match keys to a branch view.
}

@defproc[(collapse [open (or/c boolean? observable?)]
                   [child view?]
                   [#:id    id    (or/c #f string? symbol?) #f]
                   [#:class class any/c #f]
                   [#:attrs attrs html-attrs/c null])
         view?]{
Build a collapsible region.

Positional arguments:
@racket[open] controls expanded state and @racket[child] is collapsible content.
}
