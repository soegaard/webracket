;;
;; WebRacket Website
;;

;; byte->hex : Integer -> String
;; Converts an 8-bit number into a two-character uppercase hex string.
(define (byte->hex n)
  (define s (number->string n 16))
  (define padded (if (= (string-length s) 1) (string-append "0" s) s))
  (string-upcase padded))

;; make-color* : Integer Integer Integer -> String
;; Builds a CSS hex color string from RGB byte components.
(define (make-color* r g b)
  (string-append "#" (byte->hex r) (byte->hex g) (byte->hex b)))

(define purple (make-color* 101 79 240))  ; #654FF0
(define blue   (make-color* 74 108 255))  ; #4A6CFF
(define red    (make-color* 209 58 58))   ; #D13A3A
(define gold   (make-color* 242 183 5))   ; #F2B705

;; make-element : String (U #f String) (U #f String) -> Any
;; Creates a DOM element with optional class name and text content.
(define (make-element tag [class-name #f] [text #f])
  (define node (js-create-element tag))
  (when class-name
    (js-set-attribute! node "class" class-name))
  (when text
    (js-set! node "textContent" text))
  node)

;; append-children! : Any (Listof Any) -> Any
;; Appends a list of DOM nodes to a parent node and returns the parent.
(define (append-children! parent children)
  (for-each (λ (child) (js-append-child! parent child)) children)
  parent)

;; make-list : (Listof String) (U #f String) -> Any
;; Builds a <ul> node from a list of text items and an optional class.
(define (make-list items [class-name #f])
  (define list-node (make-element "ul" class-name))
  (for-each
   (λ (item)
     (define li (make-element "li" #f item))
     (js-append-child! list-node li))
   items)
  list-node)

;; section-block : String (U #f String) -> Any
;; Creates a section container with a title and optional subtitle.
(define (section-block title subtitle)
  (define section (make-element "section" "section"))
  (define header (make-element "div" "section-header"))
  (define title-node (make-element "h2" "section-title" title))
  (js-append-child! header title-node)
  (when subtitle
    (js-append-child! header (make-element "p" "section-lead" subtitle)))
  (js-append-child! section header)
  section)

;; card-grid : (Listof (Listof Any)) -> Any
;; Wraps card content lists into a grid container.
(define (card-grid cards)
  (define grid (make-element "div" "card-grid"))
  (for-each
   (λ (card)
     (define item (make-element "div" "card"))
     (append-children! item card)
     (js-append-child! grid item))
   cards)
  grid)

;; init-dom : -> Void
;; Builds and attaches the page DOM plus its CSS styles.
(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (define style (js-create-element "style"))
  (js-set!
   style "textContent"
   (format
    #<<CSS
:root {
  --purple: ~a;
  --blue: ~a;
  --red: ~a;
  --gold: ~a;
  --bg: #0C0D1A;
  --surface: #14162B;
  --surface-soft: rgba(255, 255, 255, 0.03);
  --text: #E6E8F2;
  --muted: #B6BDDD;
}
* { box-sizing: border-box; }
body {
  margin: 0;
  font-family: 'Inter', 'Fira Code', system-ui, sans-serif;
  background: radial-gradient(circle at top, rgba(101, 79, 240, 0.25), transparent 55%), var(--bg);
  color: var(--text);
  min-height: 100vh;
}
a { color: var(--blue); text-decoration: none; }
.page {
  width: min(1200px, 92vw);
  margin: 0 auto;
  padding: 56px 0 80px;
  display: flex;
  flex-direction: column;
  gap: 56px;
}
.hero {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 32px;
  align-items: center;
}
.hero-panel {
  background: linear-gradient(140deg, rgba(101, 79, 240, 0.2), rgba(74, 108, 255, 0.08));
  border: 1px solid rgba(101, 79, 240, 0.3);
  border-radius: 28px;
  padding: 36px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);
}
.hero-title {
  font-size: clamp(2rem, 4vw, 3rem);
  margin: 0 0 12px;
}
.hero-lead {
  margin: 0 0 22px;
  color: var(--muted);
  line-height: 1.6;
}
.pill-row {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
}
.pill {
  border-radius: 999px;
  padding: 6px 14px;
  background: var(--surface-soft);
  border: 1px solid rgba(255, 255, 255, 0.08);
  color: var(--muted);
  font-size: 0.85rem;
}
.logo-card {
  background: var(--surface);
  border-radius: 24px;
  padding: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
  border: 1px solid rgba(255, 255, 255, 0.08);
}
.logo-card img {
  width: min(260px, 60vw);
  height: auto;
}
.section {
  display: flex;
  flex-direction: column;
  gap: 24px;
}
.section-header {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.section-title {
  margin: 0;
  font-size: 1.6rem;
}
.section-lead {
  margin: 0;
  color: var(--muted);
  line-height: 1.6;
}
.card-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 18px;
}
.card {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 20px;
  padding: 20px;
  display: flex;
  flex-direction: column;
  gap: 10px;
}
.card h3 { margin: 0; font-size: 1.05rem; }
.card p { margin: 0; color: var(--muted); line-height: 1.5; }
.steps {
  display: grid;
  gap: 12px;
  background: var(--surface);
  border-radius: 18px;
  padding: 22px;
  border: 1px solid rgba(255, 255, 255, 0.08);
}
.steps li { margin-bottom: 10px; color: var(--muted); }
.steps li:last-child { margin-bottom: 0; }
.footer {
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 12px;
  color: var(--muted);
  border-top: 1px solid rgba(255, 255, 255, 0.08);
  padding-top: 20px;
}
.highlight { color: var(--gold); font-weight: 600; }
.accent { color: var(--blue); }
.warning { color: var(--red); }
CSS
    purple blue red gold))
  (js-append-child! head style)

  (define page (make-element "div" "page"))
  (js-append-child! body page)

  (define hero (make-element "section" "hero"))
  (define hero-panel (make-element "div" "hero-panel"))
  (define hero-pill (make-element "div" "pill" "Racket → WebAssembly"))
  (define hero-title (make-element "h1" "hero-title" "WebRacket"))
  (define hero-lead
    (make-element
     "p"
     "hero-lead"
     "A Racket-to-WebAssembly compiler and runtime that makes it possible to build practical browser experiences without leaving the Racket ecosystem."))
  (define hero-pill-row (make-element "div" "pill-row"))
  (append-children!
   hero-pill-row
   (list (make-element "span" "pill" "Wasm-GC compatible")
         (make-element "span" "pill" "JS + DOM FFI")
         (make-element "span" "pill" "Runs in browsers + Node")))
  (append-children! hero-panel (list hero-pill hero-title hero-lead hero-pill-row))

  (define logo-card (make-element "div" "logo-card"))
  (define logo (js-create-element "img"))
  (js-set-attribute! logo "src" "assets/hex-racket-wasm-logo.svg")
  (js-set-attribute! logo "alt" "WebRacket hex logo")
  (js-append-child! logo-card logo)

  (append-children! hero (list hero-panel logo-card))
  (js-append-child! page hero)

  (define intro (section-block
                 "Why WebRacket?"
                 "WebRacket is a subset of Racket that compiles to WebAssembly, so you can target modern browsers while staying in a familiar language."))
  (define intro-copy
    (make-list
     (list
      "Compile Racket programs into WebAssembly that runs in Chrome, Firefox, and Safari."
      "Leverage a JavaScript FFI for DOM, Canvas, MathJax, XtermJS, and JSXGraph integrations."
      "Use WebRacket to prototype ideas that could influence a future Racket WebAssembly backend.")))
  (js-append-child! intro intro-copy)
  (js-append-child! page intro)

  (define capabilities (section-block
                        "Language Coverage"
                        "WebRacket implements a substantial portion of Racket with a focus on practical web applications."))
  (define capability-cards
    (card-grid
     (list
      (list (make-element "h3" #f "Numbers")
            (make-element "p" #f "Flonums and fixnums with fast WebAssembly execution."))
      (list (make-element "h3" #f "Hash Tables")
            (make-element "p" #f "Mutable hash tables for eq?, eqv?, equal?, and always? comparisons."))
      (list (make-element "h3" #f "Structures")
            (make-element "p" #f "Support for structure properties, super structures, and applicable structs."))
      (list (make-element "h3" #f "Syntax")
            (make-element "p" #f "Standard Racket expander support, including for and match forms."))
      (list (make-element "h3" #f "Control Flow")
            (make-element "p" #f "Tail calls, multiple values, and upward exceptions are all supported."))
      (list (make-element "h3" #f "Concurrency")
            (make-element "p" #f "Single-threaded execution today, with a future path toward richer models."))))))
  (js-append-child! capabilities capability-cards)
  (js-append-child! page capabilities)

  (define tooling (section-block
                  "Toolchain Essentials"
                  "A small set of tools power the WebRacket workflow from source to the browser."))
  (define tooling-list
    (make-list
     (list
      (string-append "wasm-tools builds and validates the WebAssembly output. ")
      (string-append "Node.js enables running generated programs in the terminal. ")
      (string-append "raco-static-web makes it easy to serve compiled artifacts locally."))
     "steps"))
  (js-append-child! tooling tooling-list)
  (js-append-child! page tooling)

  (define pipeline (section-block
                    "Compiler Pipeline"
                    "WebRacket uses a direct-style compiler with NanoPass transformations before emitting WebAssembly."))
  (define pipeline-cards
    (card-grid
     (list
      (list (make-element "h3" #f "Frontend")
            (make-element "p" #f "Racket syntax is expanded, then normalized into a compiler-friendly core."))
      (list (make-element "h3" #f "Middle End")
            (make-element "p" #f "Nanopass phases like closure conversion and ANF shape the program."))
      (list (make-element "h3" #f "Backend")
            (make-element "p" #f "Destination-driven code generation emits folded WebAssembly code."))
      (list (make-element "h3" #f "Runtime")
            (make-element "p" #f "A custom runtime avoids reliance on host functionality where possible.")))))
  (js-append-child! pipeline pipeline-cards)
  (js-append-child! page pipeline)

  (define roadmap (section-block
                  "Roadmap"
                  "The long-term goal is full Racket support, but there is plenty more to tackle next."))
  (define roadmap-list
    (make-list
     (list
      "Fix bugs reported by early adopters and stabilize the current runtime."
      "Unlock modules by completing linklet support."
      "Add complex numbers, bignums, impersonators, and chaperones."
      "Improve regular expression support and consider CPS for continuations.")))
  (js-append-child! roadmap roadmap-list)
  (js-append-child! page roadmap)

  (define examples (section-block
                    "Example Projects"
                    "WebRacket already powers interactive demos that showcase browser APIs."))
  (define examples-list
    (make-list
     (list
      "MathJax 4 editor with live formula preview."
      "Matrix digital rain with XtermJS terminal rendering."
      "MiniScheme interactive REPL in the browser."
      "Space Invaders canvas game and Pict rendering demo.")))
  (js-append-child! examples examples-list)
  (js-append-child! page examples)

  (define footer (make-element "footer" "footer"))
  (define footer-left (make-element "span" #f "WebRacket — Racket for the browser."))
  (define footer-right (make-element "span" #f "Made for the Racket community."))
  (append-children! footer (list footer-left footer-right))
  (js-append-child! page footer))

(init-dom)
