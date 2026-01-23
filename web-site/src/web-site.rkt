;;;
;;; WebRacket Website
;;;

;;;
;;; Build Instructions
;;;

;; Compile with:
;;     racket ../../webracket.rkt --browser --ffi dom --stdlib web-site.rkt


;;;
;;; Color Helpers
;;;

;; byte->hex : Integer -> String
;;   Converts an 8-bit number into a two-character uppercase hex string.
(define (byte->hex n)
  (define s      (number->string n 16))
  (define padded (if (= (string-length s) 1) (string-append "0" s) s))
  (string-upcase padded))

;; make-color* : Integer Integer Integer -> String
;;   Builds a CSS hex color string from RGB byte components.
(define (make-color* r g b)
  (string-append "#" (byte->hex r) (byte->hex g) (byte->hex b)))

;;;
;;; Color Theme
;;;

(define purple (make-color* 101 79 240))  ; #654FF0
(define blue   (make-color* 74 108 255))  ; #4A6CFF
(define red    (make-color* 209 58 58))   ; #D13A3A
(define gold   (make-color* 242 183 5))   ; #F2B705

;;;
;;; SXML Helpers
;;;

;; make-ul-list : (Listof String) (U #f String) -> List
;;   Builds a <ul> sxml node from a list of text items and an optional class.
(define (make-ul-list items [class-name #f])
  (define list-items (map (λ (item) `(li ,item)) items))
  (if class-name
      `(ul (@ (class ,class-name)) ,@list-items)
      `(ul ,@list-items)))

;; section-block : String (U #f String) (Listof List) (U #f String) -> List
;;   Creates a section container with a title, optional subtitle, and content.
(define (section-block title subtitle content [section-id #f])
  (define attrs (if section-id
                    `(@ (class "section") (id ,section-id))
                    `(@ (class "section"))))
  `(section ,attrs
            (div (@ (class "section-header"))
                 (h2 (@ (class "section-title")) ,title)
                 ,@(if subtitle
                       (list `(p (@ (class "section-lead")) ,subtitle))
                       '()))
            ,@content))

;; card-grid : (Listof (Listof List)) -> List
;;   Wraps card content lists into a grid container.
(define (card-grid cards)
  `(div (@ (class "card-grid"))
        ,@(map (λ (card) `(div (@ (class "card")) ,@card)) cards)))

;;;
;;; Page Layout
;;;

;; init-dom : -> Void
;; Builds and attaches the page DOM plus its CSS styles.
(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (define style
    (sxml->dom
     `(style
       ,(format
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
  padding: 32px 0 80px;
  display: flex;
  flex-direction: column;
  gap: 56px;
}
.navbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 24px;
}
.nav-left {
  display: flex;
  align-items: center;
  gap: 12px;
  font-weight: 600;
  font-size: 1.05rem;
}
.nav-logo {
  width: 34px;
  height: 34px;
}
.nav-links {
  display: flex;
  flex-wrap: wrap;
  gap: 16px;
  font-size: 0.95rem;
}
.nav-link {
  color: var(--text);
  opacity: 0.8;
}
.nav-link:hover {
  opacity: 1;
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
.hero-carousel {
  display: flex;
  flex-direction: column;
  gap: 12px;
}
.carousel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 0.95rem;
  color: var(--muted);
}
.carousel-frame {
  position: relative;
  border-radius: 22px;
  overflow: hidden;
  border: 1px solid rgba(255, 255, 255, 0.1);
  box-shadow: 0 22px 40px rgba(0, 0, 0, 0.35);
  aspect-ratio: 4 / 3;
  background: var(--surface);
}
.carousel-shot {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  object-fit: cover;
  opacity: 0;
  transform: scale(1.02);
  animation: carouselFade 30s infinite;
}
@keyframes carouselFade {
  0% { opacity: 0; transform: scale(1.02); }
  6% { opacity: 1; transform: scale(1); }
  18% { opacity: 1; transform: scale(1); }
  26% { opacity: 0; transform: scale(1.02); }
  100% { opacity: 0; transform: scale(1.02); }
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
.accent    { color: var(--blue); }
.warning   { color: var(--red); }
CSS
         purple blue red gold))))
  
  (js-append-child! head style)

  (define page-structure
    `(div (@ (class "page"))
          (nav (@ (class "navbar"))
               (div (@ (class "nav-left"))
                    (img (@ (class "nav-logo")
                            (src "assets/hex-racket-wasm-logo.svg")
                            (alt "WebRacket logo")))
                    (span "WebRacket"))
               (div (@ (class "nav-links"))
                    (a (@ (class "nav-link") (href "is-webracket-for-you.html")) "For You")
                    (a (@ (class "nav-link") (href "overview.html")) "Overview")
                    (a (@ (class "nav-link") (href "roadmap.html")) "Road Ahead")
                    (a (@ (class "nav-link") (href "installation.html")) "Installation")
                    (a (@ (class "nav-link") (href "examples.html")) "Examples")))
          (section (@ (class "hero"))
                   (div (@ (class "hero-panel"))
                        (h1 (@ (class "hero-title")) "WebRacket")
                        (p  (@ (class "hero-lead"))
                "A Racket to WebAssembly compiler. Build practical browser applications with Racket.")
                        (div (@ (class "pill-row"))
                             (span (@ (class "pill")) "JS + DOM FFI")
                             (span (@ (class "pill")) "Runs in browsers + Node"))))
                   (div (@ (class "hero-carousel"))
                        (div (@ (class "carousel-header"))
                             (span "Example screenshots")
                             (span (@ (class "highlight")) "Live demos"))
                        (div (@ (class "carousel-frame"))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/mathjax4.png")
                                     (alt   "MathJax 4 editor")
                                     (style "animation-delay: 0s;")))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/matrix-rain.png")
                                     (alt   "Matrix digital rain demo")
                                     (style "animation-delay: 5s;")))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/xtermjs.png")
                                     (alt   "XtermJS terminal demo")
                                     (style "animation-delay: 10s;")))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/minischeme.png")
                                     (alt   "MiniScheme browser REPL")
                                     (style "animation-delay: 15s;")))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/space-invaders.png")
                                     (alt   "Space Invaders canvas game")
                                     (style "animation-delay: 20s;")))
                             (img (@ (class "carousel-shot")
                                     (src   "assets/examples/screenshots/pict.png")
                                     (alt   "Pict rendering demo")
                                     (style "animation-delay: 25s;")))))
          ,(section-block
            "Why WebRacket?"
            "WebRacket is a subset of Racket that compiles to WebAssembly, so you can target modern browsers while staying in a familiar language."
            (list
             (make-ul-list
              (list
               "Compile Racket programs into WebAssembly that runs in Chrome, Firefox, and Safari."
               "Leverage a JavaScript FFI for DOM, Canvas, MathJax, XtermJS, and JSXGraph integrations."
               "Use WebRacket to prototype ideas that could influence a future Racket WebAssembly backend."))))
          ,(section-block
            "Language Coverage"
            "WebRacket implements a substantial portion of Racket with a focus on practical web applications."
            (list
             (card-grid
              (list
               (list `(h3 "Numbers")
                     `(p "Flonums and fixnums."))
               (list `(h3 "Hash Tables")
                     `(p "Mutable hash tables for "
                         (strong (code "eq?"))
                         ", "
                         (strong (code "eqv?"))
                         ", "
                         (strong (code "equal?"))
                         ", and "
                         (strong (code "always?"))
                         " comparisons."))
               (list `(h3 "Structures")
                     `(p "Support for structure properties, super structures, and applicable structs."))
               (list `(h3 "Syntax")
                     `(p "Standard Racket expander support, including for and match forms."))
               (list `(h3 "Control Flow")
                     `(p "Tail calls, multiple values, and upward exceptions."))
               (list `(h3 "Builtins")
                     `(p "Large parts of racket/base is available."))
               (list `(h3 "Concurrency")
                     `(p "Single-threaded execution only."))
               (list `(h3 "Modules")
                     `(p "Work in progress. Use include for now."))))))
          ,(section-block
            "Toolchain Essentials"
            "A small set of tools power the WebRacket workflow from source to the browser."
            (list
             (make-ul-list
              (list
               (string-append "wasm-tools builds and validates the WebAssembly output. ")
               (string-append "Node.js enables running generated programs in the terminal. ")
               (string-append "raco-static-web makes it easy to serve compiled artifacts locally."))
              "steps")))
          ,(section-block
            "Compiler Pipeline"
            "WebRacket uses a direct-style compiler with NanoPass transformations before emitting WebAssembly."
            (list
             (card-grid
              (list
               (list `(h3 "Frontend")
                     `(p "Racket syntax is expanded, then normalized into a compiler-friendly core."))
               (list `(h3 "Middle End")
                     `(p "Nanopass phases like closure conversion and ANF shape the program."))
               (list `(h3 "Backend")
                     `(p "Destination-driven code generation emits folded WebAssembly code."))
               (list `(h3 "Runtime")
                     `(p "A custom runtime avoids reliance on host functionality where possible."))))))
          ,(section-block
            "Roadmap"
            "The long-term goal is full Racket support, but there is plenty more to tackle next."
            (list
             (make-ul-list
              (list
               "Fix bugs reported by early adopters and stabilize the current runtime."
               "Unlock modules by completing linklet support."
               "Add complex numbers, bignums, impersonators, and chaperones."
               "Improve regular expression support and consider CPS for continuations."))))
          ,(section-block
            "Example Projects"
            "WebRacket already powers interactive demos that showcase browser APIs."
            (list
             (make-ul-list
              (list
               "MathJax 4 editor with live formula preview."
               "Matrix digital rain with XtermJS terminal rendering."
               "MiniScheme interactive REPL in the browser."
               "Space Invaders canvas game and Pict rendering demo."))))
          (footer (@ (class "footer"))
                  (span "WebRacket — Racket for the browser.")
                  (span "Made for the Racket community."))))
  (define page (sxml->dom page-structure))
  (js-append-child! body page))

;;;
;;; Entry Point
;;;

(init-dom)
