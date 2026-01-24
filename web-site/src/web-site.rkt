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

;; section-block : String (U #f String) (Listof List) (U #f String) (U #f String) -> List
;;   Creates a section container with a title, optional subtitle, and content.
(define (section-block title subtitle content [section-id #f] [section-class #f])
  (define class-name (if section-class
                         (string-append "section " section-class)
                         "section"))
  (define attrs (if section-id
                    `(@ (class ,class-name) (id ,section-id))
                    `(@ (class ,class-name))))
  `(section ,attrs
            (div (@ (class "section-header"))
                 (h2 (@ (class "section-title")) ,title)
                 ,@(if subtitle
                       (list `(p (@ (class "section-lead")) ,subtitle))
                       '()))
            ,@content))

;; card-grid : (Listof (Listof List)) (U #f String) -> List
;;   Wraps card content lists into a grid container.
(define (card-grid cards [class-name #f])
  (define base-class (if class-name
                         (string-append "card-grid " class-name)
                         "card-grid"))
  `(div (@ (class ,base-class))
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
  --blue:   ~a;
  --red:    ~a;
  --gold:   ~a;
  --bg:           #0C0D1A;
  --surface:      #14162B;
  --surface-soft: rgba(255, 255, 255, 0.03);
  --text:         #E6E8F2;
  --muted:        #B6BDDD;
}
* { box-sizing: border-box; }
body {
  margin: 0;
  font-family: 'Inter', 'Fira Code', system-ui, sans-serif;
  background: radial-gradient(circle at top, rgba(101, 79, 240, 0.25), transparent 55%), var(--bg);
  color: var(--text);
  line-height: 1.6;
  min-height: 100vh;
}
a { color: var(--blue); text-decoration: none; }
.page {
  width: min(1200px, 92vw);
  margin: 0 auto;
  padding: 32px 0 80px;
  display: flex;
  flex-direction: column;
  gap: 0;
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
  grid-template-columns: 2fr 1fr;
  gap: 32px;
  align-items: center;
  margin-top: 32px;
}
.hero-panel {
  background: linear-gradient(140deg, rgba(101, 79, 240, 0.2), rgba(74, 108, 255, 0.08));
  border: 1px solid rgba(101, 79, 240, 0.3);
  border-radius: 28px;
  padding: 36px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);
  display: flex;
  flex-direction: column;
  gap: 18px;
}
.hero-copy {
  flex: none;
}
.hero-title {
  font-size: clamp(2rem, 4vw, 3rem);
  margin: 0 0 12px;
}
.hero-lead {
  margin: 0 0 18px;
  color: var(--muted);
  line-height: 1.6;
}
.hero-carousel {
  display: flex;
  flex-direction: column;
  gap: 12px;
}
.hero-carousel-panel {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 24px;
  padding: 20px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);
  display: flex;
  flex-direction: column;
}
.carousel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 0.8rem;
  color: var(--muted);
}
.carousel-frame {
  position: relative;
  border-radius: 16px;
  overflow: hidden;
  box-shadow: 0 12px 26px rgba(0, 0, 0, 0.35);
  aspect-ratio: 4 / 3;
  background: var(--surface);
}
.carousel-shot {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  object-fit: contain;
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
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 0;
  margin: 72px 0 0;
  padding: 28px 24px;
  border-radius: 28px;
  overflow: hidden;
}
.section::before {
  content: "";
  position: absolute;
  inset: 0;
  border-radius: inherit;
  background: var(--section-band, radial-gradient(120% 120% at 10% 0%, rgba(101, 79, 240, 0.16), rgba(12, 13, 26, 0.15) 48%, transparent 72%)); /* Section accent band token */
  opacity: var(--section-band-opacity, 0.6);
  pointer-events: none;
}
.section > * {
  position: relative;
  z-index: 1;
}
.section-header {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin-bottom: 22px;
}
.section-title {
  margin: 0;
  font-size: 1.75rem;
  font-weight: 600;
  letter-spacing: -0.01em;
  line-height: 1.2;
}
.section-title + * {
  margin-top: 22px;
}
.section-lead {
  margin: 0;
  color: var(--muted);
  line-height: 1.6;
  max-width: 72ch;
}
.section p {
  max-width: 72ch;
}
.section ul {
  padding-left: 26px;
  margin: 4px 0 0;
  max-width: 72ch;
}
.section li {
  margin-bottom: 12px;
  line-height: 1.68;
  color: var(--muted);
}
.section li:last-child { margin-bottom: 0; }
.card-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 22px;
}
.card {
  background: var(--surface);
  border: 1px solid var(--section-card-border, rgba(255, 255, 255, 0.08)); /* Per-section card tint */
  border-radius: 20px;
  padding: 20px;
  display: flex;
  flex-direction: column;
  gap: 12px;
  box-shadow: 0 14px 28px rgba(0, 0, 0, 0.25);
  transition: border-color 200ms ease, transform 200ms ease, box-shadow 200ms ease;
}
.card:nth-child(even) {
  border-color: var(--section-card-border-strong, rgba(255, 255, 255, 0.12));
  padding-top: 22px;
}
.card:hover,
.card:focus-within {
  border-color: var(--section-card-border-hover, rgba(255, 255, 255, 0.18));
  transform: translateY(-2px);
  box-shadow: 0 18px 32px rgba(0, 0, 0, 0.3);
}
.card h3 {
  margin: 0;
  font-size: 1.18rem;
  font-weight: 600;
  letter-spacing: -0.01em;
  color: #F6F7FF;
}
.card h3::after {
  content: "";
  display: block;
  height: 1px;
  margin-top: 8px;
  background: rgba(255, 255, 255, 0.08);
}
.card p {
  margin: 0;
  color: rgba(182, 189, 221, 0.92);
  line-height: 1.65;
  font-size: 0.94rem;
  max-width: none;
}
.steps {
  display: grid;
  gap: 14px;
  background: var(--surface);
  border-radius: 18px;
  padding: 26px 28px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  max-width: 70ch;
}
.steps li { margin-bottom: 12px; color: var(--muted); line-height: 1.65; }
.steps li:last-child { margin-bottom: 0; }
.section-title::after {
  content: "";
  display: block;
  height: 3px;
  width: 64px;
  margin-top: 10px;
  background: hsla(var(--accent-h, 252), 78%, 72%, var(--accent-a, 0.55));
  border-radius: 999px;
  pointer-events: none;
}

/* Section bands + per-section accent tokens */
.section {
  overflow: hidden;
  overflow: clip;
  --accent-h: 252;
  --accent-a: 0.55;
  --accent-band-a: 0.1;
  --accent-border-a: 0.1;
  --accent-border-strong-a: 0.14;
  --accent-border-hover-a: 0.18;
  --section-band-opacity: 0.65;
  --section-band: radial-gradient(140% 120% at 12% -10%, hsla(var(--accent-h), 70%, 66%, var(--accent-band-a)), rgba(12, 13, 26, 0) 60%);
  --section-card-border: hsla(var(--accent-h), 75%, 75%, var(--accent-border-a));
  --section-card-border-strong: hsla(var(--accent-h), 75%, 75%, var(--accent-border-strong-a));
  --section-card-border-hover: hsla(var(--accent-h), 75%, 80%, var(--accent-border-hover-a));
}
.section::before {
  inset: -18%;
  opacity: var(--section-band-opacity);
  filter: blur(22px);
  z-index: 0;
}
.section--coverage {
  --accent-h: 262;
}
.section--toolchain {
  --accent-h: 244;
}
.section--pipeline {
  --accent-h: 232;
}
.section--roadmap {
  --accent-h: 250;
}
.section--examples {
  --accent-h: 238;
}

/* Coverage grid */
.coverage-grid {
  gap: 24px;
}
.coverage-grid .card {
  padding: 22px;
}
.coverage-grid .card:nth-child(even) {
  background: rgba(255, 255, 255, 0.02);
  border-color: hsla(var(--accent-h), 75%, 80%, 0.14);
}
.coverage-grid .card h3 {
  font-size: 1.18rem;
  color: #F8F9FF;
}
.coverage-grid .card p {
  font-size: 0.92rem;
  line-height: 1.62;
}
@supports (-webkit-line-clamp: 2) {
  .coverage-grid .card p {
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }
}
@media (min-width: 1000px) {
  .coverage-grid {
    grid-template-columns: repeat(3, minmax(0, 1fr));
  }
}
@media (min-width: 720px) and (max-width: 999px) {
  .coverage-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

/* Toolchain panel */
.toolchain-panel {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 20px;
  padding: 30px;
  max-width: 70ch;
  box-shadow: 0 16px 30px rgba(0, 0, 0, 0.25);
  display: flex;
  flex-direction: column;
  gap: 16px;
}
.toolchain-lede {
  margin: 0;
  color: var(--text);
  font-size: 1rem;
  line-height: 1.6;
}
.toolchain-list {
  list-style: none;
  padding-left: 0;
  margin: 0;
}
.toolchain-list li {
  margin-bottom: 12px;
  padding-left: 26px;
  position: relative;
  line-height: 1.7;
}
.toolchain-list li::before {
  content: "✓";
  position: absolute;
  left: 0;
  top: 0;
  color: rgba(101, 79, 240, 0.85);
  font-weight: 700;
}

/* Pipeline flow */
.pipeline-grid {
  counter-reset: step;
  position: relative;
}
.pipeline-grid .card {
  position: relative;
  padding-top: 36px;
}
.pipeline-grid .card::before {
  counter-increment: step;
  content: counter(step);
  position: absolute;
  top: 14px;
  left: 16px;
  width: 28px;
  height: 28px;
  border-radius: 999px;
  display: grid;
  place-items: center;
  font-size: 0.85rem;
  font-weight: 600;
  color: #F3F4FF;
  background: rgba(101, 79, 240, 0.3);
  border: 1px solid rgba(101, 79, 240, 0.45);
  box-shadow: 0 8px 16px rgba(0, 0, 0, 0.3);
}
@media (min-width: 1000px) {
  .pipeline-grid::after {
    content: "";
    position: absolute;
    top: 28px;
    left: 24px;
    right: 24px;
    height: 1px;
    background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.18), transparent);
    opacity: 0.7;
  }
}

/* Hero CTA */
.hero-cta {
  display: flex;
  flex-wrap: wrap;
  gap: 14px;
  align-items: center;
  margin-top: 6px;
}
.cta-button {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding: 10px 18px;
  border-radius: 999px;
  background: linear-gradient(120deg, rgba(101, 79, 240, 0.95), rgba(74, 108, 255, 0.85));
  border: 1px solid rgba(74, 108, 255, 0.6);
  color: #F9FAFF;
  font-weight: 600;
  font-size: 0.95rem;
  box-shadow: 0 12px 24px rgba(74, 108, 255, 0.25);
  position: relative;
  overflow: hidden;
}
.cta-button:focus-visible {
  outline: 2px solid rgba(242, 183, 5, 0.8);
  outline-offset: 3px;
}
.cta-link {
  color: var(--text);
  font-weight: 500;
  opacity: 0.85;
}
.cta-link:hover,
.cta-link:focus-visible {
  opacity: 1;
  text-decoration: underline;
}
.cta-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 3px;
}

/* Examples emphasis */
.examples-grid {
  gap: 20px;
}
.examples-grid .card {
  background: linear-gradient(145deg, rgba(20, 22, 43, 0.95), rgba(20, 22, 43, 0.7));
  border-color: hsla(var(--accent-h), 75%, 78%, 0.12);
}
.example-link {
  margin-top: auto;
  color: var(--blue);
  font-weight: 600;
  font-size: 0.92rem;
}
.example-link::after {
  content: " →";
}
.example-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 3px;
}
@media (min-width: 900px) {
  .examples-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

/* Motion + reduced motion */
.cta-button::after {
  content: "";
  position: absolute;
  inset: 0;
  background: linear-gradient(120deg, transparent, rgba(255, 255, 255, 0.35), transparent);
  transform: translateX(-120%);
  transition: transform 0.6s ease;
}
.cta-button:hover::after,
.cta-button:focus-visible::after {
  transform: translateX(120%);
}
.hero-carousel-panel {
  animation: heroPulse 14s ease-in-out infinite;
}
@keyframes heroPulse {
  0%, 100% { box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35); }
  50% { box-shadow: 0 24px 46px rgba(101, 79, 240, 0.2); }
}
@media (prefers-reduced-motion: reduce) {
  .cta-button::after { transition: none; transform: none; }
  .hero-carousel-panel { animation: none; }
  .section-title::after { transition: none; background-position: 0% 50%; }
  .card { transition: none; }
  .card:hover,
  .card:focus-within { transform: none; }
}
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
@media (max-width: 900px) {
  .page { padding: 24px 0 64px; }
  .hero { grid-template-columns: 1fr; gap: 24px; }
  .section { margin-top: 56px; padding: 24px 18px; }
  .card-grid { grid-template-columns: 1fr; gap: 20px; }
}
@media (min-width: 720px) and (max-width: 900px) {
  .coverage-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
}
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
                        (div (@ (class "hero-copy"))
                             (p  (@ (class "hero-lead"))
                                 "A Racket to WebAssembly compiler. Build practical browser applications with Racket.")
                             (div (@ (class "pill-row"))
                                  (span (@ (class "pill")) "JS + DOM FFI")
                                  (span (@ (class "pill")) "Runs in browsers + Node"))
                             (div (@ (class "hero-cta"))
                                  (a (@ (class "cta-button") (href "#examples")) "Try Live Demo")
                                  (a (@ (class "cta-link") (href "https://github.com/racket/webracket")) "View on GitHub"))))
                   (div (@ (class "hero-carousel-panel"))
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
                                          (style "animation-delay: 25s;")))))))
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
                     `(p "Work in progress. Use include for now.")))
              "coverage-grid")))
            #f
            "section--coverage")
          ,(section-block
            "Toolchain Essentials"
            "A small set of tools power the WebRacket workflow from source to the browser."
            (list
             `(div (@ (class "toolchain-panel"))
                   (p (@ (class "toolchain-lede")) "Three tools cover build, run, and serve with minimal setup.")
                   ,(make-ul-list
                     (list
                      (string-append "wasm-tools builds and validates the WebAssembly output. ")
                      (string-append "Node.js enables running generated programs in the terminal. ")
                      (string-append "raco-static-web makes it easy to serve compiled artifacts locally."))
                     "toolchain-list"))))
            #f
            "section--toolchain")
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
                     `(p "A custom runtime avoids reliance on host functionality where possible.")))
              "pipeline-grid")))
            #f
            "section--pipeline")
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
            #f
            "section--roadmap")
          ,(section-block
            "Example Projects"
            "WebRacket already powers interactive demos that showcase browser APIs."
            (list
             (card-grid
              (list
               (list `(h3 "MathJax 4 Editor")
                     `(p "Live formula preview with WebRacket + MathJax.")
                     `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
               (list `(h3 "Matrix Rain")
                     `(p "Terminal-style animation powered by XtermJS.")
                     `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
               (list `(h3 "MiniScheme REPL")
                     `(p "Interactive Scheme session running in the browser.")
                     `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
               (list `(h3 "Canvas + Pict")
                     `(p "Space Invaders and Pict rendering showcase.")
                     `(a (@ (class "example-link") (href "examples.html")) "Open demo")))
              "examples-grid")))
            "examples"
            "section--examples")
          (footer (@ (class "footer"))
                  (span "WebRacket — Racket for the browser.")
                  (span "Made for the Racket community."))))
  (define page (sxml->dom page-structure))
  (js-append-child! body page))

;;;
;;; Entry Point
;;;

(init-dom)
