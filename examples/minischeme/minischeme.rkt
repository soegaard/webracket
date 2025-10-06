;;;
;;; MiniScheme
;;;

;; This is a mini Scheme repl.
;; The terminal is provided by Xtermjs.

;;;
;;; Scripts and CSS-files
;;;

;; Before the main program (the repl) starts, we need to
;; wait for the Xtermjs files to be loaded. This section
;; handles the loading of scripts and css-files. 

(define xtermjs-url
  "https://cdn.jsdelivr.net/npm/@xterm/xterm@5.5.0/lib/xterm.min.js")
(define xtermjs-css-url
  "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css")

(define scripts         (list xtermjs-url))
(define scripts-to-load (length scripts))
(define scripts-loaded  0)

(define css-files       (list xtermjs-css-url))

(define (script-ready . _)
  (set! scripts-loaded (add1 scripts-loaded))
  (when (= scripts-loaded scripts-to-load)
    (start-main)))

(define (script-error . _)
  (js-log "Failed to load xterm.js"))

(define (register-script-events! element)
  (define ready (procedure->external script-ready))
  (define error (procedure->external script-error))
  (js-add-event-listener! element "load" ready)
  (js-add-event-listener! element "error" error))

(define (load-script script-url)
  (define head (js-document-head))
  (define script (js-create-element "script"))
  (js-set-attribute! script "src" script-url)
  (register-script-events! script)
  (js-append-child! head script))

(define (load-scripts)
  (for-each load-script scripts))

(define (load-css-file css-url)
  (define head (js-document-head))
  (define link (js-create-element "link"))
  (js-set-attribute! link "rel" "stylesheet")
  (js-set-attribute! link "href" css-url)
  (js-append-child! head link))

(define (load-css-files)
  (for-each load-css-file css-files))

(load-scripts)
(load-css-files)

;;;
;;; Main
;;;

(define (start-main)
  (init-dom)
  (init-terminal)
  (start-minischeme))

;;;
;;; DOM (Html)
;;;

;; To keep the example in a single file, we dynamically
;; create the DOM. One could argue it would be simpler
;; to use an html-file instead.

(define terminal-host #f)  ; a `div` for the terminal

(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (js-set-attribute!
   body "style"
   (string-append
    "margin: 0; min-height: 100vh; display: flex;"
    "align-items: center; justify-content: center;"
    "background: #101216; font-family: 'Fira Code', monospace;"))

  (define style (js-create-element "style"))
  (js-set! style "textContent"
           (string-append
            ".minischeme-container {\n"
            "  width: min(90vw, 720px);\n"
            "  padding: 24px;\n"
            "  border-radius: 16px;\n"
            "  background: rgba(15, 18, 26, 0.85);\n"
            "  box-shadow: 0 24px 60px rgba(0, 0, 0, 0.45);\n"
            "}\n"
            ".minischeme-title {\n"
            "  margin: 0 0 16px;\n"
            "  color: #E2F1FF;\n"
            "  font-size: 18px;\n"
            "  text-align: center;\n"
            "  letter-spacing: 0.12em;\n"
            "}\n"
            ".minischeme-terminal {\n"
            "  background: rgb(0,0,0);\n"
            "  padding: 12px;"
            "  border-radius: 12px;\n"
            "  overflow: hidden;\n"
            "  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.06);\n"
            "}\n"))
  (js-append-child! head style)

  (define container (js-create-element "div"))
  (js-set-attribute! container "class" "minischeme-container")

  (define title (js-create-element "h1"))
  (js-set-attribute! title "class" "minischeme-title")
  (js-set! title "textContent" "MiniScheme")

  (set! terminal-host (js-create-element "div"))
  (js-set-attribute! terminal-host "class" "minischeme-terminal")

  (js-append-child! container title)
  (js-append-child! container terminal-host)
  (js-append-child! body container))

;;;
;;; The Terminal
;;;


(define term #f) ; the Terminal js-object

(define (init-terminal)
  (define terminal-options
    (js-object
     (vector (vector "fontSize"    16)
             (vector "cursorBlink" #t)
             (vector "theme"
                     (js-object
                      '(("foreground" "#E2F1FF")
                        ("background" "#141922")
                        ("cursor"     "#6DF7C1")
                        ("selection"  "#26465366")))))))

  (set! term (xterm-terminal-new terminal-options))
  (xterm-terminal-open term terminal-host)
  (xterm-terminal-focus term))
  

;;;
;;; Utilities
;;;

(define (printable-string? s)
  (for/and ([ch (in-string s)])
    (let ([code (char->integer ch)])
      (or (and (>= code #x20) (<= code #x7E))
          (>= code #x00A0)))))

;;;
;;; MiniScheme
;;;

(define (start-minischeme . _)
  (define intro-lines
    '("Welcome to MiniScheme."
      "Type a line and press Enter to see it echoed back."
      ""))

  (for ([line (in-list intro-lines)])
    (xterm-terminal-writeln term line (void)))

  (define prompt        "> ")
  (define current-input "")

  (define (show-prompt)
    (set! current-input "")
    (xterm-terminal-write term prompt (void)))

  (define (echo-line line)
    (xterm-terminal-writeln term line (void)))

  (define on-data
    (procedure->external
     (λ (data . _)
       (when (string? data)
         (cond
           [(string=? data "\u0003")                 ; ETX = End of Text (caused by ctrl-c)
            (xterm-terminal-write term "^C" (void))
            (show-prompt)]
           [(string=? data "\r")                     
            (xterm-terminal-writeln term "" (void))
            (echo-line current-input)
            (show-prompt)]
           [(string=? data "\u007F")                 ; DELETE
            (when (> (string-length current-input) 0)
              (set! current-input
                    (substring current-input 0 (sub1 (string-length current-input))))
              (xterm-terminal-write term "\b \b" (void)))]
           [else
            (when (printable-string? data)
              (set! current-input (string-append current-input data))
              (xterm-terminal-write term data (void)))]))
       (void))))

  (js-send term "onData" (vector on-data))
  (show-prompt)
  (void))

