;;;
;;; Xterm.js
;;;

;; The terminal emulator Xterm.js is a fast, full-featured JavaScript library
;; that runs in the browser. Use it to provide a native-like terminal experience
;; in web applications.

;; This example is a port of the demo on the xtermjs frontpage.
;;
;;     https://xtermjs.org/
;;

;; From an Xtermjs perspective, this demo illustrates how to:
;;
;;   - Terminal initialization and attachment to a DOM element
;;   - Cursor control, ANSI escape sequence handling
;;   - Theme / styling options (e.g. foreground / background)
;;   - Handling user input (onData / key events) and forwarding it to backend
;;   - Add-on usage

;; In the demo the command `chars` shows the so-called "powerline" characters.
;; These characters are commonly used to make spiffy status lines.
;; There is a snafu though - the powerline special characters are not in the Unicode spec.
;; Without special font support, these characters doesn't show up.
;; The good news is that the webgl renderer for Xtermjs renders the characters
;; without special fonts.

;; From a WebRacket perspective, we 

;;;
;;; Utitilies
;;;

(define (exact x)
  (if (and (inexact? x) (integer? x))
      (inexact->exact x)
      x))

(define (format-fixed value digits)
  ; Formats a floating points number with `digits` digits after the decimal point.
  (define factor       (expt 10 digits))
  (define scaled       (round (* value factor)))
  (define scaled-abs   (abs scaled))
  (define integer-part (exact (quotient  scaled-abs factor)))
  (define fraction     (exact (remainder scaled-abs factor)))
  (define fraction-str (let ([s (number->string fraction)])
                         (if (>= (string-length s) digits)
                             s
                             (string-append (make-string (- digits (string-length s)) #\0) s))))
  (string-append (if (< scaled 0) "-" "")
                 (number->string integer-part)
                 "."
                 fraction-str))


(define (pad-left text width)
  (define len (string-length text))
  (if (>= len width)
      text
      (string-append (make-string (- width len) #\space) text)))

(define (pad-right text width)
  (define len (string-length text))
  (if (>= len width)
      text
      (string-append text (make-string (- width len) #\space))))

  (define (printable-string? s)
    (for/and ([ch (in-string s)])
      (let ([code (char->integer ch)])
        (or (and (>= code #x20) (<= code #x7E))
            (>= code #x00A0)))))

  (define (first-space-index s)
    (let loop ([i 0])
      (if (>= i (string-length s))
          (string-length s)
          (if (char=? (string-ref s i) #\space)
              i
              (loop (add1 i))))))



;;;
;;; The Xtermjs Demo
;;;

(define (start-demo . _)
  (define head (js-document-head))
  (define body (js-document-body))

  (js-set-attribute!
   body "style"
   (string-append
    "margin: 0; min-height: 100vh;"
    "background: radial-gradient(circle at top, #3c3d39 0%, #1e1f1d 55%, #161715 100%);"
    "font-family:  'Cascadia Code', Menlo, , monospace;"
    #;"display: flex; align-items: center; justify-content: center;"
    ))


  (define style (js-create-element "style"))
  (js-set! style "textContent"
           (string-append
            ".demo { width: 100%; padding: 32px; box-sizing: border-box; }\n"
            ".demo .inner { "
            "   position: relative; margin: 0 auto; max-width: 860px;"
            "   background: rgba(27, 28, 26, 0.9); border-radius: 16px;"
            "   box-shadow: 0 24px 64px rgba(0, 0, 0, 0.4);"
            "   padding: 32px; transition: background 220ms ease, box-shadow 220ms ease; }\n"
            ".demo .inner.other-theme { "
            "   background: rgba(32, 32, 45, 0.92);"
            "   box-shadow: 0 32px 72px rgba(8, 12, 32, 0.55); }\n"
            ".demo .terminal-container {\n"
            "   height: 480px; display: flex; justify-content: center; align-items: center;\n"
            " }\n"
            ".demo .title { "
            "   color: #f8f8f8; font-size: 20px; margin: 0 0 16px;"
            "   text-transform: uppercase; letter-spacing: 0.24em; text-align: center; }\n"
            ".demo .subtitle { "
            "   color: rgba(248, 248, 248, 0.75); margin: 0 0 24px;"
            "   text-align: center; font-size: 14px; letter-spacing: 0.12em; }\n"
            ".demo .hint { "
            "   color: rgba(248, 248, 248, 0.6);"
            "   text-align: center; margin-top: 16px; font-size: 13px; }\n"
            ".demo .inner.other-theme .hint     { color: rgba(239, 240, 235, 0.7);  }\n"
            ".demo .inner.other-theme .title    { color: #eff0eb;                   }\n"
            ".demo .inner.other-theme .subtitle { color: rgba(239, 240, 235, 0.75); }\n"
            ".xterm { border-radius: 12px; overflow: hidden; box-shadow: 0 12px 36px rgba(0, 0, 0, 0.45); }\n"
            ".link-hint-decoration {"
            "   position: absolute; right: 16px; top: -28px; padding: 4px 10px;"
            "   border-radius: 999px; background: rgba(0, 0, 0, 0.65); color: #f8f8f8;"
            "   font-size: 12px; letter-spacing: 0.08em; pointer-events: none;"
            "   text-transform: uppercase; }"))
  (js-append-child! head style)

  (define root (js-create-element "div"))
  (js-set-attribute! root "class" "demo")
  (define inner (js-create-element "div"))
  (js-set-attribute! inner "class" "inner")
  (define title (js-create-element "h1"))
  (js-set-attribute! title "class" "title")
  (js-set! title "textContent" "Interactive xterm.js demo")
  (define subtitle (js-create-element "p"))
  (js-set-attribute! subtitle "class" "subtitle")
  (js-set! subtitle "textContent" "Type `help` inside the terminal to explore available commands.")
  (define hint (js-create-element "p"))
  (js-set-attribute! hint "class" "hint")
  (js-set! hint "textContent"
           "Scroll the output, click the italic text, and toggle themes via the links command.")
  (define terminal-host (js-create-element "div"))
  (js-set-attribute! terminal-host "class" "terminal-container")
  (js-append-child! inner title)
  (js-append-child! inner subtitle)
  (js-append-child! inner terminal-host)
  (js-append-child! inner hint)
  (js-append-child! root inner)
  (js-append-child! body root)

  ;;;
  ;;; Themes (Color Schemes)
  ;;;

  (define base-theme
    (js-object
     '(("foreground"    "#F8F8F8")
       ("background"    "#2D2E2C")
       ("selection"     "#5DA5D533")
       ("black"         "#1E1E1D")
       ("brightBlack"   "#262625")
       ("red"           "#CE5C5C")
       ("brightRed"     "#FF7272")
       ("green"         "#5BCC5B")
       ("brightGreen"   "#72FF72")
       ("yellow"        "#CCCC5B")
       ("brightYellow"  "#FFFF72")
       ("blue"          "#5D5DD3")
       ("brightBlue"    "#7279FF")
       ("magenta"       "#BC5ED1")
       ("brightMagenta" "#E572FF")
       ("cyan"          "#5DA5D5")
       ("brightCyan"    "#72F0FF")
       ("white"         "#F8F8F8")
       ("brightWhite"   "#FFFFFF"))))

  (define other-theme
    (js-object
     '(("foreground"    "#eff0eb")
       ("background"    "#282a36")
       ("selection"     "#97979b33")
       ("black"         "#282a36")
       ("brightBlack"   "#686868")
       ("red"           "#ff5c57")
       ("brightRed"     "#ff5c57")
       ("green"         "#5af78e")
       ("brightGreen"   "#5af78e")
       ("yellow"        "#f3f99d")
       ("brightYellow"  "#f3f99d")
       ("blue"          "#57c7ff")
       ("brightBlue"    "#57c7ff")
       ("magenta"       "#ff6ac1")
       ("brightMagenta" "#ff6ac1")
       ("cyan"          "#9aedfe")
       ("brightCyan"    "#9aedfe")
       ("white"         "#f1f1f0")
       ("brightWhite"   "#eff0eb"))))
    
  ;;;
  ;;; Terminal Creation
  ;;;

  ;; Create terminal given the options below.
  ;; After creation attach it to the `terminal-host` div node.
  
  (define terminal-options
    (js-object
     (vector (vector "fontFamily"       "\"Cascadia Code\", Menlo, monospace")
             (vector "theme"            base-theme)
             (vector "cursorBlink"      #t)
             (vector "allowProposedApi" #t))))

  (define term (xterm-terminal-new terminal-options))
  (xterm-terminal-open term terminal-host)
  (xterm-terminal-focus term)

  ; Use webgl addon (needed for the powerline glyps to render correctly)
  (define win               (js-window-window))
  (define webgl-constructor (js-ref/extern (js-ref/extern win "WebglAddon") "WebglAddon"))
  (define webgl             (js-new webgl-constructor (vector)))
  (xterm-terminal-load-addon term webgl)


  ;;;
  ;;; COMMANDS
  ;;;
  
  (define command        "")
  (define is-base-theme? #t)


  (define (wrap-text text max-length)
    (let loop ([remaining text]
               [acc       '()])
      (define trimmed (string-trim-left remaining #\space))
      (define len     (string-length trimmed))
      (cond
        [(zero? len)         (reverse acc)]
        [(<= len max-length) (reverse (cons trimmed acc))]
        [else
         (define candidate
           (let ([limit (min (sub1 len) max-length)])
             (if (char=? (string-ref trimmed max-length) #\space)
                 max-length
                 (let search ([i limit])
                   (cond
                     [(< i 0)                                 max-length]
                     [(char=? (string-ref trimmed i) #\space) (if (zero? i) max-length i)]
                     [else                                    (search (sub1 i))])))))
         (define part (substring trimmed 0 candidate))
         (define rest (substring trimmed candidate len))
         (loop rest (cons part acc))])))

  (define (prompt)
    (set! command "")
    (xterm-terminal-write term "\r\n$ " (void)))

  (define initial-lines
    (list
     "    Xterm.js is the frontend component that powers many terminals including"
     "                           \u001b[3mVS Code\u001b[0m, \u001b[3mHyper\u001b[0m and \u001b[3mTheia\u001b[0m!"
     ""
     " ┌ \u001b[1mFeatures\u001b[0m ──────────────────────────────────────────────────────────────────┐"
     " │                                                                            │"
     " │  \u001b[31;1mApps just work                         \u001b[32mPerformance\u001b[0m                        │"
     " │   Xterm.js works with most terminal      Xterm.js is fast and features a   │"
     " │   apps like bash, vim and tmux           responsive renderer               │"
     " │                                                                            │"
     " │  \u001b[33;1mAccessible                             \u001b[34mSelf-contained\u001b[0m                     │"
     " │   A screen reader mode is available      Zero external dependencies        │"
     " │                                                                            │"
     " │  \u001b[35;1mUnicode support                        \u001b[36mAnd much more...\u001b[0m                   │"
     " │   Supports CJK 語 and emoji \u2764\ufe0f            \u001b[3mLinks\u001b[0m, \u001b[3mthemes\u001b[0m, \u001b[3maddons\u001b[0m,            │"
     " │                                          \u001b[3mtyped API\u001b[0m, \u001b[3mdecorations\u001b[0m            │"
     " │                                                                            │"
     " └────────────────────────────────────────────────────────────────────────────┘"
     ""))

  (xterm-terminal-writeln term (string-join initial-lines "\n\r") (void))
  (xterm-terminal-writeln term "Below is a simple emulated backend, try running `help`." (void))

  (define (apply-theme!)
    (define options (xterm-terminal-options term))
    (js-set! options "theme" (if is-base-theme? base-theme other-theme))
    (js-send (js-ref inner "classList") "toggle" (vector "other-theme" (not is-base-theme?))))

  (define (add-decoration!)
    (define marker (xterm-terminal-register-marker term 15))
    (define decoration
      (xterm-terminal-register-decoration
       term (js-object (vector (vector "marker" marker) (vector "x" 44)))))
    (define on-render
      (procedure->external
       (λ (element . _)
         (when element
           (js-send (js-ref element "classList") "add" (vector "link-hint-decoration"))
           (js-set! element "innerText" "Try clicking italic text")
           (define style-obj (js-ref element "style"))
           (js-set! style-obj "height" "")
           (js-set! style-obj "width" ""))
         (void))))
    (js-send decoration "onRender" (vector on-render))
    (void))

  (define (link range-start-x range-start-y range-end-x range-end-y text activate)
    (js-object
     (vector
      (vector "text" text)
      (vector "range"
              (js-object
               (vector
                (vector "start"
                        (js-object (vector (vector "x" range-start-x) (vector "y" range-start-y))))
                (vector "end"
                        (js-object (vector (vector "x" range-end-x) (vector "y" range-end-y)))))))
      (vector "activate"
              (procedure->external
               (λ _
                 (activate)
                 (void)))))))

  (define (call-callback callback links)
    (if links
        (js-send callback "call"
                 (vector (js-undefined)
                         (js-array/extern (list->vector links))))
        (js-send callback "call"
                 (vector (js-undefined)
                         (js-undefined)))))

  (define (provide-links buffer-line callback . _)
    (define (open url)
      (js-window-open url "_blank" (void) (void)))
    (define case-value
      (cond [(= buffer-line 2)
             (list
              (link 28 2 34 2 "VS Code" (λ () (open "https://github.com/microsoft/vscode")))
              (link 37 2 41 2 "Hyper"   (λ () (open "https://github.com/vercel/hyper")))
              (link 47 2 51 2 "Theia"   (λ () (open "https://github.com/eclipse-theia/theia"))))]
            [(= buffer-line 14)
             (list
              (link 45 14 49 14 "Links"
                    (λ () (js-window-alert "You can handle links any way you want")))
              (link 52 14 57 14 "themes"
                    (λ ()
                      (set! is-base-theme? (not is-base-theme?))
                      (apply-theme!)
                      (xterm-terminal-write term
                                            (string-append
                                             "\r\nActivated "
                                             (if is-base-theme? "xterm.js" "snazzy")
                                             " theme")
                                            (void))
                      (prompt)))
              (link 60 14 65 14 "addons"
                    (λ () (open "https://xtermjs.org/docs/guides/using-addons/"))))]
            [(= buffer-line 15)
             (list
              (link 45 15 53 15 "typed API"
                    (λ ()
                      (open "https://github.com/xtermjs/xterm.js/blob/master/typings/xterm.d.ts")))
              (link 56 15 66 15 "decorations"
                    (λ ()
                      (open (string-append
                             "https://github.com/xtermjs/xterm.js/blob/"
                             "a351f5758a5126308b90d60b604b528462f6f051/typings/xterm.d.ts#L372")))))]
            [else #f]))
    (call-callback callback case-value))

  (define provider
    (js-object (vector (vector "provideLinks" (procedure->external provide-links)))))
  (xterm-terminal-register-link-provider term provider)

  (add-decoration!)

  (define (wheel-handler event . _)
    (define buffer (xterm-terminal-buffer term))
    (define active (js-ref buffer "active"))
    (define base-y (js-ref active "baseY"))
    (when (and (number? base-y) (> base-y 0))
      (js-send event "preventDefault" (vector)))
    (void))

  (define xterm-element (js-query-selector ".xterm"))
  (when xterm-element
    (js-add-event-listener! xterm-element "wheel" (procedure->external wheel-handler)))

  (define (ls-command)
    (xterm-terminal-writeln term (string-join '("a" "bunch" "of" "fake" "files") "\r\n") (void))
    (prompt))

  (define (loadtest-command)
    (define test-data '())
    (define byte-count 0)
    (for ([i (in-range 50)])
      (define count (add1 (random 79)))
      (define letters
        (apply string
               (for/list ([j (in-range count)])
                 (integer->char (+ #x61 (random (- #x7A #x61)))))))
      (define chunk (string-append "\n" letters "\r"))
      (set! byte-count (+ byte-count (string-length chunk)))
      (set! test-data (cons chunk test-data)))
    (set! test-data (reverse test-data))
    (define start (js-performance-now))
    (for ([i (in-range 1024)])
      (for ([chunk (in-list test-data)])
        (xterm-terminal-write term chunk (void))))
    (define callback
      (procedure->external
       (λ _
         (define elapsed (- (js-performance-now) start))
         (define time-ms (inexact->exact (round elapsed)))
         (define seconds (/ elapsed 1000.0))
         (define throughput
           (if (zero? seconds)
               0.0
               (/ (/ byte-count 1024.0) seconds)))
         (define message
           (string-append
            "\n\r\nWrote "
            (number->string byte-count)
            "kB in "
            (number->string time-ms)
            "ms ("
            (format-fixed throughput 2)
            "MB/s)"))
         (xterm-terminal-write term message (void))
         (prompt))))
    (xterm-terminal-write term "" callback))

  (define (range-list start end)
    (for/list ([i (in-range start (add1 end))]) i))
  
  (define (chars-command)
    (define one-to-eight       (range-list 1 8))
    (define zero-to-thirtyfive (range-list 0 35))
    (define one-to-twentyfour  (range-list 1 24))
    (define one-to-sixtyfour   (range-list 1 64))
    (define lines
      (list
       (list "Ascii ─" "abc123")
       (list "CJK ─" "汉语, 漢語, 日本語, 한국어")
       (list "Powerline ─" "\ue0b2\ue0b0\ue0b3\ue0b1\ue0b6\ue0b4\ue0b7\ue0b5\ue0ba\ue0b8\ue0bd\ue0b9\ue0be\ue0bc")           
 
       (list "Box drawing ┬" "┌─┬─┐ ┏━┳━┓ ╔═╦═╗ ┌─┲━┓ ╲   ╱")
       (list "            │" "│ │ │ ┃ ┃ ┃ ║ ║ ║ │ ┃ ┃  ╲ ╱")
       (list "            │" "├─┼─┤ ┣━╋━┫ ╠═╬═╣ ├─╄━┩   ╳")
       (list "            │" "│ │ │ ┃ ┃ ┃ ║ ║ ║ │ │ │  ╱ ╲")
       (list "            └" "└─┴─┘ ┗━┻━┛ ╚═╩═╝ └─┴─┘ ╱   ╲")
       (list "Block elem ─" "░▒▓█ ▁▂▃▄▅▆▇█ ▏▎▍▌▋▊▉")
       (list "Emoji ─" "😉 👋")
       (list "16 color ─"
             (apply string-append
                    (append (for/list ([n (in-list one-to-eight)])
                              (string-append "\u001b[3" (number->string (sub1 n)) "m●"))
                            (for/list ([n (in-list one-to-eight)])
                              (string-append "\u001b[1;3" (number->string (sub1 n)) "m●")))))
       (list "256 color ┬"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 0) n))
                                     "m●"))))
       (list "          │"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 1) n))
                                     "m●"))))
       (list "          │"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 2) n))
                                     "m●"))))
       (list "          │"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 3) n))
                                     "m●"))))
       (list "          │"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 4) n))
                                     "m●"))))
       (list "          │"
             (apply string-append
                    (for/list ([n (in-list zero-to-thirtyfive)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 16 (* 36 5) n))
                                     "m●"))))
       (list "          └"
             (apply string-append
                    (for/list ([n (in-list one-to-twentyfour)])
                      (string-append "\u001b[38;5;"
                                     (number->string (+ 232 (sub1 n)))
                                     "m●"))))
       (list "True color ┬"
             (apply string-append
                    (for/list ([n (in-list one-to-sixtyfour)])
                      (string-append "\u001b[38;2;"
                                     (number->string (+ -1 n))
                                     ";0;0m●"))))
       (list "           │"
             (apply string-append
                    (for/list ([n (in-list one-to-sixtyfour)])
                      (string-append "\u001b[38;2;"
                                     (number->string (+ 63 n))
                                     ";0;0m●"))))
       (list "           │"
             (apply string-append
                    (for/list ([n (in-list one-to-sixtyfour)])
                      (string-append "\u001b[38;2;"
                                     (number->string (+ 127 n))
                                     ";0;0m●"))))
       (list "           └"
             (apply string-append
                    (for/list ([n (in-list one-to-sixtyfour)])
                      (string-append "\u001b[38;2;"
                                     (number->string (+ 191 n))
                                     ";0;0m●"))))
       (list "Styles ─"
             (string-append
              "\u001b[1mBold\u001b[0m, \u001b[2mFaint\u001b[0m, \u001b[3mItalics\u001b[0m, \u001b[7mInverse\u001b[0m, "
              "\u001b[9mStrikethrough\u001b[0m, \u001b[8mInvisible\u001b[0m"))
       (list "Underlines ─"
             (string-append
              "\u001b[4:1mStraight\u001b[0m, \u001b[4:2mDouble\u001b[0m, \u001b[4:3mCurly\u001b[0m, "
              "\u001b[4:4mDotted\u001b[0m, \u001b[4:5mDashed\u001b[0m"))))
    (define max-length
      (for/fold ([acc 0]) ([entry (in-list lines)])
        (max acc (string-length (car entry)))))
    (xterm-terminal-write term "\r\n" (void))
    (xterm-terminal-writeln
     term
     (string-join
      (for/list ([entry (in-list lines)])
        (string-append (pad-left (car entry) max-length) "  " (cadr entry) "\u001b[0m"))
      "\r\n")
     (void))
    (prompt))

  (define (help-command)
    (define padding 10)
    (define max-length (max 20 (- (inexact->exact (xterm-terminal-cols term)) padding 3)))
    (define (format-message name description)
      (define wrapped (wrap-text description max-length))
      (define first-line (car wrapped))
      (define rest-lines (cdr wrapped))
      (string-append
       "  \u001b[36;1m" (pad-right name padding) "\u001b[0m " first-line
       (apply string-append
              (for/list ([line (in-list rest-lines)])
                (string-append "\r\n  " (make-string padding #\space) " " line)))))
    (xterm-terminal-writeln
     term
     (string-join
      (append
       (list "Welcome to xterm.js! Try some of the commands below." "")
       (for/list ([entry (in-list commands)])
         (define name (car entry))
         (define info (cdr entry))
         (format-message name (command-info-description info))))
      "\n\r")
     (void))
    (prompt))

  (struct command-info (handler description))

  (define commands
    (list (cons "help"     (command-info help-command     "Prints this help message"))
          (cons "ls"       (command-info ls-command       "Prints a fake directory structure"))
          (cons "loadtest" (command-info loadtest-command "Simulate a lot of data coming from a process"))
          (cons "chars"    (command-info chars-command    "Prints a wide range of characters and styles that xterm.js can handle"))))

  (define (lookup-command name)
    (assoc name commands))
  
  (define (run-command text)
    (define trimmed (string-trim text))
    (define name
      (if (zero? (string-length trimmed))
          ""
          (substring trimmed 0 (first-space-index trimmed))))
    (when (> (string-length name) 0)
      (xterm-terminal-writeln term "" (void))
      (define entry (lookup-command name))
      (if entry
          ((command-info-handler (cdr entry)))
          (begin
            (xterm-terminal-writeln term (string-append name ": command not found") (void))
            (prompt)))))

  (define on-data
    (procedure->external
     (λ (data . _)
       (when (string? data)
         (cond
           [(string=? data "\u0003")
            (xterm-terminal-write term "^C" (void))
            (prompt)]
           [(string=? data "\r")
            (run-command command)
            (set! command "")]
           [(string=? data "\u007F")
            (when (> (string-length command) 0)
              (set! command (substring command 0 (sub1 (string-length command))))
              (xterm-terminal-write term "\b \b" (void)))]
           [else
            (when (printable-string? data)
              (set! command (string-append command data))
              (xterm-terminal-write term data (void)))]))
       (void))))

  (js-send term "onData" (vector on-data))

  (prompt)
  (void))

;;;
;;; Load Xterm.js and the webgl addon.
;;;

;; When all scripts are loaded, the function `start-demo`
;; is called. 


(define xtermjs-url
  "https://cdn.jsdelivr.net/npm/@xterm/xterm@5.5.0/lib/xterm.min.js")
(define webgl-addon-url
  "https://cdn.jsdelivr.net/npm/@xterm/addon-webgl@0.18.0/lib/addon-webgl.min.js")
(define xtermjs-css-url
  "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css")

(define scripts         (list xtermjs-url webgl-addon-url))
(define scripts-to-load (length scripts))
(define scripts-loaded  0)

(define (script-ready . _)
  (set! scripts-loaded (add1 scripts-loaded))
  (when (= scripts-loaded scripts-to-load)
    (start-demo)))

(define (script-error . _)
  (js-log "ERROR"))

(define (register-script-events! element)
  (define callback (procedure->external script-ready))
  (define error    (procedure->external script-error))
  (js-add-event-listener! element "load"  callback)
  (js-add-event-listener! element "error" error))

(define (load-script script-url)
  (define head   (js-document-head))
  (define script (js-create-element "script"))
  (js-set-attribute! script "src" script-url)
  (register-script-events! script)
  (js-append-child! head script))

(define (load-scripts)
  (for-each load-script scripts))

(define (load-css-files)
  (define head (js-document-head))  
  (define link (js-create-element "link"))
  (js-set-attribute! link "rel" "stylesheet")
  (js-set-attribute! link "href" xtermjs-css-url)
  (js-append-child! head link))


;; NOTE
;;   Remember to set the variable `scripts-to-load`
;;   to the number of scripts to load.

(load-scripts)
(load-css-files)
