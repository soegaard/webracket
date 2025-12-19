;;;
;;; MiniScheme
;;;

;; This file implements a little "mini Scheme" evaluator and
;; a repl to interact with the evaluator.
;; The terminal is provided by Xtermjs.

;; Project Idea
;;   - replace the evaluator with your own interpreter


;;;
;;; Build Instructions
;;;

;; racket ../../webracket.rkt --ffi standard --ffi xtermjs --ffi js --ffi dom  --stdlib -b minischeme.rkt

;; Start a local web-server and open "minischeme.html".

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

(define xtermjs-addon-fit-url
  ; fits the terminal dimension to a containing element
  "https://cdn.jsdelivr.net/npm/@xterm/addon-fit@0.10.0/lib/addon-fit.min.js")

(define xtermjs-addon-clipboard-url
  ; enables clipboard integration for mouse copy/paste
  "https://cdn.jsdelivr.net/npm/@xterm/addon-clipboard@0.1.0/lib/addon-clipboard.min.js")

(define scripts         (list xtermjs-url xtermjs-addon-fit-url xtermjs-addon-clipboard-url))
(define scripts-to-load (length scripts))
(define scripts-loaded  0)

(define css-files       (list xtermjs-css-url))

(define (script-ready . _)
  (js-log "Scripts ready")
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
            "  width: min(90vw, 1024px);\n"
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

            ".minischeme-description {\n"
            "  margin: 0 0 20px;\n"
            "  color: #C7D7EA;\n"
            "  font-size: 14px;\n"
            "  line-height: 1.6;\n"
            "  text-align: left;\n"
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

  (define description1 (js-create-element "p"))
  (js-set-attribute! description1 "class" "minischeme-description")
  (js-set! description1 "textContent"
           "This is a small interpreter implemented in WebRacket.")

  (define description2 (js-create-element "p"))
  (js-set-attribute! description2 "class" "minischeme-description")
  (js-set! description2 "textContent"
           "The variables `keywords`, `constants`, and `primitives` are available in the repl.")

  (define description3 (js-create-element "p"))
  (js-set-attribute! description3 "class" "minischeme-description")
  (js-set! description3 "textContent"
           "Some Emacs keybindings are available.")

  
  (set! terminal-host (js-create-element "div"))
  (js-set-attribute! terminal-host "class" "minischeme-terminal")

  (js-append-child! container title)
  (js-append-child! container description1)
  (js-append-child! container description2)
  (js-append-child! container description3)
  (js-append-child! container terminal-host)
  (js-append-child! body container))


;;;
;;; The Terminal
;;;

(define term #f) ; the Terminal js-object

(define term-on-key    #f)
(define term-on-data   #f)
(define term-on-binary #f)
(define term-on-click  #f) 
(define last-key-data  #f) ; for pasting into the terminal

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

  
  ; Use Fit addon (makes terminal dimensions fit the containing element)
  (define fit-addon
    (let* ([win               (js-window-window)]
           [addon-namespace   (js-ref/extern win "FitAddon")]
           [addon-constructor (js-ref/extern addon-namespace "FitAddon")])
      (js-new addon-constructor (vector))))

  ; Use clipboard addon
  (define clipboard-addon
    (let* ([win               (js-window-window)]
           [addon-namespace   (js-ref/extern win "ClipboardAddon")]
           [addon-constructor (js-ref/extern addon-namespace "ClipboardAddon")])
      (js-new addon-constructor (vector))))

  (set! term (xterm-terminal-new terminal-options))
  (xterm-terminal-load-addon term fit-addon)
  (xterm-terminal-load-addon term clipboard-addon)
  (xterm-terminal-open term terminal-host)
  (xterm-fit-addon-fit fit-addon)
  (xterm-terminal-focus term))

  

;;;
;;; Utilities
;;;

(define (printable-char? ch)
  (let ([code (char->integer ch)])
    (or (and (>= code #x20) (<= code #x7E))
        (>= code #x00A0))))

(define (printable-string? s)
  (for/and ([ch (in-string s)])
    (printable-char? ch)))

;;;
;;; MiniScheme Editor
;;;

;       Key                 ; Emacs binding
(define ESC     "\u001b")
(define CTRL-A  "\u0001")   ; beginning-of-line
(define CTRL-D  "\u0004")   ; delete-char
(define CTRL-E  "\u0005")   ; end-of-line
(define CTRL-J  "\n")       ; newline
(define CTRL-K  "\v")       ; kill-line
(define CTRL-L  "\f")       ; recenter-top-bottom
(define CTRL-N  "\u000e")   ; next-line
(define CTRL-P  "\u0010")   ; previous-line
(define DELETE  "\u007f")   ; backward-delete-char
(define ENTER   "\r")       ; newline
(define TAB     "\t")       ; indent-for-tab-command

(define CTRL-RBRACKET "\u001d") ; ctrl-]  (inserts literal `)`)


(define DELETE-RIGHT (string-append ESC "[3~"))
(define UP           (string-append ESC "[A"))
(define DOWN         (string-append ESC "[B"))
(define RIGHT        (string-append ESC "[C"))
(define LEFT         (string-append ESC "[D"))
(define END          (string-append ESC "[F"))
(define HOME         (string-append ESC "[H"))
(define F1           (string-append ESC "[BOP"))
(define F2           (string-append ESC "[BOQ"))
(define PAGEUP       (string-append ESC "[5~"))
(define PAGEDOWN     (string-append ESC "[6~"))
(define DEL          (string-append ESC "[3~"))

(define (CSI cmd)
  (string-append ESC "[" cmd))

(define (DECSET pm)
  (string-append ESC "[" "?" pm "h"))

(struct mark   (row col)      #:mutable)
(struct line   (raw rendered) #:mutable)
(struct buffer (name lines point start prompts active-prompt locked?
                     first-screen-row screen-col)
  #:mutable)

(define (make-line raw [rendered #f])
  (line raw rendered))

(define (line-dirty! l)
  (set-line-rendered! l #f))

(define (make-empty-lines count)
  (let ([vec (make-vector count)])
    (let loop ([i 0])
      (when (< i count)
        (vector-set! vec i (make-line ""))
        (loop (add1 i))))
    vec))

(define (make-buffer name)
  (buffer name
          (make-empty-lines 1)
          (mark 0 0)
          (mark 0 0)
          '()
          0
          #t
          0
          0))

(define current-buffer #f)

(define the-prompt        "> ")
(define the-prompt-length 2)

(define ansi-black          "\u001b[30m")
(define ansi-red            "\u001b[31m")
(define ansi-green          "\u001b[32m")
(define ansi-yellow         "\u001b[33m")
(define ansi-blue           "\u001b[34m")
(define ansi-magenta        "\u001b[35m")
(define ansi-cyan           "\u001b[36m")
(define ansi-white          "\u001b[37m")

(define ansi-bright-black   "\u001b[30;1m")
(define ansi-bright-red     "\u001b[31;1m")
(define ansi-bright-green   "\u001b[32;1m")
(define ansi-bright-yellow  "\u001b[33;1m")
(define ansi-bright-blue    "\u001b[34;1m")
(define ansi-bright-magenta "\u001b[35;1m")
(define ansi-bright-cyan    "\u001b[36;1m")
(define ansi-bright-white   "\u001b[37;1m")

(define ansi-default        "\u001b[39m")
(define ansi-reset          "\u001b[0m")

(define ansi-keyword   ansi-yellow)
(define ansi-primitive ansi-cyan)
(define ansi-literal   ansi-bright-blue)
(define ansi-error     ansi-bright-red)
(define ansi-paren     ansi-bright-yellow)
(define ansi-binding   ansi-bright-green)

(define (vector-insert vec idx value)
  (let* ([len (vector-length vec)]
         [new (make-vector (add1 len))])
    (let loop ([i 0])
      (when (< i len)
        (define src (vector-ref vec i))
        (if (< i idx)
            (vector-set! new i src)
            (vector-set! new (add1 i) src))
        (loop (add1 i))))
    (vector-set! new idx value)
    new))

(define (vector-remove vec idx)
  (let* ([len (vector-length vec)]
         [new (make-vector (max 0 (sub1 len)))])
    (let loop ([i 0])
      (when (< i len)
        (when (not (= i idx))
          (define dest (if (< i idx) i (sub1 i)))
          (vector-set! new dest (vector-ref vec i)))
        (loop (add1 i))))
    new))

(define (vector-insert-list vec idx lst)
  (let* ([len (vector-length vec)]
         [add (length lst)]
         [new (make-vector (+ len add))])
    (let loop ([i 0])
      (when (< i idx)
        (vector-set! new i (vector-ref vec i))
        (loop (add1 i))))
    (let loop ([items lst]
               [i idx])
      (when (pair? items)
        (vector-set! new i (car items))
        (loop (cdr items) (add1 i))))
    (let loop ([i idx])
      (when (< i len)
        (vector-set! new (+ i add) (vector-ref vec i))
        (loop (add1 i))))
    new))

(define (buffer-lines-count b)
  (vector-length (buffer-lines b)))

(define (buffer-line-at b row)
  (vector-ref (buffer-lines b) row))

(define (buffer-line-at-point b)
  (buffer-line-at b (mark-row (buffer-point b))))

(define (prompt-line? b row)
  (let loop ([ps (buffer-prompts b)])
    (cond
      [(null? ps) #f]
      [(= (car ps) row) #t]
      [else (loop (cdr ps))])))

(define (mark-move-to! m absolute-row absolute-col)
  (set-mark-row! m absolute-row)
  (set-mark-col! m absolute-col))

(define (mark-move-relative-to! m delta-row delta-col)
  (set-mark-row! m (+ (mark-row m) delta-row))
  (set-mark-col! m (+ (mark-col m) delta-col)))

(define (mark=? m1 m2)
  (and (= (mark-row m1) (mark-row m2))
       (= (mark-col m1) (mark-col m2))))

(define (mark<? m1 m2)
  (or (< (mark-row m1) (mark-row m2))
      (and (= (mark-row m1) (mark-row m2))
           (< (mark-col m1) (mark-col m2)))))

(define (mark<=? m1 m2)
  (or (mark<? m1 m2)
      (mark=? m1 m2)))

(define (buffer-get-prompt-input b)
  (let ([start (buffer-active-prompt b)]
        [len   (buffer-lines-count b)])
    (let loop ([i      start]
               [pieces '()])
      (if (= i len)
          (if (null? pieces)
              ""
              (string-join (reverse pieces) "\n"))
          (loop (add1 i)
                (cons (line-raw (buffer-line-at b i)) pieces))))))

(define (buffer-new-prompt! b)
  (define row (mark-row (buffer-point b)))
  (set-buffer-prompts! b (append (buffer-prompts b) (list row)))
  (set-buffer-active-prompt! b row)
  (mark-move-to! (buffer-start b) row 0))

(define (buffer-can-edit-here? b)
  (let* ([p (buffer-point b)]
         [row (mark-row p)])
    (or (not (buffer-locked? b))
        (>= row (buffer-active-prompt b)))))

(define (buffer-can-delete-here? b)
  (let* ([p (buffer-point b)]
         [row (mark-row p)]
         [col (mark-col p)])
    (or (not (buffer-locked? b))
        (or (and (= row (buffer-active-prompt b)) (> col 0))
            (> row (buffer-active-prompt b))))))

(define (buffer-set-point! b r c)
  (mark-move-to! (buffer-point b) r c))

(define (point-at-last-line? b)
  (= (mark-row (buffer-point b)) (sub1 (buffer-lines-count b))))

(define (point-at-first-line? b)
  (= (mark-row (buffer-point b)) 0))

(define (point-at-end-of-line? b)
  (= (mark-col (buffer-point b))
     (string-length (line-raw (buffer-line-at-point b)))))

(define (point-at-start-of-line? b)
  (= (mark-col (buffer-point b)) 0))

(define (point-at-end-of-buffer? b)
  (and (point-at-end-of-line? b)
       (point-at-last-line? b)))

(define (point-at-start-of-buffer? b)
  (and (point-at-first-line? b)
       (point-at-start-of-line? b)))

(define (point-end! b)
  (define last-row (sub1 (buffer-lines-count b)))
  (define last-line (buffer-line-at b last-row))
  (buffer-set-point! b last-row (string-length (line-raw last-line))))

(define (point-home! b)
  (buffer-set-point! b (buffer-active-prompt b) 0))

(define (point-forward! b [n 1])
  (let loop ([remaining n])
    (when (> remaining 0)
      (cond
        [(point-at-end-of-buffer? b)
         (set! remaining 0)]
        [(point-at-end-of-line? b)
         (mark-move-to! (buffer-point b)
                        (add1 (mark-row (buffer-point b)))
                        0)
         (loop (sub1 remaining))]
        [else
         (define l (buffer-line-at-point b))
         (define m (min remaining (- (string-length (line-raw l))
                                     (mark-col (buffer-point b)))))
         (mark-move-relative-to! (buffer-point b) 0 m)
         (loop (- remaining m))]))))

(define (point-backward! b [n 1])
  (let loop ([remaining n])
    (when (> remaining 0)
      (cond
        [(point-at-start-of-buffer? b)
         (set! remaining 0)]
        [(point-at-start-of-line? b)
         (mark-move-to! (buffer-point b)
                        (sub1 (mark-row (buffer-point b)))
                        (string-length (line-raw (buffer-line-at b (sub1 (mark-row (buffer-point b)))))))
         (loop (sub1 remaining))]
        [else
          (define m (min remaining (mark-col (buffer-point b))))
          (mark-move-relative-to! (buffer-point b) 0 (- m))
          (loop (- remaining m))]))))

(define (point-down! b [n 1])
  (let loop ([remaining n])
    (when (> remaining 0)
      (cond
        [(point-at-last-line? b)
         (set! remaining 0)]
        [else
         (mark-move-to! (buffer-point b)
                        (add1 (mark-row (buffer-point b)))
                        (min (mark-col (buffer-point b))
                             (string-length (line-raw (buffer-line-at-point b)))))
         (loop (sub1 remaining))]))))

(define (point-up! b [n 1])
  (let loop ([remaining n])
    (when (> remaining 0)
      (cond
        [(point-at-first-line? b)
         (set! remaining 0)]
        [else
         (mark-move-to! (buffer-point b)
                        (sub1 (mark-row (buffer-point b)))
                        (min (mark-col (buffer-point b))
                             (string-length (line-raw (buffer-line-at-point b)))))
         (loop (sub1 remaining))]))))

(define (shift-prompts! b start delta)
  (let ([adjusted
         (let loop ([ps (buffer-prompts b)])
           (if (null? ps)
               '()
               (let* ([p (car ps)]
                      [rest (loop (cdr ps))]
                      [new (if (>= p start) (+ p delta) p)])
                 (cons new rest))))])
    (set-buffer-prompts! b adjusted))
  (when (>= (buffer-active-prompt b) start)
    (set-buffer-active-prompt! b (+ (buffer-active-prompt b) delta)))
  (when (>= (mark-row (buffer-start b)) start)
    (mark-move-to! (buffer-start b)
                   (+ (mark-row (buffer-start b)) delta)
                   (mark-col (buffer-start b)))))

(define (buffer-insert! b char)
  (define line (buffer-line-at-point b))
  (line-dirty! line)
  (define raw (line-raw line))
  (define col (mark-col (buffer-point b)))
  (define before (substring raw 0 col))
  (define after  (substring raw col (string-length raw)))
  (set-line-raw! line (string-append before char after))
  (mark-move-relative-to! (buffer-point b) 0 1))

(define (buffer-insert-raw-lines! b rls)
  (define ls (map make-line rls))
  (define new-lines (vector-insert-list (buffer-lines b)
                                       (mark-row (buffer-point b))
                                       ls))
  (set-buffer-lines! b new-lines)
  (shift-prompts! b (mark-row (buffer-point b)) (length ls)))

(define (buffer-join-lines! b row)
  (when (<= row (- (buffer-lines-count b) 2))
    (define lines (buffer-lines b))
    (define l1 (vector-ref lines row))
    (define l2 (vector-ref lines (add1 row)))
    (line-dirty! l1)
    (set-line-raw! l1 (string-append (line-raw l1) (line-raw l2)))
    (set-buffer-lines! b (vector-remove lines (add1 row)))
    (shift-prompts! b (add1 row) -1)
    (mark-move-to! (buffer-point b) row (string-length (line-raw l1)))))

(define (buffer-delete! b)
  (when (buffer-can-delete-here? b)
    (define col  (mark-col (buffer-point b)))
    (define row  (mark-row (buffer-point b)))
    (define line (buffer-line-at-point b))
    (line-dirty! line)
    (cond
      [(= col 0)
       (when (> row 0)
         (buffer-set-point! b row 0)
         (buffer-join-lines! b (sub1 row)))]
      [else
       (define raw (line-raw line))
       (define before (substring raw 0 (sub1 col)))
       (define after  (substring raw col (string-length raw)))
       (set-line-raw! line (string-append before after))
       (mark-move-relative-to! (buffer-point b) 0 -1)])))

(define (buffer-split-line! b)
  (define p (buffer-point b))
  (define c (mark-col p))
  (define r (mark-row p))
  (define l (buffer-line-at-point b))
  (line-dirty! l)
  (define n (string-length (line-raw l)))
  (define before (substring (line-raw l) 0 c))
  (define after  (substring (line-raw l) c n))
  (set-line-raw! l before)
  (set-buffer-lines! b (vector-insert (buffer-lines b) (add1 r) (make-line after)))
  (mark-move-to! p (add1 r) 0)
  (shift-prompts! b (add1 r) 1))

(define (buffer-replace-editor-with-string! b s)
  (define start-row (buffer-active-prompt b))
  (define current-lines (buffer-lines b))
  (define before-count start-row)
  (define editor-strings
    (let ([pieces (string->lines s)])
      (if (null? pieces)
          '("")
          pieces)))
  (define editor-lines (map make-line editor-strings))
  (define total (+ before-count (length editor-lines)))
  (define new-vector (make-vector total))
  (let loop ([i 0])
    (when (< i before-count)
      (vector-set! new-vector i (vector-ref current-lines i))
      (loop (add1 i))))
  (let loop ([i before-count]
             [rest editor-lines])
    (when (pair? rest)
      (vector-set! new-vector i (car rest))
      (loop (add1 i) (cdr rest))))
  (set-buffer-lines! b new-vector)
  (set-buffer-active-prompt! b start-row)
  (mark-move-to! (buffer-start b) start-row 0)
  (buffer-set-point! b start-row 0))

(define (buffer-total-visible-rows b cols)
  (let loop ([i     0]
             [len   (buffer-lines-count b)]
             [total 0])
    (if (= i len)
        total
        (loop (add1 i)
              len
              (+ total (line-visible-rows b i cols))))))

(define (buffer-display-position b row col cols)
  (define offset       (+ (prompt-offset b row) col))
  (define base-row     (visible-rows-before b row cols))
  (define wrapped-rows (quotient offset cols))
  (define display-row  (+ base-row wrapped-rows))
  (define display-col  (remainder offset cols))
  (values display-row display-col))

(define (buffer-position-from-display b display-row display-col cols)
  (define len         (buffer-lines-count b))
  (define total-rows  (max 1 (buffer-total-visible-rows b cols)))
  (define safe-row    (min (max 0 display-row) (sub1 total-rows)))
  (define safe-col    (min (max 0 display-col) (sub1 cols)))
  (let loop ([row       0]
             [row-start 0])
    (cond
      [(>= row len)
       (define fallback-row (sub1 len))
       (values fallback-row
               (string-length (line-raw (buffer-line-at b fallback-row))))]
      [else
       (define line-rows (line-visible-rows b row cols))
       (define row-end   (+ row-start line-rows))
       (if (< safe-row row-end)
           (let* ([relative    (- safe-row row-start)]
                  [offset      (+ (* relative cols) safe-col)]
                  [prompt-len  (prompt-offset b row)]
                  [raw-len     (string-length (line-raw (buffer-line-at b row)))]
                  [max-offset  (+ prompt-len raw-len)]
                  [clamped     (min (max offset prompt-len) max-offset)]
                  [col         (max 0 (- clamped prompt-len))])
             (values row col))
           (loop (add1 row) row-end))])))

(define (buffer-adjust-first-screen-row! b display-row rows total-rows)
  (define max-first     (max 0 (- total-rows rows)))
  (define current-first (min (buffer-first-screen-row b) max-first))
  (define ensure-visible
    (cond
      [(< display-row current-first)
       display-row]
      [(>= display-row (+ current-first rows))
       (max 0 (- (+ display-row 1) rows))]
      [else
       current-first]))
  (define new-first (min max-first ensure-visible))
  (set-buffer-first-screen-row! b new-first)
  new-first)

;;;
;;; Rendering
;;;

(define (render-line! line prompt-line?)
  (when (not (line-rendered line))
    (define raw (line-raw line))
    (define sanitized
      (let loop ([i 0]
                 [acc '()])
        (if (= i (string-length raw))
            (list->string (reverse acc))
            (let ([ch (string-ref raw i)])
              (loop (add1 i)
                    (cons (if (printable-char? ch) ch #\?) acc))))))
    (define highlighted (highlight sanitized))
    ; (define highlighted sanitized)
    (define prefix (if prompt-line?
                       (string-append ansi-white the-prompt ansi-reset)
                       ""))
    (set-line-rendered! line (string-append prefix highlighted))))

(define (render-buffer-lines! b)
  (let loop ([i 0]
             [len (buffer-lines-count b)])
    (when (< i len)
      (render-line! (buffer-line-at b i) (prompt-line? b i))
      (loop (add1 i) len))))

#;(define (render-buffer->string b)
  (render-buffer-lines! b)
  (define len    (buffer-lines-count b))
  (define pieces (let loop ([i 0]
                            [acc '()])
                   (if (= i len)
                       (reverse acc)
                       (loop (add1 i)
                             (cons (line-rendered (buffer-line-at b i)) acc)))))
  (if (zero? len)
      ""
      (string-join pieces "\r\n")))

(define (prompt-offset b row)
  (if (prompt-line? b row) the-prompt-length 0))

; A buffer line might be longer than a terminal line.

(define (line-visible-length b row)
  (+ (prompt-offset b row)
     (string-length (line-raw (buffer-line-at b row)))))

(define (line-visible-rows b row cols)
  (define width (max 1 cols))
  (define length (max 1 (line-visible-length b row)))
  (quotient (+ length (sub1 width)) width))

(define (visible-rows-before b row cols)
  (let loop ([i 0]
             [total 0])
    (if (= i row)
        total
        (loop (add1 i) (+ total (line-visible-rows b i cols))))))



(define (terminal-goto row col)
  (string-append ESC "[" (number->string row) ";" (number->string col) "H"))

(define (terminal-move-cursor row col)
  (when (and term current-buffer)
    (define b            current-buffer)
    (define cols0        (inexact->exact (xterm-terminal-cols term)))
    (define cols         (if (> cols0 0) cols0 1))
    (define rows0        (inexact->exact (xterm-terminal-rows term)))
    (define rows         (if (> rows0 0) rows0 1))
    (define total-rows   (buffer-total-visible-rows b cols))
    (define-values (display-row display-col)
      (buffer-display-position b row col cols))
    (define first-row (buffer-adjust-first-screen-row!
                       b display-row rows total-rows))
    (xterm-terminal-scroll-to-line term first-row)
    (define viewport-row (- display-row first-row))
    (xterm-terminal-write term
                          (terminal-goto (add1 viewport-row) (add1 display-col))
                          (void))))

(define (render-buffer-in-terminal b)
  (when term
    ; hide cursor
    (xterm-terminal-write term (CSI "?25l") (void))
    ; start renering
    (render-buffer-lines! b)
    (define len   (buffer-lines-count b))
    (define cols0 (inexact->exact (xterm-terminal-cols term)))
    (define cols  (if (> cols0 0) cols0 1))
    (define total-rows
      (let loop ([i 0]
                 [row-offset 0])
        (if (= i len)
            row-offset
            (let* ([line      (buffer-line-at b i)]
                   [rendered  (line-rendered line)]
                   [line-rows (line-visible-rows b i cols)])
              (let clear-loop ([offset 0])
                (when (< offset line-rows)
                  (define target-row (+ row-offset offset 1))
                  (xterm-terminal-write term
                                        (string-append (terminal-goto target-row 1)
                                                       (CSI "2K"))
                                        (void))
                  (clear-loop (add1 offset))))
              (when rendered
                (xterm-terminal-write term
                                      (string-append (terminal-goto (add1 row-offset) 1)
                                                     rendered)
                                      (void)))
              (loop (add1 i) (+ row-offset line-rows))))))
    (xterm-terminal-write term
                          (string-append (terminal-goto (add1 total-rows) 1)
                                         (CSI "J"))
                          (void))
    (define p (buffer-point b))
    (terminal-move-cursor (mark-row p) (mark-col p))
    ; show cursor again
    (xterm-terminal-write term (CSI "?25h") (void))))

(define (render-current-buffer)
  (when current-buffer
    (render-buffer-in-terminal current-buffer)))

;;;
;;; S-expressions
;;;

(define (matches? closer opener)
  (or (and (char=? closer #\)) (char=? opener #\())
      (and (char=? closer #\]) (char=? opener #\[))
      (and (char=? closer #\}) (char=? opener #\{))))

(define (is-balanced? s)
  (let loop ([i          0]
             [stack      '()]
             [in-string? #f]
             [escaped?   #f])
    (cond
      [(>= i (string-length s))
       (and (null? stack) (not in-string?))]
      [else
       (define ch (string-ref s i))
       (cond
         [in-string?
          (cond
            [escaped?
             (loop (add1 i) stack #t #f)]
            [(char=? ch #\\)
             (loop (add1 i) stack #t #t)]
            [(char=? ch #\")
             (loop (add1 i) stack #f #f)]
            [else
             (loop (add1 i) stack #t #f)])]
         [(char=? ch #\")
          (loop (add1 i) stack #t #f)]
         [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
          (loop (add1 i) (cons ch stack) #f #f)]
         [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
          (if (and (pair? stack) (matches? ch (car stack)))
              (loop (add1 i) (cdr stack) #f #f)
              #f)]
         [else
          (loop (add1 i) stack #f #f)])])))

(define (last-opener input)
  (define len   (string-length input))
  (define stack '())
  (let loop ([i          0]
             [in-string? #f]
             [escaped?   #f])
    (when (< i len)
      (define ch (string-ref input i))
      (cond
        [in-string?
         (cond
           [escaped?
            (loop (add1 i) #t #f)]
           [(char=? ch #\\)
            (loop (add1 i) #t #t)]
           [(char=? ch #\")
            (loop (add1 i) #f #f)]
           [else
            (loop (add1 i) #t #f)])]
        [(char=? ch #\")
         (loop (add1 i) #t #f)]
        [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
         (set! stack (cons (cons ch i) stack))
         (loop (add1 i) #f #f)]
        [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
         (when (and (pair? stack)
                    (matches? ch (caar stack)))
           (set! stack (cdr stack)))
         (loop (add1 i) #f #f)]
        [else
         (loop (add1 i) #f #f)])))
  (if (null? stack)
      #f
      (cdar stack)))

(define (matching-closer opener)
  (case opener
    [(#\() #\)]
    [(#\[) #\]]
    [(#\{) #\}]
    [else  #\]]))

(define (matching-closer-string opener)
  (string (matching-closer opener)))


(define (string-index-to-row-and-column input index)
  (define len (string-length input))
  (define row 0)
  (define col 0)
  (define result #f)
  (let loop ([i 0])
    (when (and (not result) (<= i len))
      (cond
        [(= i index)
         (set! result (cons row col))]
        [(= i len)
         (set! result (cons row col))]
        [else
         (define ch (string-ref input i))
         (if (char=? ch #\newline)
             (begin
               (set! row (add1 row))
               (set! col 0))
             (set! col (add1 col)))
         (loop (add1 i))])))
  result)


;;;
;;; Syntax highlighting
;;;

(define all-keywords-names
  '("and" "begin" "cond" "define" "if" "lambda" "let"
    "or" "quote" "set!" "unless" "when"))

(define all-primitives-names '())

(define delimiter-chars
  '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\; #\.))

(define (identifier-delimiter? ch)
  (or (char-whitespace? ch)
      (memv ch delimiter-chars)))

(define (keyword-token? token)
  (and (member token all-keywords-names) #t))

(define (primitive-token? token)
  (and (member token all-primitives-names) #t))

(define (identifier-token? token)
  ; approximate (okay - we are just using it as a filter)
  (and (> (string-length token) 0)
       (let ([first (string-ref token 0)])
         (not (char=? first #\#)))))

(define (bound-token? token)
  ; Note: We are using `get-minischeme-global-env` to make sure
  ;      `minischeme-global-env` is initialized.
  (define env (get-minischeme-global-env))
  (and env
       (identifier-token? token)
       (let ([sym (string->symbol token)])
         (env-bound? env sym))))

(define (highlight-token token)
  (cond
    [(keyword-token? token)   (string-append ansi-keyword   token ansi-reset)]
    [(primitive-token? token) (string-append ansi-primitive token ansi-reset)]
    [(bound-token? token)     (string-append ansi-binding   token ansi-reset)]
    [else token]))

(define (pieces->string pieces)
  (define ordered (reverse pieces))
  (if (null? ordered)
      ""
      (apply string-append ordered)))

(define (highlight input)
  (define len (string-length input))

  (define (emit-token pieces token-chars)
    (cond
      [(null? token-chars)
       pieces]
      [else
       (define hi-token (highlight-token (list->string (reverse token-chars))))
       (cons hi-token pieces)]))
  
  (let loop ([i           0]
             [token-chars '()]
             [pieces      '()]
             [in-string?  #f]
             [escaped?    #f])
    (cond
      [(= i len)
       (pieces->string (emit-token pieces token-chars))]
      [in-string?
       (define ch (string-ref input i))
       (define new-pieces (cons (string ch) pieces))
       (cond
         [escaped?
          (loop (add1 i) token-chars new-pieces #t #f)]
         [(char=? ch #\\)
          (loop (add1 i) token-chars new-pieces #t #t)]
         [(char=? ch #\")
          (loop (add1 i) token-chars new-pieces #f #f)]
         [else
          (loop (add1 i) token-chars new-pieces #t #f)])]
      [else
       (define ch (string-ref input i))
       (cond
         [(char=? ch #\")
          (define flushed (emit-token pieces token-chars))
          (loop (add1 i) '() (cons (string ch) flushed) #t #f)]
         [(identifier-delimiter? ch)
          (define flushed (emit-token pieces token-chars))
          (loop (add1 i) '() (cons (string ch) flushed) #f #f)]
         [else
          (loop (add1 i) (cons ch token-chars) pieces #f #f)])])))

(define (nohighlight input)
  input)

;;;
;;; Syntax highlighting
;;;

;; Temporarily disabled - until support for regular expressions arrive.

;; (define all-keywords-names
;;   '("and" "begin" "cond" "define" "if" "lambda" "let"
;;     "or" "quote" "(set[!])"
;;     "unless" "when"))

;; (define all-primitives-names
;;   '("[+]" "[-]" "[*]" "[/]"
;;     "[=]" "[<]" "[>]"
;;     "([<][=])" "([>][=])"
;;     "null?" "cons" "car" "cdr"
;;     "vector" "vector-ref" "vector-set!"
;;     "boolean?" "number?" "symbol?" "string?" "void?" "procedure?"))

;; (define all-parens
;;   '("[(]" "[)]" "[[]"
;;     "[\\]]" "[{]" "[}]"))

;; (define ansi-regexp   #px"\u001B[@-_][0-?]*[ -/]*[@-~]")

;; (define number-regexp #px"(?<![a-z])([-+]?[0-9](?:\.([0-9]*)?)?)")

;; (define all-keywords-regexp
;;   (regexp (string-append "(?<![a-z])(" (join-with all-keywords-names "|") ")(?![a-z]|[0-9])")))

;; (define all-primitives-regexp
;;   (regexp (string-append "(?<![a-z])(" (join-with all-primitives-names "|") ")(?![a-z]|[0-9])")))

;; (define all-parens-regexp
;;   (regexp (string-append "(" (join-with all-parens "|") ")")))

;; (define (replace-raw-string input needle replacement)
;;   (regexp-replace* needle input replacement))

;; (define (tokenize-ansi input)
;;   (define len (string-length input))
;;   (let loop ([matches (regexp-match-positions* ansi-regexp input)]
;;              [pos 0]
;;              [acc '()])
;;     (if (null? matches)
;;         (let ([tail (if (< pos len) (substring input pos len) "")])
;;           (if (string=? tail "")
;;               (reverse acc)
;;               (reverse (cons tail acc))))
;;         (let* ([match (car matches)]
;;                [start (car match)]
;;                [end (cdr match)]
;;                [before (substring input pos start)]
;;                [code (substring input start end)]
;;                [acc1 (if (string=? before "") acc (cons before acc))])
;;           (loop (cdr matches) end (cons code acc1))))))

;; (define (replace-ansi input needle replacement)
;;   (define tokens (tokenize-ansi input))
;;   (let loop ([ts tokens]
;;              [acc '()])
;;     (if (null? ts)
;;         (string-append* (reverse acc))
;;         (let ([token (car ts)])
;;           (if (regexp-match? ansi-regexp token)
;;               (loop (cdr ts) (cons token acc))
;;               (loop (cdr ts) (cons (regexp-replace* needle token replacement) acc)))))))

;; (define (highlight-numbers input)
;;   (regexp-replace* number-regexp input (string-append ansi-literal "\\1" ansi-reset)))

;; (define (highlight-keywords input)
;;   (replace-ansi input all-keywords-regexp (string-append ansi-keyword "\\1" ansi-reset)))

;; (define (highlight-primitives input)
;;   (replace-ansi input all-primitives-regexp (string-append ansi-primitive "\\1" ansi-reset)))

;; (define (highlight-parens input)
;;   (replace-ansi input all-parens-regexp (string-append ansi-paren "\\1" ansi-reset)))

;; (define (highlight input)
;;   (highlight-primitives
;;    (highlight-keywords
;;     (highlight-numbers input))))

;; (define (nohighlight input)
;;   input)

;;;
;;; Indentation
;;;

(define (first-non-whitespace-index s)
  (let loop ([i 0]
             [len (string-length s)])
    (cond
      [(= i len) #f]
      [(char-whitespace? (string-ref s i)) (loop (add1 i) len)]
      [else i])))

(define (indentation-level b)
  (define p (buffer-point b))
  (define prev-row (if (= (mark-row p) 0)
                       0
                       (max (buffer-active-prompt b) (sub1 (mark-row p)))))
  (define l (buffer-line-at b prev-row))
  (define idx (first-non-whitespace-index (line-raw l)))
  (define base (if idx idx 0))
  (+ base (if (prompt-line? b prev-row) the-prompt-length 0)))

;;;
;;; History
;;;

(define input-history       '())
(define input-history-count 0)
(define history-position    0)
(define history-draft       "")

(define (history-reset!)
  (set! input-history       '())
  (set! input-history-count 0)
  (set! history-position    0)
  (set! history-draft       ""))

(define (history-record-entry! entry)
  (when entry
    (set! input-history       (cons entry input-history))
    (set! input-history-count (add1 input-history-count))))

(define (history-update-draft! b)
  (when b
    (set! history-draft (buffer-get-prompt-input b))))

(define (history-update-after-edit! b)
  (when b
    (set! history-position 0)
    (history-update-draft! b)))

(define (history-prepare-for-new-prompt! b)
  (set! history-position 0)
  (history-update-draft! b))

(define (history-previous! b)
  (when (and b (> input-history-count 0))
    (when (= history-position 0)
      (history-update-draft! b))
    (when (< history-position input-history-count)
      (set! history-position (add1 history-position))
      (define entry (list-ref input-history (sub1 history-position)))
      (buffer-replace-editor-with-string! b entry)
      (point-end! b))))

(define (history-next! b)
  (when (and b (> history-position 0))
    (set! history-position (sub1 history-position))
    (if (= history-position 0)
        (begin
          (buffer-replace-editor-with-string! b history-draft)
          (point-end! b)
          (history-update-after-edit! b))
        (let ([entry (list-ref input-history (sub1 history-position))])
          (buffer-replace-editor-with-string! b entry)
          (point-end! b)))))


;;;
;;; Key and data handling
;;;

(define latest-error #f)

(define (indent-with-spaces! b spaces)
  (let loop ([i 0])
    (when (< i spaces)
      (buffer-insert! b " ")
      (loop (add1 i)))))

(define (editor-at-start? b)
  (and (= (mark-row (buffer-point b)) (mark-row (buffer-start b)))
       (= (mark-col (buffer-point b)) (mark-col (buffer-start b)))))

(define (editor-at-end? b)
  (point-at-end-of-buffer? b))

(define (ee-previous-line)
  (define b current-buffer)
  (when b
    (if (or (editor-at-start? b)
            (> history-position 0))
        (history-previous! b)
        (point-up! b 1))))

(define (ee-next-line)
  (define b current-buffer)
  (when b
    (if (editor-at-end? b)
        (history-next! b)
        (point-down! b 1))))


(define (on-tab)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (indent-with-spaces! b 2)
    (history-update-after-edit! b)))

(define (on-home)
  (define b current-buffer)
  (when b (point-home! b)))

(define (on-newline)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (insert-newline-with-indentation! b)))

(define (on-end)
  (define b current-buffer)
  (when b (point-end! b)))

(define (on-cursor-up)
  #;(define b current-buffer)
  #;(when b (point-up! b 1))
  (ee-previous-line))

(define (on-cursor-down)
  #;(define b current-buffer)
  #;(when b (point-down! b 1))
  (ee-next-line))

(define (on-cursor-right)
  (define b current-buffer)
  (when b (point-forward! b 1)))

(define (on-cursor-left)
  (define b current-buffer)
  (when b (point-backward! b 1)))

(define (on-delete)
  (define b current-buffer)
  (when b
    (buffer-delete! b)
    (history-update-after-edit! b)))

(define (on-delete-right)
  (define b current-buffer)
  (when b
    (when (and (buffer-can-edit-here? b)
               (not (point-at-end-of-buffer? b)))
      (point-forward! b 1)
      (buffer-delete! b)
      (history-update-after-edit! b))))

(define (on-printable-key key)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (buffer-insert! b key)
    (history-update-after-edit! b)))

(define (on-move-to-beginning-of-line)
  (define b current-buffer)
  (when b
    (define p (buffer-point b))
    (point-backward! b (mark-col p))))

(define (on-move-to-end-of-line)
  (define b current-buffer)
  (when b
    (define p (buffer-point b))
    (define l (buffer-line-at-point b))
    (define n (string-length (line-raw l)))
    (point-forward! b (- n (mark-col p)))))

(define (perform-close-paren! b key input index)
  (buffer-insert! b key)
  (history-update-after-edit! b)
  (when index
    (define rowcol     (string-index-to-row-and-column input index))
    (define target-row (+ (buffer-active-prompt b) (car rowcol)))
    (define target-col (cdr rowcol))
    (js-window-set-timeout
     (procedure->external (lambda () (terminal-move-cursor target-row target-col))))
    (js-window-set-timeout/delay
     (procedure->external
      (lambda ()
        (define latest-point (buffer-point b))
        (terminal-move-cursor (mark-row latest-point) (mark-col latest-point))))
     500.0)))

(define (on-close-paren key)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (define input (buffer-get-prompt-input b))
    (define index (last-opener input))
    (perform-close-paren! b key input index)))

(define (on-auto-close-paren key)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (define input      (buffer-get-prompt-input b))
    (define index      (last-opener input))
    (define opener     (and index (string-ref input index)))
    (define closer-key (matching-closer-string opener))
    (perform-close-paren! b closer-key input index)))

(define (insert-newline-with-indentation! b)
  (buffer-split-line! b)
  (define level (indentation-level b))
  (when (> level 0)
    (indent-with-spaces! b level))
  (history-update-after-edit! b))

(define (strip-trailing-return s)
  (define len (string-length s))
  (if (and (> len 0)
           (char=? (string-ref s (sub1 len)) #\return))
      (substring s 0 (sub1 len))
      s))

(define (string->lines s)
  (define len (string-length s))
  (let loop ([i     0]
             [start 0]
             [acc   '()])
    (cond
      [(= i len)
       (define final-line (strip-trailing-return (substring s start len)))
       (reverse (cons final-line acc))]
      [else
       (define ch (string-ref s i))
       (cond
         [(char=? ch #\return)
          (let* ([line     (substring s start i)]
                 [next-pos (if (and (< (add1 i) len)
                                    (char=? (string-ref s (add1 i)) #\newline))
                               (+ i 2)
                               (add1 i))])
            (loop next-pos next-pos (cons line acc)))]
         [(char=? ch #\newline)
          (let ([line (substring s start i)])
            (loop (add1 i) (add1 i) (cons line acc)))]
         [else
          (loop (add1 i) start acc)])])))


(define (on-enter)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (define input (buffer-get-prompt-input b))
    (cond
      [(is-balanced? input)
       ; Evaluate Input

       ;; (def result #f)
       ;;    (try ((:= result (evaluate-and-format input)))
       ;;         (catch error
       ;;           (:= latest-error error)
       ;;           (:= result (+ "error: " error.message))))
          
       #;(define result (with-handlers
                        ([(λ (x) #t) ; catch all
                          (lambda (e)
                            (set! latest-error e)
                            (string-append "error: " (exn-message e)))])
                        (process-input input)))

       (define result (process-input input))
       (set! latest-error #f)
       (history-record-entry! input)

       ; Insert result and make new prompt
       (point-end! b)
       (buffer-split-line! b)
       (buffer-insert-raw-lines! b (string->lines result))
       (point-end! b)
       (buffer-new-prompt! b)
       (history-prepare-for-new-prompt! b)]
      [else
       (insert-newline-with-indentation! b)])))

(define (printable-key? key)
  (and (string? key)
       (= (string-length key) 1)
       (printable-char? (string-ref key 0))))

(define (handle-key-event event . _)
  (define key (js-ref event "key"))
  (define dom-event (js-ref event "domEvent"))
  (when dom-event (js-send dom-event "preventDefault" (vector)))
  (when (string? key)
    ; user pasted something into the terminal
    (set! last-key-data key))
  (cond
    [(equal? key UP)            (on-cursor-up)]
    [(equal? key DOWN)          (on-cursor-down)]
    [(equal? key LEFT)          (on-cursor-left)]
    [(equal? key RIGHT)         (on-cursor-right)]
    [(equal? key ENTER)         (on-enter)]
    [(equal? key TAB)           (on-tab)]
    [(equal? key CTRL-A)        (on-move-to-beginning-of-line)]
    [(equal? key CTRL-D)        (on-delete-right)]
    [(equal? key CTRL-E)        (on-move-to-end-of-line)]
    [(equal? key CTRL-J)        (on-newline)]
    [(equal? key CTRL-P)        (on-cursor-up)]
    [(equal? key CTRL-N)        (on-cursor-down)]
    [(equal? key CTRL-RBRACKET) (on-close-paren "]")]
    [(equal? key HOME)          (on-home)]
    [(equal? key END)           (on-end)]
    [(equal? key DELETE)        (on-delete)]
    [(equal? key DELETE-RIGHT)  (on-delete-right)]
    [(equal? key "]")           (on-auto-close-paren key)]
    [(equal? key ")")           (on-close-paren key)]
    [(equal? key "}")           (on-close-paren key)]
    [else                       (when (printable-key? key)
                                  (on-printable-key key))])
  (render-current-buffer)
  (void))

(define (handle-data-event data . _)
  (when (string? data)
    (js-log (string-append "<data> " data)))
  (cond
    [(and last-key-data (equal? data last-key-data))
     (set! last-key-data #f)]
    [(and (string? data) current-buffer (buffer-can-edit-here? current-buffer))
     (insert-pasted-text! current-buffer data)
     (history-update-after-edit! current-buffer)
     (render-current-buffer)])
  (void))

(define (terminal-cell-dimensions)
  (js-log "terminal-cell-dimensions")
  (define core           (and term (js-ref term "_core")))
  (define render-service (and core (js-ref core "_renderService")))
  (define dims           (and render-service (js-ref render-service "dimensions")))
  (define width          (and dims (js-ref dims "actualCellWidth")))
  (define height         (and dims (js-ref dims "actualCellHeight")))

  (js-log (format "core: ~a, render-service: ~a, dims: ~a, width: ~a, height: ~a"
                  core render-service dims width height))
  
  (if (and (number? width) (number? height) (> width 0) (> height 0))
      (values width height)
      (values #f #f)))

(define (event->viewport-coordinates event)
  (js-log "event->viewport-coordinates")
  (define target (or (js-ref event "currentTarget") (js-ref event "target")))
  (js-log (format "target: ~a" target))
  (define-values (cell-width cell-height) (terminal-cell-dimensions))
  (js-log (format "w,h: ~a, ~a" cell-width cell-height))
  (if (and target cell-width cell-height)
      (let* ([rect      (js-send target "getBoundingClientRect" (vector))]
             [left      (and rect (js-ref rect "left"))]
             [top       (and rect (js-ref rect "top"))]
             [client-x  (js-ref event "clientX")]
             [client-y  (js-ref event "clientY")])
        (if (and (number? left) (number? top)
                 (number? client-x) (number? client-y))
            (let* ([col (inexact->exact (max 0 (floor (/ (- client-x left) cell-width))))]
                   [row (inexact->exact (max 0 (floor (/ (- client-y top) cell-height))))])
              (values row col))
            (values #f #f)))
      (values #f #f)))

(define (move-point-from-viewport! viewport-row viewport-col)
  (when (and term current-buffer)
    (define b            current-buffer)
    (define cols0        (inexact->exact (xterm-terminal-cols term)))
    (define cols         (if (> cols0 0) cols0 1))
    (define display-row  (+ (buffer-first-screen-row b) viewport-row))
    (define display-col  viewport-col)
    (define-values (row col)
      (buffer-position-from-display b display-row display-col cols))
    (buffer-set-point! b row col)
    (render-current-buffer)))

(define (handle-terminal-click event . _)
  (js-log "handle-terminal-click")
  (define-values (row col) (event->viewport-coordinates event))
  (js-log (format "(row,col) = (~a,~a)" row col))
  (when (and row col)
    (move-point-from-viewport! row col))
  (void))

(define (enable-mouse-events!)  
  ; (xterm-terminal-write term (DECSET "1000") (void))
  ; Keep default mouse behavior enabled so users can select text and use
  ; the clipboard via the mouse (using the Clipboard addon).
  (define screen (js-query-selector ".xterm-screen"))
  (js-log "enable-mouse-events!")
  (js-log (format "screen: ~a" screen))
  (when screen
    (set! term-on-click (procedure->external handle-terminal-click))
    (js-add-event-listener! screen "mousedown" term-on-click)))

(define (register-terminal-handlers!)
  (set! term-on-key        (procedure->external handle-key-event))
  (set! term-on-data       (procedure->external handle-data-event))
  (set! term-on-binary     (procedure->external handle-data-event))
  (js-send term "onKey"    (vector term-on-key))
  (js-send term "onData"   (vector term-on-data))
  (js-send term "onBinary" (vector term-on-binary)))

(define (last-item xs)
  (if (null? xs) #f (last xs)))

(define (insert-pasted-text! b data)
  (define row         (mark-row (buffer-point b)))
  (define col         (mark-col (buffer-point b)))
  (define line        (buffer-line-at-point b))
  (define raw         (line-raw line))
  (define before      (substring raw 0 col))
  (define after       (substring raw col (string-length raw)))
  (define pieces      (string->lines data))
  (define first-piece (car pieces))
  (define remaining   (cdr pieces))
  (define new-first   (string-append before first-piece))
  
  (line-dirty! line)
  (set-line-raw! line new-first)
  (cond
    [(null? remaining)
     (set-line-raw! line (string-append new-first after))
     (mark-move-to! (buffer-point b) row (string-length new-first))]
    [else
     (define last-piece (last-item remaining))
     (define middle-pieces
       (let loop ([rest remaining]
                  [acc  '()])
         (if (null? (cdr rest))
             (reverse acc)
             (loop (cdr rest) (cons (car rest) acc)))))
     (define insert-lines (append middle-pieces (list (string-append last-piece after))))
     (buffer-set-point! b (add1 row) 0)
     (buffer-insert-raw-lines! b insert-lines)
     (mark-move-to! (buffer-point b)
                    (+ row (length insert-lines))
                    (string-length last-piece))]))

;;;
;;; MiniScheme
;;;

(define (start-minischeme . _)
  (history-reset!)
  (reset-minischeme-state!)
  
  (define intro-lines
    '("Welcome to MiniScheme."
      ""
      "- What should we evaluate next?"
      ""))

  (define b     (make-buffer "repl"))
  (define lines (list->vector (append (map make-line intro-lines)
                                      (list (make-line "")))))
  (set-buffer-lines! b lines)
  (set-buffer-prompts! b '())
  (mark-move-to! (buffer-point b) (length intro-lines) 0)
  (mark-move-to! (buffer-start b) (length intro-lines) 0)
  (set! current-buffer b)
  (buffer-new-prompt! b)
  (history-prepare-for-new-prompt! b)
  (register-terminal-handlers!)
  (enable-mouse-events!)
  (render-current-buffer)
  (void))


  ;; (for ([line (in-list intro-lines)])
  ;;   (xterm-terminal-writeln term line (void)))

  ;; (define prompt        "> ")
  ;; (define current-input "")

  ;; (define (show-prompt)
  ;;   (set! current-input "")
  ;;   (xterm-terminal-write term prompt (void)))

  ;; (define (echo-line line)
  ;;   (xterm-terminal-writeln term line (void)))

  ;; (define on-data
  ;;   (procedure->external
  ;;    (λ (data . _)
  ;;      (when (string? data)
  ;;        (cond
  ;;          [(string=? data "\u0003")                 ; ETX = End of Text (caused by ctrl-c)
  ;;           (xterm-terminal-write term "^C" (void))
  ;;           (show-prompt)]
  ;;          [(string=? data "\r")                     
  ;;           (xterm-terminal-writeln term "" (void))
  ;;           (echo-line current-input)
  ;;           (show-prompt)]
  ;;          [(string=? data "\u007F")                 ; DELETE
  ;;           (when (> (string-length current-input) 0)
  ;;             (set! current-input
  ;;                   (substring current-input 0 (sub1 (string-length current-input))))
  ;;             (xterm-terminal-write term "\b \b" (void)))]
  ;;          [else
  ;;           (when (printable-string? data)
  ;;             (set! current-input (string-append current-input data))
  ;;             (xterm-terminal-write term data (void)))]))
  ;;      (void))))

  ;; (js-send term "onData" (vector on-data))
  ;; (show-prompt)
  ;; (void))


;;;
;;; EVALUATOR
;;;

(struct env     (table parent))
(struct closure (params body env))
(struct prim    (name proc))
(struct store   (table next) #:mutable)

(struct exn:minischeme:undefined exn:fail (identifier) #:transparent)

;;;
(struct k-apply   (args env))
(struct k-args    (proc rest env values))
(struct k-if      (then else env))
(struct k-begin   (rest env))
(struct k-set!    (addr name))
(struct k-define  (addr name))
(struct k-restore (env))

(define uninitialized (gensym 'uninitialized))
(define no-else       (gensym 'no-else))

(define (make-env parent)
  (env (make-hasheq) parent))

(define (env-bound-current? e name)
  (hash-has-key? (env-table e) name))

(define (env-bound? e name)
  (cond
    [(not e)                     #f]
    [(env-bound-current? e name) #t]
    [else                        (env-bound? (env-parent e) name)]))

(define (env-lookup e name)
  (cond
    [(hash-has-key? (env-table e) name)
     (hash-ref (env-table e) name)]
    [(env-parent e)
     (env-lookup (env-parent e) name)]
    [else
     (define who (if (symbol? name) name 'minischeme))
     (raise (exn:minischeme:undefined
             (format "~a: undefined;\n cannot reference an identifier before its definition" who)
             (current-continuation-marks)
             who))]))

(define (env-define! e name addr)
  (hash-set! (env-table e) name addr))

(define (make-store)
  (store (make-hasheq) 0))

(define (store-alloc! st value)
  (define addr (store-next st))
  (hash-set! (store-table st) addr value)
  (set-store-next! st (add1 addr))
  addr)

(define (store-ref st addr)
  (define v (hash-ref (store-table st) addr (λ () (error 'minischeme "unknown address ~a" addr))))
  (if (eq? v uninitialized)
      (error 'minischeme "accessing uninitialized binding")
      v))

(define (store-set! st addr value)
  (hash-set! (store-table st) addr value))

(define (literal? expr)
  (or (null?    expr)
      (boolean? expr)
      (number?  expr)
      (string?  expr)
      (bytes?   expr)
      (char?    expr)))

(define (ensure-identifier sym)
  (unless (symbol? sym)
    (error 'minischeme "expected identifier, got `~a`" sym)))

(define (ensure-parameters params)
  (unless (list? params)
    (error 'minischeme "invalid parameter list `~a`" params))
  (let loop ([ps params])
    (cond
      [(null? ps) params]
      [(symbol? (car ps)) (loop (cdr ps))]
      [else (error 'minischeme "invalid parameter list `~a`" params)])))

(define (check-numbers name args)
  (for-each (λ (v)
              (unless (number? v)
                (error 'minischeme "`~a` expects numbers, got `~a`" name v)))
            args))

(define (check-arg-count name args expected)
  (unless (= (length args) expected)
    (error 'minischeme "`~a` expects ~a argument~a"
           name expected (if (= expected 1) "" "s"))))

(define (check-at-least name args expected)
  (unless (>= (length args) expected)
    (error 'minischeme "`~a` expects at least ~a argument~a"
           name expected (if (= expected 1) "" "s"))))

(define (create-initial-state)
  (define base-env   (make-env #f))
  (define base-store (make-store))

  (define all-primitives '())
  (define all-constants  '())
  (define all-keywords   (map string->symbol all-keywords-names))
  
  (define (install name proc)
    (define addr (store-alloc! base-store (prim name proc)))
    (env-define! base-env name addr)
    (set! all-primitives (cons name all-primitives)))

  (define (numeric name f)
    (install name (λ (args)
                    (check-numbers name args)
                    (apply f args))))

  (define (constant name value)
    (define addr (store-alloc! base-store value))
    (env-define! base-env name addr)
    (set! all-constants (cons name all-constants)))

  (constant 'null   '())
  (constant 'empty  '())
  (constant 'true   #t)
  (constant 'false  #f)
  
  (numeric '+  +)
  (numeric '*  *)
  (install '-  (λ (args)
                 (check-at-least '- args 1)
                 (check-numbers '- args)
                 (if (null? (cdr args))
                     (- (car args))
                     (apply - args))))
  (install '/  (λ (args)
                 (check-at-least '/ args 1)
                 (check-numbers '/ args)
                 (if (null? (cdr args))
                     (/ 1 (car args))
                     (apply / args))))
  
  (install '=  (λ (args)
                 (check-numbers '= args)
                 (if (null? args) #t (apply = args))))
  (install '<  (λ (args)
                 (check-at-least '< args 2)
                 (check-numbers '< args)
                 (apply < args)))
  (install '<= (λ (args)
                 (check-at-least '<= args 2)
                 (check-numbers '<= args)
                 (apply <= args)))
  (install '>  (λ (args)
                 (check-at-least '> args 2)
                 (check-numbers '> args)
                 (apply > args)))
  (install '>= (λ (args)
                 (check-at-least '>= args 2)
                 (check-numbers '>= args)
                 (apply >= args)))
  
  (install 'cons     (λ (args)
                       (check-arg-count 'cons args 2)
                       (cons (car args) (cadr args))))
  (install 'car      (λ (args)
                       (check-arg-count 'car args 1)
                       (let ([v (car args)])
                         (unless (pair? v)
                           (error 'minischeme "car expects a non-empty pair"))
                         (car v))))
  (install 'cdr      (λ (args)
                       (check-arg-count 'cdr args 1)
                       (let ([v (car args)])
                         (unless (pair? v)
                           (error 'minischeme "cdr expects a non-empty pair"))
                         (cdr v))))
  (install 'list     (λ (args) args))
  (install 'list?    (λ (args)
                       (check-arg-count 'list? args 1)
                       (list? (car args))))
  (install 'pair?    (λ (args)
                       (check-arg-count 'pair? args 1)
                       (pair? (car args))))
  (install 'null?    (λ (args)
                       (check-arg-count 'null? args 1)
                       (null? (car args))))
  
  (install 'symbol?  (λ (args)
                       (check-arg-count 'symbol? args 1)
                       (symbol? (car args))))
  (install 'number?  (λ (args)
                       (check-arg-count 'number? args 1)
                       (number? (car args))))
  (install 'boolean? (λ (args)
                       (check-arg-count 'boolean? args 1)
                       (boolean? (car args))))
  (install 'not      (λ (args)
                       (check-arg-count 'not args 1)
                       (false? (car args))))
  (install 'equal?   (λ (args)
                       (check-arg-count 'equal? args 2)
                       (equal? (car args) (cadr args))))

  ; The syntax highlighter needs to know the names
  ; of the primitives.
  (set! all-primitives-names
        (map symbol->string (sort all-primitives (λ (x y) (symbol<? x y)))))

  (constant 'primitives (sort all-primitives (λ (x y) (symbol<? x y))))
  (constant 'constants  (sort all-constants  (λ (x y) (symbol<? x y))))
  (constant 'keywords   (sort all-keywords   (λ (x y) (symbol<? x y))))
  (constant 'help       (list "Available primitives and constants:"
                              (sort (append all-primitives all-constants)
                                    (λ (x y) (symbol<? x y)))))
  
  ; Return environment and store
  (values base-env base-store))

(define minischeme-global-env   #f)
(define minischeme-global-store #f)

(define (get-minischeme-global-env)
  minischeme-global-env)

(define (reset-minischeme-state!)
  (define-values (env store) (create-initial-state))
  (set! minischeme-global-env   env)
  (set! minischeme-global-store store))

(define (parse-program s)
  (define in (open-input-string s))
  (let loop ([acc '()])
    (define next (read in))
    (if (eof-object? next)
        (reverse acc)
        (loop (cons next acc)))))

(define (value->string v)
  (cond
    [(closure? v)  "#<closure>"]
    [(prim? v)     (format "#<primitive ~a>" (prim-name v))]
    [(void? v)     "#<void>"]
    [else          (format "~s" v)]))

(define (apply-procedure value args env store kont loop)
  (cond
    [(closure? value)
     (define params (closure-params value))
     (when (not (= (length params) (length args)))
       (error 'minischeme "arity mismatch: expected ~a arguments, got ~a"
              (length params) (length args)))
     (define new-env (make-env (closure-env value)))
     (let bind-loop ([ps params] [as args])
       (unless (null? ps)
         (ensure-identifier (car ps))
         (define addr (store-alloc! store (car as)))
         (env-define! new-env (car ps) addr)
         (bind-loop (cdr ps) (cdr as))))
     (define body (closure-body value))
     (if (null? body)
         (loop 'value (void) env store kont)
         (loop 'eval (car body) new-env store
               (cons (k-begin (cdr body) new-env)
                     (cons (k-restore env) kont))))]
    [(prim? value)
     (define result ((prim-proc value) args))
     (loop 'value result env store kont)]
    [else
     (error 'minischeme "application of non-procedure: ~a" value)]))

(define (desugar-let bindings body)
  (define vars '())
  (define vals '())
  (for-each
   (λ (binding)
     (unless (and (pair? binding)
                  (symbol? (car binding))
                  (pair? (cdr binding))
                  (null? (cddr binding)))
       (error 'minischeme "malformed let binding" binding))
     (set! vars (cons (car binding) vars))
     (set! vals (cons (cadr binding) vals)))
   bindings)
  (cons (cons 'lambda (cons (reverse vars) body))
        (reverse vals)))

(define (cesk-evaluate expr env store)
  (let loop ([mode          'eval]
             [control       expr]
             [current-env   env]
             [current-store store]
             [kont          '()])
    (define (continue mode control env store kont)
      (loop mode control env store kont))
    (define (apply-now proc args env store kont)
      (apply-procedure proc args env store kont continue))
    (define (eval-sequence forms env store kont)
      (if (null? forms)
          (continue 'value (void) env store kont)
          (continue 'eval (car forms) env store
                    (cons (k-begin (cdr forms) env) kont))))
    (cond
      [(eq? mode 'eval)
       (cond
         [(literal? control)
          (continue 'value control current-env current-store kont)]
         [(symbol? control)
          (define addr (env-lookup current-env control))
          (continue 'value (store-ref current-store addr) current-env current-store kont)]
         [(and (pair? control) (eq? (car control) 'quote))
          (let ([rest (cdr control)])
            (if (and (pair? rest) (null? (cdr rest)))
                (continue 'value (car rest) current-env current-store kont)
                (error 'minischeme "malformed quote" control)))]
         [(and (pair? control) (eq? (car control) 'lambda))
          (let ([rest (cdr control)])
            (if (and (pair? rest))
                (let ([params (car rest)]
                      [body (cdr rest)])
                  (continue 'value (closure (ensure-parameters params) body current-env)
                            current-env current-store kont))
                (error 'minischeme "malformed lambda" control)))]
         [(and (pair? control) (eq? (car control) 'if))
          (let* ([rest (cdr control)]
                 [len (length rest)])
            (cond
              [(or (< len 2) (> len 3)) (error 'minischeme "malformed if" control)]
              [else
               (define test (car rest))
               (define then (cadr rest))
               (define else-expr (if (= len 3) (caddr rest) no-else))
               (continue 'eval test current-env current-store
                         (cons (k-if then else-expr current-env) kont))]))]
         [(and (pair? control) (eq? (car control) 'begin))
          (eval-sequence (cdr control) current-env current-store kont)]
         [(and (pair? control) (eq? (car control) 'set!))
          (let ([rest (cdr control)])
            (if (and (pair? rest)
                     (symbol? (car rest))
                     (pair? (cdr rest))
                     (null? (cddr rest)))
                (let ([name (car rest)]
                      [rhs (cadr rest)])
                  (define addr (env-lookup current-env name))
                  (continue 'eval rhs current-env current-store
                            (cons (k-set! addr name) kont)))
                (error 'minischeme "malformed set!" control)))]
         [(and (pair? control) (eq? (car control) 'define))
          (let ([rest (cdr control)])
            (cond
              [(and (pair? rest) (symbol? (car rest)))
               (let ([name (car rest)]
                     [tail (cdr rest)])
                 (unless (and (pair? tail) (null? (cdr tail)))
                   (error 'minischeme "malformed define" control))
                 (define addr (if (env-bound-current? current-env name)
                                  (hash-ref (env-table current-env) name)
                                  (let ([a (store-alloc! current-store uninitialized)])
                                    (env-define! current-env name a)
                                    a)))
                 (store-set! current-store addr uninitialized)
                 (continue 'eval (car tail) current-env current-store
                           (cons (k-define addr name) kont)))]
              [(and (pair? rest)
                    (pair? (car rest))
                    (symbol? (caar rest)))
               (let* ([head (car rest)]
                      [name (car head)]
                      [params (cdr head)]
                      [body (cdr rest)])
                 (define addr (if (env-bound-current? current-env name)
                                  (hash-ref (env-table current-env) name)
                                  (let ([a (store-alloc! current-store uninitialized)])
                                    (env-define! current-env name a)
                                    a)))
                 (store-set! current-store addr uninitialized)
                 (define lambda-expr (cons 'lambda (cons params body)))
                 (continue 'eval lambda-expr current-env current-store
                           (cons (k-define addr name) kont)))]
              [else (error 'minischeme "malformed define" control)]))]
         [(and (pair? control) (eq? (car control) 'let))
          (let ([rest (cdr control)])
            (if (and (pair? rest))
                (let ([bindings (car rest)]
                      [body (cdr rest)])
                  (continue 'eval (desugar-let bindings body) current-env current-store kont))
                (error 'minischeme "malformed let" control)))]
         [(pair? control)
          (define op (car control))
          (define args (cdr control))
          (continue 'eval op current-env current-store
                    (cons (k-apply args current-env) kont))]
         [else
          (error 'minischeme "cannot evaluate expression ~a" control)])]
      [(eq? mode 'value)
       (if (null? kont)
           (values control current-env current-store)
           (let* ([frame (car kont)]
                  [rest (cdr kont)])
             (cond
               [(k-apply? frame)
                (define args (k-apply-args frame))
                (define call-env (k-apply-env frame))
                (if (null? args)
                    (apply-now control '() call-env current-store rest)
                    (continue 'eval (car args) call-env current-store
                              (cons (k-args control (cdr args) call-env '()) rest)))]
               [(k-args? frame)
                (define proc (k-args-proc frame))
                (define rest-args (k-args-rest frame))
                (define call-env (k-args-env frame))
                (define collected (cons control (k-args-values frame)))
                (if (null? rest-args)
                    (apply-now proc (reverse collected) call-env current-store rest)
                    (continue 'eval (car rest-args) call-env current-store
                              (cons (k-args proc (cdr rest-args) call-env collected) rest)))]
               [(k-if? frame)
                (define branch-env (k-if-env frame))
                (define then (k-if-then frame))
                (define else-expr (k-if-else frame))
                (if (false? control)
                    (if (eq? else-expr no-else)
                        (continue 'value (void) branch-env current-store rest)
                        (continue 'eval else-expr branch-env current-store rest))
                    (continue 'eval then branch-env current-store rest))]
               [(k-begin? frame)
                (define begin-env (k-begin-env frame))
                (define rest-forms (k-begin-rest frame))
                (if (null? rest-forms)
                    (continue 'value control begin-env current-store rest)
                    (continue 'eval (car rest-forms) begin-env current-store
                              (cons (k-begin (cdr rest-forms) begin-env) rest)))]
               [(k-set!? frame)
                (store-set! current-store (k-set!-addr frame) control)
                (continue 'value control current-env current-store rest)]
               [(k-define? frame)
                (store-set! current-store (k-define-addr frame) control)
                (continue 'value (k-define-name frame) current-env current-store rest)]
               [(k-restore? frame)
                (continue 'value control (k-restore-env frame) current-store rest)]
               [else
                (error 'minischeme "unknown continuation frame ~a" frame)])))]
      [else
       (error 'minischeme "invalid evaluation mode" mode)])))

(define (evaluate-program exprs)
  (let loop ([forms      exprs]
             [last-value (void)])
    (if (null? forms)
        last-value
        (call-with-values
         (λ ()
           (cesk-evaluate (car forms)
                          minischeme-global-env
                          minischeme-global-store))
         (λ (value _env _store)
           (loop (cdr forms) value))))))

(define (process-input s)
  (unless (and minischeme-global-env minischeme-global-store)
    (reset-minischeme-state!))

  (define-values (exprs read-error)
    (with-handlers
      ([exn:fail:read?
        (λ (e)    (values #f (string-append "=> read error: " (exn-message e))))]
       [(λ _ #t)
        (λ (e)    (values #f (string-append "=> read error: " (exn-message e))))])
      
      (values (parse-program s) #f)))

  (if read-error
      read-error
      (with-handlers
        ([exn:minischeme:undefined? (λ (e) (string-append "=> " (exn-message e)))]
         [(λ _ #t)                  (λ _ "evaluation error")])
        (if (null? exprs)
            "=> ; no input"
            (let ([value (evaluate-program exprs)])
              (string-append "=> " (value->string value)))))))
