#lang at-exp racket
;;;
;;; The Little Lisp REPL
;;;

;; This file contains the implementation of "The Little Lisp REPL".
;; The repl is used to illustrate the lisp interpreter written for
;; the blog post on how to implement (in JavaScript) the Chapter 1
;; interpreter from "LISP - Lisp in Small Pieces" by Christian Queinnec.

;; The repl uses a terminal emulator from xterm.js to display the screen.
;; The editing code is implemented here using Urlang
;; (which compiles to JavaScript).

;; /Jens Axel Søgaard

(require (for-syntax racket/base))
(require urlang urlang/for urlang/extra
         syntax/stx syntax/parse racket/syntax)

;;;
;;; Urlang Configuration
;;;

(current-urlang-run?                           #f) ; run using Node?              No: use browser
(current-urlang-echo?                          #t) ; print generated JavaScript?  Yes
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?  Yes (see console)
(current-urlang-beautify?                      #t) ; invoke js-beautify           

;;;
;;; Urlang Helpers
;;;

;; SYNTAX
;;
;; (def (name arg ...) . body)
;; (def name expr)
;;   Define a local function or local variable respectively.
;;   The scope of `name` uses the scope rules of `var`.
(define-urlang-macro def
  (λ (stx)
    (syntax-parse stx
      [(_def (name arg ...) . body)
       (syntax/loc stx
         (var [name (λ (arg ...) . body)]))]
      [(_def name expr)
       (syntax/loc stx
         (var [name expr]))])))

;;;
;;; The REPL
;;;

(urlang
 (urmodule repl
   (import this Math Object window document RegExp String Terminal)
   (import setTimeout)
   ;; from "interpreter.js"
   (import create-initial-env
           evaluate
           parse
           unparse)
   ;;;
   ;;; Helpers
   ;;;

   (define max Math.max)
   (define min Math.min)
   
   ;;;
   ;;; Characters
   ;;;

   ;; (define printable-regexp (new RegExp "^[\\P{Cc}\\P{Cn}\\P{Cs}]+$/$" "gu"))
   ;; (define (printable? c)
   ;;   (not (printable-regexp.test c)))
   
   (define (printable? c)
     (def code (c.codePointAt 0))
     (and (<= 32 code) (<= code 126)))
   
   ;;;
   ;;; Mark
   ;;;

   ;; A mark represented a location between two characters.
   ;; The row and column numbers are zero-indexed.
   ;; (The terminal uses 1-based indices.)

   (define (make-mark row col)
     (object [type "mark"]
             [row row]
             [col col]))

   (define (mark-move-to! mark absolute-row absolute-col)
     (:= mark.row absolute-row)
     (:= mark.col absolute-col))

   (define (mark-move-relative-to! mark delta-row delta-col)
     (:= mark.row (+ mark.row delta-row))
     (:= mark.col (+ mark.col delta-col)))

   (define (mark= m1 m2)
     (and (= m1.row m2.row)
          (= m1.col m2.col)))

   (define (mark< m1 m2)
     (or (< m1.row m2.row)
         (and (= m1.row m2.row)
              (< m1.col m2.col))))

   (define (mark<= m1 m2)
     (or (< m1.row m2.row)
         (and (= m1.row m2.row)
              (<= m1.col m2.col))))

   ;;;
   ;;; Control Sequences
   ;;;

   ; Characters
   (define ESC     "\x1b")
   (define CTRL_A  "\x01")
   (define CTRL_E  "\x05")
   (define CTRL_K  "\v")   
   (define CTRL_L  "\f")   
   (define DELETE  "\x7f")
   (define ENTER   "\r")
   (define TAB     "\t")
   
   ; Control sequences
   (define DELETE-RIGHT (+ ESC "[3~"))
   
   (define UP           (+ ESC "[A"))
   (define DOWN         (+ ESC "[B"))
   (define RIGHT        (+ ESC "[C"))
   (define LEFT         (+ ESC "[D"))
   (define END          (+ ESC "[F"))
   (define HOME         (+ ESC "[H"))
   (define F1           (+ ESC "[BOP"))
   (define F2           (+ ESC "[BOQ"))
   (define PAGEUP       (+ ESC "[5~"))
   (define PAGEDOWN     (+ ESC "[6~"))
       
   
   (define DEL          (+ ESC "[3~"))   ;; Delete right
   
   (define (CSI  cmd)           (+ ESC "[" cmd))
   (define (CSI1 cmd par1)      (+ ESC "[" par1 cmd))
   (define (CSI2 cmd par1 par2) (+ ESC "[" par1 ";" par2 cmd))

   (define (DECSET Pm)          (+ ESC "[" "?" Pm "h"))
   
   
   ;;;
   ;;; Terminal
   ;;;

   (define term        #f)
   (define term-config #f)

   (define (make-terminal-configuration)
     (object [type       "terminal"]
             [row-offset  0]          ; controls horizontal scroll
             [col-offset  0]          ; controls vertical scroll
             [screen-rows term.rows]  ; number of rows on screen
             [screen-cols term.cols]  ; number of columns on screen
             ))
   
   (define (create-terminal)
     (def initial-options
       (object [cursorBlink #t]
               #;[cols        80]
               #;[rows        24]))
     (:= term (new Terminal initial-options))
     (set-terminal-theme)
     (term.open (document.getElementById "terminal"))
     (term.clear)
     
     (term.onKey handle-key-event)

     ; Enable mouse events
     (term.write (DECSET 1000))
     ; (term.write (DECSET 1005))
     ; Install mouse event handler
     (term.onData   handle-data-event)
     (term.onBinary handle-data-event)
     
     (:= term-config (make-terminal-configuration)))
     ;; term.textarea.addEventListener("paste", on_paste)

   (define (set-terminal-theme)
     ;; Changes are detected "by reference", so we need a new object here.
     ;; The (Object.assign (object) source) makes a shallow copy.
     (def theme (Object.assign (object) term.options.theme))
     (:= theme.black          "#073642")
     (:= theme.red            "#DC322F")
     (:= theme.green          "#859900")
     (:= theme.yellow         "#B58900")
     (:= theme.blue           "#268BD2")
     (:= theme.magenta        "#D33682")
     (:= theme.cyan           "#2AA198")
     (:= theme.white          "#EEE8D5")  
     
     ;;  bold is shown as bright
     (:= theme.brightBlack    "#005B72")
     (:= theme.brightRed      "#CB4B16")
     (:= theme.brightGreen    "#586E75")
     (:= theme.brightYellow   "#657B83")
     (:= theme.brightBlue     "#839496")
     (:= theme.brightMagenta  "#6C71C4")
     (:= theme.brightCyan     "#93A1A1")
     (:= theme.brightWhite    "#FDF6E3")

     (:= theme.background     "#012B37") 
     (:= theme.foreground     theme.white) ; "#657B83"
     (:= theme.cursor         "#657B83")
     (:= theme.cursorAccent   "#657B83")

     (:= term.options.theme theme))

   (define (get-terminal-state)
     (object [x          term.buffer.normal.cursorX]     ; 0-indexed
             [y          term.buffer.normal.cursorY]     ; 0-indexed
             [base-y     term.buffer.normal.baseY]       ; Line in buffer where top of bottom page is (when fully scrolled down).
             [viewport-y term.buffer.normal.viewportY]   ; Line where top of viewport is.
             [cols       term.cols]                      ; number of columns
             [rows       term.rows]                      ; number of rows
             ))
   
   (define (handle-key-event e)
     (console.log "handle-key-event")
     (console.log e)
     (def k e.key)
     (def b current-buffer)
     ; Update model
     (case k
       [(UP)           (on-cursor-up    e)]
       [(DOWN)         (on-cursor-down  e)]
       [(LEFT)         (on-cursor-left  e)]
       [(RIGHT)        (on-cursor-right e)]
       [(ENTER)        (on-enter e)]
       [(TAB)          (on-tab e)]
       [(CTRL_A)       (on-move-to-beginning-of-line)]
       [(CTRL_E)       (on-move-to-end-of-line)]
       [(HOME)         (on-home e)]
       [(END)          (on-end e)]
       [(DELETE)       (on-delete e)]
       [(DELETE-RIGHT) (on-delete-right e)]
       [(")")          (on-close-paren e)]
       [else           (when (printable? k)
                         (on-printable-key e))])
     ; Update screen
     (def state (get-terminal-state))
     ; (console.log state)
     (render-buffer-in-terminal b state))
   
   (define (on-tab e)
     (console.log "<tab>"))

   (define (on-home e)
     (console.log "<home>")
     (def b current-buffer)
     (point-home! b))

   (define (on-end e)
     (console.log "<end>")
     (def b current-buffer)
     (point-end! b))

   (define (on-cursor-up e)
     (console.log "<up>")
     (def b current-buffer)
     (point-up! b 1))

   (define (on-cursor-down e)
     (console.log "<down>")
     (def b current-buffer)
     (point-down! b 1))

   (define (on-cursor-right e)
     (console.log "<right>")
     (def b current-buffer)
     (point-forward! b 1))

   (define (on-cursor-left e)
     (console.log "<left>")
     (def b current-buffer)
     (point-backward! b 1))
   
   (define (on-delete e)
     (console.log "<delete>")
     (def b current-buffer)
     (when (buffer-can-delete-here? b)
       (buffer-delete! b)))

   (define (on-delete-right e)
     (console.log "<delete-right>")
     (def b current-buffer)
     (when (buffer-can-edit-here? b)
       (point-forward! b 1)
       (buffer-delete! b)))
   
   (define (terminal-move-cursor r c)
     ; Moves the screen cursor - note that it doesn't move the point.
     ; Used in `on-close-paren`.
     (def ts (get-terminal-state))     
     (def prompt-line?  (not (= (b.prompts.indexOf r) -1)))
     (def prefix-length (if prompt-line? the-prompt-length 0))
     (term.write (+ ESC
                    "[" (max 1 (+ (- r ts.viewport-y) 1))
                    ";" (max 1 (+ c prefix-length 1))
                    "H")))
   
   (define (on-close-paren e)
     (console.log "<on-close-paren>")

     (def k e.key)
     (def input  (buffer-get-prompt-input b))
     (def index  (last-opener input))
     ; Find row and column in the active input
     (def rowcol (string-index-to-row-and-column input index))
     ; Convert to row and column numbers in the buffer
     (def r (+ b.active-prompt rowcol.row))
     (def c rowcol.column)
     ; insert as always
     (buffer-insert! b k)
     (def p b.point)
     (def new-r p.row)
     (def new-c p.col)
     (def move-to-opener (λ () (terminal-move-cursor     r     c)))
     (def move-back      (λ () (terminal-move-cursor new-r new-c)))
     (setTimeout move-to-opener   0)          
     (setTimeout move-back      500))
     
   
   (define (on-printable-key e)
     (def k e.key)
     (def b current-buffer)
     (def p b.point)
     (when (buffer-can-edit-here? b)
       (buffer-insert! b k)))

   (define (on-move-to-beginning-of-line)
     (console.log "<move-to-bol>")
     (def b current-buffer)
     (def p b.point)
     (point-backward! b p.col))

   (define (on-move-to-end-of-line)
     (console.log "<move-to-eol>")
     (def b current-buffer)
     (def p b.point)
     (def l (buffer-line-at-point b))
     (def n l.raw.length)
     (point-forward! b (- n p.col)))

   (define latest-error #f)

   (define (on-enter)
     (console.log "<enter>")
     (def b     current-buffer)
     (def input (buffer-get-prompt-input b))
     (when (buffer-can-edit-here? b)
       (cond
         [(is-balanced? input)
          ; Evaluate input
          (def result #f)
          (try ((:= result (evaluate-and-format input)))
               (catch error
                 (:= latest-error error)
                 (:= result (+ "error: " error.message))))
          ; Insert result and make new prompt
          (point-end! b)
          (buffer-split-line! b)
          (buffer-insert-raw-lines! b (array result))
          (point-end! b)
          (buffer-new-prompt! b)]
         [else
          (buffer-split-line! b)
          (def l (indentation-level b))
          (for ([i in-range 0 l])
            (buffer-insert! b " "))])))
   

   (define (handle-data-event e)
     (console.log "<data>")
     (console.log e))

   
   ;;;
   ;;; Line
   ;;;

   ;; A line consists of the raw line (plain text)
   ;; and the rendered version of the line.
   ;; The lines do not contain \r or \n at the end.

   (define (make-line raw [rendered #f])
     (object [type "line"]
             [raw raw]              ; a string
             [rendered rendered]))  ; a string possibly containing ansi codes - or false

   (define (line-dirty! l)
     (:= l.rendered #f))

   (define the-prompt        "> ") ; may contain ansi codes
   (define the-prompt-length 2)    ; length in raw charaters
   
   (define (render-line! line [prompt-line? #f])
     (def out (for/array ([c in-string line.raw])
                (if (printable? c)
                    c
                    "?")))
     (def rendered   (out.join ""))
     (def highligted (highlight rendered))    
     (def prefix (if prompt-line?
                     (+ ansi-white the-prompt ansi-reset)
                     ""))
     (:= line.rendered (+ prefix highligted)))

   ;;;
   ;;; Buffer
   ;;;

   (define current-buffer #f)

   ;; The buffer represents the text.
   ;; It consists of an array of lines.

   (define (make-buffer name)
     (object [type    "buffer"]
             [name    name]                    ; name of the buffer
             [lines   (array (make-line ""))]  ; array of line
             [point   (make-mark 0 0)]         ; location of cursor
             [start   (make-mark 0 0)]         ; location of first editable position
             
             [prompts (array 0)]               ; lines with prompts
             [active-prompt 0]                 ; start of the current prompt
             [locked? #t]                      ; if locked, only editable after the active prompt
             
             ; screen related
             [first-screen-row 0]              ; 0-indexed
             [screen-col 0]))

   (define (buffer-line-at-point b)
     (ref b.lines b.point.row))
   
   (define (buffer-get-prompt-input b)
     (def ls (b.lines.slice b.active-prompt))
     (def ss (for/array ([l in-array ls])
               l.raw))
     (ss.join "\n"))

   (define (buffer-new-prompt! b)
     (def p b.point)
     (def l p.row)
     (b.prompts.push l)
     (:= b.active-prompt l))

   (define (buffer-can-edit-here? b)
     (def p b.point)
     (or (not b.locked?)
         (>= p.row b.active-prompt)))

   (define (buffer-can-delete-here? b)
     (def p b.point)
     (or (not b.locked?)
         (or (and (= p.row b.active-prompt)
                  (> p.col 0))                  
             (> p.row b.active-prompt))))
   
   (define (buffer-set-point! b r c)
     (def p b.point)
     (:= p.row r)
     (:= p.col c))

   (define (point-at-last-line? b)
     (def p b.point)
     (= p.row (- b.lines.length 1)))

   (define (point-at-first-line? b)
     (def p b.point)
     (= p.row 0))
   
   (define (point-at-end-of-line? b)
     (def p b.point)
     (def l (buffer-line-at-point b))
     (= p.col l.raw.length))

   (define (point-at-start-of-line? b)
     (def p b.point)
     (= p.col 0))

   (define (point-at-end-of-buffer? b)
     (def p b.point)
     (def l (buffer-line-at-point b))
     (and (= p.col l.raw.length)
          (= p.row (- b.lines.length 1))))

   (define (point-at-start-of-buffer? b)
     (def p b.point)
     (def l (buffer-line-at-point b))
     (and (= p.col 0)
          (= p.row 0)))
     
   (define (point-end! b)
     (def p b.point)
     (def i (- b.lines.length 1))
     (def l (ref b.lines i))
     (def n l.raw.length)
     (:= p.row i)
     (:= p.col n))

   (define (point-home! b)
     (def p b.point)
     (def i b.active-prompt)
     (:= p.row i)
     (:= p.col 0))

   (define (point-forward! b [n 1])
     ;; Last column, last line
     ;;   - stay
     ;; Last column, other lines
     ;;   - move to first column of next line
     ;; Other columns
     ;;   - advance
     (def p b.point)     

     (while (> n 0)
       (cond
         [(point-at-end-of-buffer? b)
          (:= n 0)
          "skip"]
         [(point-at-end-of-line? b)   
          (:= p.col 0)
          (+= p.row 1)
          (-= n 1)]
         [else
          (def l (buffer-line-at-point b))
          (def m (min (- l.raw.length p.col) n))
          (+= p.col m)
          (-= n m)]))
     null)

   (define (point-backward! b [n 1])
     ;; First column, first line
     ;;   - stay
     ;; First column, other lines
     ;;   - move to last column of next line
     ;; Other columns
     ;;   - go backward
     (def p b.point)     

     (while (> n 0)
       (cond
         [(point-at-start-of-buffer? b)
          (:= n 0)
          "skip"]
         [(point-at-start-of-line? b)
          (-= p.row 1)
          (def l (buffer-line-at-point b))
          (:= p.col l.raw.length)
          (-= n 1)]
         [else
          (def l (buffer-line-at-point b))
          (def m (min p.col n))
          (-= p.col m)
          (-= n m)]))
     null)

   (define (point-down! b [n 1])
     ;; Last line
     ;;  - stay
     ;; Lines with prompt
     ;   - move point to next line,
     ;   - add length of prompt to column of point
     ;  Other lines
     ;   - move point to next line, keep column
     (def p b.point)
     (while (> n 0)
       (cond
         [(point-at-last-line? b)
          (:= n 0)
          "done"]
         ;; [(point-at-line-with-prompt? b)
         ;;  (+= p.row 1)
         ;;  (def c (+ p.col prompt.length))
         ;;  (def l (buffer-line-at-point b))
         ;;  (def m l.raw.length)
         ;;  (:= p.col (min c m))
         ;;  (-= n 1)]
         [else
          (+= p.row 1)
          (def l (buffer-line-at-point b))
          (def m l.raw.length)
          (:= p.col (min p.col m))
          (-= n 1)]))
     null)

   (define (point-up! b [n 1])
     ;; First line
     ;;  - stay
     ;; Lines with prompt
     ;   - move point to previous line,
     ;   - add length of prompt to column of point
     ;  Other lines
     ;   - move point to previous line, keep column
     (def p b.point)
     (while (> n 0)
       (cond
         [(point-at-first-line? b)
          (:= n 0)
          "done"]
         ;; [(point-at-line-with-prompt? b)
         ;;  (+= p.row 1)
         ;;  (def c (+ p.col prompt.length))
         ;;  (def l (buffer-line-at-point b))
         ;;  (def m l.raw.length)
         ;;  (:= p.col (min c m))
         ;;  (-= n 1)]
         [else
          (-= p.row 1)
          (def l (buffer-line-at-point b))
          (def m l.raw.length)
          (:= p.col (min p.col m))
          (-= n 1)]))
     null)

   (define (buffer-insert! b char)
     ; Insert the character c in the buffer b where the point is.
     ; Move point to the right.
     (def l (buffer-line-at-point b))
     (line-dirty! l)
     (def c b.point.col)
     (def r b.point.row)
     ; Insert c in l
     (def before (l.raw.slice 0 c))
     (def after  (l.raw.slice c))
     (:= l.raw   (+ before char after))
     ; Move point forward
     (:= b.point.col (+ c 1)))
   
   (define (buffer-insert-raw-lines! b rls)
     ; rls is an array of raw strings without \n at the end
     ; Assumes the point is on an empty line.
     (def l  (buffer-line-at-point b))
     (def p  b.point)
     (def ls (rls.map make-line))
     (def n  ls.length)
     (b.lines.splice p.row 0 (spread ls)))

   (define (buffer-join-lines! b row)
     ; TODO: Adjust all marks on and after row.
     ;       Here only point is adjusted.
     
     ; Join lines row and row+1 into a single line.
     ; Place point in the new line, between the two.
     ; Delete row+1.
     (def p b.point)
     (def s b.start)
     (when (<= s.row row)
       (def lines b.lines)
       (def l1 (ref lines row))
       (def l2 (ref lines (+ row 1)))
       ; Join lines
       (line-dirty! l1)
       (def n l1.raw.length)
       (:= l1.raw (+ l1.raw l2.raw))
       ; Move point
       (:= p.row row)
       (:= p.col n)
       ; Delete second line
       (lines.splice (+ row 1) 1)))
   
   (define (buffer-delete! b)
     ; Delete character before point.
     ; If point is at column 0, adjoin two lines.

     ; If the buffer is "locked", then only lines
     ; after the active prompt can be changed.     
     (def p b.point)
     (def s b.start)
     (when (mark< s p)
       (def l (buffer-line-at-point b))
       (line-dirty! l)
       (def c p.col)
       (def r p.row)
       (cond
         [(= c 0)
          (buffer-join-lines! b (- r 1))]
         [else
          ; TODO: Adjust marks on same line after point.
          (def before (l.raw.slice 0 (- c 1)))
          (def after  (l.raw.slice c))
          (:= l.raw   (+ before after))
          (:= b.point.col (- c 1))])))

   (define (buffer-split-line! b)
     ;; Split line at point.
     ;; Place point at the beginning of the second line.
     (def p b.point)
     (def c p.col)
     (def r p.row)

     (def l (buffer-line-at-point b))
     (line-dirty! l)
     (def n l.raw.length)
     
     (def before (l.raw.slice 0 c))
     (def after  (l.raw.slice c n))
     (:= l.raw before)
     (b.lines.splice (+ r 1) 0 (make-line after))
     (+= p.row 1)
     (:= p.col 0))

   
   ;;;          
   ;;; RENDERING
   ;;;          

  
   (define (render-buffer-lines! b)
     (for ([line in-array b.lines])
       (unless line.rendered
         (render-line! line))))

   (define (render-buffer! b)
     (render-buffer-lines! b)
     (def rendered-lines (for/array ([line in-array b.lines]) line.raw))
     (rendered-lines.join "\r\n"))


   (define (adjust-visible-region! b terminal-state)
     (def ts terminal-state)
     ; Make sure point is visible.     
     (def start b.first-screen-row)
     (def end   (+ start ts.rows))
     (def p     b.point)
     (cond
       [(and (<= start p.row) (< p.row end))
        "no adjustment needed"]
       [(< p.row start)
        (:= b.first-screen-row p.row)]
       [(> p.row end)
        (:= b.first-screen-row (- p.row ts.rows))]))
   

   ;       (render-buffer-in-terminal b b.first-screen-row 0                state.rows)
   ;       (render-buffer-in-terminal b row-start    screen-row-start number-of-screen-rows)
   (define (render-buffer-in-terminal b terminal-state)
     (def ts terminal-state)
     ; Make sure point is visible.
     (console.log "<render-buffer-in-terminal>")
     (console.log b)
     (adjust-visible-region! b terminal-state)
     (console.log b)
     ; And now to the rendering.
     (def screen-row-start 0)
     (def screen-row-end   (+ screen-row-start ts.rows))
     ; The lines in the buffer b with indices row-start, row-start+1, ...
     ; are rendered onto the terminal on screen lines screen-row-start, ..., screen-row-end-1.
     (def ts (get-terminal-state))
     (unless (or (< screen-row-end      ts.viewport-y)
                 (> screen-row-start (+ ts.viewport-y ts.rows)))
       ; Clear Screen
       (term.write (+ ESC "[2J")) ; Erase in Display
       ; Print rendered lines
       (def lines b.lines)
       (def from (max    ts.viewport-y          screen-row-start))
       (def to   (min (+ ts.viewport-y ts.rows) screen-row-end))       
       (for ([i in-range from to])
         (when (< i lines.length)
           (term.write (+ ESC "[" (+ i 1) ";" 1 "H"))   ; Set Cursor Position (row, col) 1-based
           (def line (ref lines (+ b.first-screen-row i)))
           (def prompt-line? (not (= (b.prompts.indexOf i) -1)))
           (render-line! line prompt-line?)
           (term.write line.rendered)))
       ; Restore cursor position
       (def p b.point)
       (def prompt-line? (not (= (b.prompts.indexOf p.row) -1)))
       (def prefix-length (if prompt-line? the-prompt-length 0))
       (term.write (+ ESC
                      "[" (max 1 (+ (- p.row ts.viewport-y) 1))
                      ";" (max 1 (+ p.col 1 prefix-length))
                      "H"))))
   
   
   ;;
   ;; S-EXPRESSIONS
   ;;

   ; (is-balanced? <string>)
   ;   Does the string s contain a balanced S-expression?
   ;   Note: For simplicity, we do not handle \" in string literals.
   (define (is-balanced? s)
     (def stack     (array))
     (def in-string? #f)

     (for ([i in-range 0 s.length])
       (def char (ref s i))
       (when (= char "\"")
         (:= in-string? (not in-string?)))

       (cond
         ;; Skip bracket processing if we're inside a string
         [in-string? "skip"]
         ;; Process brackets
         ;;   - Opening brackets
         [(or (= char "(") (= char "[") (= char "{"))
          (stack.push char)]
         ;; Closing brackets
         [(or (= char ")") (= char "]") (= char "}"))
          ; Check if the stack is empty or the top doesn't match the corresponding opener
          (swhen (= stack.length 0)
                 (return #f))
          (def opener (stack.pop))
          (cond
            [(or (and (= char ")") (not (= opener "(")))
                 (and (= char "]") (not (= opener "[")))
                 (and (= char "}") (not (= opener "{"))))
             (let () ; use let to make `return` a statement.
               (return #f)
               "not reached")])]
         ; Otherwise, skip to next
         ))
     ;; If stack is empty, all brackets are balanced
     (and (= stack.length 0)
          (not in-string?)))

   (define (last-opener input)
     ; Find index of last open parenthesis.
     (def stack (array))
     (def (last) (ref stack (- stack.length 1)))
     
     (def openers "([{")
     (def closers ")]}")
     (def matches (object [")" "("] ["]" "["] ["}" "{"]))
     
     (for ([i in-range 0 input.length])
       (def char (ref input i))
       (cond
         [(openers.includes char)
          (stack.push (object [char char] [index i]))] ; store char and its index
         [(closers.includes char)
          ; Check for matching opener
          (when (and (> stack.length 0)
                     (= (dot (last) char)
                        (ref matches char)))
            ; remove matched opener from stack
            (stack.pop))]))
     (if (> stack.length 0)
         (dot (last) index)
         #f))

   
   (define (string-index-to-row-and-column input index)
     ;; The input string contains lines separated by newlines ("\n").
     ;; Convert the index into row and column number.
     (def lines (input.split "\n"))
     (def idx   0)
     (def result #f)
     (for ([i in-range 0 lines.length])
       (unless result
         (def len (+ (dot (ref lines i) length) 1))  ; includes the newline
         (when (< index (+ idx len))
           (def col (- index idx))
           (:= result (object [row i] [column col])))
         (+= idx len)))
     result)

   
   ;;;
   ;;; SYNTAX HIGHLIGHTING
   ;;;

   (define all-keywords-names
     (array "and" "begin" "cond" "define" "if" "lambda" "let"
            "or" "quote" "(set[!])"
            "unless" "when"))
   (define all-primitives-names
     (array "[+]" "[-]" "[*]" "[/]" 
            "[=]" "[<]" "[>]"
            "([<][=])" "([>][=])" 
            "null?" "cons" "car" "cdr"
            "vector" "vector-ref" "vector-set!"
            "boolean?" "number?" "symbol?" "string?" "void?" "procedure?"))
   (define all-parens
     (array "[(]" "[)]" "[[]" "[\\]]" "[{]" "[}]"))
     
   (define ansi-black          "\x1b[30m")
   (define ansi-red            "\x1b[31m")
   (define ansi-green          "\x1b[32m")
   (define ansi-yellow         "\x1b[33m")
   (define ansi-blue           "\x1b[34m")
   (define ansi-magenta        "\x1b[35m")
   (define ansi-cyan           "\x1b[36m")
   (define ansi-white          "\x1b[37m")

   (define ansi-bright-black   "\x1b[30;1m")
   (define ansi-bright-red     "\x1b[31;1m")
   (define ansi-bright-green   "\x1b[32;1m")
   (define ansi-bright-yellow  "\x1b[33;1m")
   (define ansi-bright-blue    "\x1b[34;1m")
   (define ansi-bright-magenta "\x1b[35;1m")
   (define ansi-bright-cyan    "\x1b[36;1m")
   (define ansi-bright-white   "\x1b[37;1m")

   (define ansi-default        "\x1b[39m")
   (define ansi-reset          "\x1b[0m")

   (define ansi-keyword   ansi-yellow)
   (define ansi-primitive ansi-cyan)
   ; (define ansi-literal   ansi-bright-blue)
   (define ansi-literal   ansi-bright-blue)
   (define ansi-error     ansi-bright-red)
   (define ansi-paren     ansi-bright-yellow) ; actually light gray

   ;; The regular expressions matches an CSI control sequence

   (define (replace-raw-string input needle replacement)
     (input.replace needle replacement))

   
   ;; This is a search-and-replace that 
   ;; keeps ansi control sequences untouched.
   (define (replace-ansi input needle replacement)
     (def pos    0)
     (def result (array))
     (def match  #f)

     ; (:= ANSICODE.lastIndex 0)
     (def ANSICODE  (new RegExp "\x1B[@-_][0-?]*[ -/]*[@-~]" "g"))

     (while (not (= (:= match (ANSICODE.exec input)) null))
       (def before  (input.slice pos match.index))                  ; before sequence
       (result.push (replace-raw-string before needle replacement)) ; replace before
       (result.push (ref match 0))                                  ; keep ansi sequence untouched
       (:= pos (+ match.index (dot (ref match 0) length))))         ; repeat

     (def after   (input.slice pos))                                ; after last match
     (result.push (replace-raw-string after needle replacement)) 

     (result.join ""))

   (define (highlight-numbers input)
     (def r (new RegExp "(?<![a-z])([-+]?[0-9]([.]([0-9]*)?)?)" "g"))
     ; since highlight-numbers is called first, we don't need replace-ansi here.
     (input.replace r (+ ansi-literal "$1" ansi-reset)))

   ; The (?<![a-z])      is look behind and checks that there are no letters before the keyword.
   ; The (?![a-z]|[0-9]) is look ahead  and checks that there are no letters or digits after the keyword. 
   (define all-keywords-regexp (+ "(?<![a-z])(" (all-keywords-names.join "|") ")(?![a-z]|[0-9])"))
   (define (highlight-keywords input)
     (def r (new RegExp all-keywords-regexp "g"))
     (replace-ansi input r (+ ansi-keyword "$1" ansi-reset)))

   (define all-primitives-regexp (+ "(?<![a-z])(" (all-primitives-names.join "|") ")(?![a-z]|[0-9])"))
   (define (highlight-primitives input)
     (def r (new RegExp all-primitives-regexp "g"))
     (replace-ansi input r (+ ansi-primitive "$1" ansi-reset)))

   (define all-parens-regexp (+ "(" (all-parens.join "|") ")"))
   (define (highlight-parens input)
     (def r (new RegExp all-parens-regexp "g"))
     (replace-ansi input r (+ ansi-paren "$1" ansi-reset)))

   (define (highlight input)
     (highlight-primitives
      (highlight-keywords
       (highlight-numbers input)
       #;(highlight-parens
          (highlight-numbers input)))))

   (define (nohighlight input)
     input)

   ;;;
   ;;; INDENTATION
   ;;;

   ;; (indentation-level)
   ;;    Find indentation level at point.
   ;;    Use the previous line and find the first non-whitespace character.
   (define (indentation-level b)
     (def p b.point)
     (def r (max b.active-prompt (- p.row 1)))
     (def l (ref b.lines r))
     ;; Find the first non-whitespace character in the last line
     (def first-non-whitespace-index (l.raw.search (new RegExp "\\S")))
     ;; If there are only whitespace characters or empty string, return #f
     (def level (if (= first-non-whitespace-index -1)
                    #f
                    first-non-whitespace-index))
     ;; Add extra indentation after prompt lines
     (def prompt-line? (not (= (b.prompts.indexOf r) -1)))
     (+ level (if prompt-line? the-prompt.length 0)))

   ;;;
   ;;; INTERPRETER API
   ;;;
   
   (def env (create-initial-env))

   (define (evaluate-and-format s)
     (def input-exprs (parse s))
     (console.log "<evaluate-and-format>")
     (console.log (ref input-exprs 0))
     (def result (evaluate (ref input-exprs 0) env))
     (console.log result)
     (unparse result))

   
   ;;;
   ;;; Test
   ;;;

   (define (test)
     "--- Test ---"
     (def b (make-buffer "repl"))
     (:= b.lines 0 (make-line "a"))
     (:= b.lines 1 (make-line "b"))
     (:= b.lines 2 (make-line "c"))
     (buffer-set-point! b 0 1)
     (buffer-insert! b "x")
     (buffer-delete! b)
     (console.log (render-buffer! b))
     (console.log b))

   ;;;
   ;;; Main
   ;;;

   (create-terminal)
   (def b (make-buffer "repl"))
   (:= current-buffer b)
   (render-buffer-in-terminal b (get-terminal-state))))


(define local-xtermjs
  @~a{<link rel="stylesheet" href="node_modules/@"@"xterm/xterm/css/xterm.css" /> 
      <script src="node_modules/@"@"xterm/xterm/lib/xterm.js"></script>})
(define cdn-xtermjs
  @~a{<script src="https://cdn.jsdelivr.net/npm/xterm@"@"5.3.0/lib/xterm.min.js"></script>
      <link href="https://cdn.jsdelivr.net/npm/xterm@"@"5.3.0/css/xterm.min.css" rel="stylesheet">})



(define (generate-html)
  @~a{
 <!DOCTYPE html>
 <html lang="en">
   <head> <meta charset="utf-8">
          <title>The Little Lisp REPL</title>
          @cdn-xtermjs
          <script src="interpreter.js"></script>
  </head>
  <body>   
     <h1>The Little Lisp REPL</h1>
     <div id="terminal"></div>
  
     <p>Keybindings:</p>
     <p>Use <tt>ctrl-a</tt> to move to beginning of line.</p>
     <p>Use <tt>ctrl-e</tt> to move to end of line.</p>
     <p>Use <tt>tab</tt> to indent line.
     <p>Use <tt>enter</tt> new line or evaluate s-expression.
     <p><canvas id="the-canvas" width="350" height="400" tabindex="1"/></p>
     <script>@file->string{repl.js}</script>
 </body>
 </html>})

(define (save-html-as-file)
  (with-output-to-file "repl.html"
    (λ () (displayln (generate-html)))
    #:exists 'replace))


;;; Conveniency

; In DrRacket (send-url/contents some-html) will display the 
; html in a browser - saves time.

;(require net/sendurl)
;(send-url/contents (generate-html))

(save-html-as-file)


;;;
;;; TODO
;;;

;; [ ] Handle window resizing
;; [ ] Handle scrolling for previous-line and next-line.
;; [ ] Copy paste
;; [ ] Place cursor with mouse
;; [ ] Paste using mouse



