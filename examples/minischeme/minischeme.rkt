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

(define term-on-key    #f)
(define term-on-data   #f)
(define term-on-binary #f)

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
(define CTRL-E  "\u0005")   ; end-of-line
(define CTRL-K  "\v")       ; kill-line
(define CTRL-L  "\f")       ; recenter-top-bottom
(define DELETE  "\u007f")   ; backward-delete-char
(define ENTER   "\r")       ; newline
(define TAB     "\t")       ; indent-for-tab-command

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
(struct buffer (name lines point start prompts active-prompt locked? first-screen-row screen-col)
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
    (let loop ([i start]
               [first? #t]
               [acc ""])
      (if (= i len)
          acc
          (let* ([line (line-raw (buffer-line-at b i))]
                 [piece (if first?
                             line
                             (string-append "\n" line))])
            (loop (add1 i) #f (string-append acc piece)))))))

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
    ; Syntax highlighting is disabled until support for regular expressions arrive.
    ; (define highlighted (highlight sanitized))
    (define highlighted sanitized)
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

(define (render-buffer->string b)
  (render-buffer-lines! b)
  (let loop ([i 0]
             [len (buffer-lines-count b)]
             [acc ""])
    (if (= i len)
        acc
        (let* ([line (line-rendered (buffer-line-at b i))]
               [piece (if (= i (sub1 len))
                          line
                          (string-append line "\r\n"))])
          (loop (add1 i) len (string-append acc piece))))))

(define (prompt-offset b row)
  (if (prompt-line? b row) the-prompt-length 0))

(define (terminal-move-cursor row col)
  (when (and term current-buffer)
    (define offset (prompt-offset current-buffer row))
    (define row-str (number->string (add1 row)))
    (define col-str (number->string (add1 (+ col offset))))
    (xterm-terminal-write term (string-append ESC "[" row-str ";" col-str "H") (void))))

(define (render-buffer-in-terminal b)
  (when term
    (define rendered (render-buffer->string b))
    (xterm-terminal-write term (string-append (CSI "2J") (CSI "H")) (void))
    (xterm-terminal-write term rendered (void))
    (define p (buffer-point b))
    (terminal-move-cursor (mark-row p) (mark-col p))))

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
             [in-string? #f])
    (cond
      [(>= i (string-length s)) (and (null? stack) (not in-string?))]
      [else
       (define ch (string-ref s i))
       (cond
         [in-string?
          (if (char=? ch #\")
              (loop (add1 i) stack #f)
              (loop (add1 i) stack #t))]
         [(char=? ch #\")
          (loop (add1 i) stack #t)]
         [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
          (loop (add1 i) (cons ch stack) #f)]
         [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
          (if (and (pair? stack) (matches? ch (car stack)))
              (loop (add1 i) (cdr stack) #f)
              #f)]
         [else
          (loop (add1 i) stack #f)])])))

(define (last-opener input)
  (define len (string-length input))
  (define stack '())
  (let loop ([i 0])
    (when (< i len)
      (define ch (string-ref input i))
      (cond
        [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
         (set! stack (cons (cons ch i) stack))]
        [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
         (when (and (pair? stack)
                    (matches? ch (caar stack)))
           (set! stack (cdr stack)))])
      (loop (add1 i))))
  (if (null? stack)
      #f
      (cdar stack)))

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
;;; Evaluation helper (simple echo)
;;;

(define (process-input s)
  (string-append "=> " s))

;;;
;;; Key and data handling
;;;

(define latest-error #f)

(define (indent-with-spaces! b spaces)
  (let loop ([i 0])
    (when (< i spaces)
      (buffer-insert! b " ")
      (loop (add1 i)))))

(define (on-tab)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (indent-with-spaces! b 2)))

(define (on-home)
  (define b current-buffer)
  (when b (point-home! b)))

(define (on-end)
  (define b current-buffer)
  (when b (point-end! b)))

(define (on-cursor-up)
  (define b current-buffer)
  (when b (point-up! b 1)))

(define (on-cursor-down)
  (define b current-buffer)
  (when b (point-down! b 1)))

(define (on-cursor-right)
  (define b current-buffer)
  (when b (point-forward! b 1)))

(define (on-cursor-left)
  (define b current-buffer)
  (when b (point-backward! b 1)))

(define (on-delete)
  (define b current-buffer)
  (when b (buffer-delete! b)))

(define (on-delete-right)
  (define b current-buffer)
  (when b
    (when (buffer-can-edit-here? b)
      (point-forward! b 1)
      (buffer-delete! b))))

(define (on-printable-key key)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (buffer-insert! b key)))

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

(define (on-close-paren key)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (define input (buffer-get-prompt-input b))
    (define index (last-opener input))
    (buffer-insert! b key)
    (when index
      (define rowcol (string-index-to-row-and-column input index))
      (define target-row (+ (buffer-active-prompt b) (car rowcol)))
      (define target-col (cdr rowcol))
      (define p (buffer-point b))
      (define new-row (mark-row p))
      (define new-col (mark-col p))
      (js-window-set-timeout
       (procedure->external (lambda () (terminal-move-cursor target-row target-col))))
      (js-window-set-timeout/delay
       (procedure->external (lambda () (terminal-move-cursor new-row new-col)))
       500.0))))

(define (insert-newline-with-indentation! b)
  (buffer-split-line! b)
  (define level (indentation-level b))
  (when (> level 0)
    (indent-with-spaces! b level)))

(define (on-enter)
  (define b current-buffer)
  (when (and b (buffer-can-edit-here? b))
    (define input (buffer-get-prompt-input b))
    (cond
      [(is-balanced? input)
       (define result (with-handlers ([exn:fail?
                                       (lambda (e)
                                         (set! latest-error e)
                                         (string-append "error: " (exn-message e)))])
                         (process-input input)))
       (point-end! b)
       (buffer-split-line! b)
       (buffer-insert-raw-lines! b (list result))
       (point-end! b)
       (buffer-new-prompt! b)]
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
  (cond
    [(equal? key UP)           (on-cursor-up)]
    [(equal? key DOWN)         (on-cursor-down)]
    [(equal? key LEFT)         (on-cursor-left)]
    [(equal? key RIGHT)        (on-cursor-right)]
    [(equal? key ENTER)        (on-enter)]
    [(equal? key TAB)          (on-tab)]
    [(equal? key CTRL-A)       (on-move-to-beginning-of-line)]
    [(equal? key CTRL-E)       (on-move-to-end-of-line)]
    [(equal? key HOME)         (on-home)]
    [(equal? key END)          (on-end)]
    [(equal? key DELETE)       (on-delete)]
    [(equal? key DELETE-RIGHT) (on-delete-right)]
    [(equal? key ")")          (on-close-paren key)]
    [else                      (when (printable-key? key)
                                 (on-printable-key key))])
  (render-current-buffer)
  (void))

(define (handle-data-event data . _)
  (when (string? data)
    (js-log (string-append "<data> " data)))
  (void))

(define (enable-mouse-events!)
  (xterm-terminal-write term (DECSET "1000") (void)))

(define (register-terminal-handlers!)
  (set! term-on-key        (procedure->external handle-key-event))
  (set! term-on-data       (procedure->external handle-data-event))
  (set! term-on-binary     (procedure->external handle-data-event))
  (js-send term "onKey"    (vector term-on-key))
  (js-send term "onData"   (vector term-on-data))
  (js-send term "onBinary" (vector term-on-binary)))

;;;
;;; MiniScheme
;;;

(define (start-minischeme . _)
  (define intro-lines
    '("Welcome to MiniScheme."
      "Type a line and press Enter to see it echoed back."
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

