;;;
;;; Connections
;;;

;; Connections is a puzzle game.
;; The user is presented 16 words arranged in a 4x4 grid.
;; The job is to group the words into 4 groups,
;; such that all words in a group are related.
;; The user can find one group at a time.

;; The groups in a puzzle are color coded according to difficulty.
;;
;;   yellow: easiest group
;;   green:  medium
;;   blue:   hard
;;   purple: trickiest

;; In a normal puzzle the user is presented with words from
;; four groups - one from each difficulty rating.


(define connections-groups
  (list
   (make-hash
    (list (cons 'id 'yellow)
          (cons 'label "Planets")
          (cons 'words (list "MARS" "VENUS" "EARTH" "SATURN"))))
   (make-hash
    (list (cons 'id 'green)
          (cons 'label "Basic colors")
          (cons 'words (list "RED" "BLUE" "GREEN" "YELLOW"))))
   (make-hash
    (list (cons 'id 'blue)
          (cons 'label "Keyboard keys")
          (cons 'words (list "SHIFT" "ENTER" "SPACE" "TAB"))))
   (make-hash
    (list (cons 'id 'purple)
          (cons 'label "Programming languages")
          (cons 'words (list "RACKET" "PYTHON" "RUBY" "SWIFT"))))))

;; connections-page : -> list?
;;   Build the Connections page SXML structure.
(define (connections-page)
  `(div (@ (class "page page--connections"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel connections-hero"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Puzzle")
                           (span (@ (class "pill")) "DOM + JS FFI")
                           (span (@ (class "pill")) "State machine"))
                      (h1 (@ (class "hero-title")) "Connections")
                      (p (@ (class "hero-lead"))
                         "Find four groups of four related words. "
                         "Select four words, then submit your guess.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (class "connections-shell")
                              (id "connections-root"))
                           (div (@ (class "connections-topline"))
                                (p (@ (id "connections-status")
                                      (class "connections-status"))
                                   "Find groups of four related words.")
                                (p (@ (id "connections-mistakes")
                                      (class "connections-mistakes"))
                                   "Mistakes left: ●●●●"))
                           (div (@ (id "connections-solved")
                                   (class "connections-solved")))
                           (div (@ (id "connections-board")
                                   (class "connections-board")))
                           (div (@ (class "connections-actions"))
                                (button (@ (id "connections-shuffle")
                                           (class "connections-button connections-button--secondary"))
                                        "Shuffle")
                                (button (@ (id "connections-clear")
                                           (class "connections-button connections-button--secondary"))
                                        "Clear")
                                (button (@ (id "connections-submit")
                                           (class "connections-button connections-button--primary"))
                                        "Submit")))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details"))
                           (p "This example shows a small game loop and UI state updates using WebRacket and browser DOM bindings.")
                           (div (@ (class "mathjax-actions"))
                                ,(code-pill (gh-file "web-site-new/src/examples/connections-page.rkt")
                                            "Web-site source")))))
        ,(footer-section)))


(define connections-started? #f)

;; Page-specific injected stylesheet id.
(define connections-style-id "connections-page-style")

;; DOM nodes used by the page.
(define connections-status-node      #f)
(define connections-mistakes-node    #f)
(define connections-solved-node      #f)
(define connections-board-node       #f)
(define connections-shuffle-button   #f)
(define connections-clear-button     #f)
(define connections-submit-button    #f)

;; Stable external callbacks kept reachable from Racket state.
(define connections-submit-callback  #f)
(define connections-shuffle-callback #f)
(define connections-clear-callback   #f)
(define connections-tile-callbacks   '())

;; Mutable game state for selection, progress, and status text.
(define connections-selected-words      '())
(define connections-board-words         '())
(define connections-solved-group-ids    '())
(define connections-remaining-group-ids '())
(define connections-mistakes-left       4)
(define connections-state               'playing)
(define connections-status-text         "Find groups of four related words.")

;; Lookup table from word to owning group id.
(define connections-group-by-word
  (let ([table (make-hash)])
    (for ([group (in-list connections-groups)])
      (define group-id (hash-ref group 'id))
      (for ([word (in-list (hash-ref group 'words))])
        (hash-set! table word group-id)))
    table))

;; connections-group-ids : -> (listof symbol?)
;;   Return the configured group ids in display order.
(define (connections-group-ids)
  (map (λ (group) (hash-ref group 'id)) connections-groups))

;; connections-all-words : -> (listof string?)
;;   Flatten all puzzle words into one list.
(define (connections-all-words)
  (apply append
         (map (λ (group) (hash-ref group 'words))
              connections-groups)))

;; connections-find-group : symbol? -> (or/c hash? #f)
;;   Look up group data by id.
(define (connections-find-group group-id)
  (for/first ([group (in-list connections-groups)]
              #:when (eq? (hash-ref group 'id) group-id))
    group))

;; connections-remove-word : string? (listof string?) -> (listof string?)
;;   Remove word from words.
(define (connections-remove-word word words)
  (filter (λ (w) (not (string=? w word))) words))

;; connections-shuffle : (listof any/c) -> (listof any/c)
;;   Return a shuffled copy of words.
(define (connections-shuffle words)
  (define vec (list->vector words))
  (define len (vector-length vec))
  (for ([i (in-range (sub1 len) 0 -1)])
    (define j (random (add1 i)))
    (define tmp (vector-ref vec i))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp))
  (vector->list vec))

;; connections-unique-words : (listof string?) -> (listof string?)
;;   Keep only first occurrences while preserving order.
(define (connections-unique-words words)
  (let loop ([rest words]
             [seen '()]
             [out '()])
    (cond
      [(null? rest)
       (reverse out)]
      [(member (car rest) seen string=?)
       (loop (cdr rest) seen out)]
      [else
       (loop (cdr rest)
             (cons (car rest) seen)
             (cons (car rest) out))])))

;; connections-word-set=? : (listof string?) (listof string?) -> boolean?
;;   Compare word lists as sets of unique words.
(define (connections-word-set=? a b)
  (define a* (sort (connections-unique-words a) string<?))
  (define b* (sort (connections-unique-words b) string<?))
  (and (= (length a*) (length b*))
       (for/and ([x (in-list a*)]
                 [y (in-list b*)])
         (string=? x y))))

;; connections-overlap-size : (listof string?) (listof string?) -> exact-integer?
;;   Count selected words that also appear in words.
(define (connections-overlap-size selected words)
  (for/sum ([word (in-list (connections-unique-words selected))])
    (if (member word words string=?)
        1
        0)))

;; connections-selected-group-counts : -> hash?
;;   Count selected words by remaining group id.
(define (connections-selected-group-counts)
  (define counts (make-hash))
  (for ([word (in-list (connections-unique-words connections-selected-words))])
    (define group-id (hash-ref connections-group-by-word word #f))
    (when (and group-id (member group-id connections-remaining-group-ids))
      (hash-set! counts group-id (add1 (hash-ref counts group-id 0)))))
  counts)

;; connections-found-group-id : -> (or/c symbol? #f)
;;   Return the matched group id for the current selection.
(define (connections-found-group-id)
  (cond
    [(not (= (length connections-selected-words) 4))
     #f]
    [else
     (define counts (connections-selected-group-counts))
     (for/or ([group-id (in-list connections-remaining-group-ids)])
       (and (= (hash-ref counts group-id 0) 4)
            group-id))]))

;; connections-one-away? : -> boolean?
;;   Determine whether the current selection is one away from a valid group.
(define (connections-one-away?)
  (cond
    [(not (= (length connections-selected-words) 4))
     #f]
    [else
     (define counts (connections-selected-group-counts))
     (for/or ([group-id (in-list connections-remaining-group-ids)])
       (= (hash-ref counts group-id 0) 3))]))

;; connections-mistakes-text : -> string?
;;   Build the mistakes indicator string.
(define (connections-mistakes-text)
  (string-append "Mistakes left: "
                 (make-string (max 0 connections-mistakes-left) #\●)
                 (make-string (max 0 (- 4 connections-mistakes-left)) #\○)))

;; connections-set-text! : any/c string? -> void?
;;   Set textContent on node when present.
(define (connections-set-text! node text)
  (when node
    (js-set! node "textContent" text)))

;; connections-render-controls! : -> void?
;;   Update control disabled states from game state.
(define (connections-render-controls!)
  (define playing? (eq? connections-state 'playing))
  (when connections-submit-button
    (js-set! connections-submit-button
             "disabled"
             (or (not playing?)
                 (not (= (length connections-selected-words) 4)))))
  (when connections-shuffle-button
    (js-set! connections-shuffle-button "disabled" (not playing?)))
  (when connections-clear-button
    (js-set! connections-clear-button "disabled" (not playing?))))

;; connections-render-solved! : -> void?
;;   Render solved groups above the board.
(define (connections-render-solved!)
  (when connections-solved-node
    (js-set! connections-solved-node "innerHTML" "")
    (for ([group-id (in-list connections-solved-group-ids)])
      (define group (connections-find-group group-id))
      (when group
        (define node (js-create-element "div"))
        (js-set-attribute! node
                           "class"
                           (string-append
                            "connections-group "
                            "connections-group--"
                            (symbol->string group-id)))
        (define title (js-create-element "p"))
        (js-set-attribute! title "class" "connections-group-title")
        (js-set! title "textContent" (hash-ref group 'label))
        (define words (js-create-element "p"))
        (js-set-attribute! words "class" "connections-group-words")
        (js-set! words "textContent" (string-join (hash-ref group 'words) ", "))
        (js-append-child! node title)
        (js-append-child! node words)
        (js-append-child! connections-solved-node node)))))

;; connections-toggle-word! : string? -> void?
;;   Toggle selection state for one board word.
(define (connections-toggle-word! word)
  (cond
    [(not (eq? connections-state 'playing))
     (void)]
    [(member word connections-selected-words string=?)
     (set! connections-selected-words
           (connections-remove-word word connections-selected-words))]
    [(>= (length connections-selected-words) 4)
     (set! connections-status-text
           "Only four words can be selected at a time.")]
    [else
     (set! connections-selected-words
           (append connections-selected-words (list word)))]))

;; connections-render-board! : -> void?
;;   Rebuild board tiles and attach click handlers.
(define (connections-render-board!)
  (when connections-board-node
    (set! connections-tile-callbacks '())
    (js-set! connections-board-node "innerHTML" "")
    (for ([word (in-list connections-board-words)])
      (define tile (js-create-element "button"))
      (js-set-attribute! tile "class" "connections-tile")
      (when (member word connections-selected-words string=?)
        (classlist-add! tile "is-selected"))
      (js-set! tile "textContent" word)
      (js-set! tile "disabled" (not (eq? connections-state 'playing)))
      (define cb
        (procedure->external
         (λ (evt)
           (js-event-prevent-default evt)
           (connections-toggle-word! word)
           (connections-render!))))
      (set! connections-tile-callbacks (cons cb connections-tile-callbacks))
      (js-add-event-listener! tile "click" cb)
      (js-append-child! connections-board-node tile))))

;; connections-render! : -> void?
;;   Refresh all UI sections.
(define (connections-render!)
  (connections-set-text! connections-status-node connections-status-text)
  (connections-set-text! connections-mistakes-node (connections-mistakes-text))
  (connections-render-solved!)
  (connections-render-board!)
  (connections-render-controls!))

;; connections-submit! : -> void?
;;   Validate current selection and advance puzzle state.
(define (connections-submit!)
  (cond
    [(not (eq? connections-state 'playing))
     (void)]
    [(not (= (length connections-selected-words) 4))
     (set! connections-status-text "Select exactly four words first.")]
    [else
     (define group-id (connections-found-group-id))
     (cond
       [group-id
        (define solved-group (connections-find-group group-id))
        (set! connections-solved-group-ids
              (append connections-solved-group-ids (list group-id)))
        (set! connections-remaining-group-ids
              (filter (λ (id) (not (eq? id group-id)))
                      connections-remaining-group-ids))
        (when solved-group
          (set! connections-board-words
                (filter (λ (word)
                          (not (member word (hash-ref solved-group 'words) string=?)))
                        connections-board-words)))
        (set! connections-selected-words '())
        (cond
          [(null? connections-remaining-group-ids)
           (set! connections-state 'won)
           (set! connections-status-text "Perfect. You solved all four groups.")]
          [else
           (set! connections-status-text "Correct group found!")])]
       [else
        (set! connections-selected-words '())
        (set! connections-mistakes-left (sub1 connections-mistakes-left))
        (cond
          [(<= connections-mistakes-left 0)
           (set! connections-state 'lost)
           (set! connections-solved-group-ids (connections-group-ids))
           (set! connections-remaining-group-ids '())
           (set! connections-board-words '())
           (set! connections-status-text "No mistakes left. Puzzle revealed.")]
          [else
           (set! connections-status-text
                 (if (connections-one-away?)
                     "One away..."
                     "Not a group. Try again."))])])])
  (connections-render!))

;; connections-clear-selection! : -> void?
;;   Clear current selection while puzzle is active.
(define (connections-clear-selection!)
  (when (eq? connections-state 'playing)
    (set! connections-selected-words '())
    (set! connections-status-text "Selection cleared.")
    (connections-render!)))

;; connections-shuffle-board! : -> void?
;;   Shuffle remaining board words while puzzle is active.
(define (connections-shuffle-board!)
  (when (eq? connections-state 'playing)
    (set! connections-board-words (connections-shuffle connections-board-words))
    (set! connections-status-text "Board shuffled.")
    (connections-render!)))

;; connections-ensure-style! : -> void?
;;   Inject page-specific CSS once.
(define (connections-ensure-style!)
  (define existing (js-get-element-by-id connections-style-id))
  (unless existing
    (define style (js-create-element "style"))
    (js-set-attribute! style "id" connections-style-id)
    (js-set! style
             "textContent"
             (string-append
              ".connections-shell {"
              "  border: 1px solid rgba(255,255,255,0.14);"
              "  border-radius: 16px;"
              "  background: linear-gradient(160deg, rgba(15,19,45,0.9), rgba(14,16,35,0.96));"
              "  box-shadow: 0 18px 34px rgba(0,0,0,0.32);"
              "  padding: 16px;"
              "  display: grid;"
              "  gap: 14px;"
              "}\n"
              ".connections-topline {"
              "  display: flex; flex-wrap: wrap; gap: 10px;"
              "  justify-content: space-between; align-items: baseline;"
              "}\n"
              ".connections-status { margin: 0; color: #dbe2ff; }\n"
              ".connections-mistakes { margin: 0; color: #a8b7ea; font-family: \"Fira Code\", monospace; }\n"
              ".connections-solved { display: grid; gap: 8px; }\n"
              ".connections-group {"
              "  border-radius: 12px; padding: 10px 12px;"
              "  border: 1px solid rgba(255,255,255,0.16);"
              "}\n"
              ".connections-group-title { margin: 0 0 4px; font-weight: 700; color: #161a2f; }\n"
              ".connections-group-words { margin: 0; color: #253160; font-size: 0.92rem; }\n"
              ".connections-group--yellow { background: #f4d35e; border-color: #f1cb44; }\n"
              ".connections-group--green  { background: #96d38c; border-color: #86c97d; }\n"
              ".connections-group--blue   { background: #7fb6ff; border-color: #6aa9ff; }\n"
              ".connections-group--purple { background: #c4a3ff; border-color: #b48ef8; }\n"
              ".connections-board {"
              "  display: grid;"
              "  grid-template-columns: repeat(4, minmax(0, 1fr));"
              "  gap: 8px;"
              "}\n"
              ".connections-tile {"
              "  border: 1px solid rgba(180, 193, 242, 0.3) !important;"
              "  border-radius: 10px !important;"
              "  background: rgba(29, 36, 73, 0.8) !important;"
              "  color: #eef1ff !important;"
              "  min-height: 52px !important;"
              "  font-weight: 700 !important;"
              "  letter-spacing: 0.02em !important;"
              "  transition: transform 110ms ease, border-color 110ms ease, background 110ms ease;"
              "}\n"
              ".connections-tile:hover {"
              "  border-color: rgba(170, 190, 255, 0.65) !important;"
              "  background: rgba(41, 51, 98, 0.94) !important;"
              "  color: #f2f5ff !important;"
              "  transform: translateY(-1px);"
              "}\n"
              ".connections-tile.is-selected {"
              "  border-color: rgba(240, 204, 92, 0.95) !important;"
              "  background: rgba(88, 70, 22, 0.95) !important;"
              "  color: #fff1c0 !important;"
              "}\n"
              ".connections-tile:disabled {"
              "  opacity: 0.6;"
              "  cursor: not-allowed;"
              "  transform: none;"
              "}\n"
              ".connections-actions {"
              "  display: flex; flex-wrap: wrap; gap: 10px; justify-content: flex-end;"
              "}\n"
              ".connections-button {"
              "  border-radius: 999px !important;"
              "  border: 1px solid rgba(170, 190, 255, 0.35) !important;"
              "  font-weight: 650 !important;"
              "  padding: 7px 14px !important;"
              "}\n"
              ".connections-button--primary {"
              "  background: #0f8b8d !important;"
              "  border-color: #0f8b8d !important;"
              "  color: #ffffff !important;"
              "}\n"
              ".connections-button--primary:hover { background: #0c7678 !important; border-color: #0c7678 !important; color: #ffffff !important; }\n"
              ".connections-button--secondary {"
              "  background: rgba(255,255,255,0.06) !important;"
              "  color: #dce5ff !important;"
              "}\n"
              ".connections-button--secondary:hover {"
              "  background: rgba(255,255,255,0.14) !important;"
              "  color: #f2f5ff !important;"
              "}\n"
              ".connections-button:disabled { opacity: 0.5; cursor: not-allowed; }\n"
              "@media (max-width: 800px) {"
              "  .connections-board { grid-template-columns: repeat(2, minmax(0, 1fr)); }"
              "  .connections-actions { justify-content: stretch; }"
              "}\n"))
    (js-append-child! (js-document-head) style)))

;; init-connections-page! : -> void?
;;   Initialize DOM references, handlers, and initial state.
(define (init-connections-page!)
  (unless connections-started?
    (set! connections-started? #t)
    (connections-ensure-style!)
    (set! connections-status-node (js-get-element-by-id "connections-status"))
    (set! connections-mistakes-node (js-get-element-by-id "connections-mistakes"))
    (set! connections-solved-node (js-get-element-by-id "connections-solved"))
    (set! connections-board-node (js-get-element-by-id "connections-board"))
    (set! connections-shuffle-button (js-get-element-by-id "connections-shuffle"))
    (set! connections-clear-button (js-get-element-by-id "connections-clear"))
    (set! connections-submit-button (js-get-element-by-id "connections-submit"))
    (unless (and connections-status-node
                 connections-mistakes-node
                 connections-solved-node
                 connections-board-node
                 connections-shuffle-button
                 connections-clear-button
                 connections-submit-button)
      (error 'connections-page "missing expected DOM nodes for Connections page"))
    (set! connections-submit-callback
          (procedure->external
           (λ (evt)
             (js-event-prevent-default evt)
             (connections-submit!))))
    (set! connections-shuffle-callback
          (procedure->external
           (λ (evt)
             (js-event-prevent-default evt)
             (connections-shuffle-board!))))
    (set! connections-clear-callback
          (procedure->external
           (λ (evt)
             (js-event-prevent-default evt)
             (connections-clear-selection!))))
    (js-add-event-listener! connections-submit-button "click" connections-submit-callback)
    (js-add-event-listener! connections-shuffle-button "click" connections-shuffle-callback)
    (js-add-event-listener! connections-clear-button "click" connections-clear-callback)
    (set! connections-selected-words '())
    (set! connections-board-words (connections-shuffle (connections-all-words)))
    (set! connections-solved-group-ids '())
    (set! connections-remaining-group-ids (connections-group-ids))
    (set! connections-mistakes-left 4)
    (set! connections-state 'playing)
    (set! connections-status-text "Find groups of four related words.")
    (connections-render!)))
