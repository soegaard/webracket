;;;
;;; MiniScheme page (WebRacket site page)
;;;

(define (minischeme-sample-program lines)
  (let loop ([rest lines])
    (if (null? rest)
        ""
        (string-append (car rest) "\n" (loop (cdr rest))))))

(define minischeme-sample-programs
  (list
   (list "list-statistics"
         "List statistics"
         (minischeme-sample-program
          '(";; List statistics"
            ";;   Compute count, sum, mean, minimum, and maximum for a list of numbers."
            ";;"
            ";; Input data to edit:"
            ";;   Change `xs` below to analyze a different dataset."
            ""
            "(define xs '(12 7 5 22 9 13 18 4 11))"
            ""
            "(define (sum-list ys)"
            "  (cond"
            "    [(null? ys) 0]"
            "    [else       (+ (first ys) (sum-list (rest ys)))]))"
            ""
            "(define (min-list ys)"
            "  (cond"
            "    [(null? (rest ys)) (first ys)]"
            "    [else              (define m (min-list (rest ys)))"
            "                       (if (< (first ys) m) (first ys) m)]))"
            ""
            "(define (max-list ys)"
            "  (cond"
            "    [(null? (rest ys)) (first ys)]"
            "    [else              (define m (max-list (rest ys)))"
            "                       (if (> (first ys) m) (first ys) m)]))"
            ""
            "(define (list-statistics xs)"
            "  (define n     (length xs))"
            "  (define total (sum-list xs))"
            "  (define lo    (min-list xs))"
            "  (define hi    (max-list xs))"
            "  (define mean  (/ total n))"
            "  (list (list 'count n)"
            "        (list 'sum   total)"
            "        (list 'mean  mean)"
            "        (list 'min   lo)"
            "        (list 'max   hi)))"
            ""
            "(list-statistics xs)")))
   (list "histogram"
         "Histogram"
         (minischeme-sample-program
          '(";; Histogram"
            ";;   Group scores into fixed-size bins and count each bin."
            ";;"
            ";; Input data to edit:"
            ";;   Change `scores` and `bin-size` below."
            ""
            "(define scores   '(44 52 71 68 91 83 77 59 62 74 88 93 47 69 72 80))"
            "(define bin-size 10)"
            ""
            "(define (bucket-of x)"
            "  (* bin-size (quotient x bin-size)))"
            ""
            "(define (inc-bucket hist bucket)"
            "  (cond"
            "    [(null? hist)                (list (list bucket 1))]"
            "    [(= bucket (first (first hist)))"
            "     (cons (list bucket (+ 1 (second (first hist))))"
            "           (rest hist))]"
            "    [else"
            "     (cons (first hist)"
            "           (inc-bucket (rest hist) bucket))]))"
            ""
            "(define (build-hist ys hist)"
            "  (cond"
            "    [(null? ys) hist]"
            "    [else       (build-hist (rest ys)"
            "                            (inc-bucket hist (bucket-of (first ys))))]))"
            ""
            "(build-hist scores '())")))
   (list "merge-sort"
         "Merge sort"
         (minischeme-sample-program
          '(";; Merge sort"
            ";;   Sort a list of numbers using merge sort."
            ";;"
            ";; Input data to edit:"
            ";;   Change `xs` below."
            ""
            "(define xs '(22 7 1 45 3 12 9 18 6 30))"
            ""
            "(define (split ys)"
            "  (cond"
            "    [(or (null? ys) (null? (rest ys))) (list ys '())]"
            "    [else                               (define parts (split (rest (rest ys))))"
            "                                        (list (cons (first ys)  (first parts))"
            "                                              (cons (second ys) (second parts)))]))"
            ""
            "(define (merge left right)"
            "  (cond"
            "    [(null? left)                     right]"
            "    [(null? right)                    left]"
            "    [(<= (first left) (first right))  (cons (first left)  (merge (rest left) right))]"
            "    [else                             (cons (first right) (merge left (rest right)))]))"
            ""
            "(define (merge-sort ys)"
            "  (cond"
            "    [(or (null? ys) (null? (rest ys))) ys]"
            "    [else                              (define parts (split ys))"
            "                                       (define left  (first parts))"
            "                                       (define right (second parts))"
            "                                       (merge (merge-sort left)"
            "                                              (merge-sort right))]))"
            ""
            "(merge-sort xs)")))
   (list "run-length"
         "Run-length encode/decode"
         (minischeme-sample-program
          '(";; Run-length encode/decode"
            ";;   Encode repeated values as (value count) pairs, or decode them."
            ";;"
            ";; Input data to edit:"
            ";;   Change `mode` and `input` below."
            ""
            "(define mode  'encode) ; set to 'decode to decode input"
            "(define input '(a a a b b c c c c d a a))"
            ""
            "(define (replicate x n)"
            "  (cond"
            "    [(= n 0) '()]"
            "    [else    (cons x (replicate x (- n 1)))]))"
            ""
            "(define (decode pairs)"
            "  (cond"
            "    [(null? pairs) '()]"
            "    [else          (append (replicate (first (first pairs)) (second (first pairs)))"
            "                           (decode (rest pairs)))]))"
            ""
            "(define (encode xs)"
            "  (define (finish value count acc)"
            "    (cons (list value count) acc))"
            "  (let loop ([ys xs] [current #f] [count 0] [acc '()])"
            "    (cond"
            "      [(null? ys)                  (if (= count 0)"
            "                                      (reverse acc)"
            "                                      (reverse (finish current count acc)))]"
            "      [(= count 0)                 (loop (rest ys) (first ys) 1 acc)]"
            "      [(equal? (first ys) current) (loop (rest ys) current (+ count 1) acc)]"
            "      [else                        (loop (rest ys)"
            "                                        (first ys)"
            "                                        1"
            "                                        (finish current count acc))])))"
            ""
            "(cond"
            "  [(eq? mode 'encode) (encode input)]"
            "  [else               (decode input)])")))
   (list "prime-sieve"
         "Prime sieve up to N"
         (minischeme-sample-program
          '(";; Prime sieve up to N"
            ";;   Compute all primes from 2 up to a limit."
            ";;"
            ";; Input data to edit:"
            ";;   Change `limit` below."
            ""
            "(define limit 100)"
            ""
            "(define (range from to)"
            "  (cond"
            "    [(> from to) '()]"
            "    [else        (cons from (range (+ from 1) to))]))"
            ""
            "(define (remove-multiples p xs)"
            "  (cond"
            "    [(null? xs)                    '()]"
            "    [(= (remainder (first xs) p) 0) (remove-multiples p (rest xs))]"
            "    [else                           (cons (first xs)"
            "                                          (remove-multiples p (rest xs)))]))"
            ""
            "(define (sieve xs)"
            "  (cond"
            "    [(null? xs) '()]"
            "    [else       (cons (first xs)"
            "                      (sieve (remove-multiples (first xs) (rest xs))))]))"
            ""
            "(cond"
            "  [(< limit 2) '()]"
            "  [else        (sieve (range 2 limit))])")))
   (list "matrix-multiply"
         "Matrix multiply"
         (minischeme-sample-program
          '(";; Matrix multiply"
            ";;   Multiply matrix A by matrix B."
            ";;"
            ";; Input data to edit:"
            ";;   Change matrices `A` and `B` below."
            ""
            "(define A '((1 2 3)"
            "            (4 5 6)))"
            "(define B '((7 8)"
            "            (9 10)"
            "            (11 12)))"
            ""
            "(define (column m j)"
            "  (cond"
            "    [(null? m) '()]"
            "    [else      (cons (list-ref (first m) j)"
            "                     (column (rest m) j))]))"
            ""
            "(define (transpose m)"
            "  (cond"
            "    [(null? m) '()]"
            "    [else      (define cols (length (first m)))"
            "               (let loop ([j 0])"
            "                 (cond"
            "                   [(= j cols) '()]"
            "                   [else       (cons (column m j)"
            "                                     (loop (+ j 1)))]))]))"
            ""
            "(define (dot xs ys)"
            "  (cond"
            "    [(null? xs) 0]"
            "    [else       (+ (* (first xs) (first ys))"
            "                   (dot (rest xs) (rest ys)))]))"
            ""
            "(define (mul-row row cols)"
            "  (cond"
            "    [(null? cols) '()]"
            "    [else         (cons (dot row (first cols))"
            "                        (mul-row row (rest cols)))]))"
            ""
            "(define (matmul a b)"
            "  (define bt (transpose b))"
            "  (let loop ([rows a])"
            "    (cond"
            "      [(null? rows) '()]"
            "      [else         (cons (mul-row (first rows) bt)"
            "                         (loop (rest rows)))])))"
            ""
            "(matmul A B)")))
   (list "linear-regression"
         "Linear regression (least squares)"
         (minischeme-sample-program
          '(";; Linear regression (least squares)"
            ";;   Fit a line y = slope*x + intercept to data points."
            ";;"
            ";; Input data to edit:"
            ";;   Change `points` below."
            ""
            "(define points '((1 1.4)"
            "                 (2 1.9)"
            "                 (3 3.2)"
            "                 (4 3.8)"
            "                 (5 5.1)"
            "                 (6 5.9)))"
            ""
            "(define (sum f xs)"
            "  (apply + (map f xs)))"
            ""
            "(define n         (length points))"
            "(define sx        (sum first  points))"
            "(define sy        (sum second points))"
            "(define sxx       (sum (lambda (p) (* (first p) (first p)))   points))"
            "(define sxy       (sum (lambda (p) (* (first p) (second p)))  points))"
            "(define denom     (- (* n sxx) (* sx sx)))"
            "(define slope     (/  (- (* n sxy) (* sx sy))  denom))"
            "(define intercept (/  (- sy (* slope sx))      n))"
            ""
            "(list (list 'slope     slope)"
            "      (list 'intercept intercept))")))
   (list "bfs-shortest-path"
         "BFS shortest path"
         (minischeme-sample-program
          '(";; BFS shortest path"
            ";;   Find a shortest path between source and target in an unweighted graph."
            ";;"
            ";; Input data to edit:"
            ";;   Change `graph`, `source`, and `target` below."
            ""
            "(define graph '((A (B C))"
            "                (B (A D E))"
            "                (C (A F))"
            "                (D (B G))"
            "                (E (B G H))"
            "                (F (C H))"
            "                (G (D E I))"
            "                (H (E F I))"
            "                (I (G H))))"
            "(define source 'A)"
            "(define target 'I)"
            ""
            "(define (neighbors node g)"
            "  (define entry (assoc node g))"
            "  (cond"
            "    [entry (second entry)]"
            "    [else  '()]))"
            ""
            "(define (enqueue-neighbors ns path q seen-now)"
            "  (cond"
            "    [(null? ns) (list q seen-now)]"
            "    [else       (define n (first ns))"
            "                (cond"
            "                 [(member n seen-now)"
            "                  (enqueue-neighbors (rest ns) path q seen-now)]"
            "                 [else"
            "                  (enqueue-neighbors (rest ns)"
            "                                     path"
            "                                     (append q (list (cons n path)))"
            "                                     (cons n seen-now))])]))"
            ""
            "(define (bfs queue seen)"
            "  (cond"
            "    [(null? queue) #f]"
            "    [else          (define path (first queue))"
            "                   (define node (first path))"
            "                   (cond"
            "                     [(eq? node target) (reverse path)]"
            "                     [else              (define state"
            "                                          (enqueue-neighbors (neighbors node graph)"
            "                                                             path"
            "                                                             (rest queue)"
            "                                                             seen))"
            "                                        (define new-queue (first state))"
            "                                        (define new-seen  (second state))"
            "                                        (bfs new-queue new-seen)])]))"
            ""
            "(bfs (list (list source)) (list source))")))
   (list "continuations"
         "Continuation demo (call/cc + dynamic-wind)"
         (minischeme-sample-program
          '(";; Continuation demo (call/cc + dynamic-wind)"
            ";;   Exit early when a value is found and trace dynamic-wind enter/leave."
            ";;"
            ";; Input data to edit:"
            ";;   Change `data`, `target`, and `trace?` below."
            ""
            "(define data   '(3 7 11 18 24 31))"
            "(define target 18)"
            "(define trace? #t)"
            ""
            "(define trace-log '())"
            ""
            "(define (note x)"
            "  (cond"
            "    [trace? (set! trace-log (cons x trace-log))]"
            "    [else   'ok]))"
            ""
            "(define (find-first xs wanted)"
            "  (call/cc"
            "   (lambda (exit)"
            "     (dynamic-wind"
            "      (lambda () (note 'enter))"
            "      (lambda () (let loop ([ys xs])"
            "                  (cond"
            "                   [(null? ys)             #f]"
            "                   [(= (first ys) wanted)  (exit (first ys))]"
            "                   [else                   (loop (rest ys))])))"
            "      (lambda () (note 'leave))))))"
            ""
            "(list (list 'found (find-first data target))"
            "      (list 'trace (reverse trace-log)))")))
   (list "newton-root"
         "Newton root finder"
         (minischeme-sample-program
          '(";; Newton root finder"
            ";;   Solve `f(x) = 0` using Newton's method."
            ";;"
            ";; Input data to edit:"
            ";;   Change `target`, `guess`, `tolerance`, and `max-steps` below."
            ""
            ";; Let's solve `x^2 - 2 = 0` to find the square root of two."
            ""
            "(define (f x)"
            "  (- (* x x) target))"
            ""
            "(define (df x) ; the derivative of f"
            "  (* 2.0 x))"
            ""
            "(define target    2.0)      ; solve f(x) = target"
            "(define guess     1.0)      ; initial guess for a root"
            "(define tolerance 0.000001) "
            "(define max-steps 20)"
            ""
            "(define (newton x step)"
            "  (define fx (f x))"
            "  (cond"
            "    [(or (= step max-steps)"
            "         (< (abs fx) tolerance))"
            "     x]"
            "    [else"
            "     (newton (- x (/ fx (df x)))"
            "             (+ step 1))]))"
            ""
            "(define root (newton guess 0))"
            "(list (list 'root      root)"
            "      (list 'f-at-root (f root)))")))
   (list "maze-solver"
         "Maze solver"
         (minischeme-sample-program
          '(";; Maze solver"
            ";;   Find a path from S to G in an ASCII/Unicode maze and render the path."
            ";;"
            ";; Input data to edit:"
            ";;   Change `maze` below."
            ""
            "; S = Start"
            "; G = Goal"
            "(define maze"
            "  '(\"┌─────────────┬─────────────┐\""
            "    \"│S            │            G│\""
            "    \"│ ┌─────┐ ┌───┴───┐ ┌─────┐ │\""
            "    \"│ │     │ │       │ │     │ │\""
            "    \"│ └─────┘ └─────┬─┘ └─────┘ │\""
            "    \"│               │           │\""
            "    \"│ ┌─────┐ ┌─────┼─────┐ ┌─┐ │\""
            "    \"│ │     │ │               │ │\""
            "    \"├─┼───┐ │ │ ┌─────┬─────┐ │ ├\""
            "    \"│ │   │ │ │ │     │     │ │ │\""
            "    \"│ │   └─┘ │ └─────┼─────┘ │ │\""
            "    \"│ │       │               │ │\""
            "    \"│ └─────┐ │ ┌─────┬─────┐ │ │\""
            "    \"│       │ │ │     │     │ │ │\""
            "    \"│ ┌─────┘ └───┼───┼─────┘ │ │\""
            "    \"│ │           │           │ │\""
            "    \"│ └─────┐ ┌───┼───┐ ┌─────┘ │\""
            "    \"│       │ │       │ │       │\""
            "    \"│ ┌─────┘ └───────┘ └─────┐ │\""
            "    \"│                         │ │\""
            "    \"└───────────────────────────┘\"))"
            ""
            "(define height (length maze))"
            "(define width"
            "  (cond"
            "    [(null? maze) 0]"
            "    [else         (string-length (first maze))]))"
            ""
            "(define (cell-at cell)"
            "  (string-ref (list-ref maze (second cell)) (first cell)))"
            ""
            "(define (inside? cell)"
            "  (and (<= 0 (first cell))"
            "       (< (first cell) width)"
            "       (<= 0 (second cell))"
            "       (< (second cell) height)))"
            ""
            "(define wall-chars"
            "  '(#\\─ #\\│ #\\┌ #\\┐ #\\└ #\\┘ #\\├ #\\┤ #\\┬ #\\┴ #\\┼))"
            ""
            "(define (wall? ch)"
            "  (member ch wall-chars))"
            ""
            "(define (blocked? cell)"
            "  (wall? (cell-at cell)))"
            ""
            "(define (find-marker marker)"
            "  (let loopy ([y 0])"
            "    (cond"
            "      [(= y height) #f]"
            "      [else         (let loopx ([x 0])"
            "                      (cond"
            "                        [(= x width) (loopy (+ y 1))]"
            "                        [(char=? (string-ref (list-ref maze y) x) marker)"
            "                         (list x y)]"
            "                        [else       (loopx (+ x 1))]))])))"
            ""
            "(define start (find-marker #\\S))"
            "(define goal  (find-marker #\\G))"
            ""
            "(define (neighbors cell)"
            "  (define x (first cell))"
            "  (define y (second cell))"
            "  (define candidates"
            "    (list (list (+ x 1) y)"
            "          (list (- x 1) y)"
            "          (list x (+ y 1))"
            "          (list x (- y 1))))"
            "  (let loop ([cs candidates])"
            "    (cond"
            "      [(null? cs) '()]"
            "      [(and (inside? (first cs))"
            "            (not (blocked? (first cs))))"
            "       (cons (first cs) (loop (rest cs)))]"
            "      [else (loop (rest cs))])))"
            ""
            "(define (enqueue-neighbors ns path q seen-now)"
            "  (cond"
            "    [(null? ns) (list q seen-now)]"
            "    [else       (define n (first ns))"
            "                (cond"
            "                 [(member n seen-now)"
            "                  (enqueue-neighbors (rest ns) path q seen-now)]"
            "                 [else"
            "                  (enqueue-neighbors (rest ns)"
            "                                     path"
            "                                     (append q (list (cons n path)))"
            "                                     (cons n seen-now))])]))"
            ""
            "(define (solve queue seen)"
            "  (cond"
            "    [(null? queue) #f]"
            "    [else          (define path (first queue))"
            "                   (define node (first path))"
            "                   (cond"
            "                     [(equal? node goal) (reverse path)]"
            "                     [else                (define state"
            "                                          (enqueue-neighbors (neighbors node)"
            "                                                             path"
            "                                                             (rest queue)"
            "                                                             seen))"
            "                                        (define new-queue (first state))"
            "                                        (define new-seen  (second state))"
            "                                        (solve new-queue new-seen)])]))"
            ""
            "(define (path-cell? c path)"
            "  (and path (member c path)))"
            ""
            "(define (render-cell cell path)"
            "  (cond"
            "    [(equal? cell start)      #\\S]"
            "    [(equal? cell goal)       #\\G]"
            "    [(wall? (cell-at cell))   (cell-at cell)]"
            "    [(path-cell? cell path)   #\\*]"
            "    [else                     #\\.]))"
            ""
            "(define (render-row y path)"
            "  (let loop ([x 0])"
            "    (cond"
            "      [(= x width) '()]"
            "      [else        (cons (render-cell (list x y) path)"
            "                         (loop (+ x 1)))])))"
            ""
            "(define (join-lines lines)"
            "  (cond"
            "    [(null? lines)       \"\"]"
            "    [(null? (rest lines)) (first lines)]"
            "    [else                 (string-append (first lines)"
            "                                        \"\\n\""
            "                                         (join-lines (rest lines)))]))"
            ""
            "(define (render-maze path)"
            "  (let loop ([y 0])"
            "    (cond"
            "      [(= y height) '()]"
            "      [else         (cons (list->string (render-row y path))"
            "                         (loop (+ y 1)))])))"
            ""
            "(define path"
            "  (cond"
            "    [(and start goal) (solve (list (list start)) (list start))]"
            "    [else             #f]))"
            ""
            "(cond"
            "  [path (displayln (join-lines (render-maze path)))]"
            "  [else \"No path found.\"])")))))

(define minischeme-default-sample-id
  (if (null? minischeme-sample-programs)
      ""
      (car (car minischeme-sample-programs))))

(define minischeme-default-sample-program
  (if (null? minischeme-sample-programs)
      ""
      (caddr (car minischeme-sample-programs))))

(define (minischeme-sample-title-by-id sample-id)
  (let loop ([entries minischeme-sample-programs])
    (cond
      [(null? entries) "Sample program"]
      [(string=? (car (car entries)) sample-id) (cadr (car entries))]
      [else (loop (cdr entries))])))

(define (minischeme-sample-source-by-id sample-id)
  (let loop ([entries minischeme-sample-programs])
    (cond
      [(null? entries) minischeme-default-sample-program]
      [(string=? (car (car entries)) sample-id) (caddr (car entries))]
      [else (loop (cdr entries))])))

(define minischeme-codemirror-css-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/codemirror.min.css")
(define minischeme-codemirror-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/codemirror.min.js")
(define minischeme-codemirror-scheme-mode-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/mode/scheme/scheme.min.js")
(define minischeme-codemirror-matchbrackets-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/addon/edit/matchbrackets.min.js")
(define minischeme-codemirror-closebrackets-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/addon/edit/closebrackets.min.js")
(define minischeme-storage-key
  "webracket:minischeme:source")

(include "minischeme/docs-data.rkt")

(define (minischeme-doc-section key)
  (define entry (assq key minischeme-docs-spec))
  (if entry (cdr entry) '()))

(define (minischeme-doc-ref section key [default #f])
  (define entry (assq key section))
  (if entry
      (let ([payload (cdr entry)])
        (if (and (pair? payload) (null? (cdr payload)))
            (car payload)
            payload))
      default))

(define (minischeme-string-prefix? prefix s)
  (and (<= (string-length prefix) (string-length s))
       (string=? (substring s 0 (string-length prefix)) prefix)))

(define (minischeme-cxr-name? s)
  (define len (string-length s))
  (and (>= len 4)
       (char=? (string-ref s 0) #\c)
       (char=? (string-ref s (- len 1)) #\r)
       (let loop ([i 1])
         (cond
           [(>= i (- len 1)) #t]
           [else
            (define ch (string-ref s i))
            (and (or (char=? ch #\a) (char=? ch #\d))
                 (loop (+ i 1)))]))))

(define (minischeme-dedup-symbols xs)
  (define seen (make-hasheq))
  (let loop ([rest xs] [acc '()])
    (cond
      [(null? rest) (reverse acc)]
      [else
       (define x (car rest))
       (if (hash-has-key? seen x)
           (loop (cdr rest) acc)
           (begin
             (hash-set! seen x #t)
             (loop (cdr rest) (cons x acc))))])))

(define (minischeme-normalize-symbol-list who xs)
  (unless (list? xs)
    (error 'minischeme-page "~a: expected a list, got ~a" who xs))
  (unless (andmap symbol? xs)
    (error 'minischeme-page "~a: expected a list of symbols, got ~a" who xs))
  (sort (minischeme-dedup-symbols xs) symbol<?))

(define (minischeme-ensure-catalog-ready!)
  (when (and (null? (minischeme-keywords))
             (null? (minischeme-primitives)))
    (minischeme-reset-state!)))

(define (minischeme-get-reference-catalog)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (minischeme-ensure-catalog-ready!)
    (define raw-keywords
      (minischeme-normalize-symbol-list 'minischeme-keywords (minischeme-keywords)))
    (define keywords
      (filter (λ (sym)
                (and (not (eq? sym 'else))
                     (not (eq? sym '=>))))
              raw-keywords))
    (define primitives
      (minischeme-normalize-symbol-list 'minischeme-primitives (minischeme-primitives)))
    (cons keywords primitives)))

(define (minischeme-rule-ref rule key [default '()])
  (define entry (assq key (cdr rule)))
  (if entry (cdr entry) default))

(define (minischeme-primitive-category primitive)
  (define name (symbol->string primitive))
  (let loop ([rules minischeme-primitive-category-rules])
    (cond
      [(null? rules) #f]
      [else
       (define rule    (car rules))
       (define label   (car rule))
       (define names   (minischeme-rule-ref rule 'names))
       (define prefixes (minischeme-rule-ref rule 'prefixes))
       (define cxr? (minischeme-rule-ref rule 'cxr? #f))
       (define matches-name? (member name names))
       (define matches-prefix?
         (for/or ([prefix (in-list prefixes)])
           (minischeme-string-prefix? prefix name)))
       (define matches-cxr?
         (and cxr? (minischeme-cxr-name? name)))
       (if (or matches-name? matches-prefix? matches-cxr?)
           label
           (loop (cdr rules)))])))

(define (minischeme-group-primitives primitives)
  (define groups (make-hash))
  (for ([primitive (in-list primitives)])
    (define label (or (minischeme-primitive-category primitive) "Other"))
    (hash-set! groups label (cons primitive (hash-ref groups label '()))))
  (define ordered-labels
    (append (map car minischeme-primitive-category-rules) (list "Other")))
  (for/list ([label (in-list ordered-labels)]
             #:when (hash-has-key? groups label))
    (cons label (sort (hash-ref groups label) symbol<?))))

(define (minischeme-category-description label)
  (let loop ([entries minischeme-primitive-category-descriptions])
    (cond
      [(null? entries) "Utilities and helpers."]
      [(string=? (caar entries) label) (cdar entries)]
      [else (loop (cdr entries))])))

(define (minischeme-primitive-section-id label)
  (define (alnum? c)
    (or (char<=? #\a c #\z)
        (char<=? #\0 c #\9)))
  (define (slugify s)
    (let loop ([chars (string->list (string-downcase s))]
               [acc '()]
               [last-hyphen? #t])
      (cond
        [(null? chars)
         (let trim-tail ([rev acc])
           (cond
             [(null? rev) ""]
             [(char=? (car rev) #\-) (trim-tail (cdr rev))]
             [else (list->string (reverse rev))]))]
        [else
         (define c (car chars))
         (cond
           [(alnum? c)
            (loop (cdr chars) (cons c acc) #f)]
           [last-hyphen?
            (loop (cdr chars) acc #t)]
           [else
            (loop (cdr chars) (cons #\- acc) #t)])])))
  (define slug
    (let ([s (slugify label)])
      (if (string=? s "") "other" s)))
  (string-append "primitive-" slug))

(define (minischeme-code-list symbols)
  `(ul (@ (class "ms-terms"))
        ,@(for/list ([sym (in-list symbols)])
            (define label (symbol->string sym))
            (define entry (assq sym minischeme-r5rs-doc-links))
            (if entry
                `(li (a (@ (href ,(cdr entry))
                           (target "_blank")
                           (rel "noreferrer noopener"))
                        (code ,label)))
                `(li (span (code ,label)))))))

(define (minischeme-reference-section)
  (define reference-spec     (minischeme-doc-section 'reference))
  (define provenance-spec    (minischeme-doc-section 'provenance))
  (define toc-spec           (minischeme-doc-section 'toc))
  (define compatibility-spec (minischeme-doc-section 'compatibility))
  (define availability-spec  (minischeme-doc-section 'availability))
  (define errors-spec        (minischeme-doc-section 'errors))

  (define reference-id       (minischeme-doc-ref reference-spec 'id))
  (define reference-title    (minischeme-doc-ref reference-spec 'title))
  (define reference-intro    (minischeme-doc-ref reference-spec 'intro '()))
  (define provenance-text    (minischeme-doc-ref provenance-spec 'text #f))
  (define toc-items          (minischeme-doc-ref toc-spec 'items '()))

  (define compatibility-id    (minischeme-doc-ref compatibility-spec 'id))
  (define compatibility-title (minischeme-doc-ref compatibility-spec 'title))
  (define compatibility-points
    (minischeme-doc-ref compatibility-spec 'points '()))
  (define compatibility-subpoints
    (minischeme-doc-ref compatibility-spec 'subpoints '()))

  (define availability-id    (minischeme-doc-ref availability-spec 'id))
  (define availability-title (minischeme-doc-ref availability-spec 'title))

  (define keywords-spec     (minischeme-doc-ref availability-spec 'keywords '()))
  (define primitives-spec   (minischeme-doc-ref availability-spec 'primitives '()))
  (define keywords-id       (minischeme-doc-ref keywords-spec 'id))
  (define keywords-title    (minischeme-doc-ref keywords-spec 'title))
  (define keywords-note     (minischeme-doc-ref keywords-spec 'note #f))
  (define primitives-id     (minischeme-doc-ref primitives-spec 'id))
  (define primitives-title  (minischeme-doc-ref primitives-spec 'title))

  (define catalog-warning-text
    (minischeme-doc-ref errors-spec 'catalog-failed))

  (define catalog (minischeme-get-reference-catalog))
  (define grouped
    (if catalog (minischeme-group-primitives (cdr catalog)) '()))
  (define category-nav-items
    (for/list ([entry (in-list grouped)])
      (define label (car entry))
      (list label (minischeme-primitive-section-id label))))
  (define top-nav-items
    (append
     (for/list ([item (in-list toc-items)])
       (list (car item) (cadr item)))
     category-nav-items))

  `(div (@ (class "minischeme-details minischeme-reference")
           (id "minischeme-reference-section"))
        (div (@ (class "minischeme-ref-layout"))
             (div (@ (class "minischeme-ref-main"))
                  (h2 (@ (id ,reference-id)) ,reference-title)
                  ,@(for/list ([paragraph (in-list reference-intro)])
                      `(p ,paragraph))
                  ,@(if provenance-text
                        (list `(p ,provenance-text))
                        '())
                  (nav (@ (class "minischeme-ref-toc") (aria-label "MiniScheme reference sections"))
                       (ul
                        ,@(for/list ([item (in-list toc-items)])
                            (define label (car item))
                            (define id    (cadr item))
                            `(li (a (@ (href ,(string-append "#" id))) ,label)))))
                  (div (@ (class "minischeme-ref-mobile-jump"))
                       (label (@ (for "minischeme-ref-jump")) "Jump to")
                       (select (@ (id "minischeme-ref-jump"))
                               (option (@ (value "")) "Select a section")
                               ,@(for/list ([item (in-list top-nav-items)])
                                   (define label (car item))
                                   (define id    (cadr item))
                                   `(option (@ (value ,id)) ,label))))
                  (h3 (@ (id ,compatibility-id)) ,compatibility-title)
                  (ul
                   ,@(for/list ([item (in-list compatibility-points)])
                       `(li ,item)))
                  (ul (@ (class "minischeme-ref-subpoints"))
                      ,@(for/list ([item (in-list compatibility-subpoints)])
                          `(li ,item)))
                  (h3 (@ (id ,availability-id)) ,availability-title)
                  ,(if catalog
                       (let ([keywords (car catalog)]
                             [primitives (cdr catalog)])
                         `(div (@ (class "minischeme-ref-catalog"))
                               (h3 (@ (id ,keywords-id)) ,keywords-title)
                               ,@(if keywords-note
                                     (list `(p ,keywords-note))
                                     '())
                               (p (@ (class "minischeme-ref-count"))
                                  ,(format "~a special forms" (length keywords)))
                               ,(minischeme-code-list keywords)
                               (h3 (@ (id ,primitives-id)) ,primitives-title)
                               (p (@ (class "minischeme-ref-count"))
                                  ,(format "~a primitives" (length primitives)))
                               (div (@ (class "minischeme-ref-groups"))
                                    ,@(for/list ([entry (in-list grouped)])
                                        (define label  (car entry))
                                        (define values (cdr entry))
                                        (define section-id (minischeme-primitive-section-id label))
                                        `(section (@ (class "minischeme-ref-group")
                                                     (id ,section-id))
                                                  (h4 ,label)
                                                  (p ,(minischeme-category-description label))
                                                  (p (@ (class "minischeme-ref-count"))
                                                     ,(format "~a" (length values)))
                                                  ,(minischeme-code-list values))))))
                       `(div (@ (class "callout callout--warn"))
                             (p ,catalog-warning-text)
                             (div (@ (class "minischeme-actions-links"))
                                  ,(code-pill (gh-file "web-site/src/examples/minischeme/minischeme.rkt")
                                              "Web-site interpreter")
                                  ,(code-pill (gh-file "web-site/src/examples/minischeme-page.rkt")
                                              "Page shell"))))
                  (div (@ (class "minischeme-actions-links"))
                       ,(code-pill (gh-file "web-site/src/examples/minischeme/minischeme.rkt")
                                   "Web-site interpreter")
                       ,(code-pill (gh-file "web-site/src/examples/minischeme-page.rkt")
                                   "Page shell")))
             (aside (@ (class "minischeme-ref-sidebar")
                       (aria-label "On this page"))
                    (h3 "On this page")
                    (ul
                     ,@(for/list ([item (in-list top-nav-items)])
                         (define label (car item))
                         (define id    (cadr item))
                         `(li (a (@ (href ,(string-append "#" id))
                                    (data-ref-target ,id))
                                ,label))))))))

(define (minischeme-page)
  `(div (@ (class "page page--minischeme"))
        ,(navbar)
        (section (@ (class "minischeme-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Interpreter")
                           (span (@ (class "pill")) "REPL")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "MiniScheme")
                      (p (@ (class "hero-lead"))
                         "A tiny Scheme interpreter running entirely in the browser.")))
        (section (@ (class "section section--minischeme"))
                 (div (@ (class "section-content"))
                      (div (@ (class "minischeme-shell")
                              (id "minischeme-editor-section"))
                           (p (@ (class "minischeme-shell-note"))
                              (span (@ (class "minischeme-shell-note-label")) "TIP")
                              "Write Scheme (R5RS-style) programs and click "
                              (code "Run")
                              "."
                              (br)
                              "Definitions persist across runs until you reset the state or load a sample."
                              (br)
                              (br)
                              "The code you write is Scheme. MiniScheme is implemented in WebRacket,"
                              (br)
                              "but WebRacket itself is not the language of this editor.")
                           (div (@ (class "minischeme-sample-row"))
                                (select (@ (id "minischeme-sample-select")
                                           (class "minischeme-sample-select")
                                           (aria-label "Choose a sample program"))
                                        ,@(for/list ([entry (in-list minischeme-sample-programs)])
                                            (define sample-id    (car entry))
                                            (define sample-title (cadr entry))
                                            (if (string=? sample-id minischeme-default-sample-id)
                                                `(option (@ (value ,sample-id) (selected "selected"))
                                                         ,sample-title)
                                                `(option (@ (value ,sample-id))
                                                         ,sample-title))))
                                (button (@ (id "minischeme-load-sample")
                                           (type "button")
                                           (class "minischeme-btn minischeme-btn--sample"))
                                        "Load Sample"))
                           (div (@ (class "ms-editor-pane"))
                                (textarea
                                 (@ (id "minischeme-input")
                                    (class "minischeme-editor")
                                    (rows "25")
                                    (spellcheck "false"))
                                 ,minischeme-default-sample-program)
                                (button (@ (id "minischeme-copy-editor")
                                           (type "button")
                                           (class "ms-copy-btn ms-copy-btn--editor")
                                           (aria-label "Copy editor contents"))
                                        "Copy"))
                           (div (@ (class "minischeme-actions"))
                                (div (@ (class "ms-controls-left"))
                                     (button (@ (id "minischeme-run")
                                                (type "button")
                                                (class "minischeme-btn minischeme-btn--run"))
                                             "Run")
                                     (button (@ (id "minischeme-pause")
                                                (type "button")
                                                (class "minischeme-btn minischeme-btn--pause"))
                                             "Pause")
                                     (button (@ (id "minischeme-stop")
                                                (type "button")
                                                (class "minischeme-btn minischeme-btn--stop"))
                                             "Stop")
                                     (button (@ (id "minischeme-reset")
                                                (type "button")
                                                (class "minischeme-btn minischeme-btn--reset"))
                                             "Reset State")))
                           (div (@ (class "ms-output"))
                                (div (@ (class "ms-output-header"))
                                     (div (@ (class "ms-output-label")) "OUTPUT")
                                     (span (@ (id "minischeme-run-state")
                                              (class "ms-output-status")
                                              (data-state "idle")
                                              (aria-live "polite"))
                                           "Idle"))
                                (div (@ (class "ms-output-body")
                                        (role "region")
                                        (aria-label "Output"))
                                     (pre (@ (id "minischeme-output")
                                             (class "minischeme-output ms-output-content"))
                                          "MiniScheme ready.")
                                     (button (@ (id "minischeme-copy-output")
                                                (type "button")
                                                (class "ms-copy-btn ms-copy-btn--output")
                                                (aria-label "Copy output"))
                                             "Copy"))))))
        (section (@ (class "section section--minischeme-details"))
                 (div (@ (class "section-content"))
                      ,(minischeme-reference-section)))
        (button (@ (id "minischeme-back-to-editor")
                   (class "minischeme-back-to-editor")
                   (type "button"))
                "Back to editor")
        ,(footer-section)))

(define (js-number-value v [default 0.0])
  (cond
    [(number? v) v]
    [(external? v)
     (with-handlers ([exn:fail? (λ (_) default)])
       (external-number->flonum v))]
    [else default]))

(include "minischeme/minischeme.rkt")

(define minischeme-page-started? #f)
(define minischeme-run-handler   #f)
(define minischeme-pause-handler #f)
(define minischeme-stop-handler  #f)
(define minischeme-reset-handler #f)
(define minischeme-load-handler  #f)
(define minischeme-copy-editor-handler #f)
(define minischeme-copy-output-handler #f)
(define minischeme-input-handler #f)
(define minischeme-editor-key-handler #f)
(define minischeme-editor-change-handler #f)
(define minischeme-editor #f)
(define minischeme-keyword-table (make-hasheq))
(define minischeme-primitive-table (make-hasheq))
(define minischeme-ui-handler-store '())

(define (remember-minischeme-ui-handler! handler)
  (set! minischeme-ui-handler-store (cons handler minischeme-ui-handler-store)))

(define (minischeme-codemirror-ready?)
  (not (js-nullish? (js-var "CodeMirror"))))

(define (minischeme-codemirror-scheme-mode-ready?)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [modes (js-ref codemirror "modes")]
              [scheme-mode (and (not (js-nullish? modes))
                                (js-ref modes "scheme"))])
         (not (js-nullish? scheme-mode)))))

(define (minischeme-codemirror-option-handler-ready? option-name)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [handlers (js-ref codemirror "optionHandlers")]
              [handler (and (not (js-nullish? handlers))
                            (js-ref handlers option-name))])
         (not (js-nullish? handler)))))

(define (minischeme-codemirror-matchbrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "matchBrackets"))

(define (minischeme-codemirror-closebrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "autoCloseBrackets"))

(define (minischeme-load-saved-source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (if (js-nullish? storage)
      #f
      (let ([saved (js-send/extern storage "getItem" (vector minischeme-storage-key))])
        (if (js-nullish? saved)
            #f
            (js-value->string saved)))))

(define (minischeme-save-source source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (unless (js-nullish? storage)
    (js-send storage "setItem" (vector minischeme-storage-key source))))

(define (minischeme-restore-source! input-node)
  (define saved (minischeme-load-saved-source))
  (when (and saved (not (string=? saved "")))
    (js-set! input-node "value" saved)))

(define (minischeme-editor-get-source input-node)
  (if minischeme-editor
      (js-value->string (js-send/extern minischeme-editor "getValue" (vector)))
      (js-value->string (js-ref input-node "value"))))

(define (minischeme-editor-set-source! input-node source)
  (if minischeme-editor
      (js-send minischeme-editor "setValue" (vector source))
      (js-set! input-node "value" source))
  (minischeme-save-source source))

(define (minischeme-refresh-highlight-tables!)
  (set! minischeme-keyword-table (make-hasheq))
  (set! minischeme-primitive-table (make-hasheq))
  (for-each (λ (name) (hash-set! minischeme-keyword-table name #t))
            (minischeme-keywords))
  (for-each (λ (name) (hash-set! minischeme-primitive-table name #t))
            (minischeme-primitives)))

(define (minischeme-js-string-escape s)
  (define s1 (string-replace s "\\" "\\\\"))
  (define s2 (string-replace s1 "\"" "\\\""))
  (define s3 (string-replace s2 "\n" "\\n"))
  (define s4 (string-replace s3 "\r" "\\r"))
  (string-replace s4 "\t" "\\t"))

(define (minischeme-symbols->js-array syms)
  (string-append
   "["
   (string-join
    (map (λ (sym)
           (string-append "\"" (minischeme-js-string-escape (symbol->string sym)) "\""))
         syms)
    ",")
   "]"))

(define (minischeme-build-js-overlay!)
  ;; Ensure keyword/primitive caches are initialized before generating JS sets.
  (when (and (null? (minischeme-keywords))
             (null? (minischeme-primitives)))
    (minischeme-reset-state!))
  (define forms-js (minischeme-symbols->js-array (minischeme-keywords)))
  (define prims-js (minischeme-symbols->js-array (minischeme-primitives)))
  (define js-source
    (string-append
     "(function(){"
     "const formSet = new Set(" forms-js ");"
     "const primSet = new Set(" prims-js ");"
     "const stringRe = /\\\"(?:[^\\\"\\\\\\\\]|\\\\\\\\.)*\\\"?/;"
     "const symbolRe = /[^\\\\s()\\\\[\\\\]{}\\\"';`,.]+/;"
     "let logged = false;"
     "window.__minischemeOverlay = {"
     " token: function(stream){"
     "   if (stream.eatSpace()) return null;"
     "   if (stream.match(';', false, false)) { stream.skipToEnd(); return null; }"
     "   if (stream.match(stringRe, true, false)) return null;"
     "   if (stream.match(symbolRe, true, false)) {"
     "     const tok = stream.current();"
     "     if (!logged) { logged = true; console.log('[minischeme] overlay active'); }"
     "     if (formSet.has(tok)) return 'keyword';"
     "     if (primSet.has(tok)) return 'builtin';"
     "     return null;"
     "   }"
     "   stream.next();"
     "   return null;"
     " }"
     "};"
     "})();"))
  (js-eval js-source)
  (js-ref (js-var "window") "__minischemeOverlay"))

(define (minischeme-token-style token)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define sym (string->symbol token))
    (cond
      [(hash-has-key? minischeme-keyword-table sym) "ms-form"]
      [(hash-has-key? minischeme-primitive-table sym) "ms-builtin"]
      [else #f])))

(define (minischeme-overlay-delimiter? c)
  (or (char-whitespace? c)
      (char=? c #\()
      (char=? c #\))
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\")
      (char=? c #\')
      (char=? c #\`)
      (char=? c #\,)
      (char=? c #\;)
      (char=? c #\.)))

(define (minischeme-stream-peek-char stream)
  (define raw (js-send/extern stream "peek" (vector)))
  (if (js-nullish? raw)
      #f
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define s (js-value->string raw))
        (if (= (string-length s) 0)
            #f
            (string-ref s 0)))))

(define (minischeme-stream-next-char stream)
  (define raw (js-send/extern stream "next" (vector)))
  (if (js-nullish? raw)
      #f
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define s (js-value->string raw))
        (if (= (string-length s) 0)
            #f
            (string-ref s 0)))))

(define (make-minischeme-overlay-token-handler)
  (procedure->external
   (let ([tokenize
          (λ (stream)
            (define start-pos (js-number-value (js-ref stream "pos")))
            (define result
              (with-handlers ([exn:fail? (λ (_) #f)])
                (cond
                  [(js-send/truthy stream "eatSpace" (vector)) #f]
                  [(js-send/truthy stream "eol" (vector)) #f]
                  [else
                   (define ch (minischeme-stream-next-char stream))
                   (cond
                     [(not ch) #f]
                     [(char=? ch #\;)
                      (js-send stream "skipToEnd" (vector))
                      #f]
                     [(char=? ch #\")
                      (let loop ([escaped? #f])
                        (define c (minischeme-stream-next-char stream))
                        (cond
                          [(not c) (void)]
                          [escaped? (loop #f)]
                          [(char=? c #\\) (loop #t)]
                          [(char=? c #\") (void)]
                          [else (loop #f)]))
                      #f]
                     [(minischeme-overlay-delimiter? ch)
                      #f]
                     [else
                      (let loop ()
                        (define c (minischeme-stream-peek-char stream))
                        (when (and c (not (minischeme-overlay-delimiter? c)))
                          (minischeme-stream-next-char stream)
                          (loop)))
                      (minischeme-token-style
                       (js-value->string (js-send/extern stream "current" (vector))))])])))
            ;; Instrumentation + guard: ensure we always advance.
            (define end-pos (js-number-value (js-ref stream "pos")))
            (when (and (= start-pos end-pos)
                       (not (js-send/truthy stream "eol" (vector))))
              (js-log (format "[minischeme/overlay] non-advance start=~a end=~a" start-pos end-pos))
              (js-send stream "next" (vector)))
            result)])
     (case-lambda
       [(stream) (tokenize stream)]
       [(stream _state) (tokenize stream)]))))

(define (minischeme-expected-closer prefix)
  ;; Returns the closer that matches the most recent unmatched opener.
  (define n (string-length prefix))
  (let loop ([i 0] [stack '()] [in-string? #f] [escaped? #f] [in-comment? #f])
    (if (= i n)
        (if (null? stack) #f (car stack))
        (let ([ch (string-ref prefix i)])
          (cond
            [in-comment?
             (if (or (char=? ch #\newline) (char=? ch #\return))
                 (loop (+ i 1) stack in-string? escaped? #f)
                 (loop (+ i 1) stack in-string? escaped? #t))]
            [in-string?
             (cond
               [escaped?
                (loop (+ i 1) stack #t #f #f)]
               [(char=? ch #\\)
                (loop (+ i 1) stack #t #t #f)]
               [(char=? ch #\")
                (loop (+ i 1) stack #f #f #f)]
               [else
                (loop (+ i 1) stack #t #f #f)])]
            [else
             (cond
               [(char=? ch #\;)
                (loop (+ i 1) stack #f #f #t)]
               [(char=? ch #\")
                (loop (+ i 1) stack #t #f #f)]
               [(char=? ch #\()
                (loop (+ i 1) (cons #\) stack) #f #f #f)]
               [(char=? ch #\[)
                (loop (+ i 1) (cons #\] stack) #f #f #f)]
               [(char=? ch #\{)
                (loop (+ i 1) (cons #\} stack) #f #f #f)]
               [(and (pair? stack) (char=? ch (car stack)))
                (loop (+ i 1) (cdr stack) #f #f #f)]
               [else
                (loop (+ i 1) stack #f #f #f)])])))))

(define (minischeme-open-paren? ch)
  (or (char=? ch #\()
      (char=? ch #\[)
      (char=? ch #\{)))

(define (minischeme-close-paren? ch)
  (or (char=? ch #\))
      (char=? ch #\])
      (char=? ch #\})))

(define (minischeme-matching-close ch)
  (cond
    [(char=? ch #\() #\)]
    [(char=? ch #\[) #\]]
    [(char=? ch #\{) #\}]
    [else #f]))

(define (minischeme-matching-open ch)
  (cond
    [(char=? ch #\)) #\(]
    [(char=? ch #\]) #\[]
    [(char=? ch #\}) #\{]
    [else #f]))

(define (minischeme-find-matching-right text open-index open-ch)
  (define close-ch (minischeme-matching-close open-ch))
  (if (not close-ch)
      #f
      (let loop ([i (+ open-index 1)] [depth 1])
        (cond
          [(>= i (string-length text)) #f]
          [else
           (define ch (string-ref text i))
           (cond
             [(char=? ch open-ch) (loop (+ i 1) (+ depth 1))]
             [(char=? ch close-ch)
              (if (= depth 1)
                  i
                  (loop (+ i 1) (- depth 1)))]
             [else (loop (+ i 1) depth)])]))))

(define (minischeme-find-matching-left text close-index close-ch)
  (define open-ch (minischeme-matching-open close-ch))
  (if (not open-ch)
      #f
      (let loop ([i (- close-index 1)] [depth 1])
        (cond
          [(< i 0) #f]
          [else
           (define ch (string-ref text i))
           (cond
             [(char=? ch close-ch) (loop (- i 1) (+ depth 1))]
             [(char=? ch open-ch)
              (if (= depth 1)
                  i
                  (loop (- i 1) (- depth 1)))]
             [else (loop (- i 1) depth)])]))))

(define (minischeme-find-string-end text open-index)
  (if (or (< open-index 0)
          (>= open-index (string-length text))
          (not (char=? (string-ref text open-index) #\")))
      #f
      (let loop ([i (+ open-index 1)] [escaped? #f])
        (cond
          [(>= i (string-length text)) #f]
          [else
           (define ch (string-ref text i))
           (cond
             [escaped? (loop (+ i 1) #f)]
             [(char=? ch #\\) (loop (+ i 1) #t)]
             [(char=? ch #\") i]
             [else (loop (+ i 1) #f)])]))))

(define (minischeme-find-string-start text close-index)
  (if (or (< close-index 0)
          (>= close-index (string-length text))
          (not (char=? (string-ref text close-index) #\")))
      #f
      (let loop ([i 0] [in-string? #f] [escaped? #f] [in-comment? #f] [start-index #f])
        (cond
          [(> i close-index) #f]
          [in-comment?
           (if (or (char=? (string-ref text i) #\newline)
                   (char=? (string-ref text i) #\return))
               (loop (+ i 1) #f #f #f #f)
               (loop (+ i 1) #f #f #t #f))]
          [in-string?
           (define ch (string-ref text i))
           (cond
             [escaped? (loop (+ i 1) #t #f #f start-index)]
             [(char=? ch #\\) (loop (+ i 1) #t #t #f start-index)]
             [(char=? ch #\")
              (if (= i close-index)
                  start-index
                  (loop (+ i 1) #f #f #f #f))]
             [else (loop (+ i 1) #t #f #f start-index)])]
          [else
           (define ch (string-ref text i))
           (cond
             [(char=? ch #\;) (loop (+ i 1) #f #f #t #f)]
             [(char=? ch #\") (loop (+ i 1) #t #f #f i)]
             [else (loop (+ i 1) #f #f #f #f)])]))))

(define (minischeme-token-delimiter? ch)
  (or (char-whitespace? ch)
      (minischeme-open-paren? ch)
      (minischeme-close-paren? ch)
      (char=? ch #\")
      (char=? ch #\;)))

(define (minischeme-find-token-end text start-index)
  (let loop ([i start-index])
    (cond
      [(>= i (string-length text)) i]
      [(minischeme-token-delimiter? (string-ref text i)) i]
      [else (loop (+ i 1))])))

(define (minischeme-find-token-start text end-index)
  (let loop ([i end-index])
    (cond
      [(< i 0) 0]
      [(minischeme-token-delimiter? (string-ref text i)) (+ i 1)]
      [else (loop (- i 1))])))

(define (minischeme-next-non-whitespace-index text start-index)
  (let loop ([i start-index])
    (cond
      [(>= i (string-length text)) #f]
      [(char-whitespace? (string-ref text i)) (loop (+ i 1))]
      [else i])))

(define (minischeme-prev-non-whitespace-index text start-index)
  (let loop ([i start-index])
    (cond
      [(< i 0) #f]
      [(char-whitespace? (string-ref text i)) (loop (- i 1))]
      [else i])))

(define (minischeme-init-codemirror! input-node on-run!)
  (when (and (not minischeme-editor) (minischeme-codemirror-ready?))
    (define codemirror (js-var "CodeMirror"))
    ;; Use a full WebRacket callback for token classification.
    (minischeme-refresh-highlight-tables!)
    (define overlay
      (js-object
       (vector
        (vector "name" "minischeme-overlay-webracket")
        (vector "token" (make-minischeme-overlay-token-handler)))))
    (define universal-close-handler
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define line (js-ref cursor "line"))
         (define ch (js-ref cursor "ch"))
         (define start-pos (js-object (vector (vector "line" 0) (vector "ch" 0))))
         (define prefix (js-value->string (js-send/extern cm "getRange" (vector start-pos cursor))))
         (define expected (minischeme-expected-closer prefix))
         (define closer (if expected expected #\]))
         (define closer-text (string closer))
         (define next-pos (js-object (vector (vector "line" line) (vector "ch" (+ ch 1)))))
         (define next-char (js-value->string (js-send/extern cm "getRange" (vector cursor next-pos))))
         (if (string=? next-char closer-text)
             (js-send cm "setCursor" (vector next-pos))
             (js-send cm "replaceSelection" (vector closer-text)))
         (void))))
    (define run-shortcut-handler
      (procedure->external
       (λ (_cm)
         (on-run!)
         (void))))
    (define tab-indent-handler
      (procedure->external
       (λ (cm)
         (if (js-send/truthy cm "somethingSelected" (vector))
             (js-send cm "indentSelection" (vector "smart"))
             (let* ([cursor (js-send/extern cm "getCursor" (vector))]
                    [line (js-ref cursor "line")])
               (js-send cm "indentLine" (vector line "smart"))))
         (void))))
    (define (minischeme-move-or-select! cm target-pos selecting?)
      (if selecting?
          (begin
            (js-send cm "setExtending" (vector #t))
            (js-send cm "setCursor" (vector target-pos))
            (js-send cm "setExtending" (vector #f)))
          (js-send cm "setCursor" (vector target-pos))))
    (define (make-alt-right-handler selecting?)
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define index (inexact->exact
                        (round (js-number-value
                                (js-send/extern cm "indexFromPos" (vector cursor))))))
         (define text (js-value->string (js-send/extern cm "getValue" (vector))))
         (define open-index (minischeme-next-non-whitespace-index text index))
         (when open-index
           (define ch (string-ref text open-index))
           (cond
             [(minischeme-open-paren? ch)
              (define match-index (minischeme-find-matching-right text open-index ch))
              (when match-index
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector (+ match-index 1))))
                (minischeme-move-or-select! cm target-pos selecting?))]
             [(char=? ch #\")
              (define match-index (minischeme-find-string-end text open-index))
              (when match-index
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector (+ match-index 1))))
                (minischeme-move-or-select! cm target-pos selecting?))]
             [(minischeme-close-paren? ch)
              (define target-pos
                (js-send/extern cm "posFromIndex" (vector open-index)))
              (minischeme-move-or-select! cm target-pos selecting?)]
             [else
              (define end-index (minischeme-find-token-end text open-index))
              (when (> end-index index)
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector end-index)))
                (minischeme-move-or-select! cm target-pos selecting?))]))
         (void))))
    (define (make-alt-left-handler selecting?)
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define index (inexact->exact
                        (round (js-number-value
                                (js-send/extern cm "indexFromPos" (vector cursor))))))
         (define text (js-value->string (js-send/extern cm "getValue" (vector))))
         (define close-index (minischeme-prev-non-whitespace-index text (- index 1)))
         (when close-index
           (define ch (string-ref text close-index))
           (cond
             [(minischeme-close-paren? ch)
              (define match-index (minischeme-find-matching-left text close-index ch))
              (when match-index
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector match-index)))
                (minischeme-move-or-select! cm target-pos selecting?))]
             [(char=? ch #\")
              (define match-index (minischeme-find-string-start text close-index))
              (when match-index
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector match-index)))
                (minischeme-move-or-select! cm target-pos selecting?))]
             [(minischeme-open-paren? ch)
              (define target-pos
                (js-send/extern cm "posFromIndex" (vector (+ close-index 1))))
              (minischeme-move-or-select! cm target-pos selecting?)]
             [else
              (define start-index (minischeme-find-token-start text close-index))
              (when (< start-index index)
                (define target-pos
                  (js-send/extern cm "posFromIndex" (vector start-index)))
                (minischeme-move-or-select! cm target-pos selecting?))]))
         (void))))
    (define alt-right-paren-handler (make-alt-right-handler #f))
    (define alt-left-paren-handler  (make-alt-left-handler #f))
    (define shift-alt-right-handler (make-alt-right-handler #t))
    (define shift-alt-left-handler  (make-alt-left-handler #t))
    (define extra-keys
      (js-object
       (vector
        (vector "]" universal-close-handler)
        (vector "Tab" tab-indent-handler)
        (vector "Alt-Right" alt-right-paren-handler)
        (vector "Alt-Left" alt-left-paren-handler)
        (vector "Shift-Alt-Right" shift-alt-right-handler)
        (vector "Shift-Alt-Left" shift-alt-left-handler)
        (vector "Ctrl-Enter" run-shortcut-handler)
        (vector "Cmd-Enter" run-shortcut-handler))))
    (define options
      (js-object
       (vector
        (vector "mode" "scheme")
        (vector "lineNumbers" #t)
        (vector "lineWrapping" #t)
        (vector "indentUnit" 2)
        (vector "tabSize" 2)
        (vector "indentWithTabs" #f)
        (vector "matchBrackets" #t)
        (vector "autoCloseBrackets" #t)
        (vector "extraKeys" extra-keys))))
    (set! minischeme-editor
          (js-send/extern codemirror "fromTextArea" (vector input-node options)))
    (js-send minischeme-editor "addOverlay" (vector overlay))
    (js-send minischeme-editor "setSize" (vector "100%" "560px"))
    (js-send minischeme-editor "refresh" (vector))
    (set! minischeme-editor-change-handler
          (procedure->external
           (λ (_cm _change)
             (minischeme-save-source
              (js-value->string (js-send/extern minischeme-editor "getValue" (vector))))
             (void))))
    (js-send minischeme-editor "on" (vector "change" minischeme-editor-change-handler))
    (js-send minischeme-editor "focus" (vector))
    (void)))

(define (ensure-minischeme-codemirror-assets! input-node on-run!)
  (define head (js-document-head))

  (define (maybe-init-codemirror)
    (minischeme-init-codemirror! input-node on-run!))

  (define cm-style-id "minischeme-codemirror-css")
  (define cm-style-existing (js-get-element-by-id cm-style-id))
  (when (js-nullish? cm-style-existing)
    (define link (js-create-element "link"))
    (js-set-attribute! link "id" cm-style-id)
    (js-set-attribute! link "rel" "stylesheet")
    (js-set-attribute! link "href" minischeme-codemirror-css-url)
    (js-append-child! head link))

  (define cm-ui-style-id "minischeme-codemirror-ui-style")
  (define cm-ui-style-existing (js-get-element-by-id cm-ui-style-id))
  (when (js-nullish? cm-ui-style-existing)
    (define style (js-create-element "style"))
    (js-set-attribute! style "id" cm-ui-style-id)
    (js-set! style "textContent"
             (string-append
              ".CodeMirror {"
              "  width: 100%;"
              "  min-height: 560px;"
              "  height: 560px;"
              "  resize: both;"
              "  overflow: auto;"
              "  border-radius: 10px;"
              "  border: 1px solid rgba(120, 140, 220, 0.35);"
              "  background: #0A0C18;"
              "  color: #EEF2FF;"
              "  font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace;"
              "  font-size: 0.96rem;"
              "  line-height: 1.45;"
              "}\n"
              ".CodeMirror-gutters {"
              "  border-right: 1px solid rgba(120, 140, 220, 0.25);"
              "  background: #080a14;"
              "}\n"
              ".CodeMirror-linenumber { color: #8FA0D8; }\n"
              ".CodeMirror-cursor { border-left: 1px solid #EAF0FF !important; }\n"
              ".CodeMirror-scroll {"
              "  box-sizing: border-box;"
              "  padding-bottom: 44px;"
              "}\n"
              ".cm-s-default .cm-comment { color: #7382B0; }\n"
              ".cm-s-default .cm-keyword { color: #44D5FF; font-weight: 700; }\n"
              ".cm-s-default .cm-number { color: #FFD166; }\n"
              ".cm-s-default .cm-string { color: #8fdb9f; }\n"
              ".cm-s-default .cm-atom { color: #F29F97; }\n"
              ".cm-s-default .cm-variable { color: #F6F8FF; }\n"
              ".cm-s-default .cm-def { color: #6EE7FF; font-weight: 700; }\n"
              ".cm-s-default .cm-builtin { color: #FF7AF6; font-weight: 700; }\n"
              ".cm-s-default .cm-bracket { color: #B6BDD6; }\n"
              ".cm-s-default .cm-paren { color: #B6BDD6; }\n"
              ".cm-s-default .cm-ms-form { color: #44D5FF; font-weight: 700; }\n"
              ".cm-s-default .cm-ms-builtin { color: #FF7AF6; font-weight: 700; }\n"))
    (js-append-child! head style))

  (define script-loaded-attr "data-minischeme-loaded")

  (define (script-marked-loaded? script)
    (define raw (js-get-attribute script script-loaded-attr))
    (and (not (js-nullish? raw))
         (string=? (js-value->string raw) "1")))

  (define (mark-script-loaded! script)
    (js-set-attribute! script script-loaded-attr "1"))

  (define (ensure-script! script-id script-url ready? on-ready!)
    (define onload-external
      (procedure->external
       (λ (_)
         (define loaded-script (js-get-element-by-id script-id))
         (when (not (js-nullish? loaded-script))
           (mark-script-loaded! loaded-script))
         (on-ready!)
         (void))))
    (define existing (js-get-element-by-id script-id))
    (cond
      [(not (js-nullish? existing))
       (cond
         [(or (script-marked-loaded? existing) (ready?))
          (mark-script-loaded! existing)
          (on-ready!)]
         [else
          ;; If another page instance already inserted this script, wait for
          ;; its load event instead of assuming it is ready.
          (js-add-event-listener! existing "load" onload-external)])]
      [else
       (define script (js-create-element "script"))
       (js-set-attribute! script "id" script-id)
       (js-set-attribute! script "src" script-url)
       ;; Ensure deterministic execution order for injected scripts.
       (js-set! script "async" #f)
       (js-add-event-listener! script "load" onload-external)
       (js-append-child! head script)]))

  (define (load-addons!)
    (ensure-script! "minischeme-codemirror-scheme-mode-js"
                    minischeme-codemirror-scheme-mode-js-url
                    minischeme-codemirror-scheme-mode-ready?
                    (λ ()
                      (ensure-script! "minischeme-codemirror-matchbrackets-js"
                                      minischeme-codemirror-matchbrackets-js-url
                                      minischeme-codemirror-matchbrackets-ready?
                                      (λ ()
                                        (ensure-script! "minischeme-codemirror-closebrackets-js"
                                                        minischeme-codemirror-closebrackets-js-url
                                                        minischeme-codemirror-closebrackets-ready?
                                                        maybe-init-codemirror))))))

  (if (minischeme-codemirror-ready?)
      (load-addons!)
      (ensure-script! "minischeme-codemirror-js"
                      minischeme-codemirror-js-url
                      minischeme-codemirror-ready?
                      load-addons!)))

(define (minischeme-external-string v)
  (cond
    [(string? v) v]
    [(external? v)
     (if (js-nullish? v)
         #f
         (js-value->string v))]
    [else #f]))

(define (minischeme-prefers-reduced-motion?)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define media-query (js-window-match-media "(prefers-reduced-motion: reduce)"))
    (and (external? media-query)
         (not (js-nullish? media-query))
         (js-send/truthy media-query "matches" (vector)))))

(define (minischeme-scroll-to-target-id! target-id smooth?)
  (when (and (string? target-id) (not (string=? target-id "")))
    (define node (js-get-element-by-id target-id))
    (when (and (external? node) (not (js-nullish? node)))
      (js-send node
               "scrollIntoView"
               (vector
                (js-object
                 (vector (vector "behavior" (if smooth? "smooth" "auto"))
                         (vector "block" "start"))))))))

(define (minischeme-history-replace-hash! target-id)
  (with-handlers ([exn:fail? (λ (_) (void))])
    (when (and (string? target-id) (not (string=? target-id "")))
      (define history (js-window-history))
      (when (and (external? history) (not (js-nullish? history)))
        (js-send history
                 "replaceState"
                 (vector (js-null) "" (string-append "#" target-id)))))))

(define (init-minischeme-reference-ui!)
  (define editor-node  (js-get-element-by-id "minischeme-editor-section"))
  (define back-button  (js-get-element-by-id "minischeme-back-to-editor"))
  (define jump-select  (js-get-element-by-id "minischeme-ref-jump"))
  (define smooth?      (not (minischeme-prefers-reduced-motion?)))
  (define window-node  (js-window-window))
  (define nav-links
    (node-list->list
     (js-query-selector-all ".minischeme-ref-sidebar a[data-ref-target]")))
  (define nav-items
    (reverse
     (for/fold ([acc '()]) ([link (in-list nav-links)])
       (define target-id
         (minischeme-external-string (js-get-attribute link "data-ref-target")))
       (if (and target-id (not (string=? target-id "")))
           (let ([section (js-get-element-by-id target-id)])
             (if (and (external? section) (not (js-nullish? section)))
                 (cons (list section link target-id) acc)
                 acc))
           acc))))

  (define (set-active! target-id)
    (when (and (string? target-id) (not (string=? target-id "")))
      (define matched? #f)
      (for ([entry (in-list nav-items)])
        (define link (cadr entry))
        (define id   (caddr entry))
        (if (string=? id target-id)
            (begin
              (set! matched? #t)
              (classlist-add! link "is-active"))
            (classlist-remove! link "is-active")))
      (when (and matched?
                 (external? jump-select)
                 (not (js-nullish? jump-select)))
        (define selected-id (minischeme-external-string (js-ref jump-select "value")))
        (when (or (not selected-id) (not (string=? selected-id target-id)))
          (js-set! jump-select "value" target-id)))))

  (define (scroll-and-activate! target-id)
    (when (and (string? target-id) (not (string=? target-id "")))
      (minischeme-scroll-to-target-id! target-id smooth?)
      (set-active! target-id)
      (minischeme-history-replace-hash! target-id)))

  (define (active-from-scroll!)
    (define viewport-height (js-window-inner-height))
    (define threshold
      (if (number? viewport-height)
          (* 0.35 viewport-height)
          280.0))
    (define candidate #f)
    (for ([entry (in-list nav-items)])
      (define section (car entry))
      (define id      (caddr entry))
      (define rect    (js-get-bounding-client-rect section))
      (define top     (and rect (js-ref rect "top")))
      (when (and (number? top) (<= top threshold))
        (set! candidate id)))
    (when (not candidate)
      (when (pair? nav-items)
        (set! candidate (caddr (car nav-items)))))
    (when candidate
      (set-active! candidate)))

  (when (and (external? back-button)
             (not (js-nullish? back-button))
             (external? editor-node)
             (not (js-nullish? editor-node)))
    (define back-click-handler
      (procedure->external
       (λ (_evt)
         (minischeme-scroll-to-target-id! "minischeme-editor-section" smooth?)
         (void))))
    (remember-minischeme-ui-handler! back-click-handler)
    (js-add-event-listener! back-button "click" back-click-handler)

    (define (sync-back-button-visibility!)
      (define rect (js-get-bounding-client-rect editor-node))
      (define top (and rect (js-ref rect "top")))
      (define bottom (and rect (js-ref rect "bottom")))
      (if (and (number? top)
               (number? bottom)
               (or (< bottom 0) (< top -100)))
          (classlist-add! back-button "is-visible")
          (classlist-remove! back-button "is-visible")))

    (define back-scroll-handler
      (procedure->external
       (λ (_evt)
         (sync-back-button-visibility!)
         (void))))
    (remember-minischeme-ui-handler! back-scroll-handler)
    (js-add-event-listener! window-node "scroll" back-scroll-handler)
    (js-add-event-listener! window-node "resize" back-scroll-handler)
    (sync-back-button-visibility!))

  (when (and (external? jump-select) (not (js-nullish? jump-select)))
    (define jump-handler
      (procedure->external
       (λ (_evt)
         (define target-id (minischeme-external-string (js-ref jump-select "value")))
         (when (and target-id (not (string=? target-id "")))
           (scroll-and-activate! target-id))
         (void))))
    (remember-minischeme-ui-handler! jump-handler)
    (js-add-event-listener! jump-select "change" jump-handler))

  (for ([entry (in-list nav-items)])
    (define link      (cadr entry))
    (define target-id (caddr entry))
    (define click-handler
      (procedure->external
       (λ (evt)
         (js-event-prevent-default evt)
         (scroll-and-activate! target-id)
         (void))))
    (remember-minischeme-ui-handler! click-handler)
    (js-add-event-listener! link "click" click-handler))

  (when (pair? nav-items)
    (define hash-value (minischeme-external-string (js-ref (js-window-location) "hash")))
    (if (and hash-value (> (string-length hash-value) 1))
        (set-active! (substring hash-value 1))
        (active-from-scroll!))
    (define active-scroll-handler
      (procedure->external
       (λ (_evt)
         (active-from-scroll!)
         (void))))
    (remember-minischeme-ui-handler! active-scroll-handler)
    (js-add-event-listener! window-node "scroll" active-scroll-handler)
    (js-add-event-listener! window-node "resize" active-scroll-handler)))

(define (init-minischeme-page!)
  (when (not minischeme-page-started?)
    (set! minischeme-page-started? #t)
    (define input-node       (js-get-element-by-id "minischeme-input"))
    (define output-node      (js-get-element-by-id "minischeme-output"))
    (define copy-editor-btn  (js-get-element-by-id "minischeme-copy-editor"))
    (define copy-output-btn  (js-get-element-by-id "minischeme-copy-output"))
    (define run-button       (js-get-element-by-id "minischeme-run"))
    (define pause-button     (js-get-element-by-id "minischeme-pause"))
    (define stop-button      (js-get-element-by-id "minischeme-stop"))
    (define run-state-node   (js-get-element-by-id "minischeme-run-state"))
    (define reset-button     (js-get-element-by-id "minischeme-reset"))
    (define sample-button    (js-get-element-by-id "minischeme-load-sample"))
    (define sample-select    (js-get-element-by-id "minischeme-sample-select"))

    (when (or (js-nullish? input-node)
              (js-nullish? output-node)
              (js-nullish? run-button)
              (js-nullish? pause-button)
              (js-nullish? stop-button)
              (js-nullish? run-state-node)
              (js-nullish? reset-button)
              (js-nullish? sample-button)
              (js-nullish? sample-select)
              (js-nullish? copy-editor-btn)
              (js-nullish? copy-output-btn))
      (error 'minischeme-page "missing expected DOM nodes for MiniScheme page"))

    (minischeme-restore-source! input-node)

    (define (set-output! text)
      (js-set! output-node "textContent" text))

    (define output-style (js-ref output-node "style"))
    (define output-height-locked? #f)

    (define (lock-output-height!)
      (define height
        (max (js-number-value (js-ref output-node "offsetHeight"))
             (js-number-value (js-ref output-node "clientHeight"))))
      (when (> height 0)
        (js-set! output-style "minHeight" (format "~apx" (inexact->exact (ceiling height))))
        (set! output-height-locked? #t)))

    (define (unlock-output-height!)
      (when output-height-locked?
        (js-set! output-style "minHeight" "")
        (set! output-height-locked? #f)))

    (define (fallback-copy-text text)
      (define helper (js-create-element "textarea"))
      (define helper-style (js-ref helper "style"))
      (js-set! helper "value" text)
      (js-set-attribute! helper "readonly" "")
      (when (not (js-nullish? helper-style))
        (js-set! helper-style "position" "absolute")
        (js-set! helper-style "left" "-9999px")
        (js-set! helper-style "top" "0"))
      (js-append-child! (js-document-body) helper)
      (js-send helper "focus" (vector))
      (js-send helper "select" (vector))
      (define ok (js-send/truthy (js-document) "execCommand" (vector "copy")))
      (js-remove! helper)
      ok)

    (define (set-copy-feedback! button text)
      (js-set! button "textContent" text)
      (define reset-handler
        (procedure->external
         (lambda args
           (js-set! button "textContent" "Copy")
           (void))))
      (remember-minischeme-ui-handler! reset-handler)
      (js-send (js-var "window") "setTimeout" (vector reset-handler 1200)))

    (define (copy-text-with-feedback! button get-text)
      (with-handlers ([exn:fail? (lambda (_)
                                   (set-copy-feedback! button "Copy failed"))])
        (define text (get-text))
        (define win (js-var "window"))
        (define navigator (js-ref win "navigator"))
        (define clipboard (if (js-nullish? navigator) #f (js-ref navigator "clipboard")))
        (define write-text (if (or (not clipboard) (js-nullish? clipboard))
                               #f
                               (js-ref clipboard "writeText")))
        (if (or (not clipboard)
                (js-nullish? write-text))
            (set-copy-feedback! button (if (fallback-copy-text text) "Copied" "Copy failed"))
            (let ([promise (js-send/extern clipboard "writeText" (vector text))])
              (if (js-nullish? promise)
                  (set-copy-feedback! button (if (fallback-copy-text text) "Copied" "Copy failed"))
                  (let ([ok-handler
                         (procedure->external
                          (lambda args
                            (set-copy-feedback! button "Copied")
                            (void)))]
                        [fail-handler
                         (procedure->external
                          (lambda args
                            (set-copy-feedback! button "Copy failed")
                            (void)))])
                    (remember-minischeme-ui-handler! ok-handler)
                    (remember-minischeme-ui-handler! fail-handler)
                    (js-send promise "then" (vector ok-handler))
                    (js-send promise "catch" (vector fail-handler))))))))

    (define minischeme-step-limit 2500)
    (define job-running? #f)
    (define job-paused?  #f)
    (define run-status   'idle)
    (define job-generation 0)

    (define (set-disabled! node disabled?)
      (js-set! node "disabled" (if disabled? #t #f)))

    (define (update-controls!)
      (set-disabled! run-button job-running?)
      (set-disabled! pause-button (not job-running?))
      (set-disabled! stop-button (not job-running?))
      (set-disabled! sample-button job-running?)
      (set-disabled! sample-select job-running?)
      (js-set! pause-button
               "textContent"
               (cond
                 [(not job-running?) "Pause"]
                 [job-paused? "Restart"]
                 [else "Pause"]))
      (js-set! run-state-node
               "textContent"
               (cond
                 [(eq? run-status 'paused) "Paused"]
                 [(eq? run-status 'running) "Running"]
                 [(eq? run-status 'error) "Error"]
                 [else "Idle"]))
      (js-set-attribute! run-state-node
                         "data-state"
                         (cond
                           [(eq? run-status 'paused) "paused"]
                           [(eq? run-status 'running) "running"]
                           [(eq? run-status 'error) "error"]
                           [else "idle"])))

    (define (finish-run! text)
      (set! job-running? #f)
      (set! job-paused? #f)
      (set! run-status 'idle)
      (set! job-generation (+ job-generation 1))
      (set-output! text)
      (update-controls!)
      (unlock-output-height!))

    (define (fail-run! text)
      (set! job-running? #f)
      (set! job-paused? #f)
      (set! run-status 'error)
      (set! job-generation (+ job-generation 1))
      (set-output! text)
      (update-controls!)
      (unlock-output-height!))

    (define (schedule-next-step! run-generation)
      (when (and job-running?
                 (not job-paused?)
                 (= job-generation run-generation))
        (define callback
          (procedure->external
           (λ args
             (when (and job-running?
                        (not job-paused?)
                        (= job-generation run-generation))
               (run-step! run-generation))
             (void))))
        (js-send (js-var "window") "setTimeout" (vector callback 0))))

    (define (run-step! run-generation)
      (with-handlers ([exn:fail? (λ (e)
                                   (fail-run! (string-append "error: " (exn-message e))))])
        (define-values (status text)
          (minischeme-job-step! minischeme-step-limit))
        (if (eq? status 'running)
            (begin
              (set-output! text)
              (schedule-next-step! run-generation))
            (finish-run! text))))

    (define (start-run! source)
      (define-values (status text)
        (minischeme-start-job! source))
      (if (eq? status 'ready)
          (begin
            (lock-output-height!)
            (set! job-running? #t)
            (set! job-paused? #f)
            (set! run-status 'running)
            (set! job-generation (+ job-generation 1))
            (set-output! text)
            (update-controls!)
            (schedule-next-step! job-generation))
          (begin
            (set! job-running? #f)
            (set! job-paused? #f)
            (set! run-status 'idle)
            (set! job-generation (+ job-generation 1))
            (set-output! text)
            (update-controls!))))

    (define (run! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (fail-run! (string-append "error: " (exn-message e))))])
        (unless job-running?
          (start-run! (minischeme-editor-get-source input-node)))))

    (define (pause/restart! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (fail-run! (string-append "error: " (exn-message e))))])
        (when job-running?
          (if job-paused?
              (begin
                (set! job-paused? #f)
                (set! run-status 'running)
                (update-controls!)
                (schedule-next-step! job-generation))
              (begin
                (set! job-paused? #t)
                (set! run-status 'paused)
                (update-controls!))))))

    (define (stop! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (fail-run! (string-append "error: " (exn-message e))))])
        (when job-running?
          (set! job-paused? #f)
          (minischeme-job-stop!)
          (set! job-running? #f)
          (set! run-status 'idle)
          (set! job-generation (+ job-generation 1))
          (update-controls!)
          (unlock-output-height!))))

    (define (reset! . _)
      (set! job-running? #f)
      (set! job-paused? #f)
      (set! run-status 'idle)
      (set! job-generation (+ job-generation 1))
      (minischeme-reset-state!)
      (update-controls!)
      (set-output! (string-append "MiniScheme state reset. Build " minischeme-build-id ".")))

    (define (load-sample! . _)
      (define selected-id
        (let ([raw (js-ref sample-select "value")])
          (cond
            [(string? raw) raw]
            [(external? raw)
             (if (js-nullish? raw)
                 minischeme-default-sample-id
                 (js-value->string raw))]
            [else minischeme-default-sample-id])))
      (define title  (minischeme-sample-title-by-id selected-id))
      (define source (minischeme-sample-source-by-id selected-id))
      (minischeme-editor-set-source! input-node source)
      (set-output! (string-append "Loaded sample: " title ". Click Run to evaluate.")))

    (set! minischeme-input-handler
          (procedure->external
           (λ (_event)
             (minischeme-save-source (minischeme-editor-get-source input-node))
             (void))))
    (js-add-event-listener! input-node "input" minischeme-input-handler)

    (set! minischeme-run-handler   (procedure->external run!))
    (set! minischeme-pause-handler (procedure->external pause/restart!))
    (set! minischeme-stop-handler  (procedure->external stop!))
    (set! minischeme-reset-handler (procedure->external reset!))
    (set! minischeme-load-handler  (procedure->external load-sample!))
    (set! minischeme-copy-editor-handler
          (procedure->external
           (lambda (_event)
             (copy-text-with-feedback! copy-editor-btn
                                       (lambda ()
                                         (minischeme-editor-get-source input-node)))
             (void))))
    (set! minischeme-copy-output-handler
          (procedure->external
           (lambda (_event)
             (copy-text-with-feedback! copy-output-btn
                                       (lambda ()
                                         (define raw (js-ref output-node "textContent"))
                                         (cond
                                           [(string? raw) raw]
                                           [(external? raw)
                                            (if (js-nullish? raw) "" (js-value->string raw))]
                                           [else ""])))
             (void))))

    (js-add-event-listener! run-button    "click" minischeme-run-handler)
    (js-add-event-listener! pause-button  "click" minischeme-pause-handler)
    (js-add-event-listener! stop-button   "click" minischeme-stop-handler)
    (js-add-event-listener! reset-button  "click" minischeme-reset-handler)
    (js-add-event-listener! sample-button "click" minischeme-load-handler)
    (js-add-event-listener! copy-editor-btn "click" minischeme-copy-editor-handler)
    (js-add-event-listener! copy-output-btn "click" minischeme-copy-output-handler)

    (ensure-minischeme-codemirror-assets! input-node run!)
    (init-minischeme-reference-ui!)

    (minischeme-reset-state!)
    (update-controls!)
    (set-output! (string-append "MiniScheme ready. Build " minischeme-build-id "."))))
