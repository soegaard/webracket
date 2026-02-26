;; Browser-side test harness for stdlib/browser.rkt (sxml->dom).

(define total 0)
(define failed 0)
(define lines '())

(define (add-line! s)
  (set! lines (append lines (list s)))
  (js-log s))

(define (contains? hay needle)
  (define n (string-length hay))
  (define m (string-length needle))
  (let loop ([i 0])
    (cond
      [(> (+ i m) n) #f]
      [(string=? (substring hay i (+ i m)) needle) #t]
      [else (loop (+ i 1))])))

(define (expect-success name thunk)
  (set! total (+ total 1))
  (with-handlers ([exn? (lambda (e)
                          (set! failed (+ failed 1))
                          (add-line! (string-append "FAIL " name ": unexpected exception"))
                          (add-line! (string-append "  " (exn-message e))))])
    (thunk)
    (add-line! (string-append "PASS " name))))

(define (expect-error name thunk expected-fragment)
  (set! total (+ total 1))
  (with-handlers ([exn? (lambda (e)
                          (define msg (exn-message e))
                          (if (contains? msg expected-fragment)
                              (begin
                                (add-line! (string-append "PASS " name))
                                (add-line! (string-append "  " msg)))
                              (begin
                                (set! failed (+ failed 1))
                                (add-line! (string-append "FAIL " name ": wrong message"))
                                (add-line! (string-append "  expected fragment: " expected-fragment))
                                (add-line! (string-append "  actual: " msg)))) )])
    (thunk)
    (set! failed (+ failed 1))
    (add-line! (string-append "FAIL " name ": expected exception"))))

;; sanity: valid conversion with attribute block
(expect-success
 "valid link attrs"
 (lambda ()
   (define n
     (sxml->dom
      '(link (@ (rel "icon")
                (type "image/png")
                (href "assets/favicon/favicon-96x96.png")
                (sizes "96x96")))))
   (js-append-child! (js-document-head) n)))

;; malformed shape
(expect-error
 "shape error non-list"
 (lambda () (sxml->dom 42))
 "malformed SXML expression")

;; tag must be symbol
(expect-error
 "tag type error"
 (lambda () (sxml->dom '(1 "x")))
 "tag name must be a symbol")

;; stray attribute block
(expect-error
 "stray attribute block"
 (lambda () (sxml->dom '(@ (rel icon))))
 "attribute block (@ ...) must appear immediately")

;; malformed attribute block (improper list)
(expect-error
 "attribute block improper list"
 (lambda () (sxml->dom '(div (@ . 1) "x")))
 "attribute block must be a proper list")

;; malformed attribute entry shape
(expect-error
 "attribute entry arity"
 (lambda () (sxml->dom '(div (@ (id "a" "b")))))
 "attribute entry must be a list of exactly two values")

;; malformed attribute name
(expect-error
 "attribute name type"
 (lambda () (sxml->dom '(div (@ (1 "a")))))
 "attribute name must be a symbol or string")

;; createElement host failure wrapped with sxml context
(expect-error
 "create-element host failure"
 (lambda () (sxml->dom '(|bad tag| "x")))
 "create-element failed for tag")

(define summary
  (string-append "SUMMARY: "
                 (number->string (- total failed))
                 "/"
                 (number->string total)
                 " passed"))

(add-line! summary)

(define report
  (sxml->dom
   `(pre (@ (id "test-browser-report")
            (style "white-space: pre-wrap; font: 14px/1.4 monospace; padding: 12px;"))
         ,(apply string-append (add-between (append lines (list "")) "\n")))))

(js-append-child! (js-document-body) report)
