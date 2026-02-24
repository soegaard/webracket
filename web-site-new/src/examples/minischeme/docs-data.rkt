;;;
;;; MiniScheme reference docs data (static text + structure)
;;;

(define minischeme-docs-spec
  '((reference
     (id "minischeme-reference")
     (title "MiniScheme Reference")
     (intro
      "This page includes a compact MiniScheme reference below the editor."
      "MiniScheme is a Scheme interpreter implemented in WebRacket; the code you run in the editor is Scheme."
      "The lists of special forms and primitive procedures are generated from the interpreter."
      "Use this section as the canonical in-page overview of available forms and procedures."))
    (provenance
     (text
      "The lists below are generated at runtime from the MiniScheme interpreter and reflect exactly what is available in this build of Scheme support. Primitive names link to their detailed documentation; use this section as the in-page index of forms and procedures."))
    (toc
     (items
      ("Compatibility & scope" "compatibility-scope")
      ("Special Forms" "special-forms")
      ("Primitive procedures" "primitive-procedures")))
    (compatibility
     (id "compatibility-scope")
     (title "Compatibility & scope")
     (points
      "Aim: implement R5RS Scheme without macros and without input ports."
      "On the plus side: eval, call/cc, dynamic-wind, and proper tail recursion are available."
      "Numeric tower: currently supports only fixnums and flonums.")
     (subpoints
      "No macros => no define-syntax / syntax-rules."
      "No input ports => reading (read/open-input-file) is not supported."))
    (availability
     (id "minischeme-whats-available")
     (title "What's available")
     (keywords
      (id "special-forms")
      (title "Special Forms")
      (note
       "Special forms are language constructs with evaluation rules defined by the interpreter. They are not ordinary procedures and do not evaluate all of their arguments."))
     (primitives
      (id "primitive-procedures")
      (title "Primitive procedures")))
    (errors
     (catalog-failed "Could not retrieve keyword/primitive list from interpreter."))))

;; Category mapping for flat primitive lists.
;; Rules are evaluated in order; unmatched names go to "Other".
(define minischeme-primitive-category-rules
  '(("Numeric"
     (prefixes "fx" "fl")
     (names
      "+" "-" "*" "/" "=" "<" "<=" ">" ">="
      "abs" "add1" "sub1" "max" "min"
      "quotient" "remainder" "modulo"
      "exp" "log" "expt" "sqrt"
      "sin" "cos" "tan" "asin" "acos" "atan"
      "sinh" "cosh" "tanh" "asinh" "acosh" "atanh"
      "floor" "ceiling" "round" "truncate"
      "number?" "real?" "integer?" "exact?" "inexact?"
      "exact->inexact" "inexact->exact"
      "zero?" "positive?" "negative?" "odd?" "even?"))
    ("Predicates & equality"
     (names
      "boolean?" "char?" "pair?" "procedure?" "string?" "symbol?" "vector?"
      "null?" "not" "eq?" "eqv?" "equal?"))
    ("Pairs & lists"
     (prefixes "list-")
     (cxr? #t)
     (names
      "cons" "car" "cdr" "set-car!" "set-cdr!"
      "pair?" "null?" "list" "list*" "list?" "append" "reverse"
      "first" "second" "rest"
      "length" "list-tail" "list-ref" "list-set!"
      "make-list" "last-pair" "memq" "memv" "member"
      "assq" "assv" "assoc" "map" "for-each"))
    ("Vectors"
     (prefixes "vector-")
     (names "vector" "vector?" "make-vector" "list->vector" "vector->list"))
    ("Strings"
     (prefixes "string-")
     (names "string" "string?" "make-string" "substring"))
    ("Characters"
     (prefixes "char-")
     (names "char?" "integer->char"))
    ("Symbols"
     (prefixes "symbol-")
     (names "symbol?" "string->symbol"))
    ("Control"
     (names
      "apply" "eval" "values" "call-with-values"
      "call/cc" "call-with-current-continuation" "dynamic-wind" "force"))
    ("Environment"
     (names "interaction-environment" "scheme-report-environment" "null-environment"))
    ("I/O"
     (prefixes "display" "write")
     (names "newline" "display" "write"))))

(define minischeme-primitive-category-descriptions
  '(("Numeric"       . "Arithmetic and numeric predicates (fixnums and flonums).")
    ("Predicates & equality" . "Type checks, boolean tests, and equality predicates.")
    ("Pairs & lists" . "List constructors, selectors, predicates, and list utilities.")
    ("Vectors"       . "Vector creation, access, mutation, and conversions.")
    ("Strings"       . "String constructors, predicates, accessors, and conversions.")
    ("Characters"    . "Character predicates and char/integer case conversions.")
    ("Symbols"       . "Symbol predicates and symbol/string conversions.")
    ("Control"       . "Continuations, dynamic context, eval, and value handling.")
    ("Environment"   . "Standard environment objects for eval/reporting.")
    ("I/O"           . "Output primitives.")))

;; Documentation links for MiniScheme-exposed forms/procedures.
(define minischeme-r5rs-prefix "https://docs.racket-lang.org/r5rs/r5rs-std/")
(define minischeme-reference-prefix "https://docs.racket-lang.org/reference/")

(define (minischeme-expand-doc-url url)
  (cond
    [(string-prefix? url "r5rs:")
     (string-append minischeme-r5rs-prefix (substring url 5))]
    [(string-prefix? url "ref:")
     (string-append minischeme-reference-prefix (substring url 4))]
    [else
     url]))

(define minischeme-r5rs-doc-links
  (map (λ (entry)
         (cons (car entry)
               (minischeme-expand-doc-url (cdr entry))))
       '(
    (* . "r5rs:r5rs-Z-H-9.html#%_idx_284")
    (+ . "r5rs:r5rs-Z-H-9.html#%_idx_282")
    (- . "r5rs:r5rs-Z-H-9.html#%_idx_286")
    (/ . "r5rs:r5rs-Z-H-9.html#%_idx_292")
    (< . "r5rs:r5rs-Z-H-9.html#%_idx_260")
    (<= . "r5rs:r5rs-Z-H-9.html#%_idx_264")
    (= . "r5rs:r5rs-Z-H-9.html#%_idx_258")
    (=> . "r5rs:r5rs-Z-H-7.html#%_idx_112")
    (> . "r5rs:r5rs-Z-H-9.html#%_idx_262")
    (>= . "r5rs:r5rs-Z-H-9.html#%_idx_266")
    (abs . "r5rs:r5rs-Z-H-9.html#%_idx_298")
    (add1 . "ref:generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29")
    (acos . "r5rs:r5rs-Z-H-9.html#%_idx_338")
    (and . "r5rs:r5rs-Z-H-7.html#%_idx_120")
    (append . "r5rs:r5rs-Z-H-9.html#%_idx_438")
    (apply . "r5rs:r5rs-Z-H-9.html#%_idx_578")
    (asin . "r5rs:r5rs-Z-H-9.html#%_idx_336")
    (assoc . "r5rs:r5rs-Z-H-9.html#%_idx_456")
    (assq . "r5rs:r5rs-Z-H-9.html#%_idx_452")
    (assv . "r5rs:r5rs-Z-H-9.html#%_idx_454")
    (atan . "r5rs:r5rs-Z-H-9.html#%_idx_340")
    (begin . "r5rs:r5rs-Z-H-7.html#%_idx_138")
    (boolean? . "r5rs:r5rs-Z-H-9.html#%_idx_388")
    (caar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caaaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caaadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caadar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caaddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cadr . "r5rs:r5rs-Z-H-9.html#%_idx_422")
    (cadaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cadadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cadar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caddar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cadddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (caddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (call-with-current-continuation . "r5rs:r5rs-Z-H-9.html#%_idx_588")
    (call/cc . "r5rs:r5rs-Z-H-9.html#%_idx_588")
    (call-with-values . "r5rs:r5rs-Z-H-9.html#%_idx_596")
    (car . "r5rs:r5rs-Z-H-9.html#%_idx_410")
    (case . "r5rs:r5rs-Z-H-7.html#%_idx_116")
    (cdddar . "r5rs:r5rs-Z-H-9.html#%_idx_424")
    (cddddr . "r5rs:r5rs-Z-H-9.html#%_idx_426")
    (cdaaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdaadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdadar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdaddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cddaar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cddadr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cddar . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cddr . "r5rs:r5rs-Z-H-9.html#%_idx_420")
    (cdr . "r5rs:r5rs-Z-H-9.html#%_idx_414")
    (ceiling . "r5rs:r5rs-Z-H-9.html#%_idx_316")
    (char->integer . "r5rs:r5rs-Z-H-9.html#%_idx_498")
    (char-alphabetic? . "r5rs:r5rs-Z-H-9.html#%_idx_488")
    (char-ci<=? . "r5rs:r5rs-Z-H-9.html#%_idx_484")
    (char-ci<? . "r5rs:r5rs-Z-H-9.html#%_idx_480")
    (char-ci=? . "r5rs:r5rs-Z-H-9.html#%_idx_478")
    (char-ci>=? . "r5rs:r5rs-Z-H-9.html#%_idx_486")
    (char-ci>? . "r5rs:r5rs-Z-H-9.html#%_idx_482")
    (char-downcase . "r5rs:r5rs-Z-H-9.html#%_idx_504")
    (char-lower-case? . "r5rs:r5rs-Z-H-9.html#%_idx_496")
    (char-numeric? . "r5rs:r5rs-Z-H-9.html#%_idx_490")
    (char-upcase . "r5rs:r5rs-Z-H-9.html#%_idx_502")
    (char-upper-case? . "r5rs:r5rs-Z-H-9.html#%_idx_494")
    (char-whitespace? . "r5rs:r5rs-Z-H-9.html#%_idx_492")
    (char<=? . "r5rs:r5rs-Z-H-9.html#%_idx_474")
    (char<? . "r5rs:r5rs-Z-H-9.html#%_idx_470")
    (char=? . "r5rs:r5rs-Z-H-9.html#%_idx_468")
    (char>=? . "r5rs:r5rs-Z-H-9.html#%_idx_476")
    (char>? . "r5rs:r5rs-Z-H-9.html#%_idx_472")
    (char? . "r5rs:r5rs-Z-H-9.html#%_idx_466")
    (cond . "r5rs:r5rs-Z-H-7.html#%_idx_108")
    (cons . "r5rs:r5rs-Z-H-9.html#%_idx_408")
    (cos . "r5rs:r5rs-Z-H-9.html#%_idx_332")
    (cosh . "ref:generic-numbers.html#%28def._%28%28lib._racket%2Fmath..rkt%29._cosh%29%29")
    (define . "r5rs:r5rs-Z-H-8.html#%_idx_194")
    (delay . "r5rs:r5rs-Z-H-7.html#%_idx_146")
    (denominator . "r5rs:r5rs-Z-H-9.html#%_idx_312")
    (display . "r5rs:r5rs-Z-H-9.html#%_idx_656")
    (displayln . "ref:Writing.html#%28def._%28%28lib._racket%2Fprivate%2Fmisc..rkt%29._displayln%29%29")
    (do . "r5rs:r5rs-Z-H-7.html#%_idx_140")
    (dynamic-wind . "r5rs:r5rs-Z-H-9.html#%_idx_598")
    (else . "r5rs:r5rs-Z-H-7.html#%_idx_110")
    (eq? . "r5rs:r5rs-Z-H-9.html#%_idx_220")
    (equal? . "r5rs:r5rs-Z-H-9.html#%_idx_222")
    (eqv? . "r5rs:r5rs-Z-H-9.html#%_idx_214")
    (eval . "r5rs:r5rs-Z-H-9.html#%_idx_600")
    (even? . "r5rs:r5rs-Z-H-9.html#%_idx_276")
    (exact->inexact . "r5rs:r5rs-Z-H-9.html#%_idx_360")
    (exact? . "r5rs:r5rs-Z-H-9.html#%_idx_254")
    (exp . "r5rs:r5rs-Z-H-9.html#%_idx_326")
    (expt . "r5rs:r5rs-Z-H-9.html#%_idx_346")
    (floor . "r5rs:r5rs-Z-H-9.html#%_idx_314")
    (for-each . "r5rs:r5rs-Z-H-9.html#%_idx_582")
    (filter . "ref:pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29")
    (first . "ref:pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._first%29%29")
    (force . "r5rs:r5rs-Z-H-9.html#%_idx_584")
    (gcd . "r5rs:r5rs-Z-H-9.html#%_idx_306")
    (if . "r5rs:r5rs-Z-H-7.html#%_idx_98")
    (inexact->exact . "r5rs:r5rs-Z-H-9.html#%_idx_362")
    (inexact? . "r5rs:r5rs-Z-H-9.html#%_idx_256")
    (integer->char . "r5rs:r5rs-Z-H-9.html#%_idx_500")
    (integer? . "r5rs:r5rs-Z-H-9.html#%_idx_252")
    (interaction-environment . "r5rs:r5rs-Z-H-9.html#%_idx_606")
    (lambda . "r5rs:r5rs-Z-H-7.html#%_idx_96")
    (lcm . "r5rs:r5rs-Z-H-9.html#%_idx_308")
    (length . "r5rs:r5rs-Z-H-9.html#%_idx_436")
    (let . "r5rs:r5rs-Z-H-7.html#%_idx_126")
    (let* . "r5rs:r5rs-Z-H-7.html#%_idx_130")
    (letrec . "r5rs:r5rs-Z-H-7.html#%_idx_134")
    (list . "r5rs:r5rs-Z-H-9.html#%_idx_434")
    (list* . "ref:pairs.html#%28def._%28%28quote._~23~25kernel%29._list%2A%29%29")
    (list->string . "r5rs:r5rs-Z-H-9.html#%_idx_548")
    (list->vector . "r5rs:r5rs-Z-H-9.html#%_idx_572")
    (list-ref . "r5rs:r5rs-Z-H-9.html#%_idx_444")
    (list-tail . "r5rs:r5rs-Z-H-9.html#%_idx_442")
    (list? . "r5rs:r5rs-Z-H-9.html#%_idx_432")
    (log . "r5rs:r5rs-Z-H-9.html#%_idx_328")
    (make-string . "r5rs:r5rs-Z-H-9.html#%_idx_510")
    (make-vector . "r5rs:r5rs-Z-H-9.html#%_idx_558")
    (map . "r5rs:r5rs-Z-H-9.html#%_idx_580")
    (max . "r5rs:r5rs-Z-H-9.html#%_idx_278")
    (member . "r5rs:r5rs-Z-H-9.html#%_idx_450")
    (memq . "r5rs:r5rs-Z-H-9.html#%_idx_446")
    (memv . "r5rs:r5rs-Z-H-9.html#%_idx_448")
    (min . "r5rs:r5rs-Z-H-9.html#%_idx_280")
    (modulo . "r5rs:r5rs-Z-H-9.html#%_idx_304")
    (negative? . "r5rs:r5rs-Z-H-9.html#%_idx_272")
    (newline . "r5rs:r5rs-Z-H-9.html#%_idx_660")
    (not . "r5rs:r5rs-Z-H-9.html#%_idx_386")
    (null-environment . "r5rs:r5rs-Z-H-9.html#%_idx_604")
    (null? . "r5rs:r5rs-Z-H-9.html#%_idx_428")
    (number->string . "r5rs:r5rs-Z-H-9.html#%_idx_364")
    (number? . "r5rs:r5rs-Z-H-9.html#%_idx_244")
    (numerator . "r5rs:r5rs-Z-H-9.html#%_idx_310")
    (odd? . "r5rs:r5rs-Z-H-9.html#%_idx_274")
    (or . "r5rs:r5rs-Z-H-7.html#%_idx_122")
    (pair? . "r5rs:r5rs-Z-H-9.html#%_idx_406")
    (positive? . "r5rs:r5rs-Z-H-9.html#%_idx_270")
    (procedure? . "r5rs:r5rs-Z-H-9.html#%_idx_576")
    (promise? . "ref:Delayed_Evaluation.html#%28def._%28%28lib._racket%2Fpromise..rkt%29._promise~3f%29%29")
    (promise-forced? . "ref:Delayed_Evaluation.html#%28def._%28%28lib._racket%2Fpromise..rkt%29._promise-forced~3f%29%29")
    (promise-running? . "ref:Delayed_Evaluation.html#%28def._%28%28lib._racket%2Fpromise..rkt%29._promise-running~3f%29%29")
    (quasiquote . "r5rs:r5rs-Z-H-7.html#%_idx_154")
    (quote . "r5rs:r5rs-Z-H-7.html#%_idx_86")
    (quotient . "r5rs:r5rs-Z-H-9.html#%_idx_300")
    (rational? . "r5rs:r5rs-Z-H-9.html#%_idx_250")
    (rationalize . "r5rs:r5rs-Z-H-9.html#%_idx_322")
    (real? . "r5rs:r5rs-Z-H-9.html#%_idx_248")
    (remainder . "r5rs:r5rs-Z-H-9.html#%_idx_302")
    (rest . "ref:pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29")
    (reverse . "r5rs:r5rs-Z-H-9.html#%_idx_440")
    (round . "r5rs:r5rs-Z-H-9.html#%_idx_320")
    (scheme-report-environment . "r5rs:r5rs-Z-H-9.html#%_idx_602")
    (second . "ref:pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29")
    (set! . "r5rs:r5rs-Z-H-7.html#%_idx_104")
    (sin . "r5rs:r5rs-Z-H-9.html#%_idx_330")
    (sinh . "ref:generic-numbers.html#%28def._%28%28lib._racket%2Fmath..rkt%29._sinh%29%29")
    (sqrt . "r5rs:r5rs-Z-H-9.html#%_idx_344")
    (string . "r5rs:r5rs-Z-H-9.html#%_idx_514")
    (string->list . "r5rs:r5rs-Z-H-9.html#%_idx_546")
    (string->number . "r5rs:r5rs-Z-H-9.html#%_idx_368")
    (string->symbol . "r5rs:r5rs-Z-H-9.html#%_idx_464")
    (string-append . "r5rs:r5rs-Z-H-9.html#%_idx_544")
    (string-ci<=? . "r5rs:r5rs-Z-H-9.html#%_idx_538")
    (string-ci<? . "r5rs:r5rs-Z-H-9.html#%_idx_534")
    (string-ci=? . "r5rs:r5rs-Z-H-9.html#%_idx_524")
    (string-ci>=? . "r5rs:r5rs-Z-H-9.html#%_idx_540")
    (string-ci>? . "r5rs:r5rs-Z-H-9.html#%_idx_536")
    (string-copy . "r5rs:r5rs-Z-H-9.html#%_idx_550")
    (string-fill! . "r5rs:r5rs-Z-H-9.html#%_idx_552")
    (string-length . "r5rs:r5rs-Z-H-9.html#%_idx_516")
    (string-ref . "r5rs:r5rs-Z-H-9.html#%_idx_518")
    (string-set! . "r5rs:r5rs-Z-H-9.html#%_idx_520")
    (string<=? . "r5rs:r5rs-Z-H-9.html#%_idx_530")
    (string<? . "r5rs:r5rs-Z-H-9.html#%_idx_526")
    (string=? . "r5rs:r5rs-Z-H-9.html#%_idx_522")
    (string>=? . "r5rs:r5rs-Z-H-9.html#%_idx_532")
    (string>? . "r5rs:r5rs-Z-H-9.html#%_idx_528")
    (string? . "r5rs:r5rs-Z-H-9.html#%_idx_508")
    (substring . "r5rs:r5rs-Z-H-9.html#%_idx_542")
    (symbol->string . "r5rs:r5rs-Z-H-9.html#%_idx_462")
    (symbol? . "r5rs:r5rs-Z-H-9.html#%_idx_460")
    (sub1 . "ref:generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._sub1%29%29")
    (tan . "r5rs:r5rs-Z-H-9.html#%_idx_334")
    (tanh . "ref:generic-numbers.html#%28def._%28%28lib._racket%2Fmath..rkt%29._tanh%29%29")
    (truncate . "r5rs:r5rs-Z-H-9.html#%_idx_318")
    (unless . "ref:when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._unless%29%29")
    (unquote . "r5rs:r5rs-Z-H-7.html#%_idx_160")
    (unquote-splicing . "r5rs:r5rs-Z-H-7.html#%_idx_162")
    (values . "r5rs:r5rs-Z-H-9.html#%_idx_594")
    (vector . "r5rs:r5rs-Z-H-9.html#%_idx_562")
    (vector->list . "r5rs:r5rs-Z-H-9.html#%_idx_570")
    (vector-fill! . "r5rs:r5rs-Z-H-9.html#%_idx_574")
    (vector-length . "r5rs:r5rs-Z-H-9.html#%_idx_564")
    (vector-ref . "r5rs:r5rs-Z-H-9.html#%_idx_566")
    (vector-set! . "r5rs:r5rs-Z-H-9.html#%_idx_568")
    (vector? . "r5rs:r5rs-Z-H-9.html#%_idx_556")
    (write . "r5rs:r5rs-Z-H-9.html#%_idx_652")
    (writeln . "ref:Writing.html#%28def._%28%28lib._racket%2Fprivate%2Fmisc..rkt%29._writeln%29%29")
    (write-char . "r5rs:r5rs-Z-H-9.html#%_idx_664")
    (when . "ref:when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._when%29%29")
    (zero? . "r5rs:r5rs-Z-H-9.html#%_idx_268")
    )))
