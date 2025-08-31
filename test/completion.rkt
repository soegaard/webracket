;;;
;;; Completion of functions in Data Structure chapter
;;;

; time racket -l errortrace -t ../webracket.rkt -- --ffi ../standard.ffi --ffi ../dom.ffi -b completion.rkt 
; racket -t ../webracket.rkt -- --ffi ../standard.ffi --ffi ../dom.ffi -b completion.rkt 

; This program displays a web-page with a section for each section in
; the Data Structure chapter in the reference.
; Each section shows a gauge with the percentage completed.
; A drop down triangle reveals a list of functions in the chapter.
; Each function is linked to its documentation.

(define (string-find/index s contained start)
  (define s-len (string-length s))
  (define c-len (string-length contained))
  (cond
    [(= c-len 0) start]
    [(< s-len c-len) #f]
    [else
     (let loop ([i start] [limit (- s-len c-len)])
       (cond
         [(> i limit) #f]
         [(string=? (substring s i (+ i c-len)) contained) i]
         [else (loop (add1 i) limit)]))]))

;; Simple wrapper starting at index 0
(define (string-find s contained)
  (string-find/index s contained 0))

;; Replace occurrences of from with to in str; all? controls replacing all occurrences.
(define (string-replace str from to [all? #t])
  ; (define all? #t)
  (define pat-len (string-length from))
  (define str-len (string-length str))

  (cond
    [(zero? pat-len) (string-copy str)]
    [else
     (let loop ([start 0] [acc ""])
       (define pos (string-find/index str from start))
       (if pos
           (let* ([next (+ pos pat-len)]
                  [acc2 (string-append acc (substring str start pos) to)])
             (if all?
                 (loop next acc2)
                 (string-append acc2 (substring str next str-len))))
           (string-append acc (substring str start str-len))))]))


(define (format fmt . args)
  (let loop ([s fmt] [args args])
    (cond
      [(null? args) s]
      [else (define a (car args))
            (when (fixnum? a) (set! a (number->string a)))
            (when (symbol? a) (set! a (symbol->string a)))
            (loop (string-replace s "~a" a #f)
                  (cdr args))])))

(define (sort-symbols syms)
  (define (insert sym lst)
    (if (null? lst)
        (cons sym '())
        (if (symbol<? sym (car lst))
            (cons sym lst)
            (cons (car lst) (insert sym (cdr lst))))))
  (if (null? syms)
      '()
      (insert (car syms) (sort-symbols (cdr syms)))))

(define (add-children elem children)
  ; (js-log 'add-children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (set-elem-attributes elem attrs)
  ; (js-log 'set-elem-attributes)
  (for ([attr (in-list attrs)])
    (match attr
      [(list name value)
       (js-set-attribute! elem (symbol->string name) value)]
      [_
       (js-log "set-elem-attributes: attrs malformed, got:")
       (js-log attrs)])))

(define (sxml->dom exp)
  ; (js-log 'sxml->dom)
  (match exp
    [(? string? s)
     (js-create-text-node exp)]
    [(list (? symbol? tag) (list '@ attrs ...) children ...)     
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem children)
     elem]
    [(list (? symbol? tag) children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (add-children elem children)
     elem]
    [_
     (js-log exp)
     (js-log "huh!?!")]))


(define datatypes-primitives
  '(
    (equality
      eq-hash-code
      eq?
      equal-always-hash-code
      equal-always-hash-code/recur
      equal-always-secondary-hash-code
      equal-always?
      equal-hash-code
      equal-hash-code/recur
      equal-secondary-hash-code
      equal?
      eqv-hash-code
      eqv?
      gen:equal+hash
      gen:equal-mode+hash
      hash-code-combine
      hash-code-combine*
      hash-code-combine-unordered
      hash-code-combine-unordered*
      prop:equal+hash)
    (booleans
      boolean=?
      boolean?
      false
      false?
      immutable-box?
      immutable-bytes?
      immutable-hash?
      immutable-string?
      immutable-vector?
      immutable?
      mutable-box?
      mutable-bytes?
      mutable-hash?
      mutable-string?
      mutable-vector?
      not
      symbol=?
      true
      xor
      )
    
    
    
    (numbers
      *
      +
      <
      <=
      =
      >
      >=
      abs
      acos
      add1
      angle
      arithmetic-shift
      asin
      bitwise-and
      bitwise-bit-field
      bitwise-bit-set?
      bitwise-first-bit-set
      bitwise-ior
      bitwise-not
      bitwise-xor
      ceiling
      complex?
      conjugate
      cos
      cosh
      crypto-random-bytes
      degrees->radians
      denominator
      double-flonum?
      even?
      exact->inexact
      exact-ceiling
      exact-floor
      exact-integer?
      exact-nonnegative-integer?
      exact-positive-integer?
      exact-round
      exact-truncate
      exact?
      exp
      expt
      fixnum?
      floating-point-bytes->real
      flonum?
      floor
      gcd
      imag-part
      inexact->exact
      inexact-real?
      inexact?
      infinite?
      integer->integer-bytes
      integer-bytes->integer
      integer-length
      integer-sqrt
      integer-sqrt/remainder
      integer?
      lcm
      log
      magnitude
      make-polar
      make-pseudo-random-generator
      make-rectangular
      max
      min
      modulo
      nan?
      natural?
      negative-integer?
      negative?
      nonnegative-integer?
      nonpositive-integer?
      number->string
      number?
      numerator
      odd?
      order-of-magnitude
      pi
      pi.f
      positive-integer?
      positive?
      pseudo-random-generator->vector
      pseudo-random-generator-vector?
      pseudo-random-generator?
      quotient
      quotient/remainder
      radians->degrees
      random-ref
      random-sample
      random-seed
      rational?
      rationalize
      real->decimal-string
      real->double-flonum
      real->floating-point-bytes
      real->single-flonum
      real-part
      real?
      remainder
      round
      sgn
      sin
      single-flonum-available?
      single-flonum?
      sinh
      sqr
      sqrt
      string->number
      sub1
      system-big-endian?
      tan
      tanh
      truncate
      vector->pseudo-random-generator
      vector->pseudo-random-generator!
      zero?
      )

    (racket/fixnum
      fixnum-for-every-system?
      fl->fx
      fx*
      fx*/wraparound
      fx+
      fx+/wraparound
      fx-
      fx-/wraparound
      fx->fl
      fx<
      fx<=
      fx=
      fx>
      fx>=
      fxabs
      fxand
      fxior
      fxlshift
      fxlshift/wraparound
      fxmax
      fxmin
      fxmodulo
      fxnot
      fxpopcount
      fxpopcount16
      fxpopcount32
      fxquotient
      fxremainder
      fxrshift
      fxrshift/logical
      fxvector
      fxvector-copy
      fxvector-length
      fxvector-ref
      fxvector-set!
      fxvector?
      fxxor
      in-fxvector
      make-fxvector
      make-shared-fxvector
      most-negative-fixnum
      most-positive-fixnum
      shared-fxvector
      )

    (strings
      build-string
      list->string
      make-string
      non-empty-string?
      string
      string->immutable-string
      string->list
      string-append
      string-append*
      string-append-immutable
      string-ci<=?
      string-ci<?
      string-ci=?
      string-ci>=?
      string-ci>?
      string-contains?
      string-copy
      string-copy!
      string-downcase
      string-fill!
      string-find
      string-foldcase
      string-grapheme-count
      string-grapheme-span
      string-join
      string-length
      string-locale-ci<?
      string-locale-ci=?
      string-locale-ci>?
      string-locale-downcase
      string-locale-upcase
      string-locale<?
      string-locale=?
      string-locale>?
      string-normalize-nfc
      string-normalize-nfd
      string-normalize-nfkc
      string-normalize-nfkd
      string-normalize-spaces
      string-prefix?
      string-ref
      string-replace
      string-set!
      string-split
      string-suffix?
      string-titlecase
      string-trim
      string-upcase
      string<=?
      string<?
      string=?
      string>=?
      string>?
      string?
      substring
      )
    (bytes
      byte?
      bytes
      bytes->immutable-bytes
      bytes->list
      bytes->string/latin-1
      bytes->string/locale
      bytes->string/utf-8
      bytes-append
      bytes-append*
      bytes-close-converter
      bytes-convert
      bytes-convert-end
      bytes-converter?
      bytes-copy
      bytes-copy!
      bytes-fill!
      bytes-join
      bytes-length
      bytes-open-converter
      bytes-ref
      bytes-set!
      bytes-utf-8-index
      bytes-utf-8-length
      bytes-utf-8-ref
      bytes<?
      bytes=?
      bytes>?
      bytes?
      list->bytes
      locale-string-encoding
      make-bytes
      make-shared-bytes
      shared-bytes
      string->bytes/latin-1
      string->bytes/locale
      string->bytes/utf-8
      string-utf-8-length
      subbytes
    )
    
    (chars
      char->integer
      char-alphabetic?
      char-blank?
      char-ci<=?
      char-ci<?
      char-ci=?
      char-ci>=?
      char-ci>?
      char-downcase
      char-extended-pictographic?
      char-foldcase
      char-general-category
      char-grapheme-break-property
      char-grapheme-step
      char-graphic?
      char-iso-control?
      char-lower-case?
      char-numeric?
      char-punctuation?
      char-symbolic?
      char-title-case?
      char-titlecase
      char-upcase
      char-upper-case?
      char-utf-8-length
      char-whitespace?
      char<=?
      char<?
      char=?
      char>=?
      char>?
      char?
      integer->char
      make-known-char-range-list
      )
    (symbols
      gensym
      string->symbol
      string->uninterned-symbol
      string->unreadable-symbol
      symbol->immutable-string
      symbol->string
      symbol-interned?
      symbol-unreadable?
      symbol<?
      symbol?
      )
    (regexps
      byte-pregexp?
      byte-regexp?
      pregexp?
      regexp-capture-group-count
      regexp-match
      regexp-match*
      regexp-match-exact?
      regexp-match-peek
      regexp-match-peek-immediate
      regexp-match-peek-positions
      regexp-match-peek-positions*
      regexp-match-peek-positions-immediate
      regexp-match-peek-positions-immediate/end
      regexp-match-peek-positions/end
      regexp-match-positions
      regexp-match-positions*
      regexp-match-positions/end
      regexp-match/end
      regexp-match?
      regexp-max-lookbehind
      regexp-replace
      regexp-replace*
      regexp-replaces
      regexp-split
      regexp-try-match
      regexp?
    )
    (keywords
      keyword->immutable-string
      keyword->string
      keyword<?
      keyword?
      string->keyword
    )
    (pairs
      add-between
      andmap
      append-map
      argmax
      argmin
      build-list
      car
      cartesian-product
      cdr
      check-duplicates
      cons?
      count
      drop-common-prefix
      eighth
      eleventh
      empty
      empty?
      fifteenth
      fifth
      filter
      filter-map
      filter-not
      first
      flatten
      foldl
      foldr
      for-each
      fourteenth
      fourth
      group-by
      hash-placeholder?
      in-permutations
      inclusive-range
      index-of
      index-where
      indexes-of
      indexes-where
      last
      last-pair
      length
      list
      list-prefix?
      list-set
      list-update
      list?
      make-hash-placeholder
      make-hashalw-placeholder
      make-hasheq-placeholder
      make-hasheqv-placeholder
      make-list
      make-placeholder
      make-reader-graph
      map
      ninth
      null
      null?
      ormap
      pair?
      partition
      permutations
      placeholder-get
      placeholder-set!
      placeholder?
      remf
      remf*
      remove
      remove*
      remove-duplicates
      remq
      remq*
      remv
      remv*
      remw
      remw*
      rest
      reverse
      running-foldl
      running-foldr
      second
      seventh
      shuffle
      sixth
      slice-by
      sort
      split-common-prefix
      take-common-prefix
      tenth
      third
      thirteenth
      twelfth
      windows
    )
    (mpairs
      mcar
      mcdr
      mcons
      mpair?
      set-mcar!
      set-mcdr!
    )
    (vectors
      build-vector
      list->vector
      make-vector
      vector
      vector*-append
      vector*-copy
      vector*-extend
      vector*-length
      vector*-ref
      vector*-set!
      vector*-set/copy
      vector->immutable-vector
      vector->list
      vector->values
      vector-append
      vector-argmax
      vector-argmin
      vector-cas!
      vector-copy
      vector-copy!
      vector-count
      vector-drop
      vector-drop-right
      vector-empty?
      vector-extend
      vector-fill!
      vector-filter
      vector-filter-not
      vector-immutable
      vector-length
      vector-map
      vector-map!
      vector-member
      vector-memq
      vector-memv
      vector-ref
      vector-set!
      vector-set*!
      vector-set/copy
      vector-sort
      vector-sort!
      vector-split-at
      vector-split-at-right
      vector-take
      vector-take-right
      vector?
    )
    (stencil-vectors
      stencil-vector
      stencil-vector-length
      stencil-vector-mask
      stencil-vector-mask-width
      stencil-vector-ref
      stencil-vector-set!
      stencil-vector-update
      stencil-vector?
    )
    (boxes
      box
      box-cas!
      box-immutable
      box?
      set-box!
      set-box*!
      unbox
      unbox*
    )
    (hashes
      hash
      hash->list
      hash-clear
      hash-clear!
      hash-copy
      hash-copy-clear
      hash-count
      hash-empty?
      hash-ephemeron?
      hash-eq?
      hash-equal-always?
      hash-equal?
      hash-eqv?
      hash-filter
      hash-filter-keys
      hash-filter-values
      hash-for-each
      hash-has-key?
      hash-intersect
      hash-iterate-first
      hash-iterate-key
      hash-iterate-key+value
      hash-iterate-next
      hash-iterate-pair
      hash-iterate-value
      hash-keys
      hash-keys-subset?
      hash-map
      hash-map/copy
      hash-ref
      hash-ref!
      hash-ref-key
      hash-remove
      hash-remove!
      hash-set
      hash-set!
      hash-set*
      hash-set*!
      hash-strong?
      hash-union
      hash-union!
      hash-update
      hash-update!
      hash-values
      hash-weak?
      hash?
      hashalw
      hasheq
      hasheqv
      make-ephemeron-hash
      make-ephemeron-hashalw
      make-ephemeron-hasheq
      make-ephemeron-hasheqv
      make-hash
      make-hashalw
      make-hasheq
      make-hasheqv
      make-immutable-hash
      make-immutable-hashalw
      make-immutable-hasheq
      make-immutable-hasheqv
      make-weak-hash
      make-weak-hashalw
      make-weak-hasheq
      make-weak-hasheqv
    )
    (treelists
      chaperone-mutable-treelist
      chaperone-treelist
      empty-treelist
      impersonate-mutable-treelist
      in-mutable-treelist
      in-treelist
      list->mutable-treelist
      list->treelist
      make-mutable-treelist
      make-treelist
      mutable-treelist
      mutable-treelist->list
      mutable-treelist->vector
      mutable-treelist-add!
      mutable-treelist-append!
      mutable-treelist-cons!
      mutable-treelist-copy
      mutable-treelist-delete!
      mutable-treelist-drop!
      mutable-treelist-drop-right!
      mutable-treelist-empty?
      mutable-treelist-find
      mutable-treelist-first
      mutable-treelist-for-each
      mutable-treelist-insert!
      mutable-treelist-last
      mutable-treelist-length
      mutable-treelist-map!
      mutable-treelist-member?
      mutable-treelist-prepend!
      mutable-treelist-ref
      mutable-treelist-reverse!
      mutable-treelist-set!
      mutable-treelist-snapshot
      mutable-treelist-sort!
      mutable-treelist-sublist!
      mutable-treelist-take!
      mutable-treelist-take-right!
      mutable-treelist?
      sequence->treelist
      treelist
      treelist->list
      treelist->vector
      treelist-add
      treelist-append
      treelist-append*
      treelist-chaperone-state
      treelist-cons
      treelist-copy
      treelist-delete
      treelist-drop
      treelist-drop-right
      treelist-empty?
      treelist-filter
      treelist-find
      treelist-first
      treelist-flatten
      treelist-for-each
      treelist-index-of
      treelist-insert
      treelist-last
      treelist-length
      treelist-map
      treelist-member?
      treelist-ref
      treelist-rest
      treelist-reverse
      treelist-set
      treelist-sort
      treelist-sublist
      treelist-take
      treelist-take-right
      treelist?
      vector->mutable-treelist
      vector->treelist
    )
    (sequences
      empty-sequence
      empty-stream
      gen:stream
      generator-state
      generator?
      in-bytes
      in-bytes-lines
      in-cycle
      in-directory
      in-ephemeron-hash
      in-ephemeron-hash-keys
      in-ephemeron-hash-pairs
      in-ephemeron-hash-values
      in-immutable-hash
      in-immutable-hash-keys
      in-immutable-hash-pairs
      in-immutable-hash-values
      in-inclusive-range
      in-indexed
      in-input-port-bytes
      in-input-port-chars
      in-lines
      in-list
      in-mlist
      in-mutable-hash
      in-mutable-hash-keys
      in-mutable-hash-pairs
      in-mutable-hash-values
      in-naturals
      in-parallel
      in-port
      in-sequences
      in-slice
      in-stream
      in-string
      in-syntax
      in-value
      in-values*-sequence
      in-values-sequence
      in-vector
      in-weak-hash
      in-weak-hash-keys
      in-weak-hash-pairs
      in-weak-hash-values
      initiate-sequence
      make-do-sequence
      prop:sequence
      prop:stream
      sequence->generator
      sequence->list
      sequence->repeated-generator
      sequence->stream
      sequence-add-between
      sequence-andmap
      sequence-append
      sequence-count
      sequence-filter
      sequence-fold
      sequence-for-each
      sequence-generate
      sequence-generate*
      sequence-length
      sequence-map
      sequence-ormap
      sequence-ref
      sequence-tail
      sequence/c
      sequence?
      stop-after
      stop-before
      stream->list
      stream-add-between
      stream-andmap
      stream-append
      stream-count
      stream-empty?
      stream-filter
      stream-first
      stream-fold
      stream-for-each
      stream-force
      stream-length
      stream-map
      stream-ormap
      stream-ref
      stream-rest
      stream-tail
      stream-take
      stream/c
      stream?
      yield
    )
    (dicts
      dict->list
      dict-can-functional-set?
      dict-can-remove-keys?
      dict-clear
      dict-clear!
      dict-copy
      dict-count
      dict-empty?
      dict-for-each
      dict-has-key?
      dict-implements/c
      dict-implements?
      dict-iter-contract
      dict-iterate-first
      dict-iterate-key
      dict-iterate-next
      dict-iterate-value
      dict-key-contract
      dict-keys
      dict-map
      dict-map/copy
      dict-mutable?
      dict-ref
      dict-ref!
      dict-remove
      dict-remove!
      dict-set
      dict-set!
      dict-set*
      dict-set*!
      dict-update
      dict-update!
      dict-value-contract
      dict-values
      dict?
      in-dict
      in-dict-keys
      in-dict-pairs
      in-dict-values
      make-custom-hash
      make-custom-hash-types
      make-immutable-custom-hash
      make-weak-custom-hash
      prop:dict
      prop:dict/contract
    )
    (sets
      chaperone-hash-set
      generic-set?
      impersonate-hash-set
      in-immutable-set
      in-mutable-set
      in-set
      in-weak-set
      list->mutable-set
      list->mutable-setalw
      list->mutable-seteq
      list->mutable-seteqv
      list->set
      list->setalw
      list->seteq
      list->seteqv
      list->weak-set
      list->weak-setalw
      list->weak-seteq
      list->weak-seteqv
      make-custom-set-types
      mutable-set
      mutable-setalw
      mutable-seteq
      mutable-seteqv
      proper-subset?
      set
      set->list
      set->stream
      set-add
      set-add!
      set-clear
      set-clear!
      set-copy
      set-copy-clear
      set-count
      set-empty?
      set-eq?
      set-equal-always?
      set-equal?
      set-eqv?
      set-first
      set-for-each
      set-implements/c
      set-implements?
      set-intersect
      set-intersect!
      set-map
      set-member?
      set-mutable?
      set-remove
      set-remove!
      set-rest
      set-subtract
      set-subtract!
      set-symmetric-difference
      set-symmetric-difference!
      set-union
      set-union!
      set-weak?
      set/c
      set=?
      set?
      setalw
      seteq
      seteqv
      subset?
      weak-set
      weak-setalw
      weak-seteq
      weak-seteqv
    )
    (procedures
      apply
      arity-includes?
      arity=?
      checked-procedure-check-and-extract
      compose
      compose1
      conjoin
      const
      const*
      disjoin
      identity
      keyword-apply
      make-keyword-procedure
      negate
      normalize-arity
      normalized-arity?
      primitive-closure?
      primitive-result-arity
      primitive?
      procedure->method
      procedure-arity
      procedure-arity-includes?
      procedure-arity-mask
      procedure-arity?
      procedure-closure-contents-eq?
      procedure-extract-target
      procedure-keywords
      procedure-realm
      procedure-reduce-arity
      procedure-reduce-arity-mask
      procedure-reduce-keyword-arity
      procedure-reduce-keyword-arity-mask
      procedure-rename
      procedure-result-arity
      procedure-specialize
      procedure-struct-type?
      procedure?
      prop:arity-string
      prop:checked-procedure
      prop:procedure
    )
    (racket/flonum
      fl+
      fl-
      fl*
      fl/
      flabs
      fl=
      fl<
      fl>
      fl<=
      fl>=
      flmin
      flmax
      flround
      flfloor
      flceiling
      fltruncate
      flsingle
      flbit-field
      flsin
      flcos
      fltan
      flasin
      flacos
      flatan
      fllog
      flexp
      flsqrt
      flexpt
      ->fl
      fl->exact-integer
      make-flrectangular
      flreal-part
      flimag-part
      flrandom
      flvector?
      flvector
      make-flvector
      flvector-length
      flvector-ref
      flvector-set!
      flvector-copy
      in-flvector
      shared-flvector
      make-shared-flvector
    )
    (void
      void
      void?
    )
    (undefined
      undefined
    )
  ))
  

#;(define (symbol->title s)
    ; todo : implement string-titlecase
    (string-titlecase (symbol->string s)))

(define (symbol->title s)
  (symbol->string s))

(define implemented-constants
  '(null undefined empty true false pi))


(define implemented-primitives
  (append implemented-constants
          '(namespace-undefine-variable!
            namespace-set-variable-value!
            namespace-variable-value-simple
            make-empty-namespace
            namespace?
            unsafe-struct-set!
            unsafe-vector*-set!
            unsafe-vector*-length
            unsafe-struct-ref
            unsafe-cdr
            unsafe-car
            unsafe-fx<
            unsafe-fx=
            unsafe-flexpt
            unsafe-flmax
            unsafe-flmin
            unsafe-flsqrt
            unsafe-flexp
            unsafe-fllog
            unsafe-flatan
            unsafe-flacos
            unsafe-flasin
            unsafe-fltan
            unsafe-flcos
            unsafe-flsin
            unsafe-flsingle
            unsafe-fltruncate
            unsafe-flceiling
            unsafe-flfloor
            unsafe-flround
            unsafe-flabs
            unsafe-fl/
            unsafe-fx+
            call-with-values
            js-log
            variable-reference-constant?
            variable-reference-from-unsafe?
            primitive-result-arity
            primitive-closure?
            primitive?
            procedure-arity-includes?
            procedure-arity-mask
            procedure-arity
            procedure->external
            procedure?
            procedure-rename
            apply
            keyword<?
            string->keyword
            keyword->immutable-string
            keyword->string
            keyword?
            eq-hash-code
            hash-has-key?
            hash-clear!
            hash-remove!
            hash-set!
            hash-ref
            hash?
            make-hasheq
            make-empty-hasheq
            fasl->s-exp
            s-exp->fasl
            port-next-location
            write-byte
            get-output-bytes
            open-output-bytes
            string-port?
            symbol->immutable-string
            symbol-interned?
            string->uninterned-symbol
            symbol->string
            string->symbol
            symbol<?
            symbol=?
            symbol?
            string-contains?
            string-prefix?
            string-suffix?
            string-trim-right
            string-trim-left
            string-drop-right
            string-drop
            string-take-right
            string-take
            non-empty-string?
            string->immutable-string
            string->bytes/utf-8
            list->string
            string->list
            string-append-immutable
            string-append
            string-fill!
            string-copy
            string-copy!
            substring
            string-length
            string-set!
            string-ref
            build-string
            make-string
            string>=?
            string>?
            string<=?
            string<?
            string=?
            string?
            bytes->string/utf-8
            bytes=?
            list->bytes
            bytes->list
            bytes->immutable-bytes
            bytes-append
            bytes-fill!
            bytes-copy
            bytes-copy!
            subbytes
            bytes-length
            bytes-set!
            bytes-ref
            make-bytes
            bytes?
            vector-copy
            list->vector
            vector->list
            vector-split-at
            vector-drop-right
            vector-drop
            vector-take
            vector-empty?
            vector-copy!
            vector-fill!
            vector-length
            vector-set!
            vector-ref
            make-vector
            vector?
            vector
            byte?
            flexpt
            flmax
            flmin
            flatan
            flacos
            flasin
            fltan
            flcos
            flsin
            flsqrt
            flexp
            fllog
            flsingle
            fltruncate
            flceiling
            flfloor
            flround
            flabs
            fl>=
            fl<=
            fl>
            fl<
            fl=
            fl/
            fl*
            fl-
            fl+
            flonum?
            fl->fx
            fx->fl
            most-negative-fixnum
            most-positive-fixnum
            fxrshift/logical
            fxlshift/wraparound
            fx*/wraparound
            fx-/wraparound
            fx+/wraparound
            fxpopcount32
            fxpopcount16
            fxpopcount
            fxrshift
            fxlshift
            fxnot
            fxxor
            fxior
            fxand
            fxabs
            fxmodulo
            fxremainder
            unsafe-fxquotient
            fxquotient
            fxmax
            fxmin
            fx>=
            fx<=
            fx<
            fx>
            fx=
            fx*
            fx-
            fx+
            fxzero?
            fixnum?
            sqrt
            abs
            atan
            acos
            asin
            tan
            cos
            sin
            truncate
            ceiling
            floor
            round
            inexact->exact
            exact-positive-integer?
            exact-nonnegative-integer?
            exact-integer?
            exact?
            integer?
            number?
            sub1
            add1
            negative?
            positive?
            zero?
            >=
            <=
            >
            <
            =
            /
            *
            -
            +
            number->string
            equal?
            eqv?
            eq?
            char-whitespace?
            char-ci>=?
            char-ci>?
            char-ci<=?
            char-ci<?
            char-ci=?
            char-upcase
            char-titlecase
            char-foldcase
            char-downcase
            char>=?
            char>?
            char<=?
            char<?
            char=?
            integer->char
            char->integer
            char?
            immutable?
            not
            boolean?
            void
            make-void
            void?
            make-list
            filter
            list*
            for-each
            map
            alt-reverse
            memq
            reverse
            append
            list-tail
            list-ref
            length
            list?
            list
            cdr
            car
            cons
            null?
            pair?
            string
            bytes
            vector-immutable
            set-box!
            unbox
            box
            set-boxed!
            unboxed
            boxed
            values
            current-inspector
            struct-type?
            struct?
            struct-mutator-procedure?
            struct-accessor-procedure?
            struct-predicate-procedure?
            struct-constructor-procedure?
            make-struct-field-mutator
            make-struct-field-accessor
            make-struct-type
            raise-unbound-variable-reference)






          ))

(define sections
  (for/list ([entry (in-list datatypes-primitives)])
    (list (symbol->title (car entry)) (cdr entry))))

; (js-log sections)


;;; Gauge component: renders a flex container with a gradient-filled bar
;;; showing the percentage. The unfilled portion is covered with a grey
;;; overlay so the gradient corresponds to the entire gauge, not just the
;;; filled width.
(define (make-gauge pct title)
  ; (js-log 'make-gauge)  
  (define pct-num       (inexact->exact (round (* 100 pct))))
  (define pct-str       (number->string pct-num))
  (define remaining-str (number->string (- 100 pct-num)))
  (define container-style 
    (string-append "position:relative;"
                   "background:linear-gradient(to right, red, green);"
                   "width:200px;height:20px;"
                   "border:1px solid #000;"))
  (define overlay-style 
    (string-append "position:absolute;top:0;right:0;height:100%;width:"
                   remaining-str
                   "%;background:#ddd;"))
  `(span (@ (style "display:flex;align-items:center;gap:8px;"))
         (span (@ (style "width:5em;"))
               ,title)
         (span (@ (style ,container-style))
               (span (@ (style ,overlay-style))))
         " "
         (span (@ (style "font-size: 2em;width:3em;text-align:right;"))
               ,(string-append pct-str "%")))
  )

(define (primitive-url sym)
  (string-append "https://docs.racket-lang.org/search/index.html?q="
                 (symbol->string sym)))

(define (primitive-li sym)  
  (define checked? (memq sym implemented-primitives))
  (define box      (if checked? "☑" "☐"))
  (define str      (symbol->string sym))
  `(li
     (label
       (span ,box)
       (a (@ (href ,(primitive-url sym)))
          ,str))))

(define (section->sxml section idx)
  ; (js-log 'section->sxml)
  (match section
    [(list title primitives)
     #;(define implemented (filter (lambda (p) (memq p implemented-primitives))
                                   primitives))
     (define implemented
       (for/list ([p (in-list primitives)]
                  #:when (memq p implemented-primitives))
         p))

     (define pct (if (null? primitives)
                     0
                     (/ (length implemented) (length primitives))))

     (define list-id (format "sec-~a-list" idx))
     (define tri-id  (format "sec-~a-tri"  idx))
     
     (define toggle-script
       (format (string-append "var ul=document.getElementById('~a');"
                              "var tri=document.getElementById('~a');"
                              "if(  ul.style.display==='none'){ul.style.display='';tri.textContent='\\u25BC';}"
                              "else{ul.style.display='none';tri.textContent='\\u25B6';}")
               list-id tri-id))

     
     (js-log tri-id)
     
     `(section              
       (div (h2 ,(make-gauge pct title))
            (span (@ (id      ,tri-id)
                     (onclick ,toggle-script)
                     (style   "cursor:pointer;"))
                  "▶")
            (ul (@ (id    ,list-id)
                   (style "display:none;"))
                ,@(map primitive-li (sort-symbols primitives)))
            (hr)))]))


(define sections
  (for/list ([s (in-list sections)]
             [i (in-naturals)])
    (section->sxml s i)))

(define page
  `(div (h1 "Progress: Datatype Functions and Constants")
        ""
        ,@sections))

(js-append-child! (js-document-body) (sxml->dom page))
