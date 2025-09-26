;;;
;;; Completion of functions in the chapters:
;;;   Data Structures
;;;   Input and Output
;;;

; time racket -l errortrace -t ../webracket.rkt -- --ffi ../standard.ffi --ffi ../dom.ffi -b completion.rkt 
; racket -t ../webracket.rkt -- --ffi ../standard.ffi --ffi ../dom.ffi -b completion.rkt 

; This program displays a web-page with a section for each section in
; the Data Structure chapter in the reference.
; Each section shows a gauge with the percentage completed.
; A drop down triangle reveals a list of functions in the chapter.
; Each function is linked to its documentation.

; (define (cadr x) (car (cdr x)))

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
      equal-always?/recur
      equal-hash-code
      equal-hash-code/recur
      equal-secondary-hash-code
      equal?
      equal?/recur
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
      +
      -
      *
      /
      <
      <=
      =
      >
      >=
      abs
      acos
      acosh
      asin
      asinh
      atan
      atanh
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
      current-pseudo-random-generator
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
      random
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
     byte-pregexp
     byte-pregexp?
     byte-regexp
     byte-regexp?
     pregexp
     pregexp?
     pregexp-quote

     regexp
     regexp-quote
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
     regexp-replace-quote
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
      append
      append*
      append-map
      argmax
      argmin
      assf
      assoc
      assq
      assv
      assw
      build-list
      car
      cartesian-product
      cdr
      check-duplicates
      combinations
      cons
      cons?
      count
      drop
      drop-common-prefix
      drop-right
      dropf
      dropf-right
      eighth
      eleventh
      empty
      empty?
      fifteenth
      fifth
      filter
      filter-map
      filter-not
      findf
      first
      flatten
      foldl
      foldr
      for-each
      fourteenth
      fourth
      group-by
      hash-placeholder?
      in-combinations
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
      list*
      list-prefix?
      list-ref
      list-set
      list-tail
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
      member
      memf
      memq
      memv
      memw
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
      range
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
      split-at
      split-at-right
      split-common-prefix
      splitf-at
      splitf-at-right
      take
      take-common-prefix
      take-right
      takef
      takef-right
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
      in-hash
      in-hash-keys
      in-hash-pairs
      in-hash-values
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
      in-producer
      in-range
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
      keyword-apply/dict
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
     arity=?
     arity-at-least
     arity-includes?
     checked-procedure-check-and-extract
     compose
     compose1
     conjoin
     const
     const*
     curry
     curryr
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


(define io-primitives
  '((ports
      call-with-input-bytes
      call-with-input-file
      call-with-input-file*
      call-with-input-string
      call-with-output-bytes
      call-with-output-file
      call-with-output-file*
      call-with-output-string
      close-input-port
      close-output-port
      combine-output
      convert-stream
      copy-port
      current-error-port
      current-input-port
      current-locale
      current-output-port
      display-lines
      dup-input-port
      dup-output-port
      eof
      eof-evt
      eof-object?
      file-position
      file-position*
      file-stream-buffer-mode
      file-stream-port?
      file-truncate
      filter-read-input-port
      flush-output
      get-output-bytes
      get-output-string
      input-port-append
      input-port?
      make-input-port
      make-input-port/read-to-peek
      make-limited-input-port
      make-output-port
      make-pipe
      make-pipe-with-specials
      merge-input
      open-input-bytes
      open-input-file
      open-input-nowhere
      open-input-output-file
      open-input-string
      open-output-bytes
      open-output-file
      open-output-nowhere
      open-output-string
      output-port?
      peek-bytes-evt
      peek-string!-evt
      peek-string-evt
      peeking-input-port
      pipe-content-length
      pipe-port?
      port->bytes
      port->bytes-lines
      port->lines
      port->list
      port->string
      port-closed-evt
      port-closed?
      port-count-lines!
      port-count-lines-enabled
      port-counts-lines?
      port-file-identity
      port-file-stat
      port-file-unlock
      port-next-location
      port-try-file-lock?
      port-waiting-peer?
      port?
      prop:input-port
      prop:output-port
      read-bytes!-evt
      read-bytes-avail!-evt
      read-bytes-evt
      read-bytes-line-evt
      read-line-evt
      read-string!-evt
      read-string-evt
      reencode-input-port
      reencode-output-port
      regexp-match-evt
      relocate-input-port
      relocate-output-port
      set-port-next-location!
      special-filter-input-port
      string-port?
      terminal-port?
      transplant-input-port
      transplant-output-port
      with-input-from-bytes
      with-input-from-file
      with-input-from-string
      with-output-to-bytes
      with-output-to-file
      with-output-to-string)
    (byte-and-string-input
      byte-ready?
      char-ready?
      peek-byte
      peek-byte-or-special
      peek-bytes
      peek-bytes!
      peek-bytes-avail!
      peek-bytes-avail!*
      peek-bytes-avail!/enable-break
      peek-char
      peek-char-or-special
      peek-string
      peek-string!
      port-commit-peeked
      port-progress-evt
      port-provides-progress-evts?
      progress-evt?
      read-byte
      read-byte-or-special
      read-bytes
      read-bytes!
      read-bytes-avail!
      read-bytes-avail!*
      read-bytes-avail!/enable-break
      read-bytes-line
      read-char
      read-char-or-special
      read-line
      read-string
      read-string!)
    (byte-and-string-output
      newline
      port-writes-atomic?
      port-writes-special?
      write-byte
      write-bytes
      write-bytes-avail
      write-bytes-avail*
      write-bytes-avail-evt
      write-bytes-avail/enable-break
      write-char
      write-special
      write-special-avail*
      write-special-evt
      write-string)
    (reading
      call-with-default-reading-parameterization
      current-reader-guard
      current-readtable
      port-read-handler
      read
      read-accept-bar-quote
      read-accept-box
      read-accept-compiled
      read-accept-dot
      read-accept-graph
      read-accept-infix-dot
      read-accept-lang
      read-accept-quasiquote
      read-accept-reader
      read-case-sensitive
      read-cdot
      read-curly-brace-as-paren
      read-curly-brace-with-tag
      read-decimal-as-inexact
      read-language
      read-on-demand-source
      read-single-flonum
      read-square-bracket-as-paren
      read-square-bracket-with-tag
      read-syntax
      read-syntax-accept-graph
      read-syntax/recursive
      read/recursive)
    ))

(define operating-system-primitives
  '((paths
     path?
     path-string?
     path-for-some-system?
     string->path
     bytes->path
     path->string
     path->bytes
     string->path-element
     bytes->path-element
     path-element->string
     path-element->bytes
     path<?
     path-convention-type
     system-path-convention-type
     build-path
     build-path/convention-type
     absolute-path?
     relative-path?
     complete-path?
     path->complete-path
     path->directory-path
     resolve-path
     cleanse-path
     expand-user-path
     simplify-path
     normal-case-path
     split-path
     explode-path
     path-replace-extension
     path-add-extension
     path-replace-suffix
     path-add-suffix
     reroot-path
     file-name-from-path
     path-get-extension
     path-has-extension?
     filename-extension
     find-relative-path
     normalize-path
     path-element?
     path-only
     simple-form-path
     some-system-path->string
     string->some-system-path
     shrink-path-wrt)))

(define macros-primitives
  '((syntax-object-content
      syntax?
      identifier?
      syntax-source
      syntax-line
      syntax-column
      syntax-position
      syntax-span
      syntax-original?
      syntax-source-module
      syntax-e
      syntax->list
      syntax->datum
      datum->syntax
      syntax-binding-set?
      syntax-binding-set
      syntax-binding-set->syntax
      syntax-binding-set-extend
      datum-intern-literal
      syntax-shift-phase-level
      generate-temporaries
      identifier-prune-lexical-context
      identifier-prune-to-source-module
      syntax-recertify
      syntax-debug-info))
  )

#;(define (symbol->title s)
    ; todo : implement string-titlecase
    (string-titlecase (symbol->string s)))

(define (symbol->title s)
  (symbol->string s))

(define implemented-constants
  '(null undefined empty true false pi))


(define implemented-primitives
  (append implemented-constants
          '(*
            +
            -
            ->fl
            /
            <
            <=
            =
            >
            >=
            abs
            acos
            acosh
            add-between
            add1
            alt-reverse
            always-throw
            andmap
            append
            append*
            append-map
            apply
            argmax
            argmin
            arithmetic-shift
            asin
            asinh
            assf
            assoc
            assq
            assv
            assw
            atan
            atanh
            bitwise-and
            bitwise-bit-field
            bitwise-bit-set?
            bitwise-first-bit-set
            bitwise-ior
            bitwise-not
            bitwise-xor
            boolean=?
            boolean?
            box
            box-immutable
            box?
            boxed
            build-list
            build-string
            build-vector
            byte-ready?
            byte?
            bytes
            bytes->immutable-bytes
            bytes->list
            bytes->path
            bytes->string/utf-8
            bytes-append
            bytes-append*
            bytes-copy
            bytes-copy!
            bytes-fill!
            bytes-join
            bytes-length
            bytes-ref
            bytes-set!
            bytes<?
            bytes=?
            bytes>?
            bytes?
            caaaar
            caaadr
            caaar
            caadar
            caaddr
            caadr
            caar
            cadaar
            cadadr
            cadar
            caddar
            cadddr
            caddr
            cadr
            call-with-values
            car
            cartesian-product
            cdaaar
            cdaadr
            cdaar
            cdadar
            cdaddr
            cdadr
            cdar
            cddaar
            cddadr
            cddar
            cdddar
            cddddr
            cdddr
            cddr
            cdr
            ceiling
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
            char-graphic?
            char-iso-control?
            char-lower-case?
            char-numeric?
            char-punctuation?
            char-ready?
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
            check-list
            check-mlist
            check-naturals
            check-range
            check-range-generic
            cons
            cons?
            cos
            cosh
            count
            current-inspector
            datum->syntax
            degrees->radians
            double-flonum?
            drop
            drop-common-prefix
            drop-right
            dropf
            dropf-right
            eighth
            eleventh
            empty?
            eof-object?
            eq-hash-code
            eq?
            equal-always?
            equal-hash-code
            equal?
            eqv-hash-code
            eqv?
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
            external-number->flonum
            external-string->string
            external?
            false?
            fasl->s-exp
            fifteenth
            fifth
            filter
            filter-map
            filter-not
            findf
            first
            fixnum?
            fl*
            fl+
            fl-
            fl->exact-integer
            fl->fx
            fl/
            fl<
            fl<=
            fl=
            fl>
            fl>=
            flabs
            flacos
            flasin
            flatan
            flatten
            flbit-field
            flceiling
            flcos
            flexp
            flexpt
            flfloor
            fllog
            flmax
            flmin
            floating-point-bytes->real
            flonum?
            floor
            flrandom
            flround
            flsin
            flsingle
            flsqrt
            fltan
            fltruncate
            foldl
            foldr
            for-each
            fourteenth
            fourth
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
            fxxor
            fxzero?
            gcd
            get-output-bytes
            get-output-string
            group-by
            hash->list
            hash-clear!
            hash-count
            hash-empty?
            hash-eq?
            hash-equal-always?
            hash-equal?
            hash-eqv?
            hash-filter
            hash-filter-keys
            hash-filter-values
            hash-for-each
            hash-has-key?
            hash-keys
            hash-map
            hash-map/copy
            hash-ref
            hash-ref!
            hash-remove!
            hash-set!
            hash-update!
            hash-values
            hash?
            immutable-box?
            immutable-bytes?
            immutable-hash?
            immutable-string?
            immutable-vector?
            immutable?
            inclusive-range
            inclusive-range-proc
            index-of
            index-where
            indexes-of
            indexes-where
            inexact->exact
            inexact-real?
            inexact?
            infinite?
            integer->char
            integer-length
            integer-sqrt
            integer-sqrt/remainder
            integer?
            js-log
            keyword->immutable-string
            keyword->string
            keyword<?
            keyword?
            last
            last-pair
            lcm
            length
            list
            list*
            list->bytes
            list->string
            list->vector
            list-prefix?
            list-ref
            list-set
            list-tail
            list-update
            list?
            log
            make-bytes
            make-empty-hash
            make-empty-hashalw
            make-empty-hasheq
            make-empty-hasheqv
            make-empty-namespace
            make-hash
            make-hashalw
            make-hasheq
            make-hasheqv
            make-list
            make-srcloc
            make-string
            make-struct-field-accessor
            make-struct-field-mutator
            make-struct-type
            make-vector
            make-void
            map
            max
            mcar
            mcdr
            mcons
            member
            memf
            memq
            memv
            memw
            min
            modulo
            most-negative-fixnum
            most-positive-fixnum
            mpair?
            mutable-box?
            mutable-bytes?
            mutable-hash?
            mutable-string?
            mutable-vector?
            namespace-set-variable-value!
            namespace-undefine-variable!
            namespace-variable-value-simple
            namespace?
            nan?
            natural?
            negative-integer?
            negative?
            newline
            ninth
            non-empty-string?
            nonnegative-integer?
            nonpositive-integer?
            not
            null?
            number->string
            number?
            odd?
            open-input-bytes
            open-input-string
            open-output-bytes
            open-output-string
            order-of-magnitude
            ormap
            pair?
            partition
            path->bytes
            path->string
            path-for-some-system?
            path-string?
            path?
            peek-byte
            peek-bytes
            peek-bytes!
            peek-char
            peek-string
            peek-string!
            permutations
            port-next-location
            positive-integer?
            positive?
            primitive-closure?
            primitive-result-arity
            primitive?
            procedure->external
            procedure-arity
            procedure-arity-includes?
            procedure-arity-mask
            procedure-rename
            procedure?
            quotient
            quotient/remainder
            radians->degrees
            raise
            raise-argument-error
            raise-unbound-variable-reference
            random
            range
            range-proc
            read-byte
            read-bytes
            read-bytes!
            read-char
            read-line
            read-string
            read-string!
            real->double-flonum
            real->floating-point-bytes
            real?
            remainder
            remf
            remf*
            remove
            remove*
            remq
            remq*
            remv
            remv*
            remw
            remw*
            rest
            reverse
            round
            s-exp->fasl
            second
            set-box!
            set-boxed!
            set-mcar!
            set-mcdr!
            seventh
            sgn
            shuffle
            sin
            single-flonum-available?
            single-flonum?
            sinh
            sixth
            sort
            split-at
            split-at-right
            split-common-prefix
            splitf-at
            splitf-at-right
            sqr
            sqrt
            srcloc
            srcloc->string
            srcloc-column
            srcloc-line
            srcloc-position
            srcloc-source
            srcloc-span
            srcloc?
            string
            string->bytes/utf-8
            string->immutable-string
            string->keyword
            string->list
            string->number
            string->symbol
            string->uninterned-symbol
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
            string-drop
            string-drop-right
            string-fill!
            string-find
            string-foldcase
            string-join
            string-length
            string-port?
            string-prefix?
            string-ref
            string-replace
            string-set!
            string-split
            string-suffix?
            string-take
            string-take-right
            string-titlecase
            string-trim
            string-trim-left
            string-trim-right
            string-upcase
            string-utf-8-length
            string<=?
            string<?
            string=?
            string>=?
            string>?
            string?
            struct-accessor-procedure?
            struct-constructor-procedure?
            struct-mutator-procedure?
            struct-predicate-procedure?
            struct-type?
            struct?
            sub1
            subbytes
            substring
            symbol->immutable-string
            symbol->string
            symbol-interned?
            symbol<?
            symbol=?
            symbol?
            syntax-column
            syntax-e
            syntax-line
            syntax-position
            syntax-source
            syntax-span
            syntax?
            system-big-endian?
            take
            take-common-prefix
            take-right
            takef
            takef-right
            tan
            tanh
            tenth
            third
            thirteenth
            truncate
            twelfth
            unbox
            unboxed
            unsafe-car
            unsafe-cdr
            unsafe-fl/
            unsafe-flabs
            unsafe-flacos
            unsafe-flasin
            unsafe-flatan
            unsafe-flceiling
            unsafe-flcos
            unsafe-flexp
            unsafe-flexpt
            unsafe-flfloor
            unsafe-fllog
            unsafe-flmax
            unsafe-flmin
            unsafe-flrandom
            unsafe-flround
            unsafe-flsin
            unsafe-flsingle
            unsafe-flsqrt
            unsafe-fltan
            unsafe-fltruncate
            unsafe-fx+
            unsafe-fx<
            unsafe-fx=
            unsafe-fxquotient
            unsafe-struct-ref
            unsafe-struct-set!
            unsafe-vector*-length
            unsafe-vector*-set!
            unsafe-vector-length
            unsafe-vector-ref
            values
            variable-reference-constant?
            variable-reference-from-unsafe?
            vector
            vector->immutable-vector
            vector->list
            vector->values
            vector-append
            vector-argmax
            vector-argmin
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
            vector-set/copy
            vector-sort
            vector-sort!
            vector-split-at
            vector-split-at-right
            vector-take
            vector-take-right
            vector?
            void
            void?
            write-byte
            write-bytes
            write-char
            write-string
            xor
            zero?)


          ))

(define chapter-datasets
  (list (list "Datatype Functions and Constants"        datatypes-primitives)
        (list "Input and Output"                               io-primitives)
        (list "Operating System"                 operating-system-primitives)
        (list "Macros"                                     macros-primitives)))

(define total-primitives-cnt
  (for/sum ([chapter (in-list chapter-datasets)])
    (for/sum ([entry (in-list (cadr chapter))])
      (length (cdr entry)))))

(define missing-primitives
  (- total-primitives-cnt (length implemented-primitives)))

(define prepared-chapters
  (for/list ([chapter (in-list chapter-datasets)])
    (let ()
      (define title (car chapter))
      (define raw-sections (cadr chapter))
      (list title
            (for/list ([entry (in-list raw-sections)])
              (list (symbol->title (car entry)) (cdr entry)))))))

; (js-log prepared-chapters)


;;; Gauge component: renders a flex container with a gradient-filled bar
;;; showing the percentage. The unfilled portion is covered with a grey
;;; overlay so the gradient corresponds to the entire gauge, not just the
;;; filled width.
(define (make-gauge pct implemented-cnt primitives-cnt title)
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
               ,(string-append pct-str "%"))
         " "
         (span (@ (style "font-size: 1em;width:6em;text-align:right;"))
               "("
               ,(number->string implemented-cnt)
               " of "
               ,(number->string primitives-cnt)
               ")"))
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
       (div (h2 ,(make-gauge pct (length implemented) (length primitives) title))
            (span (@ (id      ,tri-id)
                     (class   "section-tri")
                     (onclick ,toggle-script)
                     (style   "cursor:pointer;"))
                  "▶")
            (ul (@ (id    ,list-id)
                   (class "section-list")
                   (style "display:none;"))
                ,@(map primitive-li (sort-symbols primitives)))
            (hr)))]))


(define toggle-all-script
  (string-append
   "var btn=document.getElementById('toggle-all');"
   "var open=btn.getAttribute('data-open')==='true';"
   "var lists=document.getElementsByClassName('section-list');"
   "var tris=document.getElementsByClassName('section-tri');"
   "if(open){"
   "for(var i=0;i<lists.length;i++){lists[i].style.display='none';}"
   "for(var i=0;i<tris.length;i++){tris[i].textContent='\\u25B6';}"
   "btn.setAttribute('data-open','false');"
   "btn.textContent='Expand All';"
   "}else{"
   "for(var i=0;i<lists.length;i++){lists[i].style.display='';}"
   "for(var i=0;i<tris.length;i++){tris[i].textContent='\\u25BC';}"
   "btn.setAttribute('data-open','true');"
   "btn.textContent='Collapse All';"
   "}"))


(define-values (chapters-sxml _)
  (for/fold ([nodes '()] [idx 0])
            ([chapter (in-list prepared-chapters)])
    (let ()
      (define title    (car chapter))
      (define sections (cadr chapter))
      (define rendered
        (for/list ([section (in-list sections)]
                   [i       (in-naturals idx)])
          (section->sxml section i)))
      (values (append nodes
                      (list `(h2 (@ (style "color:red; font-size: 2.5rem; font-family: sans-serif;"))
                                 ,title))
                      rendered
                      #;(add-between rendered '(div)))
              (+ idx (length sections))))))

(define page
  `(div (h1 (@ (style "font-size: 3.5rem; font-family: sans-serif;"))
            "WebRacket Implementation Progress")
        (span "Implemented: "
              ,(number->string (length implemented-primitives)))
        (div "Missing: "
             ,(number->string missing-primitives))
        (button (@ (id "toggle-all")
                    (data-open "false")
                    (onclick ,toggle-all-script)
                    (style "margin: 1em 0; padding: 0.5em 1em;"))
                "Expand All")
        ,@chapters-sxml))

(js-append-child! (js-document-body) (sxml->dom page))
