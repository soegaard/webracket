;;;
;;; Implementation Status Dashboard data + helpers.
;;; Included by web-site.rkt to render the WebRacket status page.
;;;

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

(define reflection-and-security-primitives
  '((|Linklets and the Core Compiler|
      linklet?
      compile-linklet
      recompile-linklet
      eval-linklet
      instantiate-linklet
      linklet-import-variables
      linklet-export-variables
      linklet-add-target-machine-info
      linklet-summarize-target-machine-info
      linklet-directory?
      hash->linklet-directory
      linklet-directory->hash
      linklet-bundle?
      hash->linklet-bundle
      linklet-bundle->hash
      linklet-body-reserved-symbol?
      instance?
      make-instance
      instance-name
      instance-data
      instance-variable-names
      instance-variable-value
      instance-set-variable-value!
      instance-unset-variable!
      instance-describe-variable!
      variable-reference->instance
      correlated?
      correlated-source
      correlated-line
      correlated-column
      correlated-position
      correlated-span
      correlated-e
      correlated->datum
      datum->correlated
      correlated-property
      correlated-property
      correlated-property-symbol-keys)))



(define control-flow-primitives
  '((exceptions
      raise
      error
      raise-user-error
      raise-argument-error
      raise-argument-error*
      raise-result-error
      raise-result-error*
      raise-arguments-error
      raise-arguments-error*
      raise-range-error
      raise-range-error*
      raise-type-error
      raise-mismatch-error
      raise-arity-error
      raise-arity-error*
      raise-arity-mask-error
      raise-arity-mask-error*
      raise-result-arity-error
      raise-result-arity-error*
      raise-syntax-error
      unquoted-printing-string?
      unquoted-printing-string
      unquoted-printing-string-value
      call-with-exception-handler
      uncaught-exception-handler
      ; with-handlers     syntax not a primitive
      ; with-handlers*    syntax not a primitive
      error-escape-handler
      error-display-handler
      error-print-width
      error-print-context-length
      error-print-source-location
      error-value->string-handler
      error-syntax->string-handler
      error-syntax->name-handler
      error-module-path->string-handler
      exn
      make-exn
      exn?
      exn-message
      exn-continuation-marks
      exn:fail
      make-exn:fail
      exn:fail?
      exn:fail:contract
      make-exn:fail:contract
      exn:fail:contract?
      exn:fail:contract:arity
      make-exn:fail:contract:arity
      exn:fail:contract:arity?
      exn:fail:contract:divide-by-zero
      make-exn:fail:contract:divide-by-zero
      exn:fail:contract:divide-by-zero?
      exn:fail:contract:non-fixnum-result
      make-exn:fail:contract:non-fixnum-result
      exn:fail:contract:non-fixnum-result?
      exn:fail:contract:continuation
      make-exn:fail:contract:continuation
      exn:fail:contract:continuation?
      exn:fail:contract:variable
      make-exn:fail:contract:variable
      exn:fail:contract:variable?
      exn:fail:contract:variable-id
      exn:fail:syntax
      make-exn:fail:syntax
      exn:fail:syntax?
      exn:fail:syntax-exprs
      exn:fail:syntax:unbound
      make-exn:fail:syntax:unbound
      exn:fail:syntax:unbound?
      exn:fail:syntax:missing-module
      make-exn:fail:syntax:missing-module
      exn:fail:syntax:missing-module?
      exn:fail:syntax:missing-module-path
      exn:fail:read
      make-exn:fail:read
      exn:fail:read?
      exn:fail:read-srclocs
      exn:fail:read:eof
      make-exn:fail:read:eof
      exn:fail:read:eof?
      exn:fail:read:non-char
      make-exn:fail:read:non-char
      exn:fail:read:non-char?
      exn:fail:filesystem
      make-exn:fail:filesystem
      exn:fail:filesystem?
      exn:fail:filesystem:exists
      make-exn:fail:filesystem:exists
      exn:fail:filesystem:exists?
      exn:fail:filesystem:version
      make-exn:fail:filesystem:version
      exn:fail:filesystem:version?
      exn:fail:filesystem:errno
      make-exn:fail:filesystem:errno
      exn:fail:filesystem:errno?
      exn:fail:filesystem:errno-errno
      exn:fail:filesystem:missing-module
      make-exn:fail:filesystem:missing-module
      exn:fail:filesystem:missing-module?
      exn:fail:filesystem:missing-module-path
      exn:fail:network
      make-exn:fail:network
      exn:fail:network?
      exn:fail:network:errno
      make-exn:fail:network:errno
      exn:fail:network:errno?
      exn:fail:network:errno-errno
      exn:fail:out-of-memory
      make-exn:fail:out-of-memory
      exn:fail:out-of-memory?
      exn:fail:unsupported
      make-exn:fail:unsupported
      exn:fail:unsupported?
      exn:fail:user
      make-exn:fail:user
      exn:fail:user?
      exn:break
      make-exn:break
      exn:break?
      exn:break-continuation
      exn:break:hang-up
      make-exn:break:hang-up
      exn:break:hang-up?
      exn:break:terminate
      make-exn:break:terminate
      exn:break:terminate?
      prop:exn:srclocs
      exn:srclocs?
      exn:srclocs-accessor
      srcloc
      make-srcloc
      srcloc?
      srcloc-source
      srcloc-line
      srcloc-column
      srcloc-position
      srcloc-span
      srcloc->string
      prop:exn:missing-module
      exn:missing-module?
      exn:missing-module-accessor
      exn->string
      error-message->adjusted-string
      error-contract->adjusted-string
      current-error-message-adjuster
      error-message-adjuster-key)))


#;(define (symbol->title s)
    ; todo : implement string-titlecase
    (string-titlecase (symbol->string s)))

;; symbol->title: symbol? -> string?
;;   Convert a symbol into a title string for headings.
;;   Returns the title string for the heading.
(define (symbol->title s)
  (symbol->string s))

(define implemented-constants
  '(  correlated?
  correlated-source
  correlated-line
  correlated-column
  correlated-position
  correlated-span
  correlated-e
  correlated->datum
  datum->correlated
  correlated-property
  correlated-property-symbol-keys

  make-instance
  instance?
  instance-name
  instance-data
  instance-variable-names
  instance-set-variable-value!
  instance-unset-variable!
  instance-variable-value


  null undefined unsafe-undefined empty true false pi eof
  prop:arity-string
  prop:checked-procedure  
  prop:impersonator-of
  prop:incomplete-arity
  prop:method-arity-error
  prop:object-name
  prop:procedure
  prop:authentic
  prop:custom-write
  prop:equal+hash

  struct:exn
  struct:exn:fail
  struct:exn:fail:contract
  struct:exn:fail:contract:arity
  struct:exn:fail:contract:divide-by-zero
  struct:exn:fail:contract:non-fixnum-result
  struct:exn:fail:contract:variable
  struct:exn:fail:read
  struct:exn:fail:read:eof
  struct:exn:fail:read:non-char
  struct:exn:fail:syntax
  struct:exn:fail:syntax:missing-module
  struct:exn:fail:syntax:unbound
         ))

(define standard-library-identifiers
  '(current-error-port
    current-input-port
    current-output-port
    current-write-relative-directory
    default-error-value->string-handler
    display
    displayln
    eprintf
    error
    error-print-width
    error-value->string-handler
    format
    fprintf
    fprintf*
    print
    print-as-expression
    print-boolean-long-form
    print-box
    print-graph
    print-hash-table
    print-mpair-curly-braces
    print-pair-curly-braces
    print-reader-abbreviations
    print-struct
    print-syntax-width
    print-unreadable
    print-value-columns
    print-vector-length
    printf
    println
    raise-argument-error
    read
    read-syntax
    reset-current-error-port!
    reset-current-input-port!
    reset-current-output-port!
    write
    writeln)
  )

(define pict-functions
  '((|Pict Datatype|
      explain
      explain-child)
    (|Basic Pict Constructors|
      dc
      unsafe-dc
      blank
      text
      hline
      vline
      frame
      ellipse
      circle
      filled-ellipse
      disk
      rectangle
      filled-rectangle
      rounded-rectangle
      filled-rounded-rectangle
      bitmap
      arrow
      arrowhead
      pip-line
      pip-arrow-line
      pip-arrows-line
      pin-line
      pin-arrow-line
      pin-arrows-line
      vl-append
      vc-append
      vr-append
      ht-append
      htl-append
      hc-append
      hbl-append
      hb-append
      lt-superimpose
      ltl-superimpose
      lc-superimpose
      lbl-superimpose
      lb-superimpose
      ct-superimpose
      ctl-superimpose
      cc-superimpose
      cbl-superimpose
      cb-superimpose
      rt-superimpose
      rtl-superimpose
      rc-superimpose
      rbl-superimpose
      rb-superimpose
      pin-over
      pin-under
      table
      scale
      flip-x
      flip-y
      scale-to-fit
      rotate
      shear
      translate
      ghost)
    (|Pict Drawing Adjusters|
      linewidth
      linestyle
      colorize
      cellophane
      clip
      inset/clip
      freeze)
    (|Bounding Box Adjusters|
      inset
      clip-descent
      clip-ascent
      lift-bottom-relative-to-baseline
      drop-top-relative-to-ascent
      lift-above-baseline
      drop-below-ascent
      baseless
      refocus
      panorama
      use-last
      use-last*)
    (|Pict Finders|
      lt-find
      ltl-find
      lc-find
      lbl-find
      lb-find
      ct-find
      ctl-find
      cc-find
      cbl-find
      cb-find
      rt-find
      rtl-find
      rc-find
      rbl-find
      rb-find
      pict-path?
      launder)
    (|More Pict Constructors|
      hyperlinkize
      scale-color
      color-series
      draw-pict
      pict->bitmap
      pict->argb-pixels
      argb-pixels->pict
      make-pict-drawer
      show-pict
      pict-convertible?
      pict-convert
      cloud
      file-icon
      standard-fish
      jack-o-lantern
      angel-wing
      desktop-machine
      thermometer
      standard-cat
      wrap-balloon
      pip-wrap-balloon
      pin-balloon
      balloon
      balloon?
      make-balloon
      balloon-pict
      balloon-point-x
      balloon-point-y
      face
      face*
      filled-flash
      outline-flash
      typeset-code
      code-align
      make-code-transformer
      code-transformer?
      code-pict-bottom-line-pict
      pict->code-pict
      codeblock-pict)
    (|Animation Helpers|
      fade-pict
      fade-around-pict
      slide-pict
      slide-pict/center
      sequence-animations
      reverse-animations
      fast-start
      fast-end
      fast-edges
      fast-middle
      split-phase)
    (|Color Helpers|
      red
      orange
      yellow
      green
      blue
      purple
      black
      brown
      gray
      white
      cyan
      magenta
      light
      dark)
    (Shadows
      blur
      shadow
      shadow-frame)
    (|Conditional Combinations|
      show
      hide)
    (|Tree Layout|
      tree-layout
      tree-edge
      tree-layout?
      binary-tree-layout?
      tree-edge?
      naive-layered
      binary-tidier
      hv-alternating)))

(define defined-in-pict.rkt
  '(scale-to-fit 
    explain
    explain-child
    blank
    hline
    vline
    frame
    vl-append
    vc-append
    vr-append
    ht-append
    hc-append
    hb-append
    htl-append
    hbl-append
    lt-superimpose
    lb-superimpose
    lc-superimpose
    ltl-superimpose
    lbl-superimpose
    rt-superimpose
    rb-superimpose
    rc-superimpose
    rtl-superimpose
    rbl-superimpose
    ct-superimpose
    cb-superimpose
    cc-superimpose
    ctl-superimpose
    cbl-superimpose
    table
    pin-over
    pin-under
    dc
    circle
    ellipse
    filled-ellipse
    disk
    rectangle
    filled-rectangle
    rounded-rectangle
    filled-rounded-rectangle
    bitmap
    scale
    flip-x
    flip-y
    translate
    shear
    rotate
    ghost
    text
    colorize
    cellophane
    linewidth
    linestyle
    inset/clip
    clip
    drop-below-ascent
    lift-above-baseline
    baseless
    clip-descent
    clip-ascent
    inset
    refocus
    panorama
    use-last*
    use-last
    pict-path?
    launder
    draw-pict
    make-pict-drawer
    pict-convertible?
    pict-convert
    hyperlinkize
    standard-fish
    fade-pict
    fade-around-pict
    slide-pict
    slide-pict/center
    sequence-animations
    reverse-animations
    fast-start
    fast-end
    fast-edges
    fast-middle
    split-phase
    cb-find
    cbl-find
    cc-find
    ct-find
    ctl-find
    lb-find
    lbl-find
    lc-find
    lt-find
    ltl-find
    rb-find
    rbl-find
    rc-find
    rt-find
    rtl-find
    ))


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
            andmap
            append
            append*
            append-map
            apply
            argmax
            argmin
            arithmetic-shift
            arity-at-least-value
            arity-at-least?
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
            boxed?
            build-list
            build-string
            build-vector
            byte-ready?
            byte?
            bytes
            bytes->immutable-bytes
            bytes->list
            bytes->path
            bytes->string/latin-1
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
            bytes-utf-8-length
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
            call-with-exception-handler
            call-with-output-string
            call-with-values
            car
            cartesian-product
            catch
            catch*
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
            char-grapheme-step
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
            check-string
            cons
            cons?
            correlated->datum
            correlated-column
            correlated-e
            correlated-line
            correlated-position
            correlated-property
            correlated-property-symbol-keys
            correlated-source
            correlated-span
            correlated?
            cos
            cosh
            count
            current-continuation-marks
            current-inspector
            custom-write-accessor
            custom-write?
            datum->correlated
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
            exn
            exn-continuation-marks
            exn-message
            exn:fail
            exn:fail:contract
            exn:fail:contract:arity
            exn:fail:contract:arity?
            exn:fail:contract:divide-by-zero
            exn:fail:contract:divide-by-zero?
            exn:fail:contract:variable
            exn:fail:contract:variable-id
            exn:fail:contract:variable?
            exn:fail:contract?
            exn:fail:read
            exn:fail:read-srclocs
            exn:fail:read:eof
            exn:fail:read:eof?
            exn:fail:read:non-char
            exn:fail:read:non-char?
            exn:fail:read?
            exn:fail:syntax
            exn:fail:syntax-exprs
            exn:fail:syntax:missing-module
            exn:fail:syntax:missing-module-path
            exn:fail:syntax:missing-module?
            exn:fail:syntax:unbound
            exn:fail:syntax:unbound?
            exn:fail:syntax?
            exn:fail?
            exn?
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
            fixnum-for-every-system?
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
            gensym
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
            identifier?
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
            input-port?
            instance-data
            instance-name
            instance-set-variable-value!
            instance-unset-variable!
            instance-variable-box
            instance-variable-names
            instance-variable-value
            instance?
            instantiate-linklet
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
            linklet-export-variables
            linklet-import-variables
            linklet-name
            linklet?
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
            make-compiled-linklet
            make-empty-hash
            make-empty-hashalw
            make-empty-hasheq
            make-empty-hasheqv
            make-empty-namespace
            make-exn
            make-exn:fail
            make-exn:fail:contract
            make-exn:fail:contract:arity
            make-exn:fail:contract:divide-by-zero
            make-exn:fail:contract:variable
            make-exn:fail:read
            make-exn:fail:read:eof
            make-exn:fail:read:non-char
            make-exn:fail:syntax
            make-exn:fail:syntax:missing-module
            make-exn:fail:syntax:unbound
            make-hash
            make-hashalw
            make-hasheq
            make-hasheqv
            make-input-port
            make-instance
            make-list
            make-srcloc
            make-string
            make-struct-field-accessor
            make-struct-field-mutator
            make-struct-type
            make-struct-type-property
            make-vector
            make-void
            make-weak-hash
            make-weak-hashalw
            make-weak-hasheq
            make-weak-hasheqv
            map
            match:error
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
            object-name
            odd?
            open-input-bytes
            open-input-string
            open-output-bytes
            open-output-string
            order-of-magnitude
            ormap
            output-port?
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
            peek-bytes-avail!
            peek-bytes-avail!*
            peek-char
            peek-string
            peek-string!
            permutations
            port-count-lines!
            port-counts-lines?
            port-next-location
            port?
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
            progress-evt?
            quotient
            quotient/remainder
            radians->degrees
            raise
            raise-read-eof-error
            raise-read-error
            raise-unbound-variable-reference
            random
            range
            range-proc
            read-byte
            read-bytes
            read-bytes!
            read-bytes-avail!
            read-bytes-avail!*
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
            struct->list
            struct->vector
            struct-accessor-procedure?
            struct-constructor-procedure?
            struct-mutator-procedure?
            struct-predicate-procedure?
            struct-type-authentic?
            struct-type-property-accessor-procedure?
            struct-type-property-predicate-procedure?
            struct-type-property?
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
            syntax->datum
            syntax->list
            syntax-column
            syntax-e
            syntax-line
            syntax-position
            syntax-source
            syntax-span
            syntax-srcloc
            syntax-srclocs
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
            unquoted-printing-string
            unquoted-printing-string-value
            unquoted-printing-string?
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
            unsafe-fx*
            unsafe-fx*/wraparound
            unsafe-fx+
            unsafe-fx+/wraparound
            unsafe-fx-
            unsafe-fx-/wraparound
            unsafe-fx<
            unsafe-fx<=
            unsafe-fx=
            unsafe-fx>
            unsafe-fx>=
            unsafe-fxabs
            unsafe-fxand
            unsafe-fxior
            unsafe-fxlshift
            unsafe-fxlshift/wraparound
            unsafe-fxmax
            unsafe-fxmin
            unsafe-fxmodulo
            unsafe-fxnot
            unsafe-fxpopcount
            unsafe-fxpopcount16
            unsafe-fxpopcount32
            unsafe-fxquotient
            unsafe-fxremainder
            unsafe-fxrshift
            unsafe-fxrshift/logical
            unsafe-fxxor
            unsafe-string-length
            unsafe-bytes-length
            unsafe-struct-ref
            unsafe-struct-set!
            unsafe-vector*-length
            unsafe-vector*-set!
            unsafe-vector-length
            unsafe-vector-ref
            unsafe-vector-set!
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
  ; #t = count as primitive, #f = do not
  (list (list "Datatype Functions and Constants"        datatypes-primitives implemented-primitives #t)
        (list "Input and Output"                               io-primitives implemented-primitives #t)
        (list "Operating System"                 operating-system-primitives implemented-primitives #t)
        (list "Macros"                                     macros-primitives implemented-primitives #t)
        (list "Control Flow"                         control-flow-primitives implemented-primitives #t)
        (list "Reflection and Security"   reflection-and-security-primitives implemented-primitives #t)
        (list "Pict"                                         pict-functions  defined-in-pict.rkt    #f)))

(define total-primitives-cnt
  (for/sum ([chapter (in-list chapter-datasets)])
    (match chapter
      [(list _ sections _ #t)
       (for/sum ([entry (in-list sections)])
         (length (cdr entry)))]
      [_ 0])))

(define total-standard-library-identifiers
  (for/sum ([chapter (in-list chapter-datasets)])
    (match chapter
      [(list _ sections _ #t)
       (for/sum ([entry (in-list sections)])
         (for/sum ([id (in-list entry)]
                   #:when (member id standard-library-identifiers))
           1))]
      [_ 0])))

(define missing-primitives
  (- total-primitives-cnt
     (length implemented-primitives)
     total-standard-library-identifiers))

(define prepared-chapters
  (for/list ([chapter (in-list chapter-datasets)])
    (match chapter
      [(list title raw-sections implemented _)
       (list title
             (for/list ([entry (in-list raw-sections)])
               (list (symbol->title (car entry)) (cdr entry)))
             implemented)])))


;; format-percent: real? -> exact-integer?
;;   Convert a 0-1 progress ratio into a rounded percentage.
;;   Returns the rounded percentage as an exact integer.
(define (format-percent pct)
  (inexact->exact (round (* 100 pct))))

;; primitive-url: symbol? -> string?
;;   Build the Racket documentation URL for a primitive.
;;   Returns the documentation URL as a string.
(define (primitive-url sym)
  (string-append "https://docs.racket-lang.org/search/index.html?q="
                 (symbol->string sym)))

;; primitive-status: symbol? (listof symbol?) -> (values string? string?)
;;   Classify a primitive by implementation status.
;;   Returns the status CSS class and the status label.
(define (primitive-status sym implemented-set)
  (cond
    [(memq sym standard-library-identifiers) (values "status-chip status-chip--stdlib" "Stdlib")]
    [(memq sym implemented-set)              (values "status-chip status-chip--done"   "Implemented")]
    [else                                    (values "status-chip status-chip--todo"   "Missing")]))

;; primitive-item: symbol? (listof symbol?) -> list?
;;   Render a primitive list item with status badge and link.
;;   Returns an S-expression representing the list item markup.
(define (primitive-item sym implemented-set)
  (define-values (status-class status-label) (primitive-status sym implemented-set))
  (define name (symbol->string sym))
  (define row-class
    (cond
      [(memq sym standard-library-identifiers) "prim-row--stdlib"]
      [(memq sym implemented-set)              "prim-row--implemented"]
      [else                                    "prim-row--missing"]))
  `(li
    (a (@ (class ,(string-append "prim-row prim-row--link " row-class))
          (href ,(primitive-url sym))
          (target "_blank")
          (rel "noreferrer noopener"))
       (span (@ (class ,(string-append status-class " prim-badge"))) ,status-label)
       (span (@ (class "prim-name") (title ,name)) ,name))))

;; section-id: string? -> string?
;;   Create a stable HTML id from a section title.
;;   Returns the derived HTML id string.
(define (section-id title)
  ;; ascii-alnum?: char? -> boolean?
  ;;   Build a slug without regex: ASCII alphanumerics separated by single hyphens.
  ;;   Recognize ASCII lowercase letters and digits.
  ;;   Returns #t when the character is ASCII alphanumeric.
  (define (ascii-alnum? ch)
    (define code (char->integer ch))
    (or (and (<= (char->integer #\a) code)
             (<= code (char->integer #\z)))
        (and (<= (char->integer #\0) code)
             (<= code (char->integer #\9)))))
  (define lower (string-downcase title))
  (define-values (rev-chars _last-hyphen? _saw-char?)
    (for/fold ([chars '()] [last-hyphen? #f] [saw-char? #f])
              ([ch (in-string lower)])
      (cond
        [(ascii-alnum? ch)
         (values (cons ch chars) #f #t)]
        [(or last-hyphen? (not saw-char?))
         (values chars #t saw-char?)]
        [else
         (values (cons #\- chars) #t saw-char?)])))
  (define trimmed-rev
    (if (and (pair? rev-chars) (char=? (car rev-chars) #\-))
        (cdr rev-chars)
        rev-chars))
  (define cleaned (list->string (reverse trimmed-rev)))
  (string-append "section-" (if (string=? cleaned "") "untitled" cleaned)))

;; section-implemented-count: (listof symbol?) (listof symbol?) -> exact-nonnegative-integer?
;;   Count implemented primitives in a section.
;;   Returns the number of implemented primitives in the section.
(define (section-implemented-count primitives implemented-set)
  (for/sum ([p (in-list primitives)]
            #:when (memq p implemented-set))
    1))

;; section-progress-tier: real? -> string?
;;   Map progress percentage to a UI tier label.
;;   Returns the tier label string.
(define (section-progress-tier pct-num)
  (cond
    [(< pct-num 20) "low"]
    [(> pct-num 80) "strong"]
    [else           "mid"]))

;; section-stats: string? (listof symbol?) (listof symbol?) -> list?
;;   Compute summary stats for a section.
;;   Returns a list of stats (title, primitives, counts, and ids).
(define (section-stats title primitives implemented-set)
  (define implemented-count (section-implemented-count primitives implemented-set))
  (define total             (length primitives))
  (define missing           (- total implemented-count))
  (define pct               (if (zero? total) 0 (/ implemented-count total)))
  (define pct-num           (format-percent pct))
  (define tier              (section-progress-tier pct-num))
  (list title primitives implemented-count total missing pct-num tier (section-id title)))

;; section-card: list? (listof symbol?) -> list?
;;   Render a status card for a section of primitives.
;;   Returns an S-expression representing the card markup.
(define (section-card section implemented-set)
  (match section
    [(list title primitives)
     (define implemented
       (for/list ([p (in-list primitives)]
                  #:when (memq p implemented-set))
         p))

     (define stats             (section-stats title primitives implemented-set))
     (define implemented-count (list-ref stats 2))
     (define total             (list-ref stats 3))
     (define pct-num           (list-ref stats 5))
     (define tier              (list-ref stats 6))
     (define anchor-id         (list-ref stats 7))
     (define aria-label        (format "~a: ~a%, ~a of ~a primitives implemented"
                                       title pct-num implemented-count total))
     `(details (@ (class ,(format "status-section status-section--~a" tier))
                  (id ,anchor-id))
       (summary (@ (class "status-summary")
                   (aria-label ,aria-label))
                (div (@ (class "status-summary-main"))
                     (h3 (@ (class "status-title")
                            (title ,title))
                         ,title)
                     (p (@ (class "status-count"))
                        ,(format "~a of ~a primitives" implemented-count total))
                     (div (@ (class "status-summary-action"))
                          (span (@ (class "status-summary-text")) "View")
                          (span (@ (class "status-summary-chevron")) "▸")))
                (div (@ (class "status-summary-metric"))
                     (span (@ (class "status-percent")) ,(format "~a%" pct-num))
                     (div (@ (class         "status-bar")
                             (role          "progressbar")
                             (aria-label    ,aria-label)
                             (aria-valuemin "0")
                             (aria-valuemax "100")
                             (aria-valuenow ,(number->string pct-num))
                             (style         ,(format "--pct: ~a;" pct-num)))
                          (div (@ (class ,(format "status-bar-fill status-bar-fill--~a"
                                                  tier))))))
                )
       (div (@ (class "status-body"))
            (div (@ (class "status-body-header"))
                 (div (@ (class "status-body-legend"))
                      (button (@ (class "status-chip status-chip--done status-chip--filter status-chip--active")
                                 (type              "button")
                                 (aria-pressed      "true")
                                 (data-status-group "implemented"))
                              "Implemented")
                      (button (@ (class "status-chip status-chip--stdlib status-chip--filter")
                                 (type              "button")
                                 (aria-pressed      "false")
                                 (data-status-group "stdlib"))
                              "Stdlib")
                      (button (@ (class "status-chip status-chip--todo status-chip--filter")
                                 (type              "button")
                                 (aria-pressed      "false")
                                 (data-status-group "missing"))
                              "Missing"))
                 (button (@ (class "status-body-hint")
                            (type "button"))
                         "Close to collapse"))
            (ul (@ (class "status-list"))
                ,@(map (λ (sym) (primitive-item sym implemented-set))
                       (sort primitives symbol<?)))))]))

;; chapter-block: list? -> list?
;;   Render a chapter block containing section cards.
;;   Returns an S-expression representing the chapter markup.
(define (chapter-block chapter)
  (match chapter
    [(list title sections implemented-set)
     `(div (@ (class "status-chapter"))
           (div (@ (class "status-chapter-header"))
                (h3 ,title))
           (div (@ (class "status-chapter-grid"))
                 ,@(map (λ (section) (section-card section implemented-set))
                        sections)))]))

(define all-section-stats
  (for*/list ([chapter (in-list prepared-chapters)]
              [section (in-list (second chapter))])
    (match chapter
      [(list _ _ implemented-set)
       (match section
         [(list title primitives)
          (section-stats title primitives implemented-set)])])))

(define attention-metadata
  (list (list "Exceptions" "Exceptions"
              "Signals core error paths and user-facing diagnostics.")
        (list "Procedures" "Procedures"
              "Enables higher-order functions and arity-sensitive APIs.")
        (list "Ports" "Ports"
              "Unlocks I/O, REPL workflows, and stream-based tooling.")
        (list "Hashes" "Hashes"
              "Powerful dictionaries used throughout stdlib.")
        (list "Sequences" "Sequences"
              "Backbone for iteration forms and comprehensions.")
        (list "Paths" "Paths"
              "OS path handling for files, dirs, and tooling.")
        (list "Macros" "Syntax Object Content"
              "Macro expansion and syntax tooling depend on this set.")))

;; attention-candidates: -> list?
;;   Build attention metadata entries that match known sections.
;;   Returns the matching attention metadata entries.
(define (attention-candidates)
  (for/list ([entry (in-list attention-metadata)]
             #:when (match entry
                     [(list _ section-title _)
                      (memf (λ (stat)
                              (string-ci=? (car stat) section-title))
                            all-section-stats)]))
    (match entry
      [(list display section-title note)
       (define stat
         (car (memf (λ (item) (string-ci=? (car item) section-title))
                    all-section-stats)))
       (define title             (car stat))
       (define implemented-count (list-ref stat 2))
       (define total             (list-ref stat 3))
       (define missing           (list-ref stat 4))
       (define pct-num           (list-ref stat 5))
       (define anchor-id         (list-ref stat 7))
       (list display title note implemented-count total missing pct-num anchor-id)])))

;; needs-attention-items: -> list?
;;   Select top attention items with low completion.
;;   Returns a list of attention items to highlight.
(define (needs-attention-items)
  (define filtered
    (filter (λ (item)
              (define pct-num (list-ref item 6))
              (define total (list-ref item 4))
              (and (< pct-num 60) (> total 4)))
            (attention-candidates)))
  (define sorted
    (sort filtered
          (λ (a b)
            (define pct-a (list-ref a 6))
            (define pct-b (list-ref b 6))
            (define missing-a (list-ref a 5))
            (define missing-b (list-ref b 5))
            (cond
              [(< pct-a pct-b) #t]
              [(> pct-a pct-b) #f]
              [else (> missing-a missing-b)]))))
  (take sorted 4))

;; attention-card: list? -> list?
;;   Render an attention card summary for a section.
;;   Returns an S-expression representing the attention card markup.
(define (attention-card item)
  (define display           (list-ref item 0))
  (define title             (list-ref item 1))
  (define note              (list-ref item 2))
  (define implemented-count (list-ref item 3))
  (define total             (list-ref item 4))
  (define pct-num           (list-ref item 6))
  (define anchor-id         (list-ref item 7))
  (define label
    (format "~a: ~a%, ~a of ~a primitives implemented"
            title pct-num implemented-count total))
  (list `(div (@ (class "attention-header"))
              (h3 ,display)
              (span (@ (class "attention-percent")) ,(format "~a%" pct-num)))
        `(p (@ (class "attention-count"))
            ,(format "~a of ~a primitives implemented" implemented-count total))
        `(p (@ (class "attention-note")) ,note)
        `(a (@ (class "attention-link")
               (href ,(string-append "#" anchor-id))
               (data-attention-target ,anchor-id)
               (aria-label ,(string-append "View missing for " label)))
            "View missing")))

;; status-legend: -> list?
;;   Render the legend explaining status chips.
;;   Returns an S-expression representing the legend markup.
(define (status-legend)
  `(div (@ (class "status-legend"))
        (div (@ (class "status-legend-title")) "Legend")
        (ul (@ (class "status-legend-list"))
            (li (span (@ (class "status-legend-term")) "Implemented primitives")
                " implemented in the WebRacket runtime.")
            (li (span (@ (class "status-legend-term")) "Stdlib coverage")
                " identifiers provided by the stdlib (counted as covered).")
            (li (span (@ (class "status-legend-term")) "Missing primitives")
                " not yet implemented in the runtime.")
            (li "Pict is tracked separately."))))

;; status-insight-callout: -> list?
;;   Render a callout highlighting strongest and weakest sections.
;;   Returns an S-expression representing the callout markup.
(define (status-insight-callout)
  (define strong-candidates
    (list "Pict Datatype" "Booleans" "Keywords" "Mpairs"))
  (define weak-candidates
    (list "Procedures" "Ports" "Sequences" "Paths" "Exceptions"
          "Syntax Object Content"))
  (define sorted-strong
    (sort all-section-stats (λ (a b) (> (list-ref a 5) (list-ref b 5)))))
  (define sorted-weak
    (sort all-section-stats (λ (a b) (< (list-ref a 5) (list-ref b 5)))))
  (define strong-match
    (memf (λ (stat) (member (car stat) strong-candidates))
          sorted-strong))
  (define strong
    (if strong-match
        (car strong-match)
        (car sorted-strong)))
  (define weak-match
    (memf (λ (stat) (member (car stat) weak-candidates))
          sorted-weak))
  (define weak
    (if weak-match
        (car weak-match)
        (car sorted-weak)))
  (define strong-title       (car strong))
  (define strong-implemented (list-ref strong 2))
  (define strong-total       (list-ref strong 3))
  (define strong-pct         (list-ref strong 5))
  (define weak-title         (car weak))
  (define weak-implemented   (list-ref weak 2))
  (define weak-total         (list-ref weak 3))
  (define weak-pct           (list-ref weak 5))

  (callout
   'info
   "What this tells us"
   `(p ,(format "Coverage is strongest in ~a at ~a% (~a of ~a)."
                strong-title strong-pct strong-implemented strong-total))
   `(p ,(format "The thinnest areas remain ~a at ~a% (~a of ~a), so shoring up"
                weak-title weak-pct weak-implemented weak-total)
       " these gaps will unlock more programs.")))

;; node-list->list: any/c -> list?
;;   Convert a DOM NodeList into a Racket list.
;;   Returns a list of nodes in the same order.
(define (node-list->list node-list)
  (define len   (js-ref node-list "length"))
  (define count (if (number? len) (inexact->exact len) 0))
  (let loop ([idx 0]
             [acc '()])
    (if (>= idx count)
        (reverse acc)
        (loop (add1 idx)
              (cons (js-send node-list "item" (vector idx)) acc)))))

(define status-handler-store '())

;; remember-status-handler!: procedure? -> void?
;;   Store event handlers so they stay reachable.
;;   Returns (void) after storing the handler.
(define (remember-status-handler! handler)
  (set! status-handler-store (cons handler status-handler-store)))

;; classlist-contains?: any/c string? -> boolean?
;;   Check whether an element's classList contains a class.
;;   Returns #t when the class is present.
(define (classlist-contains? element class-name)
  (define selector (string-append "." class-name))
  (define result   (and element (js-matches element selector)))
  (and (number? result) (not (zero? result))))

;; element-text: any/c -> string?
;;   Extract trimmed textContent from an element.
;;   Returns the trimmed text or "" when unavailable.
(define (element-text element)
  (define content (and element (js-ref element "textContent")))
  (if (string? content)
      (string-trim content)
      ""))

;; count-grid-columns: string? -> exact-positive-integer?
;;   Count grid template columns from a computed style string.
;;   Returns the number of columns (minimum 1).
(define (count-grid-columns template)
  (define len (string-length template))
  (define count
    (let loop ([idx 0]
               [depth 0]
               [in-token? #f]
               [acc 0])
      (if (>= idx len)
          (if in-token? (add1 acc) acc)
          (let ([ch (string-ref template idx)])
            (cond
              [(char=? ch #\()
               (loop (add1 idx) (add1 depth) in-token? acc)]
              [(char=? ch #\))
               (loop (add1 idx) (max 0 (sub1 depth)) in-token? acc)]
              [(and (char-whitespace? ch) (= depth 0))
               (if in-token?
                   (loop (add1 idx) depth #f (add1 acc))
                   (loop (add1 idx) depth #f acc))]
              [else
               (loop (add1 idx) depth #t acc)])))))
  (if (> count 0) count 1))

;; grid-template-columns: any/c -> string?
;;   Read the computed grid-template-columns string for an element.
;;   Returns the computed grid template columns (or "" if unavailable).
(define (grid-template-columns element)
  (define style   (and element (js-window-get-computed-style element)))
  (define columns (and style (js-ref style "gridTemplateColumns")))
  (define legacy  (and style (js-ref style "grid-template-columns")))
  (cond
    [(string? columns) columns]
    [(string? legacy)  legacy]
    [else              ""]))

;; status-grid-column-count: any/c -> exact-positive-integer?
;;   Resolve the current grid column count for a status grid.
;;   Returns the column count (minimum 1).
(define (status-grid-column-count grid)
  (count-grid-columns (grid-template-columns grid)))

;; status-grid-title: any/c -> string?
;;   Extract the section title for a status card.
;;   Returns the card title string.
(define (status-grid-title card)
  (define title-node (and card (js-element-query-selector card ".status-title")))
  (element-text title-node))

;; status-grid-sort: list? -> list?
;;   Sort cards by title, case-insensitive, with stable tiebreakers.
;;   Returns the sorted list of cards.
(define (status-grid-sort cards)
  (define decorated
    (for/list ([card (in-list cards)]
               [idx (in-naturals)])
      (list card idx (status-grid-title card))))
  (define (card<? a b)
    (define title-a (list-ref a 2))
    (define title-b (list-ref b 2))
    (cond
      [(string-ci<? title-a title-b) #t]
      [(string-ci>? title-a title-b) #f]
      [else (< (list-ref a 1) (list-ref b 1))]))
  (map car (sort decorated card<?)))

;; status-grid-column-major: list? exact-positive-integer? -> list?
;;   Reorder cards into column-major visual order.
;;   Returns the reordered list of cards.
(define (status-grid-column-major cards columns)
  (define count (length cards))
  (if (zero? count)
      '()
      (let* ([cols      (max 1 columns)]
             [row-count (inexact->exact (ceiling (/ count cols)))]
             [reordered (make-vector count #f)])
        (for ([card (in-list cards)]
              [idx (in-naturals)])
          (define target (+ (* (remainder idx row-count) cols)
                            (quotient idx row-count)))
          (when (< target count)
            (vector-set! reordered target card)))
        (filter (λ (item) item) (vector->list reordered)))))

;; status-grid-current-columns: any/c -> (or/c exact-positive-integer? #f)
;;   Read stored column count metadata for a status grid.
;;   Returns the stored column count or #f.
(define (status-grid-current-columns grid)
  (define raw (js-get-attribute grid "data-status-columns"))
  (define parsed (and (string? raw) (string->number raw)))
  (and (integer? parsed) (positive? parsed) parsed))

;; status-reorder-grid!: any/c -> void?
;;   Reorder a status grid's cards into column-major alphabetical order.
;;   Returns (void) after reordering.
(define (status-reorder-grid! grid)
  (define columns (status-grid-column-count grid))
  (define previous (status-grid-current-columns grid))
  (when (or (not previous) (not (= previous columns)))
    (define cards (node-list->list (js-ref grid "children")))
    (when (pair? cards)
      (define sorted (status-grid-sort cards))
      (define reordered (status-grid-column-major sorted columns))
      (for ([card (in-list reordered)])
        (js-append-child! grid card)))
    (js-set-attribute! grid "data-status-columns" (number->string columns))))

;; status-reorder-card-grids!: -> void?
;;   Reorder all status chapter grids to column-major alphabetical order.
;;   Returns (void) after reordering.
(define (status-reorder-card-grids!)
  (define grids (node-list->list (js-query-selector-all ".status-chapter-grid")))
  (for ([grid (in-list grids)])
    (status-reorder-grid! grid)))

;; init-status-card-grid-order!: -> void?
;;   Initialize column-major ordering and resize handling for chapter grids.
;;   Returns (void) after wiring handlers.
(define status-grid-resize-timer #f)

(define (init-status-card-grid-order!)
  (define (refresh!)
    (status-reorder-card-grids!))
  (define external-refresh (procedure->external refresh!))
  (define (schedule-refresh . _)
    (when status-grid-resize-timer
      (js-window-clear-timeout status-grid-resize-timer))
    (set! status-grid-resize-timer
          (js-window-set-timeout/delay external-refresh 80.)))
  (status-reorder-card-grids!)
  (define handler (procedure->external schedule-refresh))
  (remember-status-handler! handler)
  (js-add-event-listener! (js-window-window) "resize" handler))

;; status-open-target!: string? -> void?
;;   Expand and scroll to a status section by id.
;;   Returns (void) after handling the scroll.
(define (status-open-target! target-id)
  (define section (js-get-element-by-id target-id))
  (when section
    (js-set! section "open" #t)
    (define summary (js-element-query-selector section "summary"))
    (js-send section "scrollIntoView"
             (vector (js-object (vector (vector "behavior" "smooth")
                                        (vector "block"    "start")))))
    (when summary
      (js-send summary "focus"
               (vector (js-object (vector (vector "preventScroll" #t))))))))

;; init-status-smooth-scroll!: -> void?
;;   Wire smooth scrolling for attention card links.
;;   Returns (void) after wiring link handlers.
(define (init-status-smooth-scroll!)
  (define links (node-list->list (js-query-selector-all "[data-attention-target]")))
  (for ([link (in-list links)])
    (define handler
      (procedure->external
       (lambda (evt)
         (define target-id (js-get-attribute link "data-attention-target"))
         (when (and (string? target-id) (not (string=? target-id "")))
           (js-event-prevent-default evt)
           (status-open-target! target-id)
           (js-send (js-window-history) "replaceState"
                    (vector (js-null) "" (string-append "#" target-id)))))))
    (remember-status-handler! handler)
    (js-add-event-listener! link "click" handler))
  (define hash (js-ref (js-window-location) "hash"))
  (when (and (string? hash) (> (string-length hash) 1))
    (status-open-target! (substring hash 1))))

;; status-row-status: any/c -> string?
;;   Derive the status label from a primitive row element.
;;   Returns the status label string.
(define (status-row-status row)
  (cond
    [(and row (classlist-contains? row "prim-row--implemented")) "implemented"]
    [(and row (classlist-contains? row "prim-row--stdlib"))      "stdlib"]
    [else                                                        "missing"]))

;; status-row-name: any/c any/c -> string?
;;   Resolve the display name for a row for sorting.
;;   Returns the resolved row name string.
(define (status-row-name row item)
  (define name-node (and row (js-element-query-selector row ".prim-name")))
  (define name (if name-node (element-text name-node) ""))
  (if (string=? name "")
      (element-text (or row item))
      name))

;; status-group-order: string? -> (listof string?)
;;   Return status precedence ordering for a group filter.
;;   Returns the ordered list of status labels.
(define (status-group-order group)
  (cond
    [(string=? group "stdlib")  (list "stdlib"  "implemented" "missing")]
    [(string=? group "missing") (list "missing" "implemented" "stdlib")]
    [else                       (list "implemented" "stdlib" "missing")]))

;; status-order-index: string? (listof string?) -> exact-nonnegative-integer?
;;   Find the index of a status in an ordering list.
;;   Returns the index of the status in the ordering list.
(define (status-order-index status order)
  (let loop ([rest order] [idx 0])
    (cond
      [(null? rest)                 idx]
      [(string=? status (car rest)) idx]
      [else                         (loop (cdr rest) (add1 idx))])))

;; status-sort-list!: any/c string? string? -> void?
;;   Sort a status list in place by group and name.
;;   Returns (void) after sorting the list in place.
(define (status-sort-list! list-el active-group active-dir)
  (define items     (node-list->list (js-element-query-selector-all list-el "li")))
  (define priority  (status-group-order active-group))
  (define decorated (for/list ([item (in-list items)]
                               [index (in-naturals)])
                      (define row    (js-element-query-selector item ".prim-row"))
                      (define status (status-row-status row))
                      (define name   (status-row-name row item))
                      (list item index status name)))
  ;; item<?: list? list? -> boolean?
  ;;   Compare two decorated rows for sorting order.
  ;;   Returns #t when the first row sorts before the second.
  (define (item<? a b)
    (define status-a (list-ref a 2))
    (define status-b (list-ref b 2))
    (define diff (- (status-order-index status-a priority)
                    (status-order-index status-b priority)))
    (cond
      [(< diff 0) #t]
      [(> diff 0) #f]
      [else
       (define dir (if (string=? status-a active-group) active-dir "asc"))
       (define name-a (list-ref a 3))
       (define name-b (list-ref b 3))
       (cond
         [(string-ci<? name-a name-b) (string=? dir "asc")]
         [(string-ci>? name-a name-b) (string=? dir "desc")]
         [else (< (list-ref a 1) (list-ref b 1))])]))
  
  (for ([entry (in-list (sort decorated item<?))])
    (js-append-child! list-el (list-ref entry 0))))

;; status-group->string: any/c -> (or/c string? #f)
;;   Normalize DOM attribute values to a string (or #f).
;;   Returns a string value for JS external strings.
(define (status-group->string group)
  (cond
    [(external? group) (external-string->string group)]
    [else group]))

;; status-sync-buttons!: list? string? -> void?
;;   Sync filter button states to active group.
;;   Returns (void) after updating button state.
(define (status-sync-buttons! buttons active-group)
  (for ([button (in-list buttons)])
    (define group      (status-group->string
                        (js-get-attribute button "data-status-group")))
    (define is-active  (and (string? group) (string=? group active-group)))
    (js-set-attribute! button "aria-pressed" (if is-active "true" "false"))
    (define class-list (js-ref button "classList"))
    (when class-list
      (js-send class-list "toggle"
               (vector "status-chip--active" (and is-active #t))))))

;; init-status-filter!: -> void?
;;   Initialize filter buttons and sorting per section.
;;   Returns (void) after wiring filter interactions.
(define (init-status-filter!)
  (define sections (node-list->list (js-query-selector-all ".status-section")))

  (for ([section (in-list sections)])
    (define list-el     (js-element-query-selector section ".status-list"))
    (define button-list (node-list->list
                         (js-element-query-selector-all section "[data-status-group]")))

    (when (and list-el (pair? button-list))
      (define (active-group)
        (define current (status-group->string
                         (js-get-attribute section "data-status-active-group")))
        (if (string? current) current "implemented"))
        
      (define (active-dir)
        (define current (status-group->string
                         (js-get-attribute section "data-status-active-dir")))
        (if (string? current) current "asc"))

      (define (set-active-state! group dir)
        (js-set-attribute! section "data-status-active-group" group)
        (js-set-attribute! section "data-status-active-dir"   dir))
      
      ;; sync-and-sort!: -> void?
      ;;   Refresh button state and re-sort the list.
      ;;   Returns (void) after syncing state.
      (define (sync-and-sort! group dir)
        (status-sync-buttons! button-list group)
        (status-sort-list! list-el group dir))
      
      (for ([button (in-list button-list)])
        (define handler
          (procedure->external
           (lambda (_evt)
             (define group (status-group->string
                            (js-get-attribute button "data-status-group")))
             (when (and (string? group) (not (string=? group "")))
               (define current-group (active-group))
               (define current-dir   (active-dir))
               (define next-dir      (if (string=? group current-group)
                                         (if (string=? current-dir "asc") "desc" "asc")
                                         "asc"))
               (define next-group    group)
               (set-active-state! next-group next-dir)
               (sync-and-sort!    next-group next-dir)))))

        (remember-status-handler! handler)
        (js-add-event-listener! button "click" handler))

      (set-active-state! (active-group) (active-dir))
      (sync-and-sort!    (active-group) (active-dir)))))

;; init-status-collapse-buttons!: -> void?
;;   Attach handlers to collapse expanded sections.
;;   Returns (void) after wiring collapse handlers.
(define (init-status-collapse-buttons!)
  (define buttons (node-list->list (js-query-selector-all ".status-body-hint")))
  (for ([button (in-list buttons)])
    (define handler
      (procedure->external
       (lambda (_evt)
         (define details (js-send button "closest" (vector "details")))
         (when details
           (js-remove-attribute! details "open")))))
    (remember-status-handler! handler)
    (js-add-event-listener! button "click" handler)))

;; init-status-page-handlers!: -> void?
;;   Initialize all behavior for the status page.
;;   Returns (void) after initializing handlers.
(define (init-status-page-handlers!)
  (init-status-card-grid-order!)
  (init-status-smooth-scroll!)
  (init-status-filter!)
  (init-status-collapse-buttons!))

;; implementation-status-page: -> list?
;;   Render the implementation status dashboard page.
;;   Returns an S-expression representing the page markup.
(define (implementation-status-page)
  `(div (@ (class "page page--status"))
        ,(navbar)
        (section (@ (class "status-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Runtime primitives")
                           (span (@ (class "pill")) "Stdlib coverage")
                           (span (@ (class "pill")) "Documentation links"))
                      (h1 (@ (class "hero-title")) "WebRacket Implementation Status Dashboard")
                      (p (@ (class "hero-lead"))
                         "A progress tracker for contributors and users. "
                         "See which Racket primitives are implemented, what's missing, "
                         "and where to help next.")
                      (p (@ (class "hero-sublead"))
                         "Each function links to the Racket docs; sections expand to show "
                         "implemented, stdlib-backed, and missing primitives.")
                      (p (@ (class "hero-note"))
                         "How to read this: Implemented means built into the WebRacket runtime, "
                         "while stdlib identifiers count as covered.")))
        ,(section-block
          "At a glance"
          "Summary metrics across the tracked chapters."
          (list
           (card-grid
            (list
             (list `(h3 "Implemented primitives")
                   `(p (@ (class "status-metric"))
                       ,(number->string (length implemented-primitives)))
                   `(p "Runtime primitives. Implemented in WebAssembly."))
             (list `(h3 "Missing primitives")
                   `(p (@ (class "status-metric"))
                       ,(number->string missing-primitives))
                   `(p "Still to be implemented in the runtime."))
             (list `(h3 "Stdlib coverage")
                   `(p (@ (class "status-metric"))
                       ,(number->string total-standard-library-identifiers))
                   `(p "Identifiers provided by the standard library. Implemented in WebRacket."))
             (list `(h3 "Stdlib-only identifiers")
                   `(p (@ (class "status-metric"))
                       ,(number->string (- (length standard-library-identifiers)
                                           total-standard-library-identifiers)))
                   `(p "Identifiers that exist only in WebRacket (not full Racket).")))))
            "status-summary-grid")
        ,(callout
          'info
          "About the counts"
          `(p "Pict functions are tracked separately and excluded from primitive totals. "
              "Stdlib identifiers count as covered for completeness."))
        ,(section-block
          "Needs attention"
          "Highest-impact gaps to unlock more programs."
          (list
           (card-grid (map attention-card (needs-attention-items))
                      "attention-grid"))
          #f
          "section--status")
        `(div (@ (class "status-insight"))
              ,(status-insight-callout))
        ,(section-block
          "Completion by chapter"
          "Expand a section to see which primitives are implemented or still missing."
          (list
           (status-legend)
           `(p (@ (class "status-helper"))
               "Click a card to expand missing primitives.")
           `(div (@ (class "status-chapters"))
                 ,@(map chapter-block prepared-chapters)))
          #f
          "section--status")

        ,(section-block
          "Help close the gaps"
          "Contribute runtime support or stdlib coverage to unlock more programs."
          (list
           `(div (@ (class "status-cta"))
                 (div (@ (class "status-cta-actions"))
                      (a (@ (class "cta-button")
                            (href ,(gh-file "runtime-wasm.rkt"))
                            (target "_blank")
                            (rel "noreferrer noopener"))
                         "Open runtime sources")
                      (a (@ (class "cta-link")
                            (href ,(gh-dir "stdlib"))
                            (target "_blank")
                            (rel "noreferrer noopener"))
                         "Browse stdlib")
                      (a (@ (class "cta-link")
                            (href "https://github.com/soegaard/webracket/issues")
                            (target "_blank")
                            (rel "noreferrer noopener"))
                         "See issues"))))
         #f
         "section--status")
        ,(footer-section)))
