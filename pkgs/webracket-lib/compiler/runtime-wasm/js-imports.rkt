#lang racket

(provide js-host-import-forms
         js-host-import-function-names)

;; js-host-import-forms : (listof datum?)
;;   Declare static JavaScript host imports, grouped by runtime theme.
(define js-host-import-forms
  '(
         ;; Output bridge imports.
         (func $js_output
               (import "primitives" "js_output")
               (param i32))
         (func $js_print_fasl
               (import "primitives" "js_print_fasl")
               (param i32) (param i32))

         ;; Virtual filesystem imports.
         (func $js-vfs-stat-kind
               (import "primitives" "vfs_stat_kind")
               (param i32) (param i32) (result i32))
         (func $js-vfs-file-size
               (import "primitives" "vfs_file_size")
               (param i32) (param i32) (result i32))
         (func $js-vfs-stat
               (import "primitives" "vfs_stat")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-read-file
               (import "primitives" "vfs_read_file")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-write-file
               (import "primitives" "vfs_write_file")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-delete-file
               (import "primitives" "vfs_delete_file")
               (param i32) (param i32) (result i32))
         (func $js-vfs-delete-directory
               (import "primitives" "vfs_delete_directory")
               (param i32) (param i32) (result i32))
         (func $js-vfs-delete-directory/files
               (import "primitives" "vfs_delete_directory_files")
               (param i32) (param i32) (result i32))
         (func $js-vfs-make-directory
               (import "primitives" "vfs_make_directory")
               (param i32) (param i32) (result i32))
         (func $js-vfs-make-directory*
               (import "primitives" "vfs_make_directory_star")
               (param i32) (param i32) (result i32))
         (func $js-vfs-make-parent-directory*
               (import "primitives" "vfs_make_parent_directory_star")
               (param i32) (param i32) (result i32))
         (func $js-vfs-list-directory
               (import "primitives" "vfs_list_directory")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-root-list
               (import "primitives" "vfs_root_list")
               (param i32) (param i32) (result i32))
         (func $js-vfs-rename
               (import "primitives" "vfs_rename")
               (param i32) (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-copy-file
               (import "primitives" "vfs_copy_file")
               (param i32) (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-copy-directory/files
               (import "primitives" "vfs_copy_directory_files")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-modify-seconds
               (import "primitives" "vfs_modify_seconds")
               (param i32) (param i32) (param i32) (param i32) (result i32))
         (func $js-vfs-permissions
               (import "primitives" "vfs_permissions")
               (param i32) (param i32) (param i32) (param i32) (result i32))
        (func $js-vfs-identity
              (import "primitives" "vfs_identity")
              (param i32) (param i32) (result i32))

         ;; Time and date imports. The exact millisecond import returns
         ;; already-tagged fixnum bits so JavaScript can apply WebRacket's
         ;; current 30-bit fixnum wrapping policy without trapping in Wasm.
         (func $js-current-inexact-milliseconds
               (import "primitives" "current_inexact_milliseconds")
               (result f64))
         (func $js-current-milliseconds-bits
               (import "primitives" "current_milliseconds_bits")
               (result i32))
         (func $js-date-field
               (import "primitives" "date_field")
               (param f64) (param i32) (param i32) (result i32))

         ;; Character Unicode imports.
         (func $char-upcase/ucs
               (import "primitives" "char_upcase")
               (param i32) (result i32))

         (func $char-downcase/ucs
               (import "primitives" "char_downcase")
               (param i32) (result i32))

         (func $char-titlecase/ucs
               (import "primitives" "char_titlecase")
               (param i32) (result i32))

         (func $char-foldcase/ucs
               (import "primitives" "char_foldcase")
               (param i32) (result i32))

        ;; Host provides an approximate grapheme break property mapping.
        ;; It currently covers only a subset of Unicode ranges.
        (func $char-grapheme-break-property/ucs
              (import "primitives" "char_grapheme_break_property")
              (param i32) (result i32))

        (func $char-general-category/ucs
              (import "primitives" "char_general_category")
              (param i32) (result i32))

        (func $char-alphabetic?/ucs
              (import "primitives" "char_alphabetic")
              (param i32) (result i32))

        (func $char-lower-case?/ucs
              (import "primitives" "char_lower_case")
              (param i32) (result i32))

        (func $char-upper-case?/ucs
              (import "primitives" "char_upper_case")
              (param i32) (result i32))

        (func $char-title-case?/ucs
              (import "primitives" "char_title_case")
              (param i32) (result i32))

        (func $char-numeric?/ucs
              (import "primitives" "char_numeric")
              (param i32) (result i32))

        (func $char-symbolic?/ucs
              (import "primitives" "char_symbolic")
              (param i32) (result i32))

        (func $char-punctuation?/ucs
              (import "primitives" "char_punctuation")
              (param i32) (result i32))

        (func $char-graphic?/ucs
              (import "primitives" "char_graphic")
              (param i32) (result i32))

        (func $char-extended-pictographic?/ucs
              (import "primitives" "char_extended_pictographic")
              (param i32) (result i32))

        ;; Flonum math imports.
        (func $js-raw-math-abs
              (import "math" "abs")
              (param f64) (result f64))
        (func $js-raw-math-acos
              (import "math" "acos")
              (param f64) (result f64))
        (func $js-raw-math-acosh
              (import "math" "acosh")
              (param f64) (result f64))
        (func $js-raw-math-asin
              (import "math" "asin")
              (param f64) (result f64))
        (func $js-raw-math-asinh
              (import "math" "asinh")
              (param f64) (result f64))
        (func $js-raw-math-atan
              (import "math" "atan")
              (param f64) (result f64))
        (func $js-raw-math-atan2
              (import "math" "atan2")
              (param f64) (param f64) (result f64))
        (func $js-raw-math-atanh
              (import "math" "atanh")
              (param f64) (result f64))
        (func $js-raw-math-cbrt
              (import "math" "cbrt")
              (param f64) (result f64))
        (func $js-raw-math-ceil
              (import "math" "ceil")
              (param f64) (result f64))
        (func $js-raw-math-clz32
              (import "math" "clz32")
              (param i32) (result i32))
        (func $js-raw-math-cos
              (import "math" "cos")
              (param f64) (result f64))
        (func $js-raw-math-cosh
              (import "math" "cosh")
              (param f64) (result f64))
        (func $js-raw-math-exp
              (import "math" "exp")
              (param f64) (result f64))
        (func $js-raw-math-expm1
              (import "math" "expm1")
              (param f64) (result f64))
        (func $js-raw-math-floor
              (import "math" "floor")
              (param f64) (result f64))
        (func $js-raw-math-fround
              (import "math" "fround")
              (param f64) (result f64))
        (func $js-raw-math-hypot
              (import "math" "hypot")
              (param f64) (param f64) (result f64))
        (func $js-raw-math-imul
              (import "math" "imul")
              (param i32) (param i32) (result i32))
        (func $js-raw-math-log
              (import "math" "log")
              (param f64) (result f64))
        (func $js-raw-math-log10
              (import "math" "log10")
              (param f64) (result f64))
        (func $js-raw-math-log1p
              (import "math" "log1p")
              (param f64) (result f64))
        (func $js-raw-math-log2
              (import "math" "log2")
              (param f64) (result f64))
        (func $js-raw-math-max
              (import "math" "max")
              (param f64) (param f64) (result f64))
        (func $js-raw-math-min
              (import "math" "min")
              (param f64) (param f64) (result f64))
        (func $js-raw-math-pow
              (import "math" "pow")
              (param f64) (param f64) (result f64))
        (func $js-raw-math-random
              (import "math" "random")
              (result f64))
        (func $js-raw-math-round
              (import "math" "round")
              (param f64) (result f64))
        (func $js-raw-math-sign
              (import "math" "sign")
              (param f64) (result f64))
        (func $js-raw-math-sin
              (import "math" "sin")
              (param f64) (result f64))
        (func $js-raw-math-sinh
              (import "math" "sinh")
              (param f64) (result f64))
        (func $js-raw-math-sqrt
              (import "math" "sqrt")
              (param f64) (result f64))
        (func $js-raw-math-tan
              (import "math" "tan")
              (param f64) (result f64))
        (func $js-raw-math-tanh
              (import "math" "tanh")
              (param f64) (result f64))
        (func $js-raw-math-trunc
              (import "math" "trunc")
              (param f64) (result f64))

         ;; FFI and browser bridge imports.
         (func $js-make-callback
               (import "primitives" "make_callback")
               (param i32)
               (result (ref extern)))

         (func $js-register-external
               (import "primitives" "register_external")
               (param (ref null extern))
               (result i32))

        (func $js-lookup-external
              (import "primitives" "lookup_external")
              (param i32)
              (result (ref extern)))

        (func $js-external-number->f64
              (import "primitives" "external_number_to_f64")
              (param (ref extern)) (result f64))

        (func $js-flonum->string
              (import "primitives" "flonum_to_string")
              (param f64) (result i32))

        (func $js-external-string->string
              (import "primitives" "external_string_to_string")
              (param externref) (result i32))

        ;; Predicates used by define-foreign return-type adapters.
        (func $js-external-nullish?
              (import "primitives" "external_nullish")
              (param externref) (result i32))

        (func $js-external-undefined?
              (import "primitives" "external_undefined")
              (param externref) (result i32))

        ;; FFI host exception tag imported from the host runtime.
        ;; Foreign imports can throw this tag to communicate host-language
        ;; errors that must become ordinary WebRacket exceptions.
        (tag $ffi-host-exn
             (import "primitives" "foreign_error_tag")
             (param externref))))

;; js-host-import-function-names : (listof symbol?)
;;   Enumerate static JavaScript host function imports.
(define js-host-import-function-names
  (for/list ([form (in-list js-host-import-forms)]
             #:when (match form
                      [`(func ,(? symbol?) . ,_) #t]
                      [_ #f]))
    (second form)))
