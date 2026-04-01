;;;
;;; dom.ffi
;;;

;; Focused tests for the Image wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-image.rkt

(include-lib image)

(define (check-equal got want label)
  (unless (if (and (number? got) (number? want))
              (= got want)
              (equal? got want))
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-false got label)
  (when got
    (error 'check-false label)))

(define (expect-contract-error thunk)
  (with-handlers ([exn:fail:contract? (lambda (_e) #t)])
    (thunk)
    #f))

(define (install!)
  (js-eval
   "globalThis.Image = class Image {
      constructor(width, height) {
        this.width = width;
        this.height = height;
        this.alt = '';
        this.src = '';
        this.decoding = 'auto';
        this.loading = 'eager';
        this.crossOrigin = 'anonymous';
        this.complete = 0;
      }
    };
    globalThis.HTMLImageElement = globalThis.Image;"))

(list
 (list "Image wrappers"
       (let ()
         (install!)
         (define image (image-new 10 20))
         (check-equal (image-width image) 10 "image width")
         (check-equal (image-height image) 20 "image height")
         (check-equal (image-alt image) "" "image alt")
         (check-equal (image-src image) "" "image src")
         (check-equal (image-decoding image) "auto" "image decoding")
         (check-equal (image-loading image) "eager" "image loading")
         (check-false (image-complete? image) "image complete")
         (check-equal (image-cross-origin image) "anonymous" "image cross origin")
         (image-set-alt! image "decorative")
         (check-equal (image-alt image) "decorative" "image alt set")
         (image-set-src! image "icon.png")
         (check-equal (image-src image) "icon.png" "image src set")
         (image-set-width! image 11)
         (check-equal (image-width image) 11 "image width set")
         (image-set-height! image 21)
         (check-equal (image-height image) 21 "image height set")
         (image-set-decoding! image "sync")
         (check-equal (image-decoding image) "sync" "image decoding set")
         (image-set-loading! image "lazy")
         (check-equal (image-loading image) "lazy" "image loading set")
         (image-set-cross-origin! image "use-credentials")
         (check-equal (image-cross-origin image) "use-credentials" "image cross origin set")
         (check-true (expect-contract-error (lambda () (image-set-alt! image 1))) "image alt validation")
         (check-true (expect-contract-error (lambda () (image-set-src! image 1))) "image src validation")
         #t)))
