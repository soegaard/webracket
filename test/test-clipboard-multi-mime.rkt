#lang webracket/base

(require clipboard
         js/promise
         (only-in webracket/unsafe/standard
                  js-eval
                  js-ref
                  js-set!
                  js-var))

;; Browser smoke for ordered multi-MIME ClipboardItem construction and reads.

;; check-true : any/c string? -> void?
;;   Raise a labeled failure unless value is true.
(define (check-true value label)
  (unless value
    (error 'test-clipboard-multi-mime label)))

;; check-equal : any/c any/c string? -> void?
;;   Raise a labeled failure unless got equals expected.
(define (check-equal got expected label)
  (unless (equal? got expected)
    (error 'test-clipboard-multi-mime label)))

;; raises? : procedure? -> boolean?
;;   Report whether thunk raises a normal WebRacket failure.
(define (raises? thunk)
  (with-handlers ([exn:fail? (lambda (_failure) #t)])
    (thunk)
    #f))

(js-eval
 "globalThis.__wrClipboardMultiMime = {
    constructions: 0,
    readCalls: 0,
    readItems: [],
    rejectReads: false
  };
  globalThis.__wrClipboardMultiMimeFulfilled = (value) => ({
    tag: 'fulfilled',
    then(onFulfilled) {
      return globalThis.__wrClipboardMultiMimeFulfilled(
        typeof onFulfilled === 'function' ? onFulfilled(value) : value);
    }
  });
  globalThis.__wrClipboardMultiMimeRejected = (reason) => ({
    tag: 'rejected',
    reason,
    then(_onFulfilled, onRejected) {
      return typeof onRejected === 'function'
        ? globalThis.__wrClipboardMultiMimeFulfilled(onRejected(reason))
        : globalThis.__wrClipboardMultiMimeRejected(reason);
    }
  });
  globalThis.ClipboardItem = class ClipboardItem {
    constructor(flavors) {
      globalThis.__wrClipboardMultiMime.constructions += 1;
      this.flavors = flavors;
      this.types = Object.keys(flavors);
    }
    getType(mimeType) {
      return globalThis.__wrClipboardMultiMimeFulfilled(this.flavors[mimeType]);
    }
  };
  Object.defineProperty(window.navigator, 'clipboard', {
    configurable: true,
    value: {
      readText() { return globalThis.__wrClipboardMultiMimeFulfilled(''); },
      writeText() { return globalThis.__wrClipboardMultiMimeFulfilled(undefined); },
      read() {
        const state = globalThis.__wrClipboardMultiMime;
        state.readCalls += 1;
        return state.rejectReads
          ? globalThis.__wrClipboardMultiMimeRejected('read denied')
          : globalThis.__wrClipboardMultiMimeFulfilled(state.readItems);
      }
    }
  });")

(define first-blob  (make-text-blob "native" "application/x-test-native"))
(define second-blob (make-text-blob "latex" "application/x-test-latex"))
(define third-blob  (make-text-blob "plain" "text/plain"))
(define state       (js-var "__wrClipboardMultiMime"))

;; Every invalid flavor list must fail before calling the host constructor.
(check-true (raises? (lambda () (make-clipboard-item/flavors '())))
            "empty flavor list is rejected")
(check-true (raises? (lambda ()
                       (make-clipboard-item/flavors
                        (list (cons 'text/plain first-blob)))))
            "non-string MIME type is rejected")
(check-true (raises? (lambda ()
                       (make-clipboard-item/flavors
                        (list (cons "text/plain" "not-a-blob")))))
            "non-Blob flavor value is rejected")
(check-true (raises? (lambda ()
                       (make-clipboard-item/flavors
                        (list (cons "text/plain" first-blob)
                              (cons "text/plain" second-blob)))))
            "duplicate MIME type is rejected")
(check-equal (js-ref state "constructions") 0
             "invalid flavors do not call ClipboardItem")

(define first-item
  (make-clipboard-item/flavors
   (list (cons "application/x-test-native" first-blob)
         (cons "application/x-test-latex" second-blob)
         (cons "text/plain" third-blob))))
(define second-item
  (make-clipboard-item/flavors
   (list (cons "text/plain" third-blob))))

(check-true (clipboard-item? first-item) "multi-MIME item wrapper")
(check-equal (clipboard-item-types first-item)
             #("application/x-test-native" "application/x-test-latex" "text/plain")
             "multi-MIME constructor preserves type order")
(check-equal (js-ref state "constructions") 2 "valid flavor lists construct native items")

;; Make the browser read result deliberately put the one-flavor item first.
(js-set! state
         "readItems"
         (vector (clipboard-item-raw second-item)
                 (clipboard-item-raw first-item)))

(define received-items #f)
(define read-result
  (clipboard-read-items (clipboard)))
(check-true (js-promise-thenable? read-result) "read-items returns a wrapped promise")
(js-promise-then
 read-result
 (lambda (items)
   (set! received-items items)
   (void)))

(check-true (vector? received-items) "read-items resolves to a vector")
(check-equal (vector-length received-items) 2 "read-items preserves item count")
(check-true (clipboard-item? (vector-ref received-items 0)) "first read item is wrapped")
(check-true (clipboard-item? (vector-ref received-items 1)) "second read item is wrapped")
(check-equal (clipboard-item-types (vector-ref received-items 0))
             #("text/plain")
             "read-items preserves native item order")
(check-equal (clipboard-item-types (vector-ref received-items 1))
             #("application/x-test-native" "application/x-test-latex" "text/plain")
             "read-items keeps every MIME representation")

;; A rejected browser read remains a rejected promise because no rejection
;; handler is installed while transforming fulfilled values.
(js-set! state "rejectReads" #t)
(define rejected-read (clipboard-read-items (clipboard)))
(check-equal (js-ref (js-promise-unwrap rejected-read) "tag")
             "rejected"
             "read-items preserves browser rejection")
(check-equal (js-ref state "readCalls") 2 "read-items calls Clipboard.read")

(js-eval
 "delete globalThis.__wrClipboardMultiMime;
  delete globalThis.__wrClipboardMultiMimeFulfilled;
  delete globalThis.__wrClipboardMultiMimeRejected;
  delete globalThis.ClipboardItem;")

#t
