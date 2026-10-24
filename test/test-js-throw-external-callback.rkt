#lang webracket/base

;; Browser regression for preserving an explicit JavaScript throw across a
;; procedure->external callback.

(require (only-in webracket/primitives
                  procedure->external)
         (only-in webracket/js
                  js-log)
         (only-in webracket/unsafe/standard
                  js-eval
                  js-new
                  js-ref
                  js-send
                  js-throw
                  js-var))

(define expected-message
  "external callback TypeError must cross the callback boundary")

(define native-type-error
  (js-new (js-var "TypeError") (vector expected-message)))

(define callback
  (procedure->external
   (lambda ()
     (js-throw native-type-error))))

(define catcher
  (js-eval
   "({
       invoke(callback) {
         try {
           callback();
           return { name: 'no-throw', message: '' };
         } catch (error) {
           return { name: error.name, message: error.message };
         }
       }
     })"))

(define outcome
  (js-send catcher "invoke" (vector callback)))

(unless (and (equal? (js-ref outcome "name") "TypeError")
             (equal? (js-ref outcome "message") expected-message))
  (error 'js-throw-external-callback
         "original native TypeError did not cross the callback boundary"))

(js-log "js-throw-external-callback-ok")
#t
