#lang webracket

;;;
;;; Smoke Capsule: Parity profile
;;;

;; Isolated parity capsule for parity-profile.
;;
;; Exports:
;;   parity-profile-make-page      Build and mount the parity page under root.
;;   parity-profile-run-test       Execute capsule-local setup checks.
;;   parity-profile-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-profile-make-page parity-profile-run-test parity-profile-cleanup)
  (let ()
    ;; Constants for parity-profile capsule state.
    (define parity-profile-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-profile-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-profile-make-page root)
      ;;;
      ;;; web-easy Browser Parity Profile Settings Example
      ;;;
      
      ;; Parity example: profile/settings panel using group, radios, image, and menu actions.
      
      
      ;; Constants for profile mode values.
      (define mode/photo    "photo")    ; Photo avatar mode.
      (define mode/initials "initials") ; Initials avatar mode.
      (define mode/icon     "icon")     ; Icon avatar mode.
      
      ;; Constants for example-local observable state.
      (define @profile-name (@ "Alice"))
      (define @mode         (@ mode/photo))
      (define @img-src      (@ "avatar-photo.png"))
      (define @status       (@ "ready"))
      (define @preset       (@ #f))
      
      ;; mode->src : string? -> string?
      ;;   Map a profile mode value to an image source string.
      (define (mode->src mode)
        (cond
          [(string=? mode mode/photo) "avatar-photo.png"]
          [(string=? mode mode/initials) "avatar-initials.png"]
          [(string=? mode mode/icon) "avatar-icon.png"]
          [else "avatar-photo.png"]))
      
      ;; mode->text : string? -> string?
      ;;   Format mode value for status text.
      (define (mode->text mode)
        (cond
          [(string=? mode mode/photo) "photo"]
          [(string=? mode mode/initials) "initials"]
          [(string=? mode mode/icon) "icon"]
          [else "photo"]))
      
      ;; normalize-mode : any/c -> string?
      ;;   Convert event payloads to a known profile mode value.
      (define (normalize-mode mode)
        (cond
          [(symbol? mode)
           (case mode
             [(photo) mode/photo]
             [(initials) mode/initials]
             [(icon) mode/icon]
             [else mode/photo])]
          [(string? mode)
           (cond
             [(or (string=? mode mode/photo) (string=? mode "0")) mode/photo]
             [(or (string=? mode mode/initials) (string=? mode "1")) mode/initials]
             [(or (string=? mode mode/icon) (string=? mode "2")) mode/icon]
             [else mode/photo])]
          [else mode/photo]))
      
      ;; set-mode! : string? -> void?
      ;;   Update selected mode and preview image source.
      (define (set-mode! mode)
        (define normalized (normalize-mode mode))
        (:= @mode normalized)
        (:= @img-src (mode->src normalized)))
      
      ;; reset-profile! : -> void?
      ;;   Restore default profile values.
      (define (reset-profile!)
        (:= @profile-name "Alice")
        (set-mode! mode/photo)
        (:= @status "reset"))
      
      ;; rename-bob! : -> void?
      ;;   Change profile name to Bob.
      (define (rename-bob!)
        (:= @profile-name "Bob")
        (:= @status "renamed"))
      
      ;; save-preset! : -> void?
      ;;   Save current profile values in preset state.
      (define (save-preset!)
        (:= @preset (list (obs-peek @profile-name)
                          (obs-peek @mode)
                          (obs-peek @img-src)))
        (:= @status "saved"))
      
      ;; load-preset! : -> void?
      ;;   Restore profile values from preset when present.
      (define (load-preset!)
        (define preset (obs-peek @preset))
        (cond
          [preset
           (:= @profile-name (list-ref preset 0))
           (set-mode! (list-ref preset 1))
           (:= @status "loaded")]
          [else
           (:= @status "no-preset")]))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (group (~> @profile-name
                      (lambda (name)
                        (string-append "Profile: " name)))
                  (text (~> @mode
                            (lambda (mode)
                              (string-append "mode:" (mode->text mode)))))
                  (radios (list mode/photo mode/initials mode/icon)
                          @mode
                          (lambda (new-value)
                            (set-mode! new-value)))
                  (image (~> @img-src
                             (lambda (src)
                               (string-append "../" src))))
                  (text (~> @img-src
                            (lambda (src)
                              (string-append "img:" src))))
                  (text (~> @status
                            (lambda (status)
                              (string-append "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Rename Bob" rename-bob!)
                  (menu-item "Save preset" save-preset!)
                  (menu-item "Load preset" load-preset!)
                  (menu-item "Reset" reset-profile!)))))))
      
      (set! parity-profile-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-profile-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-profile-run-test _root)
      (and parity-profile-renderer #t))

    ;; parity-profile-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-profile-cleanup _root)
      (when parity-profile-renderer
        (renderer-destroy parity-profile-renderer)
        (set! parity-profile-renderer #f))
      (void))

    (values parity-profile-make-page parity-profile-run-test parity-profile-cleanup)))
