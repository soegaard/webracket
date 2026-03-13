#lang webracket

;;;
;;; Smoke Capsule: Parity workspace
;;;

;; Isolated parity capsule for parity-workspace.
;;
;; Exports:
;;   parity-workspace-make-page      Build and mount the parity page under root.
;;   parity-workspace-run-test       Execute capsule-local setup checks.
;;   parity-workspace-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-workspace-make-page parity-workspace-run-test parity-workspace-cleanup)
  (let ()
    ;; Constants for parity-workspace capsule state.
    (define parity-workspace-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-workspace-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-workspace-make-page root)
      ;; Constants for theme choice values.
      (define theme/system "system") ; System theme mode.
      (define theme/light  "light")  ; Light theme mode.
      (define theme/dark   "dark")   ; Dark theme mode.

      ;; Constants for tab ids.
      (define tab/account       'account)       ; Account tab id.
      (define tab/security      'security)      ; Security tab id.
      (define tab/notifications 'notifications) ; Notifications tab id.

      ;; Constants for default workspace state.
      (define default/name          "Alice")             ; Default display name.
      (define default/email         "alice@example.com") ; Default e-mail.
      (define default/two-factor?   #f)                  ; Default 2FA setting.
      (define default/notify-email? #t)                  ; Default email-notify setting.
      (define default/theme         theme/system)        ; Default theme selection.

      ;; Constants for example-local observable state.
      (define @tab           (@ tab/account))
      (define @name          (@ default/name))
      (define @email         (@ default/email))
      (define @two-factor?   (@ default/two-factor?))
      (define @notify-email? (@ default/notify-email?))
      (define @theme         (@ default/theme))
      (define @status        (@ "ready"))
      (define @summary-rows  (@ '()))
      (define @preset        (@ #f))

      (struct workspace-preset (name email two-factor? notify-email? theme) #:transparent)

      ;; summary-rows-now : -> list?
      ;;   Build one-column summary rows from current workspace state.
      (define (summary-rows-now)
        (list (~a "name:" (obs-peek @name))
              (~a "email:" (obs-peek @email))
              (~a "two-factor:" (if (obs-peek @two-factor?) "on" "off"))
              (~a "notify-email:" (if (obs-peek @notify-email?) "on" "off"))
              (~a "theme:" (obs-peek @theme))))

      ;; refresh-summary! : -> void?
      ;;   Refresh summary rows observable from current state.
      (define (refresh-summary!)
        (:= @summary-rows (summary-rows-now)))

      ;; save-preset! : -> void?
      ;;   Store current workspace settings as a preset.
      (define (save-preset!)
        (:= @preset
            (workspace-preset (obs-peek @name)
                              (obs-peek @email)
                              (obs-peek @two-factor?)
                              (obs-peek @notify-email?)
                              (obs-peek @theme)))
        (:= @status "saved")
        (refresh-summary!))

      ;; load-preset! : -> void?
      ;;   Restore workspace settings from saved preset when present.
      (define (load-preset!)
        (define preset (obs-peek @preset))
        (if preset
            (begin
              (:= @name (workspace-preset-name preset))
              (:= @email (workspace-preset-email preset))
              (:= @two-factor? (workspace-preset-two-factor? preset))
              (:= @notify-email? (workspace-preset-notify-email? preset))
              (:= @theme (workspace-preset-theme preset))
              (:= @status "loaded")
              (refresh-summary!))
            (:= @status "no-preset")))

      ;; reset-workspace! : -> void?
      ;;   Restore all workspace settings to defaults.
      (define (reset-workspace!)
        (:= @name default/name)
        (:= @email default/email)
        (:= @two-factor? default/two-factor?)
        (:= @notify-email? default/notify-email?)
        (:= @theme default/theme)
        (:= @tab tab/account)
        (:= @status "reset")
        (refresh-summary!))

      (define app-renderer
        (render
         (window
          (vpanel
           (tab-panel
            @tab
            (list (cons tab/account
                        (group "Account"
                               (text "name")
                               (input @name
                                      (lambda (new-value)
                                        (:= @name new-value)
                                        (refresh-summary!)))
                               (text "email")
                               (input @email
                                      (lambda (new-value)
                                        (:= @email new-value)
                                        (refresh-summary!)))))
                  (cons tab/security
                        (group "Security"
                               (text (~> @two-factor?
                                         (lambda (enabled?)
                                           (if enabled?
                                               "2FA: enabled"
                                               "2FA: disabled"))))
                               (checkbox @two-factor?
                                         (lambda (enabled?)
                                           (:= @two-factor? enabled?)
                                           (refresh-summary!)))))
                  (cons tab/notifications
                        (group "Notifications"
                               (text (~> @notify-email?
                                         (lambda (enabled?)
                                           (if enabled?
                                               "email alerts: on"
                                               "email alerts: off"))))
                               (checkbox @notify-email?
                                         (lambda (enabled?)
                                           (:= @notify-email? enabled?)
                                           (refresh-summary!)))
                               (text "theme")
                               (choice (list theme/system theme/light theme/dark)
                                       @theme
                                       (lambda (new-theme)
                                         (:= @theme new-theme)
                                         (refresh-summary!)))))))
           (group "Workspace Summary"
                  (table
                            '(summary)
                            @summary-rows
                            #:density 'normal)
                  (text (~> @status
                            (lambda (status)
                              (~a "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Save preset" save-preset!)
                  (menu-item "Load preset" load-preset!)
                  (menu-item "Reset workspace" reset-workspace!)))))))

      (refresh-summary!)
      (set! parity-workspace-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-workspace-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-workspace-run-test _root)
      (and parity-workspace-renderer #t))

    ;; parity-workspace-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-workspace-cleanup _root)
      (when parity-workspace-renderer
        (renderer-destroy parity-workspace-renderer)
        (set! parity-workspace-renderer #f))
      (void))

    (values parity-workspace-make-page parity-workspace-run-test parity-workspace-cleanup)))
