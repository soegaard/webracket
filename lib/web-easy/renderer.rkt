#lang webracket

;;;
;;; web-easy Renderer
;;;

;; Renderer runtime that builds and updates a DOM-like node tree from view values.
;;
;; Exports:
;;   renderer?               Predicate for renderer values.
;;   render                  Render a view into runtime nodes.
;;   renderer-root           Return renderer root node.
;;   renderer-destroy        Destroy renderer and run cleanups.
;;   current-web-easy-warning-handler  Current warning sink procedure.
;;   set-current-web-easy-warning-handler!  Replace warning sink procedure.
;;   call-with-web-easy-warning-handler  Run thunk with temporary warning sink.
;;   dom-node-click!         Invoke node click callback when present.
;;   dom-node-change!        Invoke node change callback when present.
;;   dom-node-toggle!        Toggle checkbox state and invoke change callback.
;;   dom-node-select!        Set selected value and invoke change callback.
;;   dom-node-slide!         Set slider value and invoke change callback.
;;   dom-node-radio-select!  Set radio selection and invoke change callback.
;;   dom-node-keydown!       Invoke node keydown callback when present.
;;
;; Backend contract used by this renderer:
;;   dom-node
;;   dom-node?
;;   dom-node-tag
;;   dom-node-attrs
;;   dom-node-children
;;   dom-node-text
;;   dom-node-on-click
;;   dom-node-on-change
;;   set-dom-node-tag!
;;   set-dom-node-attrs!
;;   set-dom-node-children!
;;   set-dom-node-text!
;;   set-dom-node-on-click!
;;   set-dom-node-on-change!
;;   backend-append-child!
;;   backend-set-single-child!
;;   backend-replace-children!
;;   backend-scrollspy-observe-scroll!
;;   backend-scrollspy-scroll-into-view!
;;   backend-scrollspy-active-id

(define-values
  (renderer?
   render
   renderer-root
   renderer-destroy
   current-web-easy-warning-handler
   set-current-web-easy-warning-handler!
   call-with-web-easy-warning-handler
   dom-node-click!
   dom-node-change!
   dom-node-toggle!
   dom-node-select!
   dom-node-slide!
   dom-node-radio-select!
   dom-node-keydown!)
  (let ()
    (struct renderer-state (root cleanups destroyed?) #:mutable #:transparent)

    ;; current-web-easy-warning-handler : (-> string? any/c)
    ;;   Procedure controlling where non-fatal web-easy warnings are sent.
    (define current-web-easy-warning-handler displayln)

    ;; set-current-web-easy-warning-handler! : (-> string? any/c) -> void?
    ;;   Replace warning sink procedure for non-fatal web-easy warnings.
    (define (set-current-web-easy-warning-handler! handler)
      (unless (procedure? handler)
        (raise-arguments-error 'set-current-web-easy-warning-handler!
                               "expected procedure?"
                               "handler"
                               handler))
      (set! current-web-easy-warning-handler handler)
      (void))

    ;; call-with-web-easy-warning-handler : (-> string? any/c) (-> any/c) -> any/c
    ;;   Run thunk while temporarily overriding warning sink; always restore previous sink.
    (define (call-with-web-easy-warning-handler handler thunk)
      (unless (procedure? handler)
        (raise-arguments-error 'call-with-web-easy-warning-handler
                               "expected handler as procedure?"
                               "handler"
                               handler))
      (unless (procedure? thunk)
        (raise-arguments-error 'call-with-web-easy-warning-handler
                               "expected thunk as procedure?"
                               "thunk"
                               thunk))
      (define old-handler current-web-easy-warning-handler)
      (set-current-web-easy-warning-handler! handler)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (set-current-web-easy-warning-handler! old-handler)
                         (raise e))])
        (define result (thunk))
        (set-current-web-easy-warning-handler! old-handler)
        result))

    ;; emit-web-easy-warning! : string? -> void?
    ;;   Send warning message through current warning handler.
    (define (emit-web-easy-warning! msg)
      (current-web-easy-warning-handler msg))

    ;; Constants for node attributes and fallbacks.
    (define attr/role             'role)      ; Attribute key for semantic role.
    (define text/fallback         "#<value>") ; Fallback when value cannot be rendered as text.
    (define table/density-normal  'normal)    ; Default table spacing density.
    (define table/density-compact 'compact)   ; Compact table spacing density.
    (define tab-panel-counter     0)          ; Monotonic counter for tab-panel ids.
    (define accordion-panel-counter 0)        ; Monotonic counter for accordion panel ids.
    (define tooltip-counter       0)          ; Monotonic counter for tooltip ids.
    (define popover-panel-counter 0)          ; Monotonic counter for popover panel ids.
    (define dialog-body-counter   0)          ; Monotonic counter for dialog body ids.

    ;; Legacy visual style constants (kept for reference during migration).
    ;; These are intentionally not injected by renderer. Visual styling is theme-owned.
    (define legacy-visual-tab-panel-style-text ; CSS for class-based tab styles.
      ".we-tab-panel{display:flex;flex-direction:column;align-items:stretch;}\
       .we-tab-list{display:flex;gap:0;align-items:flex-end;border-bottom:1px solid var(--we-border-muted,#999);padding:0 var(--we-space-xs,2px);margin:0;}\
       .we-tab-btn{min-width:88px;padding:var(--we-space-sm,4px) var(--we-space-md,10px);border:1px solid var(--we-border-muted,#999);border-bottom-color:var(--we-border-muted,#999);border-radius:6px 6px 0 0;background:var(--we-bg-subtle,#f3f3f3);font-weight:normal;margin:0;}\
       .we-tab-btn+.we-tab-btn{margin-left:var(--we-space-xs,2px);}\
       .we-tab-btn.is-selected{border-color:var(--we-border-strong,#333);border-bottom-color:var(--we-tab-active-border,var(--we-bg-selected,#ececec));background:var(--we-bg-selected,#ececec);font-weight:bold;position:relative;z-index:1;}\
       .we-tab-btn.is-disabled{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-disabled,#f3f3f3);color:var(--we-fg-muted,#777);opacity:.7;}\
       .we-tab-btn:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-tab-content{border:1px solid var(--we-border-muted,#999);border-top:none;background:var(--we-bg,#fff);padding:var(--we-space-md,8px);}") 
    (define legacy-visual-accordion-style-text ; CSS for accordion trigger and section visuals.
      ".we-accordion{display:flex;flex-direction:column;gap:var(--we-space-xs,2px);align-self:stretch;}\
       .we-accordion-section{border:1px solid var(--we-border-muted,#999);border-radius:6px;background:var(--we-bg,#fff);}\
       .we-accordion-trigger{width:100%;display:flex;align-items:center;justify-content:space-between;text-align:left;padding:var(--we-space-sm,4px) var(--we-space-md,8px);border:none;border-radius:6px;background:var(--we-bg-subtle,#f3f3f3);color:var(--we-fg,#111);font-weight:600;cursor:pointer;}\
       .we-accordion-trigger::after{content:'▸';display:inline-block;margin-left:var(--we-space-md,8px);color:var(--we-fg-muted,#777);transform:rotate(0deg);transition:transform .18s ease,color .18s ease;}\
       .we-accordion-trigger:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-accordion-trigger.is-open{background:var(--we-bg-selected,#ececec);border-bottom:1px solid var(--we-border-soft,#bbb);border-radius:6px 6px 0 0;}\
       .we-accordion-trigger.is-open::after{transform:rotate(90deg);color:var(--we-border-strong,#333);}\
       .we-accordion-trigger:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}")
    (define legacy-visual-dialog-style-text ; CSS for dialog overlay and panel.
      ".we-dialog,.we-modal{position:fixed;inset:0;display:none;align-items:center;justify-content:center;background:var(--we-overlay,rgba(0,0,0,0.45));z-index:2000;}\
       .we-dialog.is-open,.we-modal.is-open{display:flex;}\
       .we-dialog-panel{min-width:280px;max-width:520px;background:var(--we-bg,#fff);border:1px solid var(--we-border,#888);border-radius:8px;padding:14px;box-shadow:0 8px 22px var(--we-shadow,rgba(0,0,0,.28));}\
       .we-dialog-panel.we-dialog-size-sm{max-width:360px;}\
       .we-dialog-panel.we-dialog-size-md{max-width:520px;}\
       .we-dialog-panel.we-dialog-size-lg{max-width:720px;}\
       .we-dialog-panel.we-dialog-size-xl{max-width:960px;}\
       .we-dialog-panel:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.14)),var(--we-focus-tint,rgba(10,102,194,.14)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:2px;}") 
    (define legacy-visual-menu-style-text      ; CSS for popup menu keyboard focus visibility and layout.
      ".we-menu-item:focus,.we-menu-item:focus-visible{outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;background-color:var(--we-focus-tint,rgba(10,102,194,.20));position:relative;z-index:1;}\
       .we-menu-label:focus,.we-menu-label:focus-visible{outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;background:var(--we-focus-tint,rgba(10,102,194,.20));color:var(--we-fg,#111);border-color:var(--we-border-soft,#bbb);}\
       .we-menu-bar{display:flex;flex-wrap:wrap;gap:var(--we-space-xs,2px);align-items:center;padding:var(--we-space-xs,2px) var(--we-space-sm,4px);border:1px solid var(--we-border-menu,#aaa);border-radius:4px;background:var(--we-bg-subtle,#f3f3f3);box-sizing:border-box;}\
       .we-menu{position:relative;display:inline-block;}\
       .we-menu-label{padding:var(--we-space-xs,2px) var(--we-space-md,8px);border:1px solid transparent;border-radius:3px;background:transparent;color:var(--we-fg,#111);cursor:pointer;user-select:none;}\
       .we-menu-label:hover{background:var(--we-bg-hover,#e8e8e8);color:var(--we-fg,#111);border-color:var(--we-border-soft,#bbb);}\
       .we-menu-label[aria-expanded='true']{background:var(--we-bg-selected,#ececec);color:var(--we-fg,#111);border-color:var(--we-border-soft,#bbb);position:relative;z-index:1001;}\
       .we-dropdown .we-menu-label::after{content:'▾';display:inline-block;margin-left:var(--we-space-xs,2px);color:var(--we-fg-muted,#777);transition:transform .16s ease,color .16s ease;}\
       .we-dropdown .we-menu-label[aria-expanded='true']::after{transform:rotate(180deg);color:var(--we-fg,#111);}\
       .we-menu-popup{position:absolute;top:calc(100% + var(--we-space-xs,2px));left:0;min-width:150px;display:none;flex-direction:column;gap:0;padding:var(--we-space-xs,2px);border:1px solid var(--we-border,#888);border-radius:4px;background:var(--we-bg,#fff);z-index:1000;box-shadow:0 4px 10px var(--we-shadow,rgba(0,0,0,.18));}\
       .we-dropdown.we-dropdown-up .we-menu-popup{top:auto;bottom:calc(100% + var(--we-space-xs,2px));left:0;right:auto;}\
       .we-dropdown.we-dropdown-start .we-menu-popup{top:0;left:auto;right:calc(100% + var(--we-space-xs,2px));bottom:auto;}\
       .we-dropdown.we-dropdown-end .we-menu-popup{top:0;left:calc(100% + var(--we-space-xs,2px));right:auto;bottom:auto;}\
       .we-menu-popup.is-open{display:flex;}\
       .we-menu-item{display:block;width:100%;text-align:left;padding:var(--we-space-xs,2px) var(--we-space-md,8px);background:var(--we-bg,#fff);color:var(--we-fg,#111);border:none;border-radius:3px;}\
       .we-menu-item:hover{background:var(--we-menu-item-hover-bg,var(--we-bg-hover,#e8e8e8));color:var(--we-menu-item-hover-fg,var(--we-fg,#111));}") 
    (define legacy-visual-tooltip-popover-style-text ; CSS for tooltip and popover trigger/panel visuals.
      ".we-tooltip{display:inline-flex;align-self:flex-start;position:relative;}\
       .we-tooltip-trigger{display:inline-flex;}\
       .we-tooltip-bubble{position:absolute;left:50%;bottom:calc(100% + var(--we-space-xs,2px));transform:translate(-50%,0);display:block;padding:var(--we-space-xs,2px) var(--we-space-md,8px);border:1px solid var(--we-border,#888);border-radius:4px;background:var(--we-bg,#fff);color:var(--we-fg,#111);white-space:nowrap;pointer-events:none;opacity:0;z-index:1200;box-shadow:0 4px 10px var(--we-shadow,rgba(0,0,0,.18));transition:opacity .14s ease;}\
       .we-tooltip.we-tooltip-left .we-tooltip-bubble{left:auto;right:calc(100% + var(--we-space-xs,2px));top:50%;bottom:auto;transform:translateY(-50%);}\
       .we-tooltip.we-tooltip-right .we-tooltip-bubble{left:calc(100% + var(--we-space-xs,2px));right:auto;top:50%;bottom:auto;transform:translateY(-50%);}\
       .we-tooltip.we-tooltip-bottom .we-tooltip-bubble{left:50%;right:auto;top:calc(100% + var(--we-space-xs,2px));bottom:auto;transform:translate(-50%,0);}\
       .we-tooltip.we-tooltip-top .we-tooltip-bubble{left:50%;right:auto;top:auto;bottom:calc(100% + var(--we-space-xs,2px));transform:translate(-50%,0);}\
       .we-tooltip:hover .we-tooltip-bubble,.we-tooltip:focus-within .we-tooltip-bubble,.we-tooltip.is-open .we-tooltip-bubble{opacity:1;}\
       .we-popover{display:inline-flex;align-self:flex-start;position:relative;z-index:1201;}\
       .we-popover-trigger{align-self:flex-start;position:relative;z-index:1201;}\
       .we-popover-backdrop{position:fixed;inset:0;display:none;background:transparent;z-index:1190;}\
       .we-popover-backdrop.is-open{display:block;}\
       .we-popover-panel{position:absolute;left:0;top:calc(100% + var(--we-space-xs,2px));min-width:220px;display:none;flex-direction:column;gap:var(--we-gap,0.5rem);padding:var(--we-space-md,8px);border:1px solid var(--we-border,#888);border-radius:8px;background:var(--we-bg,#fff);color:var(--we-fg,#111);z-index:1200;box-shadow:0 8px 22px var(--we-shadow,rgba(0,0,0,.28));}\
       .we-popover.we-popover-left .we-popover-panel{left:auto;right:calc(100% + var(--we-space-xs,2px));top:50%;transform:translateY(-50%);}\
       .we-popover.we-popover-right .we-popover-panel{left:calc(100% + var(--we-space-xs,2px));right:auto;top:50%;transform:translateY(-50%);}\
       .we-popover.we-popover-top .we-popover-panel{left:50%;right:auto;top:auto;bottom:calc(100% + var(--we-space-xs,2px));transform:translateX(-50%);}\
       .we-popover.we-popover-bottom .we-popover-panel{left:50%;right:auto;top:calc(100% + var(--we-space-xs,2px));bottom:auto;transform:translateX(-50%);}\
       .we-popover-panel.is-open{display:flex;}\
       .we-popover-panel:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.14)),var(--we-focus-tint,rgba(10,102,194,.14)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}")
    (define legacy-visual-control-style-text ; CSS defaults for controls and table density classes.
      ":root{--we-focus:#0a66c2;--we-focus-tint:rgba(10,102,194,.20);--we-fg:#111;--we-bg:#fff;--we-bg-subtle:#f3f3f3;--we-bg-selected:#ececec;--we-bg-disabled:#f3f3f3;--we-bg-hover:#e8e8e8;--we-border:#888;--we-border-menu:#aaa;--we-border-muted:#999;--we-border-soft:#bbb;--we-border-hover:#c0c0c0;--we-border-strong:#333;--we-fg-muted:#777;--we-overlay:rgba(0,0,0,0.45);--we-shadow:rgba(0,0,0,.28);--we-progress-success:#3a9147;--we-progress-warning:#b57c1c;--we-progress-danger:#b24545;--we-heading-fg:var(--we-fg,#111);--we-display-heading-fg:var(--we-heading-fg,var(--we-fg,#111));--we-heading-subtitle-fg:var(--we-fg-muted,#777);--we-lead-fg:var(--we-fg-muted,#777);--we-heading-space-compact:0;--we-heading-space-normal:0 0 var(--we-space-xs,2px) 0;--we-heading-space-loose:0 0 var(--we-space-sm,4px) 0;--we-menu-item-hover-bg:var(--we-bg-hover,#e8e8e8);--we-menu-item-hover-fg:var(--we-fg,#111);--we-tab-active-border:var(--we-bg-selected,#ececec);--we-input-placeholder:var(--we-fg-muted,#777);--we-space-xs:2px;--we-space-sm:4px;--we-space-md:8px;--we-space-lg:10px;--we-gap:0.5rem;--we-form-gap:var(--we-gap,0.5rem);--we-gap-tab:0.375rem;}\
       .we-vpanel,.we-group,.we-if-view,.we-cond-view,.we-case-view,.we-observable-view,.we-list-view{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);}\
       .we-stack{display:flex;flex-direction:column;gap:var(--we-stack-gap,var(--we-gap,0.5rem));}\
       .we-container{width:min(1200px,calc(100vw - 28px));max-width:1200px;margin:0 auto;}\
       .we-grid{display:grid;grid-template-columns:var(--we-grid-columns,repeat(auto-fit,minmax(320px,1fr)));gap:var(--we-grid-gap,12px);align-items:stretch;}\
       .we-inline{display:flex;flex-direction:row;align-items:center;gap:var(--we-gap,0.5rem);flex-wrap:wrap;}\
       .we-spacer{display:block;flex:1 1 auto;min-width:0;min-height:0;}\
       .we-alert{align-self:stretch;padding:var(--we-space-sm,4px) var(--we-space-md,8px);border:1px solid var(--we-border-soft,#bbb);border-radius:4px;background:var(--we-bg-subtle,#f3f3f3);color:var(--we-fg,#111);}\
       .we-alert-title{display:block;font-weight:700;margin:0 0 var(--we-space-xs,2px) 0;}\
       .we-alert-body{display:block;}\
       .we-alert-link{display:inline-block;margin-top:var(--we-space-xs,2px);}\
       .we-alert-info{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);}\
       .we-alert-success{border-color:#6a9b73;background:#e8f4e8;}\
       .we-alert-warning{border-color:#b79256;background:#fff4df;}\
       .we-alert-danger{border-color:#b25a5a;background:#fdeaea;}\
       .we-toast{position:fixed;right:var(--we-space-md,8px);bottom:var(--we-space-md,8px);display:none;align-items:flex-start;gap:var(--we-space-sm,4px);min-width:220px;max-width:min(420px,calc(100vw - 2 * var(--we-space-md,8px)));padding:var(--we-space-sm,4px) var(--we-space-md,8px);border:1px solid var(--we-border-soft,#bbb);border-radius:6px;background:var(--we-bg,#fff);color:var(--we-fg,#111);box-shadow:0 6px 18px var(--we-shadow,rgba(0,0,0,.28));transform:translateY(6px);opacity:0;transition:opacity .18s ease,transform .18s ease;}\
       .we-toast.is-open{display:flex;transform:translateY(0);opacity:1;}\
       .we-toast-info{border-color:var(--we-border-soft,#bbb);background:var(--we-bg,#fff);}\
       .we-toast-success{border-color:#6a9b73;background:#e8f4e8;}\
       .we-toast-warning{border-color:#b79256;background:#fff4df;}\
       .we-toast-danger{border-color:#b25a5a;background:#fdeaea;}\
       .we-toast-title{display:block;font-weight:600;}\
       .we-toast-message{flex:1 1 auto;}\
       /* Keep close-button content-sized in vpanel/hpanel flex layouts (avoid default stretch-to-full-width). */\
       .we-close-button{align-self:flex-start;width:auto;padding:0 6px;border:1px solid transparent;border-radius:4px;background:transparent;color:var(--we-fg,#111);line-height:1.2;cursor:pointer;}\
       /* The icon glyph is CSS-driven, so users can replace it in stylesheets via `.we-close-button-icon::before`. */\
       .we-close-button-icon{display:inline-block;min-width:1ch;text-align:center;font-weight:700;line-height:1;}\
       .we-close-button-icon::before{content:var(--we-close-glyph,'×');}\
       .we-close-button:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-close-button:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-toast-close{padding:0 6px;border:1px solid transparent;border-radius:4px;background:transparent;color:inherit;line-height:1.2;}\
       .we-toast-close:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-toast-close:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-badge{display:inline-block;align-self:flex-start;padding:1px 8px;border:1px solid var(--we-border-soft,#bbb);border-radius:999px;background:var(--we-bg-subtle,#f3f3f3);color:var(--we-fg,#111);font-size:.85em;font-weight:600;line-height:1.4;}\
       .we-badge-info{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);}\
       .we-badge-primary{border-color:var(--we-border-strong,#333);background:var(--we-bg-selected,#ececec);}\
       .we-badge-secondary{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);}\
       .we-badge-success{border-color:#6a9b73;background:#e8f4e8;}\
       .we-badge-light{border-color:var(--we-border-soft,#bbb);background:var(--we-bg,#fff);}\
       .we-badge-dark{border-color:var(--we-border-strong,#333);background:var(--we-border-strong,#333);color:var(--we-bg,#fff);}\
       .we-badge-warning{border-color:#b79256;background:#fff4df;}\
       .we-badge-danger{border-color:#b25a5a;background:#fdeaea;}\
       .we-spinner{display:inline-flex;align-items:center;gap:var(--we-space-sm,4px);align-self:flex-start;color:var(--we-fg,#111);}\
       .we-spinner-icon{width:12px;height:12px;border:2px solid var(--we-border-soft,#bbb);border-top-color:var(--we-border-strong,#333);border-radius:50%;animation:we-spin .8s linear infinite;}\
       .we-spinner-label{color:var(--we-fg,#111);}\
       .we-placeholder{display:inline-block;align-self:flex-start;border:1px solid var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);color:transparent;}\
       .we-placeholder-text{height:0.9em;width:7em;border-radius:4px;}\
       .we-placeholder-rect{height:2.4em;width:7em;border-radius:6px;}\
       .we-placeholder-circle{height:2.2em;width:2.2em;border-radius:50%;}\
       @keyframes we-spin{to{transform:rotate(360deg);}}\
       .we-collapse{display:grid;grid-template-rows:0fr;opacity:0;visibility:hidden;overflow:hidden;align-self:stretch;transition:grid-template-rows .18s ease,opacity .18s ease;}\
       .we-collapse>*{min-height:0;overflow:hidden;}\
       .we-collapse.is-open{grid-template-rows:1fr;opacity:1;visibility:visible;}\
       .we-hpanel{display:flex;flex-direction:row;align-items:center;gap:var(--we-gap,0.5rem);}\
       .we-button-toolbar{display:flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:var(--we-space-sm,4px);}\
       .we-button-group{display:inline-flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:0;border:1px solid var(--we-border-soft,#bbb);border-radius:6px;overflow:hidden;background:var(--we-bg,#fff);}\
       .we-button-group>.we-button{border:0;border-right:1px solid var(--we-border-soft,#bbb);border-radius:0;margin:0;}\
       .we-button-group>.we-button:last-child{border-right:0;}\
       .we-button{align-self:flex-start;width:auto;}\
       .we-button:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-input,.we-textarea{align-self:stretch;width:100%;box-sizing:border-box;}\
       .we-input::placeholder{color:var(--we-input-placeholder,var(--we-fg-muted,#777));}\
       .we-textarea::placeholder{color:var(--we-input-placeholder,var(--we-fg-muted,#777));}\
       .we-input:focus-visible,.we-textarea:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.14)),var(--we-focus-tint,rgba(10,102,194,.14)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-checkbox,.we-choice,.we-slider,.we-progress,.we-radios,.we-image{align-self:flex-start;}\
       .we-pagination{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-xs,2px);align-self:flex-start;}\
       .we-page-btn{min-width:30px;padding:2px 8px;border:1px solid var(--we-border-soft,#bbb);border-radius:4px;background:var(--we-bg,#fff);color:var(--we-fg,#111);}\
       .we-page-btn.is-current{border-color:var(--we-border-strong,#333);background:var(--we-bg-selected,#ececec);text-decoration:underline;text-underline-offset:3px;text-decoration-thickness:2px;}\
       .we-page-btn.is-disabled{border-color:var(--we-border-soft,#bbb);background:var(--we-bg-disabled,#f3f3f3);color:var(--we-fg-muted,#777);}\
       .we-page-btn:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-page-ellipsis{color:var(--we-fg-muted,#777);padding:0 4px;}\
       .we-breadcrumb{display:flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:var(--we-space-xs,2px);}\
       .we-breadcrumb-item{padding:2px 6px;border:1px solid transparent;border-radius:4px;background:transparent;color:var(--we-fg,#111);}\
       .we-breadcrumb-item:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-breadcrumb-item:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-breadcrumb-item.is-current{color:var(--we-fg,#111);cursor:default;text-decoration:underline;text-underline-offset:3px;text-decoration-thickness:2px;}\
       .we-breadcrumb-sep{color:var(--we-fg-muted,#777);padding:0 2px;}\
       .we-list-group{display:flex;flex-direction:column;align-self:flex-start;border:1px solid var(--we-border-soft,#bbb);border-radius:6px;overflow:hidden;background:var(--we-bg,#fff);}\
       .we-list-group-item{padding:6px 10px;border:0;border-bottom:1px solid var(--we-border-soft,#bbb);background:transparent;color:var(--we-fg,#111);text-align:left;}\
       .we-list-group-item:last-child{border-bottom:0;}\
       .we-list-group-item:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-list-group-item:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-list-group-item.is-current{background:var(--we-bg-selected,#ececec);text-decoration:underline;text-underline-offset:3px;text-decoration-thickness:2px;}\
       .we-heading{margin:0;line-height:1.2;font-weight:700;color:var(--we-heading-fg,var(--we-fg,#111));}\
       .we-heading-1{font-size:2em;}\
       .we-heading-2{font-size:1.5em;}\
       .we-heading-3{font-size:1.17em;}\
       .we-heading-4{font-size:1em;}\
       .we-heading-5{font-size:0.83em;}\
       .we-heading-6{font-size:0.67em;}\
       .we-heading-align-left{text-align:left;}\
       .we-heading-align-center{text-align:center;}\
       .we-heading-align-right{text-align:right;}\
       .we-heading-space-compact{margin:var(--we-heading-space-compact,0);}\
       .we-heading-space-normal{margin:var(--we-heading-space-normal,0 0 var(--we-space-xs,2px) 0);}\
       .we-heading-space-loose{margin:var(--we-heading-space-loose,0 0 var(--we-space-sm,4px) 0);}\
       .we-display-heading{margin:0;line-height:1.1;font-weight:700;color:var(--we-display-heading-fg,var(--we-heading-fg,var(--we-fg,#111)));}\
       .we-display-heading-1{font-size:3.2em;}\
       .we-display-heading-2{font-size:2.8em;}\
       .we-display-heading-3{font-size:2.4em;}\
       .we-display-heading-4{font-size:2em;}\
       .we-display-heading-5{font-size:1.7em;}\
       .we-display-heading-6{font-size:1.4em;}\
       .we-display-heading-align-left{text-align:left;}\
       .we-display-heading-align-center{text-align:center;}\
       .we-display-heading-align-right{text-align:right;}\
       .we-display-heading-space-compact{margin:var(--we-heading-space-compact,0);}\
       .we-display-heading-space-normal{margin:var(--we-heading-space-normal,0 0 var(--we-space-xs,2px) 0);}\
       .we-display-heading-space-loose{margin:var(--we-heading-space-loose,0 0 var(--we-space-sm,4px) 0);}\
       .we-heading-with-subtitle{display:flex;flex-direction:column;gap:2px;margin:0;line-height:1.2;font-weight:700;color:var(--we-heading-fg,var(--we-fg,#111));}\
       .we-heading-with-subtitle-1{font-size:2em;}\
       .we-heading-with-subtitle-2{font-size:1.5em;}\
       .we-heading-with-subtitle-3{font-size:1.17em;}\
       .we-heading-with-subtitle-4{font-size:1em;}\
       .we-heading-with-subtitle-5{font-size:0.83em;}\
       .we-heading-with-subtitle-6{font-size:0.67em;}\
       .we-heading-with-subtitle-align-left{text-align:left;}\
       .we-heading-with-subtitle-align-center{text-align:center;}\
       .we-heading-with-subtitle-align-right{text-align:right;}\
       .we-heading-with-subtitle-space-compact{margin:var(--we-heading-space-compact,0);}\
       .we-heading-with-subtitle-space-normal{margin:var(--we-heading-space-normal,0 0 var(--we-space-xs,2px) 0);}\
       .we-heading-with-subtitle-space-loose{margin:var(--we-heading-space-loose,0 0 var(--we-space-sm,4px) 0);}\
       .we-display-heading-with-subtitle{display:flex;flex-direction:column;gap:2px;margin:0;line-height:1.1;font-weight:700;color:var(--we-display-heading-fg,var(--we-heading-fg,var(--we-fg,#111)));}\
       .we-display-heading-with-subtitle-1{font-size:3.2em;}\
       .we-display-heading-with-subtitle-2{font-size:2.8em;}\
       .we-display-heading-with-subtitle-3{font-size:2.4em;}\
       .we-display-heading-with-subtitle-4{font-size:2em;}\
       .we-display-heading-with-subtitle-5{font-size:1.7em;}\
       .we-display-heading-with-subtitle-6{font-size:1.4em;}\
       .we-display-heading-with-subtitle-align-left{text-align:left;}\
       .we-display-heading-with-subtitle-align-center{text-align:center;}\
       .we-display-heading-with-subtitle-align-right{text-align:right;}\
       .we-display-heading-with-subtitle-space-compact{margin:var(--we-heading-space-compact,0);}\
       .we-display-heading-with-subtitle-space-normal{margin:var(--we-heading-space-normal,0 0 var(--we-space-xs,2px) 0);}\
       .we-display-heading-with-subtitle-space-loose{margin:var(--we-heading-space-loose,0 0 var(--we-space-sm,4px) 0);}\
       .we-heading-subtitle{font-weight:400;font-size:0.7em;color:var(--we-heading-subtitle-fg,var(--we-fg-muted,#777));}\
       .we-lead{margin:0;font-size:1.15em;line-height:1.45;color:var(--we-lead-fg,var(--we-fg-muted,#555));}\
       .we-blockquote{margin:0 0 var(--we-space-md,8px) 0;}\
       .we-blockquote-align-center{text-align:center;}\
       .we-blockquote-align-right{text-align:right;}\
       .we-blockquote-align-center :is(.we-blockquote-quote,.we-blockquote-text,.we-blockquote-attrib){text-align:center;}\
       .we-blockquote-align-right :is(.we-blockquote-quote,.we-blockquote-text,.we-blockquote-attrib){text-align:right;}\
       .we-blockquote-quote{margin:0 0 var(--we-space-sm,4px) 0;font-size:1.25em;line-height:1.35;}\
       .we-blockquote-text{margin:0;}\
       .we-blockquote-attrib{margin:0;font-size:0.875em;color:var(--we-fg-muted,#777);}\
       .we-blockquote-attrib::before{content:\"— \";}\
       .we-choice:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.14)),var(--we-focus-tint,rgba(10,102,194,.14)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-progress-info{}\
       .we-progress-success{accent-color:var(--we-progress-success,#3a9147);}\
       .we-progress-warning{accent-color:var(--we-progress-warning,#b57c1c);}\
       .we-progress-danger{accent-color:var(--we-progress-danger,#b24545);}\
       .we-dropdown{display:inline-block;align-self:flex-start;}\
       .we-card{display:flex;flex-direction:column;align-self:stretch;border:1px solid var(--we-border-soft,#bbb);border-radius:8px;background:var(--we-bg,#fff);overflow:hidden;}\
       .we-card-header{padding:var(--we-space-sm,4px) var(--we-space-md,8px);border-bottom:1px solid var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);font-weight:600;}\
       .we-card-body{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);padding:var(--we-space-md,8px);}\
       .we-card-footer{padding:var(--we-space-sm,4px) var(--we-space-md,8px);border-top:1px solid var(--we-border-soft,#bbb);background:var(--we-bg-subtle,#f3f3f3);}\
       .we-navigation-bar{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-sm,4px);align-self:stretch;padding:var(--we-space-sm,4px) var(--we-space-md,8px);border:1px solid var(--we-border-menu,#aaa);border-radius:6px;background:var(--we-bg-subtle,#f3f3f3);}\
       .we-navigation-bar.is-vertical{flex-direction:column;align-items:stretch;}\
       .we-navigation-bar-toggle{display:inline-flex;align-self:flex-start;}\
       .we-navigation-bar-items{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-sm,4px);flex:1 1 auto;}\
       .we-navigation-bar.is-collapsed .we-navigation-bar-items{display:none;}\
       .we-navigation-bar.is-expanded .we-navigation-bar-items{display:flex;}\
       .we-offcanvas{position:fixed;inset:0;display:none;z-index:2100;}\
       .we-offcanvas.is-open{display:block;}\
       .we-offcanvas-backdrop{position:absolute;inset:0;background:var(--we-overlay,rgba(0,0,0,0.45));}\
       .we-offcanvas-panel{position:absolute;top:0;bottom:0;width:min(380px,85vw);display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);padding:var(--we-space-md,8px);background:var(--we-bg,#fff);border:1px solid var(--we-border,#888);box-shadow:0 8px 22px var(--we-shadow,rgba(0,0,0,.28));overflow:auto;}\
       .we-offcanvas-panel.is-end{right:0;border-radius:8px 0 0 8px;}\
       .we-offcanvas-panel.is-start{left:0;border-radius:0 8px 8px 0;}\
       .we-carousel{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);align-self:stretch;border:1px solid var(--we-border-soft,#bbb);border-radius:8px;background:var(--we-bg,#fff);padding:var(--we-space-sm,4px);}\
       .we-carousel-viewport{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);padding:var(--we-space-sm,4px);}\
       .we-carousel-controls{display:flex;align-items:center;justify-content:space-between;gap:var(--we-space-sm,4px);}\
       .we-carousel-indicators{display:flex;flex-wrap:wrap;gap:var(--we-space-xs,2px);}\
       .we-carousel-indicator{width:1.6em;height:1.6em;border:1px solid var(--we-border-soft,#bbb);border-radius:999px;background:var(--we-bg,#fff);}\
       .we-carousel-indicator.is-current{background:var(--we-bg-selected,#ececec);border-color:var(--we-border-strong,#333);}\
       .we-carousel-nav.is-disabled{opacity:.55;cursor:not-allowed;}\
       .we-scrollspy{display:flex;flex-direction:column;align-self:stretch;gap:var(--we-space-xs,2px);padding:var(--we-space-xs,2px);border:1px solid var(--we-border-soft,#bbb);border-radius:6px;background:var(--we-bg,#fff);}\
       .we-scrollspy-nav{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-xs,2px);}\
       .we-scrollspy-sections{display:flex;flex-direction:column;gap:var(--we-space-sm,4px);max-height:240px;overflow:auto;padding:var(--we-space-xs,2px);border-top:1px solid var(--we-border-soft,#bbb);}\
       .we-scrollspy-section{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);padding:var(--we-space-sm,4px);border:1px solid var(--we-border-soft,#bbb);border-radius:4px;background:var(--we-bg,#fff);}\
       .we-scrollspy-item{padding:2px 8px;border:1px solid transparent;border-radius:4px;background:transparent;color:var(--we-fg,#111);}\
       .we-scrollspy-item:hover{background:var(--we-bg-hover,#e8e8e8);}\
       .we-scrollspy-item.is-current{background:var(--we-bg-selected,#ececec);border-color:var(--we-border-soft,#bbb);text-decoration:underline;text-underline-offset:3px;text-decoration-thickness:2px;}\
       .we-table{border-collapse:separate;border:1px solid var(--we-border-muted,#999);margin-bottom:6px;align-self:flex-start;}\
       .we-table.we-density-normal{border-spacing:2px 0;}\
       .we-table.we-density-compact{border-spacing:0 0;}\
       .we-table-header-cell.we-density-normal{padding:2px 8px;text-align:left;border-bottom:1px solid var(--we-border-soft,#bbb);}\
       .we-table-header-cell.we-density-compact{padding:1px 4px;text-align:left;border-bottom:1px solid var(--we-border-soft,#bbb);}\
       .we-table-data-cell.we-density-normal{padding:2px 8px;}\
       .we-table-data-cell.we-density-compact{padding:1px 4px;}\
       .we-table-header-cell.we-align-left,.we-table-data-cell.we-align-left{text-align:left;}\
       .we-table-header-cell.we-align-center,.we-table-data-cell.we-align-center{text-align:center;}\
       .we-table-header-cell.we-align-right,.we-table-data-cell.we-align-right{text-align:right;}")
    (define legacy-visual-component-extra-style-text ; Additional CSS for newer layout/content primitives.
      ".we-link{display:inline-flex;align-items:center;align-self:flex-start;gap:var(--we-space-xs,2px);color:var(--we-fg,#111);text-decoration:underline;text-underline-offset:3px;text-decoration-thickness:1px;}\
       .we-link:hover{color:var(--we-fg,#111);background:var(--we-bg-hover,#e8e8e8);}\
       .we-link:focus-visible{background-image:linear-gradient(var(--we-focus-tint,rgba(10,102,194,.20)),var(--we-focus-tint,rgba(10,102,194,.20)));outline:1px solid var(--we-focus,#0a66c2);outline-offset:0;}\
       .we-toolbar{display:flex;flex-wrap:wrap;align-items:center;align-self:stretch;gap:var(--we-space-sm,4px);}\
       .we-toolbar-group{display:inline-flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:var(--we-space-xs,2px);}\
       .we-divider{align-self:stretch;border:0;border-top:1px solid var(--we-border-soft,#bbb);margin:0;}\
       .we-divider-vertical{width:1px;min-height:1.5em;height:100%;align-self:stretch;border-top:0;border-left:1px solid var(--we-border-soft,#bbb);}\
       .we-button-label,.we-menu-item-label{display:inline-block;}\
       .we-button-icon,.we-menu-item-icon{display:inline-flex;align-items:center;justify-content:center;min-width:1ch;}\
       .we-button-icon-leading,.we-menu-item-icon-leading{margin-right:var(--we-space-xs,2px);}\
       .we-button-icon-trailing,.we-menu-item-icon-trailing{margin-left:var(--we-space-xs,2px);}\
       .we-card-compact .we-card-header{padding:2px 6px;}\
       .we-card-compact .we-card-body{padding:4px 6px;gap:2px;}\
       .we-card-compact .we-card-footer{padding:2px 6px;}\
       .we-card-flat{border:none;border-radius:0;background:transparent;}\
       .we-card-flat .we-card-header,.we-card-flat .we-card-footer{background:transparent;}")
    ;; Structural base only:
    ;; - includes layout, positioning, and open/closed mechanics required for behavior.
    ;; - excludes visual theme concerns (colors, borders, padding, shadows, typography).
    (define structural-base-style-text ; Minimal structural CSS injected once per window root.
      ".we-vpanel,.we-group,.we-if-view,.we-cond-view,.we-case-view,.we-observable-view,.we-list-view{display:flex;flex-direction:column;gap:var(--we-gap,0.5rem);}\
       .we-stack{display:flex;flex-direction:column;gap:var(--we-stack-gap,var(--we-gap,0.5rem));}\
       .we-hpanel{display:flex;flex-direction:row;align-items:center;gap:var(--we-gap,0.5rem);}\
       .we-container{width:min(1200px,calc(100vw - 28px));max-width:1200px;margin:0 auto;}\
       .we-grid{display:grid;grid-template-columns:var(--we-grid-columns,repeat(auto-fit,minmax(320px,1fr)));gap:var(--we-grid-gap,12px);align-items:stretch;}\
       .we-inline{display:flex;flex-direction:row;align-items:center;gap:var(--we-gap,0.5rem);flex-wrap:wrap;}\
       .we-spacer{display:block;flex:1 1 auto;min-width:0;min-height:0;}\
       .we-toolbar{display:flex;flex-wrap:wrap;align-items:center;align-self:stretch;gap:var(--we-space-sm,4px);}\
       .we-toolbar-group{display:inline-flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:var(--we-space-xs,2px);}\
       .we-top-bar{display:flex;flex-wrap:wrap;align-items:center;align-self:stretch;gap:var(--we-space-sm,4px);}\
       .we-button-toolbar{display:flex;flex-wrap:wrap;align-items:center;align-self:flex-start;gap:var(--we-space-sm,4px);}\
       .we-button-group{display:inline-flex;flex-wrap:wrap;align-items:center;align-self:flex-start;}\
       .we-button-group>.we-button{margin:0;}\
       .we-tab-panel{display:flex;flex-direction:column;align-items:stretch;}\
       .we-menu{position:relative;display:inline-block;}\
       .we-menu-label{-webkit-appearance:none;appearance:none;font:inherit;}\
       .we-menu-bar{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-xs,2px);}\
       .we-menu-popup{position:absolute;top:calc(100% + var(--we-space-xs,2px));left:0;min-width:150px;display:none;flex-direction:column;z-index:1000;}\
       .we-dropdown.we-dropdown-up .we-menu-popup{top:auto;bottom:calc(100% + var(--we-space-xs,2px));left:0;right:auto;}\
       .we-dropdown.we-dropdown-start .we-menu-popup{top:0;left:auto;right:calc(100% + var(--we-space-xs,2px));bottom:auto;}\
       .we-dropdown.we-dropdown-end .we-menu-popup{top:0;left:calc(100% + var(--we-space-xs,2px));right:auto;bottom:auto;}\
       .we-menu-popup.is-open{display:flex;}\
       .we-top-bar{display:flex;flex-wrap:wrap;align-items:center;align-self:stretch;gap:var(--we-space-sm,4px);}\
       .we-navigation-bar{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-sm,4px);align-self:stretch;}\
       .we-navigation-bar.is-vertical{flex-direction:column;align-items:stretch;}\
       .we-navigation-bar-toggle{display:inline-flex;align-self:flex-start;}\
       .we-navigation-bar-items{display:flex;flex-wrap:wrap;align-items:center;gap:var(--we-space-sm,4px);flex:1 1 auto;}\
       .we-navigation-bar.is-collapsed .we-navigation-bar-items{display:none;}\
       .we-navigation-bar.is-expanded .we-navigation-bar-items{display:flex;}\
       .we-table-header-cell.we-align-left,.we-table-data-cell.we-align-left{text-align:left;}\
       .we-table-header-cell.we-align-center,.we-table-data-cell.we-align-center{text-align:center;}\
       .we-table-header-cell.we-align-right,.we-table-data-cell.we-align-right{text-align:right;}\
       .we-dialog,.we-modal{position:fixed;inset:0;display:none;align-items:center;justify-content:center;z-index:2000;}\
       .we-dialog.is-open,.we-modal.is-open{display:flex;}\
       .we-popover{display:inline-flex;align-self:flex-start;position:relative;z-index:1201;}\
       .we-popover-trigger{align-self:flex-start;position:relative;z-index:1201;}\
       .we-popover-backdrop{position:fixed;inset:0;display:none;z-index:1190;}\
       .we-popover-backdrop.is-open{display:block;}\
       .we-popover-panel{position:absolute;left:0;top:calc(100% + var(--we-space-xs,2px));display:none;z-index:1200;}\
       .we-popover.we-popover-left .we-popover-panel{left:auto;right:calc(100% + var(--we-space-xs,2px));top:50%;transform:translateY(-50%);}\
       .we-popover.we-popover-right .we-popover-panel{left:calc(100% + var(--we-space-xs,2px));right:auto;top:50%;transform:translateY(-50%);}\
       .we-popover.we-popover-top .we-popover-panel{left:50%;right:auto;top:auto;bottom:calc(100% + var(--we-space-xs,2px));transform:translateX(-50%);}\
       .we-popover.we-popover-bottom .we-popover-panel{left:50%;right:auto;top:calc(100% + var(--we-space-xs,2px));bottom:auto;transform:translateX(-50%);}\
       .we-popover-panel.is-open{display:flex;}\
       .we-tooltip{display:inline-flex;align-self:flex-start;position:relative;}\
       .we-tooltip-bubble{position:absolute;left:50%;bottom:calc(100% + var(--we-space-xs,2px));display:none;z-index:1200;}\
       .we-tooltip.we-tooltip-left .we-tooltip-bubble{left:auto;right:calc(100% + var(--we-space-xs,2px));top:50%;bottom:auto;transform:translateY(-50%);}\
       .we-tooltip.we-tooltip-right .we-tooltip-bubble{left:calc(100% + var(--we-space-xs,2px));right:auto;top:50%;bottom:auto;transform:translateY(-50%);}\
       .we-tooltip.we-tooltip-top .we-tooltip-bubble{left:50%;right:auto;top:auto;bottom:calc(100% + var(--we-space-xs,2px));transform:translateX(-50%);}\
       .we-tooltip.we-tooltip-bottom .we-tooltip-bubble{left:50%;right:auto;top:calc(100% + var(--we-space-xs,2px));bottom:auto;transform:translateX(-50%);}\
       .we-tooltip:hover .we-tooltip-bubble,.we-tooltip:focus-within .we-tooltip-bubble,.we-tooltip.is-open .we-tooltip-bubble{display:block;}\
       .we-collapse{display:none;align-self:stretch;}\
       .we-collapse.is-open{display:block;}\
       .we-offcanvas{position:fixed;inset:0;display:none;z-index:2100;}\
       .we-offcanvas.is-open{display:block;}\
       .we-offcanvas-backdrop{position:absolute;inset:0;}\
       .we-offcanvas-panel{position:absolute;top:0;bottom:0;overflow:auto;}\
       .we-offcanvas-panel.is-end{right:0;}\
       .we-offcanvas-panel.is-start{left:0;}")
    (define shared-style-text ; Shared stylesheet injected once per window root.
      structural-base-style-text)
    (define backend-set-dom-node-attrs! set-dom-node-attrs!) ; Backend attr setter (unwrapped).

    ;; renderer? : any/c -> boolean?
    ;;   Check whether v is a renderer state value.
    (define (renderer? v)
      (renderer-state? v))

    ;; renderer-root : renderer? -> dom-node?
    ;;   Return the root node managed by renderer r.
    (define (renderer-root r)
      (renderer-state-root r))

    ;; renderer-destroy : renderer? -> void?
    ;;   Run all cleanup hooks and mark renderer as destroyed.
    (define (renderer-destroy r)
      (unless (renderer-state-destroyed? r)
        (for-each (lambda (cleanup) (cleanup))
                  (renderer-state-cleanups r))
        (set-renderer-state-cleanups! r '())
        (set-renderer-state-destroyed?! r #t))
      (void))

    ;; dom-node-click! : dom-node? -> void?
    ;;   Invoke the node click callback when present.
    (define (dom-node-click! n)
      (define on-click (dom-node-on-click n))
      (when on-click
        (on-click)))

    ;; dom-node-change! : dom-node? any/c -> void?
    ;;   Update node text and invoke the change callback when present.
    (define (dom-node-change! n value)
      (define text-value (value->text value))
      (set-dom-node-text! n text-value)
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-toggle! : dom-node? boolean? -> void?
    ;;   Update checkbox checked attribute and invoke the change callback.
    (define (dom-node-toggle! n checked?)
      (set-dom-node-attrs!
       n
       (list (cons 'checked (not (not checked?)))))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change (not (not checked?)))))

    ;; dom-node-select! : dom-node? any/c -> void?
    ;;   Update selected attribute and invoke the change callback.
    (define (dom-node-select! n selected)
      (define attrs (dom-node-attrs n))
      (define option-pairs-pair (assq 'option-pairs attrs))
      (define option-pairs-value (if option-pairs-pair (cdr option-pairs-pair) '()))
      (set-dom-node-attrs!
       n
       (list (cons 'choices (cdr (assq 'choices attrs)))
             (cons 'option-pairs option-pairs-value)
             (cons 'selected selected)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-slide! : dom-node? number? -> void?
    ;;   Update slider value attribute and invoke the change callback.
    (define (dom-node-slide! n value)
      (define attrs     (dom-node-attrs n))
      (define min-pair  (assq 'min attrs))
      (define max-pair  (assq 'max attrs))
      (define min-value (if min-pair (cdr min-pair) 0))
      (define max-value (if max-pair (cdr max-pair) 100))
      (set-dom-node-attrs!
       n
       (list (cons 'min min-value)
             (cons 'max max-value)
             (cons 'value value)))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-radio-select! : dom-node? any/c -> void?
    ;;   Update radio selected attribute and invoke the change callback.
    (define (dom-node-radio-select! n selected)
      (define attrs         (dom-node-attrs n))
      (define choices-pair  (assq 'choices attrs))
      (define choices-value (if choices-pair (cdr choices-pair) '()))
      (define widget-pair   (assq 'data-we-widget attrs))
      (define class-pair    (assq 'class attrs))
      (set-dom-node-attrs!
       n
       (append (if widget-pair (list widget-pair) '())
               (if class-pair (list class-pair) '())
               (list (cons 'choices choices-value)
                     (cons 'selected selected))))
      (define on-change (dom-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-keydown! : dom-node? string? -> void?
    ;;   Dispatch keydown payload for tabs, input Enter actions, and menu-item key activation.
    (define (dom-node-keydown! n key)
      (define on-click      (dom-node-on-click n))
      (define on-change     (dom-node-on-change n))
      (define on-enter-pair (assq 'on-enter-action (dom-node-attrs n)))
      (define role-pair     (assq 'role (dom-node-attrs n)))
      (define widget-pair   (assq 'data-we-widget (dom-node-attrs n)))
      (define tag           (dom-node-tag n))
      (when (and on-enter-pair
                 (procedure? (cdr on-enter-pair))
                 (string=? key "Enter"))
        ((cdr on-enter-pair)))
      (when (and on-click
                 (or (eq? tag 'button)
                     (and role-pair
                          (or (eq? (cdr role-pair) 'button)
                              (eq? (cdr role-pair) 'menuitem))))
                 (or (string=? key "Enter")
                     (string=? key " ")))
        (on-click))
      (when (and on-change
                 (or (and role-pair
                          (or (eq? (cdr role-pair) 'tab)
                              (eq? (cdr role-pair) 'button)
                              (eq? (cdr role-pair) 'menuitem)
                              (eq? (cdr role-pair) 'dialog)))
                     (and widget-pair
                          (eq? (cdr widget-pair) "carousel")
                          (or (string=? key "ArrowLeft")
                              (string=? key "ArrowRight")
                              (string=? key "Home")
                              (string=? key "End")))))
        (on-change key)))

    ;; alist-ref : (listof pair?) symbol? symbol? -> any/c
    ;;   Look up key in props, raising an argument error when missing.
    (define (alist-ref props key who)
      (define p (assq key props))
      (if p
          (cdr p)
          (raise-arguments-error who
                                 "missing property"
                                 "key"
                                 key
                                 "props"
                                 props)))

    ;; ensure-list : any/c symbol? symbol? -> list?
    ;;   Validate that v is a list for argument-name in who.
    (define (ensure-list v who argument-name)
      (unless (list? v)
        (raise-arguments-error who
                               "expected list?"
                               argument-name
                               v))
      v)

    ;; value->text : any/c -> string?
    ;;   Convert supported primitive values to display text.
    (define (value->text v)
      (cond
        [(string? v) v]
        [(number? v) (number->string v)]
        [(symbol? v) (symbol->string v)]
        [else        text/fallback]))

    ;; attr-remove-key : list? symbol? -> list?
    ;;   Return attrs without entries for key.
    (define (attr-remove-key attrs key)
      (cond
        [(null? attrs) '()]
        [(eq? (caar attrs) key)
         (attr-remove-key (cdr attrs) key)]
        [else
         (cons (car attrs)
               (attr-remove-key (cdr attrs) key))]))

    ;; attr-set : list? symbol? any/c -> list?
    ;;   Set key to value in attrs, replacing any prior key entry.
    (define (attr-set attrs key value)
      (cons (cons key value)
            (attr-remove-key attrs key)))

    ;; class-value->list : any/c -> list?
    ;;   Convert class value to list of class-name strings.
    (define (class-string->list value)
      (define n (string-length value))
      (let loop ([i 0]
                 [start #f]
                 [acc '()])
        (cond
          [(= i n)
           (if start
               (reverse (cons (substring value start i) acc))
               (reverse acc))]
          [else
           (define ch (string-ref value i))
           (if (or (char=? ch #\space)
                   (char=? ch #\tab)
                   (char=? ch #\newline)
                   (char=? ch #\return))
               (if start
                   (loop (add1 i) #f (cons (substring value start i) acc))
                   (loop (add1 i) #f acc))
               (if start
                   (loop (add1 i) start acc)
                   (loop (add1 i) i acc)))])))

    (define (class-value->list value)
      (cond
        [(string? value)
         (class-string->list value)]
        [(symbol? value)
         (list (symbol->string value))]
        [(list? value)
         (let loop ([remaining value])
           (cond
             [(null? remaining) '()]
             [(string? (car remaining))
              (cons (car remaining)
                    (loop (cdr remaining)))]
             [(symbol? (car remaining))
              (cons (symbol->string (car remaining))
                    (loop (cdr remaining)))]
             [else
              (loop (cdr remaining))]))]
        [else
         '()]))

    ;; props-extra-classes : list? -> list?
    ;;   Return normalized extra classes from props.
    (define (props-extra-classes props)
      (class-value->list (let ([p (assq 'extra-class props)])
                           (if p (cdr p) '()))))

    ;; props-extra-attrs : list? -> list?
    ;;   Return normalized extra attrs from props.
    (define (props-extra-attrs props)
      (define p (assq 'extra-attrs props))
      (if (and p (list? (cdr p)))
          (let loop ([remaining (cdr p)])
            (cond
              [(null? remaining) '()]
              [(and (pair? (car remaining))
                    (symbol? (caar remaining)))
               (cons (car remaining)
                     (loop (cdr remaining)))]
              [else
               (loop (cdr remaining))]))
          '()))

    ;; merge-class-values : list? list? -> string?
    ;;   Build final class string by concatenating base and extra class tokens.
    (define (merge-class-values base-classes extra-classes)
      (define (member-string? xs needle)
        (cond
          [(null? xs) #f]
          [(string=? (car xs) needle) #t]
          [else (member-string? (cdr xs) needle)]))
      (define (dedupe-classes classes)
        (let loop ([remaining classes]
                   [seen '()]
                   [acc '()])
          (cond
            [(null? remaining) (reverse acc)]
            [(member-string? seen (car remaining))
             (loop (cdr remaining) seen acc)]
            [else
             (loop (cdr remaining)
                   (cons (car remaining) seen)
                   (cons (car remaining) acc))])))
      (define all-classes (dedupe-classes (append base-classes extra-classes)))
      (if (null? all-classes)
          ""
          (let loop ([remaining all-classes]
                     [acc ""])
            (cond
              [(null? remaining) acc]
              [(string=? acc "")
               (loop (cdr remaining) (car remaining))]
              [else
               (loop (cdr remaining)
                     (string-append acc " " (car remaining)))]))))

    ;; merge-root-extra-attrs : view? list? -> list?
    ;;   Merge with-attrs/with-class props into base root attrs.
    (define (internal-action-attr-key? attr-key)
      (or (eq? attr-key 'on-click-action)
          (eq? attr-key 'on-change-action)))

    (define (procedure-allowed-attr-key? attr-key)
      (or (internal-action-attr-key? attr-key)
          (eq? attr-key 'on-enter-action)))

    (define (merge-root-extra-attrs v attrs)
      (define props (view-props v))
      (define (valid-attr-value? attr-key attr-value)
        (if (and (procedure? attr-value)
                 (not (procedure-allowed-attr-key? attr-key)))
            (begin
              (emit-web-easy-warning!
              (string-append "web-easy: ignored procedure-valued attribute "
                             (symbol->string attr-key)))
              #f)
            #t))
      (define base-classes
        (let loop ([remaining attrs])
          (cond
            [(null? remaining) '()]
            [(eq? (caar remaining) 'class)
             (define class-value (maybe-observable-value (cdar remaining)))
             (append (if (valid-attr-value? 'class class-value)
                         (class-value->list class-value)
                         '())
                     (loop (cdr remaining)))]
            [else
             (loop (cdr remaining))])))
      (define extra-attrs/raw (props-extra-attrs props))
      (define extra-class-from-attrs
        (let loop ([remaining extra-attrs/raw])
          (cond
            [(null? remaining) '()]
            [(eq? (caar remaining) 'class)
             (define class-value (maybe-observable-value (cdar remaining)))
             (append (if (valid-attr-value? 'class class-value)
                         (class-value->list class-value)
                         '())
                     (loop (cdr remaining)))]
            [else
             (loop (cdr remaining))])))
      (define extra-classes
        (append extra-class-from-attrs
                (props-extra-classes props)))
      (define attrs/without-class (attr-remove-key attrs 'class))
      (define attrs/merged
        (let loop ([remaining extra-attrs/raw]
                   [acc attrs/without-class])
          (cond
            [(null? remaining) acc]
            [(eq? (caar remaining) 'class)
             (loop (cdr remaining) acc)]
            [(eq? (caar remaining) 'data-we-widget)
             (if (assq 'data-we-widget attrs)
                 (loop (cdr remaining) acc)
                 (let ([widget-value (maybe-observable-value (cdar remaining))])
                   (loop (cdr remaining)
                         (if (valid-attr-value? 'data-we-widget widget-value)
                             (attr-set acc
                                       'data-we-widget
                                       widget-value)
                             acc))))]
            [(internal-action-attr-key? (caar remaining))
             (loop (cdr remaining) acc)]
            [else
             (define attr-value (maybe-observable-value (cdar remaining)))
             (loop (cdr remaining)
                   (if (valid-attr-value? (caar remaining) attr-value)
                       (attr-set acc
                                 (caar remaining)
                                 attr-value)
                       acc))])))
      (define final-class (merge-class-values base-classes extra-classes))
      (if (string=? final-class "")
          attrs/merged
          (attr-set attrs/merged 'class final-class)))

    ;; next-tab-panel-id : -> string?
    ;;   Allocate a unique id string for tab-panel content region.
    (define (next-tab-panel-id)
      (set! tab-panel-counter (add1 tab-panel-counter))
      (string-append "tab-panel-" (number->string tab-panel-counter)))

    ;; next-accordion-panel-id : -> string?
    ;;   Allocate a unique id string for accordion section content region.
    (define (next-accordion-panel-id)
      (set! accordion-panel-counter (add1 accordion-panel-counter))
      (string-append "accordion-panel-" (number->string accordion-panel-counter)))

    ;; next-tooltip-id : -> string?
    ;;   Allocate a unique id string for tooltip bubble region.
    (define (next-tooltip-id)
      (set! tooltip-counter (add1 tooltip-counter))
      (string-append "tooltip-" (number->string tooltip-counter)))

    ;; next-popover-panel-id : -> string?
    ;;   Allocate a unique id string for popover panel region.
    (define (next-popover-panel-id)
      (set! popover-panel-counter (add1 popover-panel-counter))
      (string-append "popover-panel-" (number->string popover-panel-counter)))

    ;; normalize-overlay-placement : any/c symbol? -> symbol?
    ;;   Normalize tooltip/popover placement symbols, falling back to default-placement.
    (define (normalize-overlay-placement raw-placement default-placement)
      (define p (if (obs? raw-placement) (obs-peek raw-placement) raw-placement))
      (if (memq p '(top right bottom left))
          p
          default-placement))

    ;; normalize-dialog-size : any/c -> symbol?
    ;;   Normalize dialog/modal size to one of sm/md/lg/xl.
    (define (normalize-dialog-size raw-size)
      (define s (if (obs? raw-size) (obs-peek raw-size) raw-size))
      (if (memq s '(sm md lg xl))
          s
          'md))

    ;; normalize-tab-variants : any/c -> list?
    ;;   Normalize tab variants to a list of symbols.
    (define (normalize-tab-variants raw-variants)
      (define v (if (obs? raw-variants) (obs-peek raw-variants) raw-variants))
      (cond
        [(symbol? v) (list v)]
        [(list? v)
         (let loop ([xs v] [acc '()])
           (cond
             [(null? xs) (reverse acc)]
             [else
              (define x (car xs))
              (loop (cdr xs)
                    (if (symbol? x) (cons x acc) acc))]))]
        [else '(default)]))

    ;; next-dialog-body-id : -> string?
    ;;   Allocate a unique id string for dialog descriptive text.
    (define (next-dialog-body-id)
      (set! dialog-body-counter (add1 dialog-body-counter))
      (string-append "dialog-body-" (number->string dialog-body-counter)))

    ;; normalize-tab-entry : any/c -> list?
    ;;   Normalize tab entry to (list id view disabled?) supporting pair or list forms.
    (define (normalize-tab-entry tab)
      (cond
        [(list? tab)
         (define n (length tab))
         (cond
           [(= n 2)
            (list (list-ref tab 0) (list-ref tab 1) #f)]
           [(= n 3)
            (list (list-ref tab 0) (list-ref tab 1) (not (not (list-ref tab 2))))]
           [else
            (raise-arguments-error 'tab-panel
                                   "expected tab entry of arity 2 or 3"
                                   "tab"
                                   tab)])]
        [(pair? tab)
         (list (car tab) (cdr tab) #f)]
        [else
         (raise-arguments-error 'tab-panel
                                "expected pair? or list?"
                                "tab"
                                tab)]))

    ;; render-list-items : dom-node? list? list? procedure? procedure? procedure? -> list?
    ;;   Render entries into parent using keyed node reuse and return new item state.
    (define (render-list-items parent entries old-items key-proc make-view-proc register-cleanup!)
      (define new-items
        (map (lambda (entry)
               (define key      (key-proc entry))
               (define old-item (assoc key old-items))
               (cond
                 [(and old-item (equal? (cadr old-item) entry))
                  (list key entry (caddr old-item))]
                 [else
                  (define child-view (make-view-proc key entry))
                  (define child-node (build-node child-view register-cleanup!))
                  (list key entry child-node)]))
             entries))
      (backend-replace-children! parent (map caddr new-items))
      new-items)

    ;; TODO: The new version of render-list-items is the one to use.
    ;;       The new example `dynamic-counters` use the new version.
    ;;       Before a switch we need to update the smoke tests.
    ;; TODO: future improvement: row-local cleanup when keyed rows are removed
    
    ;; render-list-items : dom-node? list? list? procedure? procedure? procedure? -> list?
    ;;   Render entries into parent using keyed node reuse.
    ;;   Each returned item state is (list key entry item-obs child-node).
    (define (new-render-list-items
             parent
             entries
             old-items
             key-proc
             make-view-proc
             register-cleanup!)
      (define new-items
        (for/list ([entry (in-list entries)])
          (define key (key-proc entry))
          (define old-item (assoc key old-items))
          (if old-item
              (let ([old-entry  (list-ref old-item 1)]
                    [item-obs   (list-ref old-item 2)]
                    [child-node (list-ref old-item 3)])
                (unless (equal? old-entry entry)
                  (obs-set! item-obs entry))
                (list key entry item-obs child-node))
              (let* ([item-obs   (obs entry)]
                     [child-view (make-view-proc key item-obs)]
                     [child-node (build-node child-view register-cleanup!)])
                (list key entry item-obs child-node)))))
      (backend-replace-children! parent
                                 (map (lambda (item) (list-ref item 3))
                                      new-items))
      new-items)
    

    ;; replace-with-single-child! : dom-node? view? procedure? -> void?
    ;;   Replace parent children with a single child rendered from child-view.
    (define (replace-with-single-child! parent child-view register-cleanup!)
      (backend-set-single-child! parent (build-node child-view register-cleanup!)))

    ;; cond-clause-active? : any/c -> boolean?
    ;;   Check whether a cond clause test value counts as true.
    (define (cond-clause-active? v)
      (not (eq? v #f)))

    ;; maybe-observable-value : any/c -> any/c
    ;;   Read observable content when v is observable, otherwise return v.
    (define (maybe-observable-value v)
      (if (obs? v) (obs-peek v) v))

    ;; base-order/url-attr-keys : list?
    ;;   Attribute keys treated as URL-bearing for conservative Base ordering checks.
    (define base-order/url-attr-keys
      '(href src srcset action formaction cite poster data usemap ping imagesrcset longdesc))

    ;; primitive-html-view-tag : any/c -> (or/c symbol? #f)
    ;;   Return primitive HTML tag symbol for html-element/html-element-children views.
    (define (primitive-html-view-tag child)
      (if (view? child)
          (let ([kind (view-kind child)])
            (if (or (eq? kind 'html-element)
                    (eq? kind 'html-element-children))
                (let ([tag-pair (assq 'tag (view-props child))])
                  (if tag-pair
                      (let ([tag-value (maybe-observable-value (cdr tag-pair))])
                        (if (symbol? tag-value) tag-value #f))
                      #f))
                #f))
          #f))

    ;; primitive-view-has-url-attrs? : any/c -> boolean?
    ;;   Check whether primitive HTML view carries URL-bearing root attributes.
    (define (primitive-view-has-url-attrs? child)
      (define (url-attr-value-present? attr-value)
        (cond
          [(eq? attr-value #f) #f]
          [(and (string? attr-value)
                (string=? attr-value ""))
           #f]
          [else #t]))
      (if (view? child)
          (let ([attrs (props-extra-attrs (view-props child))])
            (let loop ([rest attrs])
              (cond
                [(null? rest) #f]
                [else
                 (define attr-key (caar rest))
                 (define attr-value (maybe-observable-value (cdar rest)))
                 (if (and (memq attr-key base-order/url-attr-keys)
                          (url-attr-value-present? attr-value))
                     #t
                     (loop (cdr rest)))])))
          #f))

    ;; validate-window-base-order! : view? -> void?
    ;;   Enforce conservative Base ordering among direct window children.
    (define (validate-window-base-order! window-view)
      (define has-base?
        (let loop ([rest (view-children window-view)])
          (cond
            [(null? rest) #f]
            [(eq? (primitive-html-view-tag (car rest)) 'base) #t]
            [else (loop (cdr rest))])))
      (when has-base?
        (define seen-url-bearing-child? #f)
        (for-each
         (lambda (child)
           (define tag (primitive-html-view-tag child))
           (cond
             [(eq? tag 'base)
              (when seen-url-bearing-child?
                (error 'Base
                       "must appear before URL-bearing primitive elements in direct window children"))]
             [else
              (when (and tag
                         (primitive-view-has-url-attrs? child))
                (set! seen-url-bearing-child? #t))]))
         (view-children window-view))))

    ;; options-ref : any/c symbol? any/c -> any/c
    ;;   Read option key from options alist, returning default when missing.
    (define (options-ref options key default)
      (if (list? options)
          (let ([p (assq key options)])
            (if p (cdr p) default))
          default))

    ;; normalize-table-density : any/c -> symbol?
    ;;   Normalize density to 'normal or 'compact.
    (define (normalize-table-density density)
      (if (symbol? density)
          (case density
            [(normal compact) density]
            [else             table/density-normal])
          table/density-normal))

    ;; normalize-table-align : any/c -> symbol?
    ;;   Normalize alignment to left/center/right.
    (define (normalize-table-align align)
      (if (symbol? align)
          (case align
            [(left center right) align]
            [else                'left])
          'left))

    ;; normalize-table-column : any/c -> list?
    ;;   Normalize column spec to (list label align).
    (define (normalize-table-column column)
      (cond
        [(and (list? column) (= (length column) 2))
         (list (list-ref column 0)
               (normalize-table-align (list-ref column 1)))]
        [else
         (list column 'left)]))

    ;; positive-number-list? : any/c -> boolean?
    ;;   Check whether v is a non-empty list of positive numbers.
    (define (positive-number-list? v)
      (cond
        [(not (list? v)) #f]
        [(null? v)       #f]
        [else
         (let loop ([xs v])
           (cond
             [(null? xs) #t]
             [(and (number? (car xs)) (> (car xs) 0))
              (loop (cdr xs))]
             [else #f]))]))

    ;; grid-columns-template : any/c -> string?
    ;;   Normalize grid columns value to CSS template expression string.
    (define (grid-columns-template columns)
      (cond
        [(or (eq? columns 'auto) (eq? columns 'responsive))
         "repeat(auto-fit,minmax(320px,1fr))"]
        [(number? columns)
         (if (> columns 0)
             (string-append "repeat(" (number->string columns) ",minmax(0,1fr))")
             "repeat(auto-fit,minmax(320px,1fr))")]
        [(positive-number-list? columns)
         (let loop ([xs columns] [acc ""])
           (if (null? xs)
               acc
               (let ([part (string-append "minmax(0," (number->string (car xs)) "fr)")])
                 (loop (cdr xs)
                       (if (string=? acc "")
                           part
                           (string-append acc " " part))))))]
        [(string? columns) columns]
        [else
         "repeat(auto-fit,minmax(320px,1fr))"]))

    ;; grid-gap-template : any/c -> string?
    ;;   Normalize grid gap value to CSS length expression string.
    (define (grid-gap-template gap)
      (cond
        [(number? gap)
         (if (>= gap 0)
             (string-append (number->string gap) "px")
             "12px")]
        [(string? gap) gap]
        [else "12px"]))

    ;; normalize-alert-level : any/c -> symbol?
    ;;   Normalize alert level to supported semantic/tone variants.
    (define (normalize-alert-level level)
      (if (symbol? level)
          (case level
            [(warning)                                'warning]
            [(danger)                                 'danger]
            [(info success warning danger
                   primary secondary light dark)      level]
            [else                                     'info])
          'info))

    ;; alert-level-class : symbol? -> string?
    ;;   Return CSS class suffix for alert level.
    (define (alert-level-class level)
      (case level
        [(primary)   "we-alert-primary"]
        [(secondary) "we-alert-secondary"]
        [(success)   "we-alert-success"]
        [(warning)   "we-alert-warning"]
        [(danger)    "we-alert-danger"]
        [(light)     "we-alert-light"]
        [(dark)      "we-alert-dark"]
        [else        "we-alert-info"]))

    ;; alert-level-role : symbol? -> symbol?
    ;;   Return semantic role for alert level severity.
    (define (alert-level-role level)
      (case level
        [(warning danger) 'alert]
        [else         'status]))

    ;; invoke-close-callback : any/c symbol? -> void?
    ;;   Call close callback with reason when arity allows, else call without args.
    (define (invoke-close-callback on-close reason)
      (when (procedure? on-close)
        (if (procedure-arity-includes? on-close 1)
            (on-close reason)
            (on-close))))

    ;; normalize-alert-layout : any/c -> symbol?
    ;;   Normalize rich alert layout to stack or inline.
    (define (normalize-alert-layout value)
      (if (symbol? value)
          (case value
            [(inline) 'inline]
            [else     'stack])
          'stack))

    ;; normalize-alert-scale : any/c -> symbol?
    ;;   Normalize rich alert title scale to normal or major.
    (define (normalize-alert-scale value)
      (if (symbol? value)
          (case value
            [(major) 'major]
            [else    'normal])
          'normal))

    ;; toast-level-class : symbol? -> string?
    ;;   Return CSS class suffix for toast level.
    (define (toast-level-class level)
      (case level
        [(success) "we-toast-success"]
        [(warning) "we-toast-warning"]
        [(danger)  "we-toast-danger"]
        [else      "we-toast-info"]))

    ;; normalize-badge-level : any/c -> symbol?
    ;;   Normalize badge level to supported variants.
    (define (normalize-badge-level level)
      (if (symbol? level)
          (case level
            [(primary secondary success info warning danger light dark) level]
            [else                                                      'info])
          'info))

    ;; badge-level-class : symbol? -> string?
    ;;   Return CSS class suffix for badge level.
    (define (badge-level-class level)
      (case level
        [(primary)   "we-badge-primary"]
        [(secondary) "we-badge-secondary"]
        [(success)   "we-badge-success"]
        [(warning)   "we-badge-warning"]
        [(danger)    "we-badge-danger"]
        [(light)     "we-badge-light"]
        [(dark)      "we-badge-dark"]
        [else        "we-badge-info"]))

    ;; contains-equal? : list? any/c -> boolean?
    ;;   Check whether xs contains v using equal?.
    (define (contains-equal? xs v)
      (cond
        [(null? xs) #f]
        [else
         (if (equal? (car xs) v)
             #t
             (contains-equal? (cdr xs) v))]))


    ;; choice-entry-id : any/c -> any/c
    ;;   Extract selectable entry id from scalar, pair, or 2-element list.
    (define (choice-entry-id entry)
      (cond
        [(and (list? entry) (pair? entry) (pair? (cdr entry)))
         (car entry)]
        [(pair? entry)
         (car entry)]
        [else
         entry]))

    ;; choice-entry-label : any/c -> any/c
    ;;   Extract selectable entry label from scalar, pair, or 2-element list.
    (define (choice-entry-label entry)
      (cond
        [(and (list? entry) (pair? entry) (pair? (cdr entry)))
         (cadr entry)]
        [(pair? entry)
         (cdr entry)]
        [else
         entry]))

    ;; normalize-placeholder-shape : any/c -> symbol?
    ;;   Normalize placeholder shape to text/rect/circle.
    (define (normalize-placeholder-shape shape)
      (if (symbol? shape)
          (case shape
            [(text rect circle) shape]
            [else               'text])
          'text))

    ;; normalize-offcanvas-side : any/c -> symbol?
    ;;   Normalize offcanvas side to start/end.
    (define (normalize-offcanvas-side side)
      (if (symbol? side)
          (case side
            [(start end) side]
            [else        'end])
          'end))

    ;; normalize-toast-duration : any/c -> number?
    ;;   Normalize toast auto-hide duration to non-negative integer milliseconds.
    (define (normalize-toast-duration duration-ms)
      (cond
        [(and (number? duration-ms)
              (integer? duration-ms)
              (>= duration-ms 0))
         duration-ms]
        [else
         0]))

    ;; normalize-heading-level : any/c -> number?
    ;;   Normalize heading level to integer in the closed interval 1..6.
    (define (normalize-heading-level level)
      (cond
        [(and (number? level)
              (integer? level)
              (>= level 1)
              (<= level 6))
         level]
        [else
         1]))

    ;; normalize-heading-align : any/c -> symbol?
    ;;   Normalize heading text alignment style to left/center/right.
    (define (normalize-heading-align align)
      (if (symbol? align)
          (case align
            [(left center right) align]
            [else                'left])
          'left))

    ;; normalize-heading-spacing : any/c -> symbol?
    ;;   Normalize heading spacing style to compact/normal/loose.
    (define (normalize-heading-spacing spacing)
      (if (symbol? spacing)
          (case spacing
            [(compact normal loose) spacing]
            [else                   'normal])
          'normal))

    ;; heading-align-class : string? symbol? -> string?
    ;;   Build alignment class token for heading class prefix.
    (define (heading-align-class prefix align)
      (string-append prefix "-align-" (symbol->string (normalize-heading-align align))))

    ;; heading-spacing-class : string? symbol? -> string?
    ;;   Build spacing class token for heading class prefix.
    (define (heading-spacing-class prefix spacing)
      (string-append prefix "-space-" (symbol->string (normalize-heading-spacing spacing))))

    ;; density-class : symbol? -> string?
    ;;   Return CSS class for table density variants.
    (define (density-class density)
      (case density
        [(compact) "we-density-compact"]
        [else      "we-density-normal"]))

    ;; normalize-table-variants : any/c -> list?
    ;;   Normalize variant value to accepted table variant symbols.
    (define (normalize-table-variants raw)
      (define (allowed-variant? v)
        (and (symbol? v)
             (case v
               [(striped hover borderless sm) #t]
               [else #f])))
      (define (loop xs)
        (cond
          [(null? xs) '()]
          [(allowed-variant? (car xs))
           (cons (car xs) (loop (cdr xs)))]
          [else
           (loop (cdr xs))]))
      (cond
        [(allowed-variant? raw)
         (list raw)]
        [(list? raw)
         (loop raw)]
        [else
         '()]))

    ;; table-variant-class : list? -> string?
    ;;   Build CSS class fragment for table variants.
    (define (table-variant-class variants)
      (define striped?    (contains-equal? variants 'striped))
      (define hover?      (contains-equal? variants 'hover))
      (define borderless? (contains-equal? variants 'borderless))
      (define small?      (contains-equal? variants 'sm))
      (string-append
       (if striped? " we-table-striped" "")
       (if hover? " we-table-hover" "")
       (if borderless? " we-table-borderless" "")
       (if small? " we-table-sm" "")))

    ;; normalize-table-row-variant : any/c -> symbol?
    ;;   Normalize table row variant to accepted symbols or #f.
    (define (normalize-table-row-variant raw)
      (if (symbol? raw)
          (case raw
            [(active primary secondary success danger warning info light dark) raw]
            [else #f])
          #f))

    ;; normalize-table-row-variants : any/c -> list?
    ;;   Normalize row-variants option to a list aligned with data rows.
    (define (normalize-table-row-variants raw)
      (cond
        [(list? raw)
         (map normalize-table-row-variant raw)]
        [else
         '()]))

    ;; normalize-table-row-header-column : any/c -> any/c
    ;;   Normalize row-header-column to non-negative integer index or #f.
    (define (normalize-table-row-header-column raw)
      (cond
        [(and (number? raw) (exact-integer? raw) (>= raw 0)) raw]
        [else #f]))

    ;; table-row-variant-class : any/c -> string?
    ;;   Return CSS class for a normalized table row variant.
    (define (table-row-variant-class variant)
      (if variant
          (string-append "we-table-row-" (symbol->string variant))
          ""))

    ;; normalize-card-variants : any/c -> list?
    ;;   Normalize variant value to accepted card variant symbols.
    (define (normalize-card-variants raw)
      (define (allowed-variant? v)
        (and (symbol? v)
             (case v
               [(default compact flat headerless) #t]
               [else #f])))
      (define (loop xs)
        (cond
          [(null? xs) '()]
          [(allowed-variant? (car xs))
           (cons (car xs) (loop (cdr xs)))]
          [else
           (loop (cdr xs))]))
      (cond
        [(allowed-variant? raw)
         (list raw)]
        [(list? raw)
         (loop raw)]
        [else
         (list 'default)]))

    ;; card-variant-class : list? -> string?
    ;;   Build card class string from variant symbols.
    (define (card-variant-class variants)
      (define compact? (contains-equal? variants 'compact))
      (define flat?    (contains-equal? variants 'flat))
      (string-append
       "we-card"
       (if compact? " we-card-compact" "")
       (if flat? " we-card-flat" "")))

    ;; normalize-card-tone : any/c -> any/c
    ;;   Normalize card tone option to accepted symbols or #f.
    (define (normalize-card-tone raw)
      (if (symbol? raw)
          (case raw
            [(primary secondary success danger warning info light dark) raw]
            [else #f])
          #f))

    ;; normalize-card-tone-style : any/c -> any/c
    ;;   Normalize card tone-style option to fill/outline or #f.
    (define (normalize-card-tone-style raw)
      (if (symbol? raw)
          (case raw
            [(fill outline) raw]
            [else #f])
          #f))

    ;; icon-node : string? string? -> dom-node?
    ;;   Construct icon span node with data-we-widget/class and text content.
    (define (icon-node widget class-name icon-text)
      (dom-node 'span
                (list (cons 'data-we-widget widget)
                      (cons 'class class-name)
                      (cons 'aria-hidden "true"))
                '()
                icon-text
                #f
                #f))

    ;; render-table-rows! : dom-node? list? list? symbol? any/c list? any/c -> void?
    ;;   Replace table rows with optional caption, a header row, and data rows.
    (define (render-table-rows! table-node columns rows density caption row-variants row-header-column)
      (define normalized-rows (ensure-list rows 'table "rows"))
      (define normalized-columns (map normalize-table-column columns))
      (define normalized-caption
        (let ([value (maybe-observable-value caption)])
          (if (eq? value #f)
              #f
              (value->text value))))
      (define (align-class align)
        (case align
          [(center) "we-align-center"]
          [(right)  "we-align-right"]
          [else     "we-align-left"]))
      (define caption-row
        (if normalized-caption
            (list (dom-node 'caption
                            (list (cons 'data-we-widget "table-caption")
                                  (cons 'class "we-table-caption"))
                            '()
                            normalized-caption
                            #f
                            #f))
            '()))
      (define (header-cell column-spec)
        (define density-css (density-class density))
        (define align (list-ref column-spec 1))
        (define align-css (align-class align))
        (dom-node 'th
                  (list (cons 'data-we-widget "table-header-cell")
                        (cons 'class (string-append "we-table-header-cell " density-css " " align-css)))
                  '()
                  (value->text (list-ref column-spec 0))
                  #f
                  #f))
      (define (data-cell index cell-value)
        (define density-css (density-class density))
        (define align
          (if (< index (length normalized-columns))
              (list-ref (list-ref normalized-columns index) 1)
              'left))
        (define align-css (align-class align))
        (if (and row-header-column (= index row-header-column))
            (dom-node 'th
                      (list (cons 'data-we-widget "table-row-header-cell")
                            (cons 'scope "row")
                            (cons 'class (string-append "we-table-data-cell " density-css " " align-css)))
                      '()
                      (value->text cell-value)
                      #f
                      #f)
            (dom-node 'td
                      (list (cons 'data-we-widget "table-data-cell")
                            (cons 'class (string-append "we-table-data-cell " density-css " " align-css)))
                      '()
                      (value->text cell-value)
                      #f
                      #f)))
      (define (row-values row)
        (if (list? row)
            row
            (list row)))
      (define (build-row cell-values build-cell)
        (define row-node (dom-node 'tr (list (cons 'data-we-widget "table-row")) '() #f #f #f))
        (backend-replace-children! row-node (map build-cell cell-values))
        row-node)
      (define (build-data-row row index)
        (define row-variant
          (if (< index (length row-variants))
              (list-ref row-variants index)
              #f))
        (define variant-css (table-row-variant-class row-variant))
        (define row-attrs
          (if (equal? variant-css "")
              (list (cons 'data-we-widget "table-row"))
              (list (cons 'data-we-widget "table-row")
                    (cons 'class variant-css))))
        (define row-node (dom-node 'tr row-attrs '() #f #f #f))
        (define cells
          (let loop ([rest (row-values row)]
                     [index 0])
            (if (null? rest)
                '()
                (cons (data-cell index (car rest))
                      (loop (cdr rest) (add1 index))))))
        (backend-replace-children! row-node cells)
        row-node)
      (define header-row
        (if (null? normalized-columns)
            '()
            (list (build-row normalized-columns header-cell))))
      (define data-rows
        (let loop ([rest normalized-rows]
                   [index 0])
          (if (null? rest)
              '()
              (cons (build-data-row (car rest) index)
                    (loop (cdr rest) (add1 index))))))
      (backend-replace-children! table-node (append caption-row header-row data-rows)))

    ;; build-node : view? (-> (-> void?) void?) -> dom-node?
    ;;   Build a dom-node tree from v and register lifecycle cleanups.
    (define (build-node v register-cleanup!)
      (define kind      (view-kind v))
      (define root-node #f)
      ;; append-view-child! : dom-node? view? -> void?
      ;;   Append child view to parent, flattening Fragment children without wrapper nodes.
      (define (append-view-child! parent child)
        (if (and (view? child)
                 (eq? (view-kind child) 'fragment))
            (for-each (lambda (grand-child)
                        (append-view-child! parent grand-child))
                      (view-children child))
            (backend-append-child! parent (build-node child register-cleanup!))))
      ;; set-dom-node-attrs! : dom-node? list? -> void?
      ;;   Set attrs, applying view-level style hooks only on this view's root node.
      (define (set-dom-node-attrs! n attrs)
        (backend-set-dom-node-attrs!
         n (if (and root-node (eq? n root-node))
               (merge-root-extra-attrs v attrs)
               attrs)))
      (define node
        (case kind
          [(window)
           (define node       (dom-node 'div (list (cons attr/role 'window)
                                                   (cons 'data-we-widget "window")) '() #f #f #f))
           (define style-node (dom-node 'style '() '() shared-style-text #f #f))
           (validate-window-base-order! v)
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           (backend-append-child! node style-node)
           node]
          [(vpanel)
           (define node (dom-node 'div (list (cons 'data-we-widget "vpanel")
                                             (cons 'class          "we-vpanel")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(hpanel)
           (define node (dom-node 'div (list (cons 'data-we-widget "hpanel")
                                             (cons 'class          "we-hpanel")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(container)
           (define node (dom-node 'div (list (cons 'data-we-widget "container")
                                             (cons 'class          "we-container")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(grid)
           (define raw-columns      (alist-ref (view-props v) 'columns 'render))
           (define raw-gap          (alist-ref (view-props v) 'gap #f))
           (define columns-template (grid-columns-template (maybe-observable-value raw-columns)))
           (define gap-template
             (grid-gap-template (maybe-observable-value raw-gap)))
           (define node (dom-node 'div (list (cons 'data-we-widget "grid")
                                             (cons 'class "we-grid")
                                             (cons 'style (string-append "--we-grid-columns:"
                                                                         columns-template
                                                                         ";--we-grid-gap:"
                                                                         gap-template
                                                                         ";")))
                                  '()
                                  #f
                                  #f
                                  #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(stack)
           (define node (dom-node 'div (list (cons 'data-we-widget "stack")
                                             (cons 'class          "we-stack")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(inline)
           (define node (dom-node 'div (list (cons 'data-we-widget "inline")
                                             (cons 'class          "we-inline")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(fragment)
           (error 'Fragment "cannot be rendered directly; use as child content")]
          [(html-element)
           (define raw-tag   (alist-ref (view-props v) 'tag 'render))
           (define raw-value (alist-ref (view-props v) 'value 'render))
           (define initial-tag
             (let ([v0 (maybe-observable-value raw-tag)])
               (if (symbol? v0) v0 'div)))
           (define node (dom-node initial-tag '() '() "" #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
          (define (callback-from-action-attr attr-key)
            (define p (assq attr-key extra-attrs/raw))
            (if p
                (let ([v0 (maybe-observable-value (cdr p))])
                  (if (procedure? v0) v0 #f))
                #f))
           (define (valid-observable-attr-update? attr-key updated-value)
             (if (and (procedure? updated-value)
                      (not (procedure-allowed-attr-key? attr-key)))
                 (begin
                   (emit-web-easy-warning!
                    (string-append "web-easy: ignored procedure-valued observable attribute update "
                                   (symbol->string attr-key)))
                   #f)
                 #t))
           (define (refresh-root-attrs!)
             (set-dom-node-attrs! node (attr-remove-key (dom-node-attrs node) 'class)))
           (define (set-tag! tag-value)
             (set-dom-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div)))
           (define (set-text! value0)
             (set-dom-node-text! node (value->text value0)))
           (cond
             [(obs? raw-tag)
              (set-tag! (obs-peek raw-tag))
              (define (tag-listener updated-tag)
                (set-tag! updated-tag))
              (obs-observe! raw-tag tag-listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-tag tag-listener)))]
             [else
              (set-tag! raw-tag)])
           (cond
             [(obs? raw-value)
              (set-text! (obs-peek raw-value))
              (define (listener updated)
                (set-text! updated))
              (obs-observe! raw-value listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
             [else
              (set-text! raw-value)])
           (define on-click-callback (callback-from-action-attr 'on-click-action))
           (define on-change-callback (callback-from-action-attr 'on-change-action))
           (when on-click-callback
             (set-dom-node-on-click! node on-click-callback))
           (when on-change-callback
             (set-dom-node-on-change! node on-change-callback))
           (for-each
            (lambda (entry)
              (when (and (pair? entry)
                         (symbol? (car entry))
                         (obs? (cdr entry)))
                (define attr-obs (cdr entry))
                (define (attr-listener _updated)
                  (if (valid-observable-attr-update? (car entry) _updated)
                      (refresh-root-attrs!)
                      (void)))
                (obs-observe! attr-obs attr-listener)
                (register-cleanup! (lambda () (obs-unobserve! attr-obs attr-listener)))))
            extra-attrs/raw)
           node]
          [(html-element-children)
           (define raw-tag (alist-ref (view-props v) 'tag 'render))
           (define initial-tag
             (let ([v0 (maybe-observable-value raw-tag)])
               (if (symbol? v0) v0 'div)))
           (define node (dom-node initial-tag '() '() #f #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
           (define (callback-from-action-attr attr-key)
             (define p (assq attr-key extra-attrs/raw))
             (if p
                 (let ([v0 (maybe-observable-value (cdr p))])
                   (if (procedure? v0) v0 #f))
                 #f))
           (define (valid-observable-attr-update? attr-key updated-value)
             (if (and (procedure? updated-value)
                      (not (procedure-allowed-attr-key? attr-key)))
                 (begin
                   (emit-web-easy-warning!
                    (string-append "web-easy: ignored procedure-valued observable attribute update "
                                   (symbol->string attr-key)))
                   #f)
                 #t))
           (define (refresh-root-attrs!)
             (set-dom-node-attrs! node (attr-remove-key (dom-node-attrs node) 'class)))
           (define (set-tag! tag-value)
             (set-dom-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div)))
           (cond
             [(obs? raw-tag)
              (set-tag! (obs-peek raw-tag))
              (define (tag-listener updated-tag)
                (set-tag! updated-tag))
              (obs-observe! raw-tag tag-listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-tag tag-listener)))]
             [else
              (set-tag! raw-tag)])
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           (define on-click-callback (callback-from-action-attr 'on-click-action))
           (define on-change-callback (callback-from-action-attr 'on-change-action))
           (when on-click-callback
             (set-dom-node-on-click! node on-click-callback))
           (when on-change-callback
             (set-dom-node-on-change! node on-change-callback))
           (for-each
            (lambda (entry)
              (when (and (pair? entry)
                         (symbol? (car entry))
                         (obs? (cdr entry)))
                (define attr-obs (cdr entry))
                (define (attr-listener updated)
                  (if (valid-observable-attr-update? (car entry) updated)
                      (refresh-root-attrs!)
                      (void)))
                (obs-observe! attr-obs attr-listener)
                (register-cleanup! (lambda () (obs-unobserve! attr-obs attr-listener)))))
            extra-attrs/raw)
           node]
          [(if-view)
           (define raw-cond  (alist-ref (view-props v) 'cond 'render))
           (define then-view (alist-ref (view-props v) 'then 'render))
           (define else-view (alist-ref (view-props v) 'else 'render))
           (define node (dom-node 'div (list (cons 'data-we-widget "if-view")
                                             (cons 'class "we-if-view")) '() #f #f #f))
           (define (render-branch! cond-value)
             (replace-with-single-child! node
                                         (if (cond-clause-active? cond-value) then-view else-view)
                                         register-cleanup!))
           (cond
             [(obs? raw-cond)
              (render-branch! (obs-peek raw-cond))
              (define (listener updated)
                (render-branch! updated))
              (obs-observe! raw-cond listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-cond listener)))]
             [else
              (render-branch! raw-cond)])
           node]
          [(cond-view)
           (define clauses   (ensure-list (alist-ref (view-props v) 'clauses 'render)
                                          'cond-view
                                          "clauses"))
           (define else-view (alist-ref (view-props v) 'else 'render))
           (define node (dom-node 'div (list (cons 'data-we-widget "cond-view")
                                             (cons 'class "we-cond-view")) '() #f #f #f))
           (define (choose-view)
             (define selected
               (let loop ([cs clauses])
                 (cond
                   [(null? cs) #f]
                   [else
                    (define clause (car cs))
                    (define test-value (maybe-observable-value (car clause)))
                    (if (cond-clause-active? test-value)
                        (cdr clause)
                        (loop (cdr cs)))])))
             (if selected selected else-view))
           (define (render-branch!)
             (replace-with-single-child! node (choose-view) register-cleanup!))
           (for-each (lambda (clause)
                       (define raw-test (car clause))
                       (when (obs? raw-test)
                         (define (listener _updated)
                           (render-branch!))
                         (obs-observe! raw-test listener)
                         (register-cleanup! (lambda () (obs-unobserve! raw-test listener)))))
                     clauses)
           (render-branch!)
           node]
          [(case-view)
           (define raw-value (alist-ref (view-props v) 'value 'render))
           (define clauses   (ensure-list (alist-ref (view-props v) 'clauses 'render)
                                          'case-view
                                          "clauses"))
           (define else-view (alist-ref (view-props v) 'else 'render))
           (define node (dom-node 'div (list (cons 'data-we-widget "case-view")
                                             (cons 'class "we-case-view")) '() #f #f #f))
           (define (choose-view v*)
             (define selected
               (let loop ([cs clauses])
                 (cond
                   [(null? cs) #f]
                   [else
                    (define clause (car cs))
                    (define lits (ensure-list (car clause) 'case-view "clause literals"))
                    (if (member v* lits)
                        (cdr clause)
                        (loop (cdr cs)))])))
             (if selected selected else-view))
           (define (render-branch! v*)
             (replace-with-single-child! node (choose-view v*) register-cleanup!))
           (cond
             [(obs? raw-value)
              (render-branch! (obs-peek raw-value))
              (define (listener updated)
                (render-branch! updated))
              (obs-observe! raw-value listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
             [else
              (render-branch! raw-value)])
           node]
          [(tab-panel)
           (define raw-selected (alist-ref (view-props v) 'selected 'render))
           (define raw-variants (alist-ref (view-props v) 'variants 'render))
           (define tab-variants (normalize-tab-variants raw-variants))
           (define tab-variant-class
             (let loop ([rest tab-variants] [acc ""])
               (cond
                 [(null? rest) acc]
                 [else
                  (define variant-class
                    (if (eq? (car rest) 'default)
                        "we-tab-style-default"
                        (string-append "we-tab-style-" (symbol->string (car rest)))))
                  (loop (cdr rest)
                        (if (string=? acc "")
                            variant-class
                            (string-append acc " " variant-class)))])))
           (define tab-panel-class
             (string-append "we-tab-panel"
                            (if (string=? tab-variant-class "") "" (string-append " " tab-variant-class))))
           (define tabs/raw     (ensure-list (alist-ref (view-props v) 'tabs 'render)
                                             'tab-panel
                                             "tabs"))
           (define tabs         (map normalize-tab-entry tabs/raw))
           (define panel-id     (next-tab-panel-id))
           (define node (dom-node 'tab-panel (list (cons 'selected #f)
                                                   (cons 'data-we-widget "tab-panel")
                                                   (cons 'class    tab-panel-class))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define tabs-node  (dom-node 'div (list (cons attr/role   'tablist)
                                                   (cons 'data-we-widget "tab-list")
                                                   (cons 'class      "we-tab-list"))
                                        '()
                                        #f
                                        #f
                                        #f))
           (define content-node (dom-node 'div (list (cons attr/role 'tabpanel)
                                                     (cons 'id       panel-id)
                                                     (cons 'data-we-widget "tab-content")
                                                     (cons 'aria-labelledby "")
                                                     (cons 'class    "we-tab-content"))
                                          '()
                                          #f
                                          #f
                                          #f))
           (backend-append-child! node tabs-node)
           (backend-append-child! node content-node)
           (define tab-buttons    '())
           (define selected-value #f)
           (define enabled-tab-ids
             (map car (filter (lambda (tab) (not (list-ref tab 2))) tabs)))
           (define (choose-view selected)
             (define selected-view
               (let loop ([ts tabs])
                 (cond
                   [(null? ts) #f]
                   [else
                    (define tab (car ts))
                    (if (and (equal? (car tab) selected)
                             (not (list-ref tab 2)))
                        (list-ref tab 1)
                        (loop (cdr ts)))])))
             (cond
               [selected-view selected-view]
               [(null? enabled-tab-ids)
                (if (null? tabs)
                    (spacer)
                    (list-ref (car tabs) 1))]
               [else
                (let loop ([ts tabs])
                  (define tab (car ts))
                  (if (equal? (car tab) (car enabled-tab-ids))
                      (list-ref tab 1)
                      (loop (cdr ts))))]))
           (define (tab-disabled? tab-id)
             (let loop ([ts tabs])
               (cond
                 [(null? ts) #t]
                 [else
                  (define tab (car ts))
                  (if (equal? (car tab) tab-id)
                      (list-ref tab 2)
                      (loop (cdr ts)))])))
           (define (set-selected! selected)
             (define selected-button-id
               (let loop ([entries tab-buttons])
                 (cond
                   [(null? entries) ""]
                   [else
                    (define entry (car entries))
                    (if (equal? (list-ref entry 0) selected)
                        (list-ref entry 1)
                        (loop (cdr entries)))])))
             (set-dom-node-attrs!
              content-node
              (list (cons attr/role 'tabpanel)
                    (cons 'id panel-id)
                    (cons 'data-we-widget "tab-content")
                    (cons 'aria-labelledby selected-button-id)
                    (cons 'class "we-tab-content")))
             (set-dom-node-attrs! node (list (cons 'selected selected)
                                             (cons 'data-we-widget "tab-panel")
                                             (cons 'class tab-panel-class)))
             (set! selected-value selected)
             (for-each (lambda (entry)
                         (define tab-id (list-ref entry 0))
                         (define button-id (list-ref entry 1))
                         (define button-node (list-ref entry 2))
                         (define disabled? (tab-disabled? tab-id))
                         (set-dom-node-attrs!
                          button-node
                          (list (cons 'tab-id tab-id)
                                (cons 'id button-id)
                                (cons 'role 'tab)
                                (cons 'data-we-widget "tab-button")
                                (cons 'aria-controls panel-id)
                                (cons 'aria-disabled disabled?)
                                (cons 'aria-selected (and (equal? tab-id selected) (not disabled?)))
                                (cons 'tabindex (if (and (equal? tab-id selected) (not disabled?)) 0 -1))
                                (cons 'class (cond
                                               [disabled? "we-tab-btn is-disabled"]
                                               [(equal? tab-id selected) "we-tab-btn is-selected"]
                                               [else "we-tab-btn"])))))
                       tab-buttons))
           (define (index-of-tab selected)
             (let loop ([i 0] [ids enabled-tab-ids])
               (cond
                 [(null? ids) 0]
                 [(equal? (car ids) selected) i]
                 [else (loop (add1 i) (cdr ids))])))
           (define (tab-at index)
             (list-ref enabled-tab-ids index))
           (define (next-tab-id)
             (if (null? enabled-tab-ids)
                 #f
                 (let* ([count (length enabled-tab-ids)]
                        [i (index-of-tab selected-value)]
                        [j (modulo (+ i 1) count)])
                   (tab-at j))))
           (define (prev-tab-id)
             (if (null? enabled-tab-ids)
                 #f
                 (let* ([count (length enabled-tab-ids)]
                        [i (index-of-tab selected-value)]
                        [j (modulo (+ i (- count 1)) count)])
                   (tab-at j))))
           (define (first-tab-id)
             (if (null? enabled-tab-ids) #f (car enabled-tab-ids)))
           (define (last-tab-id)
             (if (null? enabled-tab-ids) #f (list-ref enabled-tab-ids (- (length enabled-tab-ids) 1))))
           (define (select-if-possible tab-id)
             (when (and tab-id (obs? raw-selected) (not (tab-disabled? tab-id)))
               (obs-set! raw-selected tab-id)))
           (define (handle-tab-key key)
             (case (string->symbol key)
               [(ArrowRight) (select-if-possible (next-tab-id))]
               [(ArrowLeft)  (select-if-possible (prev-tab-id))]
               [(Home)       (select-if-possible (first-tab-id))]
               [(End)        (select-if-possible (last-tab-id))]
               [else (void)]))
           (define (init-tabs!)
             (set! tab-buttons
                   (let loop ([remaining tabs] [idx 0])
                     (cond
                       [(null? remaining) '()]
                       [else
                        (define tab (car remaining))
                        (define tab-id (car tab))
                        (define button-id
                          (string-append panel-id "-tab-" (number->string idx)))
                        (define disabled? (list-ref tab 2))
                        (define button-node
                          (dom-node 'button
                                    (list (cons 'tab-id   tab-id)
                                          (cons 'id button-id)
                                          (cons 'selected #f))
                                    '()
                                    (value->text tab-id)
                                    #f
                                    #f))
                        (when (obs? raw-selected)
                          (set-dom-node-on-click!
                           button-node
                           (lambda ()
                             (unless disabled?
                               (obs-set! raw-selected tab-id))))
                          (set-dom-node-on-change!
                           button-node
                           (lambda (key)
                             (unless disabled?
                               (handle-tab-key key)))))
                        (cons (list tab-id button-id button-node)
                              (loop (cdr remaining) (add1 idx)))])))
             (backend-replace-children! tabs-node (map (lambda (entry) (list-ref entry 2)) tab-buttons)))
           (define (render-tab! selected)
             (set-selected! selected)
             (backend-set-single-child! content-node (build-node (choose-view selected) register-cleanup!)))
           (init-tabs!)
           (cond
             [(obs? raw-selected)
              (render-tab! (obs-peek raw-selected))
              (define (listener updated)
                (render-tab! updated))
              (obs-observe! raw-selected listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-selected listener)))]
             [else
              (render-tab! raw-selected)])
           node]
          [(accordion)
           (define raw-selected (alist-ref (view-props v) 'selected 'render))
           (define sections
             (ensure-list (alist-ref (view-props v) 'sections 'render)
                          'accordion
                          "sections"))
           (define node
             (dom-node 'div
                       (list (cons 'data-we-widget "accordion")
                             (cons 'class "we-accordion"))
                       '()
                       #f
                       #f
                       #f))

           ;; normalize-section : any/c -> list?
           ;;   Validate section shape and return (list id label body-view).
           (define (normalize-section section)
             (unless (list? section)
               (raise-arguments-error 'accordion
                                      "expected section row as list"
                                      "section"
                                      section))
             (unless (= (length section) 3)
               (raise-arguments-error 'accordion
                                      "expected section row of arity 3: (list id label view)"
                                      "section"
                                      section))
             section)

           (define sections/normalized (map normalize-section sections))
           (define section-state '())

           ;; section-id-index : any/c -> number?
           ;;   Return index of section-id in section-state, or #f when missing.
           (define (section-id-index section-id)
             (let loop ([entries section-state]
                        [i 0])
               (cond
                 [(null? entries) #f]
                 [else
                  (if (equal? (list-ref (car entries) 0) section-id)
                      i
                      (loop (cdr entries) (add1 i)))])))

           ;; select-section! : any/c -> void?
           ;;   Select section-id when selected is observable.
           (define (select-section! section-id)
             (when (obs? raw-selected)
               (obs-set! raw-selected section-id)))

           ;; toggle-section! : any/c -> void?
           ;;   Toggle section-id open/closed when selected is observable.
           (define (toggle-section! section-id)
             (when (obs? raw-selected)
               (if (equal? (obs-peek raw-selected) section-id)
                   (obs-set! raw-selected #f)
                   (obs-set! raw-selected section-id))))

           ;; move-selection! : any/c number? -> void?
           ;;   Move selection by delta in section order with wrapping.
           (define (move-selection! section-id delta)
             (define count (length section-state))
             (when (and (obs? raw-selected)
                        (> count 0))
               (define index (section-id-index section-id))
               (define base-index (if index index 0))
               (define next-index (modulo (+ base-index delta count) count))
               (define next-id (list-ref (list-ref section-state next-index) 0))
               (select-section! next-id)))

           ;; section-open? : any/c -> boolean?
           ;;   Determine whether the section id is currently selected/open.
           (define (section-open? section-id)
             (if (obs? raw-selected)
                 (equal? (obs-peek raw-selected) section-id)
                 (equal? raw-selected section-id)))

           ;; set-trigger-open! : dom-node? string? boolean? -> void?
           ;;   Update accordion trigger ARIA and class for open/closed state.
           (define (set-trigger-open! trigger panel-id open?)
             (set-dom-node-attrs!
              trigger
              (list (cons attr/role 'button)
                    (cons 'data-we-widget "accordion-trigger")
                    (cons 'aria-controls panel-id)
                    (cons 'aria-expanded (if open? "true" "false"))
                    (cons 'class (if open?
                                     "we-accordion-trigger is-open"
                                     "we-accordion-trigger")))))

           ;; update-trigger-states! : -> void?
           ;;   Refresh trigger classes/ARIA from current selected section.
           (define (update-trigger-states!)
             (for-each (lambda (entry)
                         (define section-id (list-ref entry 0))
                         (define panel-id   (list-ref entry 1))
                         (define trigger    (list-ref entry 2))
                         (set-trigger-open! trigger panel-id (section-open? section-id)))
                       section-state))

           (for-each
            (lambda (section)
              (define section-id    (list-ref section 0))
              (define section-label (list-ref section 1))
              (define section-view  (list-ref section 2))
              (define panel-id      (next-accordion-panel-id))
              (define section-node
                (dom-node 'div
                          (list (cons 'data-we-widget "accordion-section")
                                (cons 'class "we-accordion-section"))
                          '()
                          #f
                          #f
                          #f))
              (define trigger-node
                (dom-node 'button
                          (list (cons attr/role 'button)
                                (cons 'data-we-widget "accordion-trigger")
                                (cons 'aria-controls panel-id)
                                (cons 'aria-expanded "false")
                                (cons 'class "we-accordion-trigger"))
                          '()
                          (value->text section-label)
                          #f
                          #f))
              (define collapse-open
                (if (obs? raw-selected)
                    (~> raw-selected
                        (lambda (current-id)
                          (equal? current-id section-id)))
                    (equal? raw-selected section-id)))
              (define collapse-node
                (build-node (collapse
                             collapse-open
                             section-view
                             #:class "we-accordion-content")
                            register-cleanup!))
              (set-dom-node-attrs!
               collapse-node
               (attr-set (dom-node-attrs collapse-node)
                         'id
                         panel-id))
              (when (obs? raw-selected)
                (set-dom-node-on-click!
                 trigger-node
                 (lambda ()
                   (toggle-section! section-id)))
                (set-dom-node-on-change!
                 trigger-node
                 (lambda (key)
                   (cond
                     [(string=? key "ArrowDown")
                      (move-selection! section-id 1)]
                     [(string=? key "ArrowUp")
                      (move-selection! section-id -1)]
                     [(string=? key "Home")
                      (when (pair? section-state)
                        (select-section! (list-ref (car section-state) 0)))]
                     [(string=? key "End")
                      (when (pair? section-state)
                        (select-section! (list-ref (list-ref section-state
                                                             (- (length section-state) 1))
                                                   0)))]
                     [else
                      (void)]))))
              (set! section-state
                    (append section-state
                            (list (list section-id panel-id trigger-node))))
              (backend-append-child! section-node trigger-node)
              (backend-append-child! section-node collapse-node)
              (backend-append-child! node section-node))
            sections/normalized)

           (update-trigger-states!)
           (when (obs? raw-selected)
             (define (listener _updated)
               (update-trigger-states!))
             (obs-observe! raw-selected listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-selected listener))))
           node]
          [(offcanvas)
           (define raw-open (alist-ref (view-props v) 'open 'render))
           (define on-close (alist-ref (view-props v) 'on-close 'render))
           (define raw-side (alist-ref (view-props v) 'side 'render))
           (define node
             (dom-node 'div
                       (list (cons attr/role 'dialog)
                             (cons 'data-we-widget "offcanvas")
                             (cons 'class "we-offcanvas")
                             (cons 'aria-hidden "true"))
                       '()
                       #f
                       #f
                       #f))
           (define backdrop-node
             (dom-node 'div
                       (list (cons 'data-we-widget "offcanvas-backdrop")
                             (cons 'class "we-offcanvas-backdrop"))
                       '()
                       #f
                       (lambda ()
                         (invoke-close-callback on-close 'backdrop))
                       #f))
           (define panel-node
             (dom-node 'div
                       (list (cons 'data-we-widget "offcanvas-panel")
                             (cons 'class "we-offcanvas-panel is-end"))
                       '()
                       #f
                       #f
                       #f))
           (backend-append-child!
            panel-node
            (build-node
             (close-button (lambda () (invoke-close-callback on-close 'button))
                           "Close panel")
             register-cleanup!))
           (for-each (lambda (child)
                       (append-view-child! panel-node child))
                     (view-children v))
           (backend-append-child! node backdrop-node)
           (backend-append-child! node panel-node)
           (define (refresh-offcanvas!)
             (define open? (not (eq? (maybe-observable-value raw-open) #f)))
             (define side (normalize-offcanvas-side (maybe-observable-value raw-side)))
             (set-dom-node-attrs!
              node
              (list (cons attr/role 'dialog)
                    (cons 'data-we-widget "offcanvas")
                    (cons 'class (if open? "we-offcanvas is-open" "we-offcanvas"))
                    (cons 'aria-hidden (if open? "false" "true"))))
             (set-dom-node-attrs!
              panel-node
              (list (cons 'data-we-widget "offcanvas-panel")
                    (cons 'class (string-append "we-offcanvas-panel "
                                                (if (eq? side 'start) "is-start" "is-end"))))))
           (when (obs? raw-open)
             (define (open-listener _updated)
               (refresh-offcanvas!))
             (obs-observe! raw-open open-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-open open-listener))))
           (when (obs? raw-side)
             (define (side-listener _updated)
               (refresh-offcanvas!))
             (obs-observe! raw-side side-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-side side-listener))))
           (refresh-offcanvas!)
           node]
          [(dialog)
           (define raw-open  (alist-ref (view-props v) 'open 'render))
           (define on-close  (alist-ref (view-props v) 'on-close 'render))
           (define panel-size
             (normalize-dialog-size (alist-ref (view-props v) 'size 'render)))
           (define options        (alist-ref (view-props v) 'options 'render))
           (define raw-title      (options-ref options 'title #f))
           (define raw-description (options-ref options 'description #f))
           (define raw-footer     (options-ref options 'footer #f))
           (define raw-show-close? (options-ref options 'show-close? #f))
           (define raw-close-label (options-ref options 'close-label "Close dialog"))
           (define raw-tone       (options-ref options 'tone #f))
           (define raw-tone-style (options-ref options 'tone-style #f))
           (define panel-class-base
             (string-append "we-dialog-panel we-dialog-size-" (symbol->string panel-size)))
           (define node (dom-node 'dialog
                                  (list (cons attr/role 'dialog)
                                        (cons 'data-we-widget "dialog")
                                        (cons 'class "we-dialog")
                                        (cons 'tabindex -1)
                                        (cons 'aria-modal "true")
                                        (cons 'aria-hidden "true"))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define panel-node (dom-node 'div
                                        (list (cons 'class panel-class-base)
                                              (cons 'data-we-widget "dialog-panel")
                                              (cons 'tabindex -1))
                                        '()
                                        #f
                                        #f
                                        #f))
           (define header-node (dom-node 'div
                                         (list (cons 'data-we-widget "dialog-header")
                                               (cons 'class "we-dialog-header"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define title-node (dom-node 'h2
                                        (list (cons 'data-we-widget "dialog-title")
                                              (cons 'class "we-dialog-title"))
                                        '()
                                        ""
                                        #f
                                        #f))
           (define close-node (dom-node 'button
                                        (list (cons attr/role 'button)
                                              (cons 'data-we-widget "dialog-close")
                                              (cons 'class "we-close-button we-dialog-close")
                                              (cons 'aria-label "Close dialog"))
                                        '()
                                        "×"
                                        #f
                                        #f))
           (define body-node (dom-node 'div
                                       (list (cons 'data-we-widget "dialog-body")
                                             (cons 'class "we-dialog-body"))
                                       '()
                                       #f
                                       #f
                                       #f))
           (define description-node (dom-node 'p
                                              (list (cons 'data-we-widget "dialog-description")
                                                    (cons 'class "we-dialog-description"))
                                              '()
                                              ""
                                              #f
                                              #f))
           (define footer-node (dom-node 'div
                                         (list (cons 'data-we-widget "dialog-footer")
                                               (cons 'class "we-dialog-footer"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define dialog-desc-id #f)
           (define body-content-nodes '())
           (define (rebuild-dialog-structure!)
             (set! dialog-desc-id #f)
             (define title-value       (maybe-observable-value raw-title))
             (define description-value (maybe-observable-value raw-description))
             (define footer-value      (maybe-observable-value raw-footer))
             (define show-close?       (not (eq? (maybe-observable-value raw-show-close?) #f)))
             (define close-label-value (maybe-observable-value raw-close-label))
             (when (and (eq? description-value #f)
                        (pair? body-content-nodes))
               (let ([first-body-child (car body-content-nodes)])
                 (when (equal? (alist-ref (dom-node-attrs first-body-child) 'data-we-widget #f) "text")
                   (set! dialog-desc-id (next-dialog-body-id))
                   (set-dom-node-attrs!
                    first-body-child
                    (attr-set (dom-node-attrs first-body-child) 'id dialog-desc-id)))))
             (define header-children
               (append (if (eq? title-value #f)
                           '()
                           (begin
                             (set-dom-node-text! title-node (value->text title-value))
                             (list title-node)))
                       (if show-close?
                           (begin
                             (set-dom-node-attrs!
                              close-node
                              (list (cons attr/role 'button)
                                    (cons 'data-we-widget "dialog-close")
                                    (cons 'class "we-close-button we-dialog-close")
                                    (cons 'aria-label (value->text close-label-value))))
                             (set-dom-node-on-click!
                              close-node
                              (if (procedure? on-close)
                                  (lambda () (on-close))
                                  #f))
                             (list close-node))
                           '())))
             (define body-children
               (append (if (eq? description-value #f)
                           '()
                           (begin
                             (set! dialog-desc-id (next-dialog-body-id))
                             (set-dom-node-text! description-node (value->text description-value))
                             (set-dom-node-attrs!
                              description-node
                              (list (cons 'id dialog-desc-id)
                                    (cons 'data-we-widget "dialog-description")
                                    (cons 'class "we-dialog-description")))
                             (list description-node)))
                       body-content-nodes))
             (define footer-children
               (cond
                 [(eq? footer-value #f)
                  '()]
                 [(view? footer-value)
                  (list (build-node footer-value register-cleanup!))]
                 [else
                  (list (dom-node 'span
                                  (list (cons 'data-we-widget "dialog-footer-text")
                                        (cons 'class "we-dialog-footer-text"))
                                  '()
                                  (value->text footer-value)
                                  #f
                                  #f))]))
             (backend-replace-children! header-node header-children)
             (backend-replace-children! body-node body-children)
             (backend-replace-children! footer-node footer-children)
             (backend-replace-children!
              panel-node
              (append (if (null? header-children) '() (list header-node))
                      (list body-node)
                      (if (null? footer-children) '() (list footer-node)))))
           (define (set-open! open?)
             (define open-value (not (eq? open? #f)))
             (define tone-value      (normalize-card-tone (maybe-observable-value raw-tone)))
             (define tone-style-value (normalize-card-tone-style (maybe-observable-value raw-tone-style)))
             (define panel-class
               (string-append
                panel-class-base
                (if tone-value
                    (string-append " we-dialog-tone-" (symbol->string tone-value))
                    "")
                (if tone-style-value
                    (string-append " we-dialog-tone-" (symbol->string tone-style-value))
                    "")))
             (define panel-attrs/base
               (list (cons 'class panel-class)
                     (cons 'data-we-widget "dialog-panel")
                     (cons 'tabindex -1)))
             (define panel-attrs
               (if dialog-desc-id
                   (append panel-attrs/base (list (cons 'aria-describedby dialog-desc-id)))
                   panel-attrs/base))
             (set-dom-node-attrs!
              node
              (list (cons attr/role 'dialog)
                    (cons 'data-we-widget "dialog")
                    (cons 'open open-value)
                    (cons 'class (if open-value "we-dialog is-open" "we-dialog"))
                    (cons 'tabindex -1)
                    (cons 'aria-modal "true")
                    (cons 'aria-hidden (if open-value "false" "true"))))
             (set-dom-node-attrs! panel-node panel-attrs))
           (when (procedure? on-close)
             (set-dom-node-on-change!
              node
              (lambda (key)
                (when (string=? key "Escape")
                  (on-close)))))
           (for-each (lambda (child)
                       (append-view-child! body-node child))
                     (view-children v))
           (set! body-content-nodes (dom-node-children body-node))
           (backend-append-child! node panel-node)
           (rebuild-dialog-structure!)
           (when (obs? raw-title)
             (define (title-listener _updated)
               (rebuild-dialog-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-title title-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-title title-listener))))
           (when (obs? raw-description)
             (define (description-listener _updated)
               (rebuild-dialog-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-description description-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-description description-listener))))
           (when (obs? raw-footer)
             (define (footer-listener _updated)
               (rebuild-dialog-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-footer footer-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-footer footer-listener))))
           (when (obs? raw-show-close?)
             (define (show-close-listener _updated)
               (rebuild-dialog-structure!))
             (obs-observe! raw-show-close? show-close-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-show-close? show-close-listener))))
           (when (obs? raw-close-label)
             (define (close-label-listener _updated)
               (rebuild-dialog-structure!))
             (obs-observe! raw-close-label close-label-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-close-label close-label-listener))))
           (when (obs? raw-tone)
             (define (tone-listener _updated)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-tone tone-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone tone-listener))))
           (when (obs? raw-tone-style)
             (define (tone-style-listener _updated)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-tone-style tone-style-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone-style tone-style-listener))))
           (cond
             [(obs? raw-open)
              (set-open! (obs-peek raw-open))
              (define (listener updated)
                (set-open! updated))
              (obs-observe! raw-open listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-open listener)))]
             [else
              (set-open! raw-open)])
           node]
          [(modal)
           (define raw-open  (alist-ref (view-props v) 'open 'render))
           (define on-close  (alist-ref (view-props v) 'on-close 'render))
           (define panel-size
             (normalize-dialog-size (alist-ref (view-props v) 'size 'render)))
           (define options        (alist-ref (view-props v) 'options 'render))
           (define raw-title      (options-ref options 'title #f))
           (define raw-description (options-ref options 'description #f))
           (define raw-footer     (options-ref options 'footer #f))
           (define raw-show-close? (options-ref options 'show-close? #f))
           (define raw-close-label (options-ref options 'close-label "Close modal"))
           (define raw-tone       (options-ref options 'tone #f))
           (define raw-tone-style (options-ref options 'tone-style #f))
           (define panel-class-base
             (string-append "we-dialog-panel we-dialog-size-" (symbol->string panel-size)))
           (define node (dom-node 'dialog
                                  (list (cons attr/role 'dialog)
                                        (cons 'data-we-widget "modal")
                                        (cons 'class "we-modal")
                                        (cons 'tabindex -1)
                                        (cons 'aria-modal "true")
                                        (cons 'aria-hidden "true"))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define panel-node (dom-node 'div
                                        (list (cons 'class panel-class-base)
                                              (cons 'data-we-widget "modal-panel")
                                              (cons 'tabindex -1))
                                        '()
                                        #f
                                        #f
                                        #f))
           (define header-node (dom-node 'div
                                         (list (cons 'data-we-widget "modal-header")
                                               (cons 'class "we-modal-header"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define title-node (dom-node 'h2
                                        (list (cons 'data-we-widget "modal-title")
                                              (cons 'class "we-modal-title"))
                                        '()
                                        ""
                                        #f
                                        #f))
           (define close-node (dom-node 'button
                                        (list (cons attr/role 'button)
                                              (cons 'data-we-widget "modal-close")
                                              (cons 'class "we-close-button we-modal-close")
                                              (cons 'aria-label "Close modal"))
                                        '()
                                        "×"
                                        #f
                                        #f))
           (define body-node (dom-node 'div
                                       (list (cons 'data-we-widget "modal-body")
                                             (cons 'class "we-modal-body"))
                                       '()
                                       #f
                                       #f
                                       #f))
           (define description-node (dom-node 'p
                                              (list (cons 'data-we-widget "modal-description")
                                                    (cons 'class "we-modal-description"))
                                              '()
                                              ""
                                              #f
                                              #f))
           (define footer-node (dom-node 'div
                                         (list (cons 'data-we-widget "modal-footer")
                                               (cons 'class "we-modal-footer"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define modal-desc-id #f)
           (define body-content-nodes '())
           (define (rebuild-modal-structure!)
             (set! modal-desc-id #f)
             (define title-value       (maybe-observable-value raw-title))
             (define description-value (maybe-observable-value raw-description))
             (define footer-value      (maybe-observable-value raw-footer))
             (define show-close?       (not (eq? (maybe-observable-value raw-show-close?) #f)))
             (define close-label-value (maybe-observable-value raw-close-label))
             (when (and (eq? description-value #f)
                        (pair? body-content-nodes))
               (let ([first-body-child (car body-content-nodes)])
                 (when (equal? (alist-ref (dom-node-attrs first-body-child) 'data-we-widget #f) "text")
                   (set! modal-desc-id (next-dialog-body-id))
                   (set-dom-node-attrs!
                    first-body-child
                    (attr-set (dom-node-attrs first-body-child) 'id modal-desc-id)))))
             (define header-children
               (append (if (eq? title-value #f)
                           '()
                           (begin
                             (set-dom-node-text! title-node (value->text title-value))
                             (list title-node)))
                       (if show-close?
                           (begin
                             (set-dom-node-attrs!
                              close-node
                              (list (cons attr/role 'button)
                                    (cons 'data-we-widget "modal-close")
                                    (cons 'class "we-close-button we-modal-close")
                                    (cons 'aria-label (value->text close-label-value))))
                             (set-dom-node-on-click!
                              close-node
                              (if (procedure? on-close)
                                  (lambda () (on-close))
                                  #f))
                             (list close-node))
                           '())))
             (define body-children
               (append (if (eq? description-value #f)
                           '()
                           (begin
                             (set! modal-desc-id (next-dialog-body-id))
                             (set-dom-node-text! description-node (value->text description-value))
                             (set-dom-node-attrs!
                              description-node
                              (list (cons 'id modal-desc-id)
                                    (cons 'data-we-widget "modal-description")
                                    (cons 'class "we-modal-description")))
                             (list description-node)))
                       body-content-nodes))
             (define footer-children
               (cond
                 [(eq? footer-value #f)
                  '()]
                 [(view? footer-value)
                  (list (build-node footer-value register-cleanup!))]
                 [else
                  (list (dom-node 'span
                                  (list (cons 'data-we-widget "modal-footer-text")
                                        (cons 'class "we-modal-footer-text"))
                                  '()
                                  (value->text footer-value)
                                  #f
                                  #f))]))
             (backend-replace-children! header-node header-children)
             (backend-replace-children! body-node body-children)
             (backend-replace-children! footer-node footer-children)
             (backend-replace-children!
              panel-node
              (append (if (null? header-children) '() (list header-node))
                      (list body-node)
                      (if (null? footer-children) '() (list footer-node)))))
           (define (set-open! open?)
             (define open-value (not (eq? open? #f)))
             (define tone-value      (normalize-card-tone (maybe-observable-value raw-tone)))
             (define tone-style-value (normalize-card-tone-style (maybe-observable-value raw-tone-style)))
             (define panel-class
               (string-append
                panel-class-base
                (if tone-value
                    (string-append " we-dialog-tone-" (symbol->string tone-value))
                    "")
                (if tone-style-value
                    (string-append " we-dialog-tone-" (symbol->string tone-style-value))
                    "")))
             (define panel-attrs/base
               (list (cons 'class panel-class)
                     (cons 'data-we-widget "modal-panel")
                     (cons 'tabindex -1)))
             (define panel-attrs
               (if modal-desc-id
                   (append panel-attrs/base (list (cons 'aria-describedby modal-desc-id)))
                   panel-attrs/base))
             (set-dom-node-attrs!
              node
              (list (cons attr/role 'dialog)
                    (cons 'data-we-widget "modal")
                    (cons 'open open-value)
                    (cons 'class (if open-value "we-modal is-open" "we-modal"))
                    (cons 'tabindex -1)
                    (cons 'aria-modal "true")
                    (cons 'aria-hidden (if open-value "false" "true"))))
             (set-dom-node-attrs! panel-node panel-attrs))
           (when (procedure? on-close)
             (set-dom-node-on-change!
              node
              (lambda (key)
                (when (string=? key "Escape")
                  (on-close)))))
           (for-each (lambda (child)
                       (append-view-child! body-node child))
                     (view-children v))
           (set! body-content-nodes (dom-node-children body-node))
           (backend-append-child! node panel-node)
           (rebuild-modal-structure!)
           (when (obs? raw-title)
             (define (title-listener _updated)
               (rebuild-modal-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-title title-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-title title-listener))))
           (when (obs? raw-description)
             (define (description-listener _updated)
               (rebuild-modal-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-description description-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-description description-listener))))
           (when (obs? raw-footer)
             (define (footer-listener _updated)
               (rebuild-modal-structure!)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-footer footer-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-footer footer-listener))))
           (when (obs? raw-show-close?)
             (define (show-close-listener _updated)
               (rebuild-modal-structure!))
             (obs-observe! raw-show-close? show-close-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-show-close? show-close-listener))))
           (when (obs? raw-close-label)
             (define (close-label-listener _updated)
               (rebuild-modal-structure!))
             (obs-observe! raw-close-label close-label-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-close-label close-label-listener))))
           (when (obs? raw-tone)
             (define (tone-listener _updated)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-tone tone-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone tone-listener))))
           (when (obs? raw-tone-style)
             (define (tone-style-listener _updated)
               (set-open! (maybe-observable-value raw-open)))
             (obs-observe! raw-tone-style tone-style-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone-style tone-style-listener))))
           (cond
             [(obs? raw-open)
              (set-open! (obs-peek raw-open))
              (define (listener updated)
                (set-open! updated))
              (obs-observe! raw-open listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-open listener)))]
             [else
              (set-open! raw-open)])
           node]
          [(observable-view)
           (define raw-data    (alist-ref (view-props v) 'data       'render))
           (define make-view   (alist-ref (view-props v) 'make-view  'render))
           (define equal-proc  (alist-ref (view-props v) 'equal-proc 'render))

           (define node (dom-node 'div (list (cons 'data-we-widget "observable-view")
                                             (cons 'class          "we-observable-view"))
                                  '() #f #f #f))
           (define last-value #f)
           (define have-last? #f)
           (define (render-from-value! value)
             (set! have-last? #t)
             (set! last-value value)
             (replace-with-single-child! node (make-view value) register-cleanup!))
           (cond
             [(obs? raw-data)
              (render-from-value! (obs-peek raw-data))
              (define (listener updated)
                (unless (and have-last? (equal-proc updated last-value))
                  (render-from-value! updated)))
              (obs-observe! raw-data listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-data listener)))]
             [else
              (render-from-value! raw-data)])
           node]
          [(observable-element-children)
           (define raw-tag (alist-ref (view-props v) 'tag 'render))
           (define raw-data (alist-ref (view-props v) 'data 'render))
           (define make-children (alist-ref (view-props v) 'make-children 'render))
           (define equal-proc (alist-ref (view-props v) 'equal-proc 'render))
           (define raw-after-render (alist-ref (view-props v) 'after-render 'render))
           (define initial-tag
             (let ([v0 (maybe-observable-value raw-tag)])
               (if (symbol? v0) v0 'div)))
           (define node (dom-node initial-tag '() '() #f #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
           (define (callback-from-action-attr attr-key)
             (define p (assq attr-key extra-attrs/raw))
             (if p
                 (let ([v0 (maybe-observable-value (cdr p))])
                   (if (procedure? v0) v0 #f))
                 #f))
           (define (dom-node-attr-ref n key [default #f])
             (define p (assq key (dom-node-attrs n)))
             (if p
                 (cdr p)
                 default))
           (define (find-node-by-widget root widget-name)
             (if (equal? (dom-node-attr-ref root 'data-we-widget #f)
                         widget-name)
                 root
                 (let loop ([rest (dom-node-children root)])
                   (cond
                     [(null? rest)
                      #f]
                     [else
                      (define found
                        (find-node-by-widget (car rest) widget-name))
                     (if found
                          found
                          (loop (cdr rest)))]))))
           (define after-render
             (let ([v0 (maybe-observable-value raw-after-render)])
               (if (procedure? v0) v0 #f)))
           (define after-render-api
             (list (cons 'dom-node-attr-ref dom-node-attr-ref)
                   (cons 'find-node-by-widget find-node-by-widget)
                   (cons 'dom-node-children dom-node-children)
                   (cons 'dom-node-on-click dom-node-on-click)
                   (cons 'set-dom-node-on-click! set-dom-node-on-click!)
                   (cons 'backend-set-timeout! backend-set-timeout!)
                   (cons 'backend-clear-timeout! backend-clear-timeout!)
                   (cons 'backend-scrollspy-observe-scroll! backend-scrollspy-observe-scroll!)
                   (cons 'backend-scrollspy-scroll-into-view! backend-scrollspy-scroll-into-view!)
                   (cons 'backend-scrollspy-active-id backend-scrollspy-active-id)))
           (define (valid-observable-attr-update? attr-key updated-value)
             (if (and (procedure? updated-value)
                      (not (procedure-allowed-attr-key? attr-key)))
                 (begin
                   (emit-web-easy-warning!
                    (string-append "web-easy: ignored procedure-valued observable attribute update "
                                   (symbol->string attr-key)))
                   #f)
                 #t))
           (define (refresh-root-attrs!)
             (set-dom-node-attrs! node (attr-remove-key (dom-node-attrs node) 'class)))
           (define (set-tag! tag-value)
             (set-dom-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div)))
           (define on-click-callback (callback-from-action-attr 'on-click-action))
           (define on-change-callback (callback-from-action-attr 'on-change-action))
           (when on-click-callback
             (set-dom-node-on-click! node on-click-callback))
           (when on-change-callback
             (set-dom-node-on-change! node on-change-callback))
           (define last-value #f)
           (define have-last? #f)
           (define (render-from-value! value)
             (set! have-last? #t)
             (set! last-value value)
             (define children
               (map (lambda (child)
                      (build-node child register-cleanup!))
                    (ensure-list (make-children value)
                                 'observable-element-children
                                 "children")))
             (backend-replace-children! node children)
             (when after-render
               (after-render node value register-cleanup! after-render-api)))
           (cond
             [(obs? raw-tag)
              (set-tag! (obs-peek raw-tag))
              (define (tag-listener updated-tag)
                (set-tag! updated-tag))
              (obs-observe! raw-tag tag-listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-tag tag-listener)))]
             [else
              (set-tag! raw-tag)])
           (cond
             [(obs? raw-data)
              (render-from-value! (obs-peek raw-data))
              (define (listener updated)
                (unless (and have-last? (equal-proc updated last-value))
                  (render-from-value! updated)))
              (obs-observe! raw-data listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-data listener)))]
             [else
              (render-from-value! raw-data)])
           (for-each
            (lambda (entry)
              (when (and (pair? entry)
                         (symbol? (car entry))
                         (obs? (cdr entry)))
                (define attr-obs (cdr entry))
                (define (attr-listener updated)
                  (if (valid-observable-attr-update? (car entry) updated)
                      (refresh-root-attrs!)
                      (void)))
                (obs-observe! attr-obs attr-listener)
                (register-cleanup! (lambda () (obs-unobserve! attr-obs attr-listener)))))
            extra-attrs/raw)
           node]
          [(table)
           (define columns (ensure-list (alist-ref (view-props v) 'columns 'render)
                                        'table
                                        "columns"))
           (define raw-rows     (alist-ref (view-props v) 'rows    'render))
           (define raw-density  (alist-ref (view-props v) 'density 'render))
           (define raw-options  (alist-ref (view-props v) 'options 'render))
           (define options      (if (list? raw-options) raw-options '()))
           (define raw-caption
             (let ([p (assq 'caption options)])
               (if p (cdr p) #f)))
           (define raw-variants
             (let ([p (assq 'variants options)])
               (if p (cdr p) '())))
           (define raw-row-variants
             (let ([p (assq 'row-variants options)])
               (if p (cdr p) '())))
           (define raw-row-header-column
             (let ([p (assq 'row-header-column options)])
               (if p (cdr p) #f)))
           (define variants     (normalize-table-variants (maybe-observable-value raw-variants)))
           (define row-variants (normalize-table-row-variants (maybe-observable-value raw-row-variants)))
           (define row-header-column
             (normalize-table-row-header-column (maybe-observable-value raw-row-header-column)))
           (define density      (normalize-table-density (maybe-observable-value raw-density)))
           (define density-css  (density-class density))
           (define variant-css  (table-variant-class variants))
           (define caption-value
             (let ([v (maybe-observable-value raw-caption)])
               (if (eq? v #f) #f (value->text v))))
           (define node (dom-node 'table
                                  (list (cons 'data-we-widget "table")
                                        (cons 'columns columns)
                                        (cons 'variants variants)
                                        (cons 'row-variants row-variants)
                                        (cons 'row-header-column row-header-column)
                                        (cons 'caption caption-value)
                                        (cons 'density density)
                                        (cons 'class (string-append "we-table " density-css variant-css)))
                                  '()
                                  #f
                                  #f
                                  #f))
           (cond
             [(obs? raw-rows)
              (render-table-rows! node columns (obs-peek raw-rows) density raw-caption row-variants row-header-column)
              (define (listener updated)
                (render-table-rows! node columns updated density raw-caption row-variants row-header-column))
              (obs-observe! raw-rows listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-rows listener)))]
             [else
              (render-table-rows! node columns raw-rows density raw-caption row-variants row-header-column)])
           node]
          [(tooltip)
           (define raw-message (alist-ref (view-props v) 'message 'render))
           (define options     (alist-ref (view-props v) 'options 'render))
           (define raw-title   (options-ref options 'title #f))
           (define raw-footer  (options-ref options 'footer #f))
           (define tooltip-placement
             (normalize-overlay-placement
              (alist-ref (view-props v) 'placement 'render)
              'top))
           (define tooltip-base-class
             (string-append "we-tooltip we-tooltip-" (symbol->string tooltip-placement)))
           (define child-view
             (if (null? (view-children v))
                 (spacer)
                 (car (view-children v))))
           (define bubble-id (next-tooltip-id))
           (define node (dom-node 'div
                                  (list (cons 'data-we-widget "tooltip")
                                        (cons 'class tooltip-base-class))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define trigger-node (dom-node 'div
                                          (list (cons 'data-we-widget "tooltip-trigger")
                                                (cons 'class "we-tooltip-trigger"))
                                          '()
                                          #f
                                          #f
                                          #f))
           (define bubble-node (dom-node 'span
                                         (list (cons attr/role 'tooltip)
                                               (cons 'id bubble-id)
                                               (cons 'data-we-widget "tooltip-bubble")
                                               (cons 'class "we-tooltip-bubble")
                                               (cons 'aria-hidden "true"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define header-node (dom-node 'div
                                         (list (cons 'data-we-widget "tooltip-header")
                                               (cons 'class "we-tooltip-header"))
                                         '()
                                         #f
                                         #f
                                         #f))
           (define body-node (dom-node 'span
                                       (list (cons 'data-we-widget "tooltip-body")
                                             (cons 'class "we-tooltip-body"))
                                       '()
                                       #f
                                       #f
                                       #f))
           (define footer-node (dom-node 'div
                                         (list (cons 'data-we-widget "tooltip-footer")
                                               (cons 'class "we-tooltip-footer"))
                                         '()
                                         #f
                                         #f
                                         #f))
           ;; Constants for tooltip runtime state.
           (define tooltip-open? #f) ; Current tooltip open state from hover/focus.
           ;; set-tooltip-open! : boolean? -> void?
           ;;   Toggle tooltip class and bubble aria-hidden state.
           (define (set-tooltip-open! next-open?)
             (set! tooltip-open? (not (not next-open?)))
             (set-dom-node-attrs!
              node
              (list (cons 'data-we-widget "tooltip")
                    (cons 'class (if tooltip-open?
                                     (string-append tooltip-base-class " is-open")
                                     tooltip-base-class))))
             (set-dom-node-attrs!
              bubble-node
              (list (cons attr/role 'tooltip)
                    (cons 'id bubble-id)
                    (cons 'data-we-widget "tooltip-bubble")
                    (cons 'class "we-tooltip-bubble")
                    (cons 'aria-hidden (if tooltip-open? "false" "true")))))
           ;; set-trigger-describedby! : dom-node? string? -> void?
           ;;   Add aria-describedby to trigger child attrs while preserving other attrs.
           (define (set-trigger-describedby! child-node desc-id)
             (define old-attrs (dom-node-attrs child-node))
             (define filtered-attrs
               (let loop ([attrs old-attrs])
                 (cond
                   [(null? attrs) '()]
                   [(eq? (caar attrs) 'aria-describedby)
                    (loop (cdr attrs))]
                   [else
                    (cons (car attrs) (loop (cdr attrs)))])))
             (set-dom-node-attrs! child-node
                                  (append filtered-attrs
                                          (list (cons 'aria-describedby desc-id)))))
           (define child-node (build-node child-view register-cleanup!))
           (set-trigger-describedby! child-node bubble-id)
           (set-dom-node-on-change!
            node
            (lambda (event-key)
              (case (string->symbol event-key)
                [(mouseenter) (set-tooltip-open! #t)]
                [(mouseleave focusout Escape) (set-tooltip-open! #f)]
                [else (void)])))
           (set-dom-node-on-change! trigger-node (dom-node-on-change node))
           (backend-append-child! trigger-node child-node)
           ;; refresh-tooltip-structure! : -> void?
           ;;   Rebuild tooltip bubble regions from title/message/footer values.
           (define (refresh-tooltip-structure!)
             (define title-value   (maybe-observable-value raw-title))
             (define message-value (maybe-observable-value raw-message))
             (define footer-value  (maybe-observable-value raw-footer))
             (set-dom-node-text! bubble-node (value->text message-value))
             (set-dom-node-text! body-node (value->text message-value))
             (set-dom-node-text! header-node (if (eq? title-value #f) "" (value->text title-value)))
             (set-dom-node-text! footer-node (if (eq? footer-value #f) "" (value->text footer-value)))
             (backend-replace-children!
              bubble-node
              (append (if (eq? title-value #f) '() (list header-node))
                      (list body-node)
                      (if (eq? footer-value #f) '() (list footer-node)))))
           (cond
             [(obs? raw-message)
              (define (listener updated)
                (refresh-tooltip-structure!))
              (obs-observe! raw-message listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-message listener)))]
             [else (void)])
           (when (obs? raw-title)
             (define (title-listener _updated)
               (refresh-tooltip-structure!))
             (obs-observe! raw-title title-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-title title-listener))))
           (when (obs? raw-footer)
             (define (footer-listener _updated)
               (refresh-tooltip-structure!))
             (obs-observe! raw-footer footer-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-footer footer-listener))))
           (refresh-tooltip-structure!)
           (backend-append-child! node trigger-node)
           (backend-append-child! node bubble-node)
           (set-tooltip-open! #f)
           node]
          [(popover)
           (define raw-label (alist-ref (view-props v) 'label 'render))
           (define options   (alist-ref (view-props v) 'options 'render))
           (define raw-title  (options-ref options 'title #f))
           (define raw-footer (options-ref options 'footer #f))
           (define popover-placement
             (normalize-overlay-placement
              (alist-ref (view-props v) 'placement 'render)
              'bottom))
           (define popover-class
             (string-append "we-popover we-popover-" (symbol->string popover-placement)))
           (define panel-id (next-popover-panel-id))
           (define open? #f)
           (define node (dom-node 'div
                                  (list (cons 'data-we-widget "popover")
                                        (cons 'class popover-class))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define trigger-node (dom-node 'button
                                          (list (cons attr/role 'button)
                                                (cons 'data-we-widget "popover-trigger")
                                                (cons 'class "we-popover-trigger")
                                                (cons 'tabindex 0)
                                                (cons 'aria-haspopup "dialog")
                                                (cons 'aria-controls panel-id)
                                                (cons 'aria-expanded "false"))
                                          '()
                                          ""
                                          (lambda ()
                                            (set-open! (not open?)))
                                          (lambda (key)
                                            (case (string->symbol key)
                                              [(Escape)
                                               (set-open! #f)]
                                              [else
                                               (void)]))))
           (define backdrop-node (dom-node 'div
                                           (list (cons 'data-we-widget "popover-backdrop")
                                                 (cons 'aria-hidden "true")
                                                 (cons 'class "we-popover-backdrop"))
                                           '()
                                           #f
                                           (lambda ()
                                             (set-open! #f))
                                           #f))
           (define panel-node (dom-node 'div
                                        (list (cons attr/role 'dialog)
                                              (cons 'id panel-id)
                                              (cons 'tabindex -1)
                                              (cons 'aria-hidden "true")
                                              (cons 'data-we-widget "popover-panel")
                                              (cons 'class "we-popover-panel"))
                                        '()
                                        #f
                                        #f
                                        (lambda (key)
                                          (case (string->symbol key)
                                            [(Escape)
                                             (set-open! #f)]
                                            [else
                                             (void)]))))
           (define panel-body-node (dom-node 'div
                                             (list (cons 'data-we-widget "popover-body")
                                                   (cons 'class "we-popover-body"))
                                             '()
                                             #f
                                             #f
                                             #f))
           (define panel-header-node #f)
           (define panel-footer-node #f)
           ;; set-open! : boolean? -> void?
           ;;   Toggle panel visibility and aria state.
           (define (set-open! next-open?)
             (set! open? (not (not next-open?)))
             (set-dom-node-attrs!
              trigger-node
              (list (cons attr/role 'button)
                    (cons 'data-we-widget "popover-trigger")
                    (cons 'class "we-popover-trigger")
                    (cons 'tabindex 0)
                    (cons 'aria-haspopup "dialog")
                    (cons 'aria-controls panel-id)
                    (cons 'aria-expanded (if open? "true" "false"))))
             (set-dom-node-attrs!
              panel-node
              (list (cons attr/role 'dialog)
                    (cons 'id panel-id)
                    (cons 'tabindex -1)
                    (cons 'aria-hidden (if open? "false" "true"))
                    (cons 'data-we-widget "popover-panel")
                    (cons 'class (if open? "we-popover-panel is-open" "we-popover-panel"))))
             (set-dom-node-attrs!
              backdrop-node
              (list (cons 'data-we-widget "popover-backdrop")
                    (cons 'aria-hidden (if open? "false" "true"))
                    (cons 'class (if open? "we-popover-backdrop is-open" "we-popover-backdrop")))))
           ;; refresh-popover-structure! : -> void?
           ;;   Rebuild popover panel regions from title/body/footer values.
           (define (refresh-popover-structure!)
             (define title-value  (maybe-observable-value raw-title))
             (define footer-value (maybe-observable-value raw-footer))
             (set! panel-header-node
                   (if (eq? title-value #f)
                       #f
                       (dom-node 'div
                                 (list (cons 'data-we-widget "popover-header")
                                       (cons 'class "we-popover-header"))
                                 '()
                                 (value->text title-value)
                                 #f
                                 #f)))
             (set! panel-footer-node
                   (if (eq? footer-value #f)
                       #f
                       (dom-node 'div
                                 (list (cons 'data-we-widget "popover-footer")
                                       (cons 'class "we-popover-footer"))
                                 '()
                                 (value->text footer-value)
                                 #f
                                 #f)))
             (backend-replace-children!
              panel-node
              (append (if panel-header-node (list panel-header-node) '())
                      (list panel-body-node)
                      (if panel-footer-node (list panel-footer-node) '()))))
           (define (set-label! label-value)
             (set-dom-node-text! trigger-node (value->text label-value)))
           (cond
             [(obs? raw-label)
              (set-label! (obs-peek raw-label))
              (define (listener updated)
                (set-label! updated))
              (obs-observe! raw-label listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-label listener)))]
             [else
              (set-label! raw-label)])
           (for-each (lambda (child)
                       (append-view-child! panel-body-node child))
                     (view-children v))
           (when (obs? raw-title)
             (define (title-listener _updated)
               (refresh-popover-structure!))
             (obs-observe! raw-title title-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-title title-listener))))
           (when (obs? raw-footer)
             (define (footer-listener _updated)
               (refresh-popover-structure!))
             (obs-observe! raw-footer footer-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-footer footer-listener))))
           (refresh-popover-structure!)
           (backend-append-child! node trigger-node)
           (backend-append-child! node backdrop-node)
           (backend-append-child! node panel-node)
           node]
          [(card)
           (define raw-title  (alist-ref (view-props v) 'title  'render))
           (define raw-footer (alist-ref (view-props v) 'footer 'render))
           (define raw-variants (alist-ref (view-props v) 'variants 'render))
           (define options      (alist-ref (view-props v) 'options 'render))
           (define raw-subtitle (options-ref options 'subtitle #f))
           (define raw-media    (options-ref options 'media #f))
           (define raw-actions  (options-ref options 'actions '()))
           (define raw-tone      (options-ref options 'tone #f))
           (define raw-tone-style (options-ref options 'tone-style #f))
           (define variants (normalize-card-variants raw-variants))
           ;; card-class : list? any/c any/c -> string?
           ;;   Build card class string from variants and optional tone/tone-style.
           (define (card-class variants tone tone-style)
             (string-append
              (card-variant-class variants)
              (if tone
                  (string-append " we-card-tone-" (symbol->string tone))
                  "")
              (if tone-style
                  (string-append " we-card-tone-" (symbol->string tone-style))
                  "")))
           (define node (dom-node 'div
                                  (list (cons attr/role 'group)
                                        (cons 'data-we-widget "card")
                                        (cons 'class (card-class variants #f #f)))
                                  '()
                                  #f
                                  #f
                                  #f))
           (define body-node (dom-node 'div
                                       (list (cons 'data-we-widget "card-body")
                                             (cons 'class "we-card-body"))
                                       '()
                                       #f
                                       #f
                                       #f))
           (define media-node (dom-node 'div
                                        (list (cons 'data-we-widget "card-media")
                                              (cons 'class "we-card-media"))
                                        '()
                                        #f
                                        #f
                                        #f))
           (define actions-node (dom-node 'div
                                          (list (cons 'data-we-widget "card-actions")
                                                (cons 'class "we-card-actions"))
                                          '()
                                          #f
                                          #f
                                          #f))
           (define title-node #f)
           (define subtitle-node #f)
           (define footer-node #f)
           ;; refresh-card-structure! : -> void?
           ;;   Rebuild card children from current title/footer/options values and body node.
           (define (refresh-card-structure!)
             (define title-value    (maybe-observable-value raw-title))
             (define subtitle-value (maybe-observable-value raw-subtitle))
             (define footer-value   (maybe-observable-value raw-footer))
             (define media-value    (maybe-observable-value raw-media))
             (define actions-value  (maybe-observable-value raw-actions))
             (define tone-value      (normalize-card-tone (maybe-observable-value raw-tone)))
             (define tone-style-value (normalize-card-tone-style (maybe-observable-value raw-tone-style)))
             (define headerless?  (contains-equal? variants 'headerless))
             (set-dom-node-attrs!
              node
              (list (cons attr/role 'group)
                    (cons 'data-we-widget "card")
                    (cons 'class (card-class variants tone-value tone-style-value))))
             (set! title-node
                   (if (or headerless? (eq? title-value #f))
                       #f
                       (dom-node 'div
                                 (list (cons 'data-we-widget "card-header")
                                       (cons 'class "we-card-header"))
                                 '()
                                 (value->text title-value)
                                 #f
                                 #f)))
             (set! subtitle-node
                   (if (or headerless? (eq? subtitle-value #f))
                       #f
                       (dom-node 'small
                                 (list (cons 'data-we-widget "card-subtitle")
                                       (cons 'class "we-card-subtitle"))
                                 '()
                                 (value->text subtitle-value)
                                 #f
                                 #f)))
             (set! footer-node
                   (if (eq? footer-value #f)
                       #f
                       (dom-node 'div
                                 (list (cons 'data-we-widget "card-footer")
                                       (cons 'class "we-card-footer"))
                                 '()
                                 (value->text footer-value)
                                 #f
                                 #f)))
             (define header-nodes
               (append (if title-node (list title-node) '())
                       (if subtitle-node (list subtitle-node) '())))
             (define media-children
               (cond
                 [(eq? media-value #f)
                  '()]
                 [(view? media-value)
                  (list (build-node media-value register-cleanup!))]
                 [else
                  (list (dom-node 'span
                                  (list (cons 'data-we-widget "card-media-text")
                                        (cons 'class "we-card-media-text"))
                                  '()
                                  (value->text media-value)
                                  #f
                                  #f))]))
             (define action-items
               (if (list? actions-value) actions-value '()))
             (define action-children
               (map (lambda (entry)
                      (if (view? entry)
                          (build-node entry register-cleanup!)
                          (dom-node 'span
                                    (list (cons 'data-we-widget "card-action-text")
                                          (cons 'class "we-card-action-text"))
                                    '()
                                    (value->text entry)
                                    #f
                                    #f)))
                    action-items))
             (backend-replace-children! media-node media-children)
             (backend-replace-children! actions-node action-children)
             (define nodes (append header-nodes
                                   (if (null? media-children) '() (list media-node))
                                   (list body-node)
                                   (if (null? action-children) '() (list actions-node))
                                   (if footer-node (list footer-node) '())))
             (backend-replace-children! node nodes))
           (for-each (lambda (child)
                       (append-view-child! body-node child))
                     (view-children v))
           (when (obs? raw-title)
             (define (title-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-title title-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-title title-listener))))
           (when (obs? raw-footer)
             (define (footer-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-footer footer-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-footer footer-listener))))
           (when (obs? raw-subtitle)
             (define (subtitle-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-subtitle subtitle-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-subtitle subtitle-listener))))
           (when (obs? raw-media)
             (define (media-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-media media-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-media media-listener))))
           (when (obs? raw-actions)
             (define (actions-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-actions actions-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-actions actions-listener))))
           (when (obs? raw-tone)
             (define (tone-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-tone tone-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone tone-listener))))
           (when (obs? raw-tone-style)
             (define (tone-style-listener _updated)
               (refresh-card-structure!))
             (obs-observe! raw-tone-style tone-style-listener)
             (register-cleanup! (lambda () (obs-unobserve! raw-tone-style tone-style-listener))))
           (refresh-card-structure!)
           node]
          [(list-view)
           (define raw-entries (alist-ref (view-props v) 'entries   'render))
           (define key-proc    (alist-ref (view-props v) 'key       'render))
           (define make-view   (alist-ref (view-props v) 'make-view 'render))

           (define node (dom-node 'div (list (cons 'data-we-widget "list-view")
                                             (cons 'class          "we-list-view"))
                                  '() #f #f #f))
           (define items '())
           (define (render-from-entries entries)
             (set! items
                   (render-list-items node
                                      (ensure-list entries 'list-view "entries")
                                      items
                                      key-proc
                                      make-view
                                      register-cleanup!)))
           (cond
             [(obs? raw-entries)
              (render-from-entries (obs-peek raw-entries))
              (define (listener updated-entries)
                (render-from-entries updated-entries))
              (obs-observe! raw-entries listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-entries listener)))]
             [else
              (render-from-entries raw-entries)])
           node]
          [else
           (raise-arguments-error 'render
                                  "unknown view kind"
                                  "kind"
                                  kind)]))
      (set! root-node node)
      (set-dom-node-attrs! node (dom-node-attrs node))
      node)

    ;; render : view? [renderer?] -> renderer?
    ;;   Create a renderer for v, optionally as a child of parent.
    ;;   Optional parameter parent defaults to #f.
    (define (render v [parent #f])
      (define cleanups '())
      (define (register-cleanup! thunk)
        (set! cleanups (cons thunk cleanups)))
      (define node (build-node v register-cleanup!))
      (when parent
        (unless (renderer? parent)
          (raise-arguments-error 'render
                                 "expected #f or renderer?"
                                 "parent"
                                 parent))
        (backend-append-child! (renderer-root parent) node))
      (renderer-state node cleanups #f))

    (values renderer?
            render
            renderer-root
            renderer-destroy
            current-web-easy-warning-handler
            set-current-web-easy-warning-handler!
            call-with-web-easy-warning-handler
            dom-node-click!
            dom-node-change!
            dom-node-toggle!
            dom-node-select!
            dom-node-slide!
            dom-node-radio-select!
            dom-node-keydown!)))
