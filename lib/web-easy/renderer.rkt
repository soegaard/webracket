#lang webracket

;;;
;;; web-easy Renderer
;;;

;; Renderer runtime that builds and updates a DOM-like node tree from view values.
;;
;; Exports:
;;   renderer?                              Predicate for renderer values.
;;   render                                 Render a view into runtime nodes.
;;   renderer-root                          Return renderer root node.
;;   renderer-destroy                       Destroy renderer and run cleanups.
;;   current-web-easy-warning-handler       Current warning sink procedure.
;;   set-current-web-easy-warning-handler!  Replace warning sink procedure.
;;   call-with-web-easy-warning-handler     Run thunk with temporary warning sink.
;;   dom-node-click!                        Invoke node click callback when present.
;;   dom-node-change!                       Invoke node change callback when present.
;;   dom-node-toggle!                       Toggle checkbox state and invoke change callback.
;;   dom-node-select!                       Set selected value and invoke change callback.
;;   dom-node-slide!                        Set slider value and invoke change callback.
;;   dom-node-radio-select!                 Set radio selection and invoke change callback.
;;   dom-node-keydown!                      Invoke node keydown callback when present.
;;
;; Backend contract used by this renderer:
;;   view-node
;;   view-node?
;;   view-node-tag
;;   view-node-attrs
;;   view-node-children
;;   view-node-text
;;   view-node-on-click
;;   view-node-on-change
;;   set-view-node-tag!
;;   set-view-node-attrs!
;;   set-view-node-children!
;;   set-view-node-text!
;;   set-view-node-on-click!
;;   set-view-node-on-change!
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
       .we-container{width:min(1200px,calc(100% - 28px));max-width:1200px;margin:0 auto;}\
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
       .we-list-group{display:flex;flex-direction:column;align-self:flex-start;border:1px solid var(--we-border-soft,#bbb);border-radius:6px;overflow:visible;background:var(--we-bg,#fff);}\
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
    (define backend-set-view-node-attrs! set-view-node-attrs!) ; Backend attr setter (unwrapped).

    ;; renderer? : any/c -> boolean?
    ;;   Check whether v is a renderer state value.
    (define (renderer? v)
      (renderer-state? v))

    ;; renderer-root : renderer? -> view-node?
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

    ;; dom-node-click! : view-node? -> void?
    ;;   Invoke the node click callback when present.
    (define (attr-bool-true?/internal attrs key)
      (define p (assq key attrs))
      (and p
           (let ([v (cdr p)])
             (not (or (eq? v #f)
                      (equal? v "false"))))))

    ;; class-append-token/internal : any/c string? -> string?
    ;;   Append token to class string unless already present.
    (define (attr-ref/default/internal attrs key default)
      (define p (assq key attrs))
      (if p (cdr p) default))

    (define (class-append-token/internal current token)
      (define tokens
        (filter (lambda (s) (not (string=? s "")))
                (string-split (if (string? current) current "") " ")))
      (if (member token tokens)
          (string-join tokens " ")
          (string-join (append tokens (list token)) " ")))

    ;; attr-set/internal : list? symbol? any/c -> list?
    ;;   Return attrs with key set to value, replacing any earlier occurrence.
    (define (attr-set/internal attrs key value)
      (append (filter (lambda (entry) (not (eq? (car entry) key))) attrs)
              (list (cons key value))))

    ;; normalize-menu-item-attrs/internal : list? -> list?
    ;;   Ensure disabled menu items expose consistent class, aria, and tabindex attrs.
    (define (normalize-menu-item-attrs/internal attrs)
      (if (and (attr-bool-true?/internal attrs 'disabled)
               (equal? (attr-ref/default/internal attrs 'data-we-widget #f) "menu-item"))
          (let* ([class0 (attr-ref/default/internal attrs 'class "")]
                 [attrs1 (attr-set/internal attrs 'class (class-append-token/internal class0 "is-disabled"))]
                 [attrs2 (attr-set/internal attrs1 'aria-disabled "true")])
            (attr-set/internal attrs2 'tabindex -1))
          attrs))

    ;; dom-node-disabled? : view-node? -> boolean?
    ;;   Check whether node should ignore activation because it is disabled.
    (define (dom-node-disabled? n)
      (define attrs (view-node-attrs n))
      (or (attr-bool-true?/internal attrs 'disabled)
          (equal? (attr-ref/default/internal attrs 'aria-disabled #f) "true")))

    (define (dom-node-click! n)
      (define on-click (view-node-on-click n))
      (when (and on-click
                 (not (dom-node-disabled? n)))
        (on-click)))

    ;; dom-node-change! : view-node? any/c -> void?
    ;;   Update node text and invoke the change callback when present.
    (define (dom-node-change! n value)
      (define text-value (value->text value))
      (set-view-node-text! n text-value)
      (define on-change (view-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-toggle! : view-node? boolean? -> void?
    ;;   Update checkbox checked attribute and invoke the change callback.
    (define (dom-node-toggle! n checked?)
      (set-view-node-attrs!
       n
       (list (cons 'checked (not (not checked?)))))
      (define on-change (view-node-on-change n))
      (when on-change
        (on-change (not (not checked?)))))

    ;; dom-node-select! : view-node? any/c -> void?
    ;;   Update selected attribute and invoke the change callback.
    (define (dom-node-select! n selected)
      (define attrs (view-node-attrs n))
      (define option-pairs-pair (assq 'option-pairs attrs))
      (define option-pairs-value (if option-pairs-pair (cdr option-pairs-pair) '()))
      (set-view-node-attrs!
       n
       (list (cons 'choices (cdr (assq 'choices attrs)))
             (cons 'option-pairs option-pairs-value)
             (cons 'selected selected)))
      (define on-change (view-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-slide! : view-node? number? -> void?
    ;;   Update slider value attribute and invoke the change callback.
    (define (dom-node-slide! n value)
      (define attrs     (view-node-attrs n))
      (define min-pair  (assq 'min attrs))
      (define max-pair  (assq 'max attrs))
      (define min-value (if min-pair (cdr min-pair) 0))
      (define max-value (if max-pair (cdr max-pair) 100))
      (set-view-node-attrs!
       n
       (list (cons 'min min-value)
             (cons 'max max-value)
             (cons 'value value)))
      (define on-change (view-node-on-change n))
      (when on-change
        (on-change value)))

    ;; dom-node-radio-select! : view-node? any/c -> void?
    ;;   Update radio selected attribute and invoke the change callback.
    (define (dom-node-radio-select! n selected)
      (define attrs         (view-node-attrs n))
      (define choices-pair  (assq 'choices attrs))
      (define choices-value (if choices-pair (cdr choices-pair) '()))
      (define widget-pair   (assq 'data-we-widget attrs))
      (define class-pair    (assq 'class attrs))
      (set-view-node-attrs!
       n
       (append (if widget-pair (list widget-pair) '())
               (if class-pair (list class-pair) '())
               (list (cons 'choices choices-value)
                     (cons 'selected selected))))
      (define on-change (view-node-on-change n))
      (when on-change
        (on-change selected)))

    ;; dom-node-keydown! : view-node? string? -> void?
    ;;   Dispatch keydown payload for tabs, input Enter actions, and menu-item key activation.
    (define (dom-node-keydown! n key)
      (define on-click      (view-node-on-click n))
      (define on-change     (view-node-on-change n))
      (define on-enter-pair (assq 'on-enter-action (view-node-attrs n)))
      (define role-pair     (assq 'role (view-node-attrs n)))
      (define widget-pair   (assq 'data-we-widget (view-node-attrs n)))
      (define tag           (view-node-tag n))
      (when (and on-enter-pair
                 (procedure? (cdr on-enter-pair))
                 (string=? key "Enter"))
        ((cdr on-enter-pair)))
      (when (and on-click
                 (not (dom-node-disabled? n))
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

    ;; html-string-only-text-tag? : any/c -> boolean?
    ;;   Check whether tag is a primitive HTML element that keeps string-only text semantics.
    (define (html-string-only-text-tag? tag)
      (or (eq? tag 'title)
          (eq? tag 'style)))

    ;; html-string-only-text-value->string : any/c -> string?
    ;;   Convert allowed string-only element text values to DOM text.
    (define (html-string-only-text-value->string value)
      (if (string? value) value ""))

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
    (define primitive-dom-event-attr-keys
      '(on-click
        on-doubleclick
        on-contextmenu
        on-copy
        on-cut
        on-paste
        on-compositionstart
        on-compositionupdate
        on-compositionend
        on-keydown
        on-keyup
        on-focus
        on-blur
        on-focusin
        on-focusout
        on-input
        on-change
        on-beforeinput
        on-submit
        on-reset
        on-invalid
        on-wheel
        on-scroll
        on-drag
        on-dragstart
        on-dragend
        on-dragenter
        on-dragleave
        on-dragover
        on-drop
        on-touchstart
        on-touchmove
        on-touchend
        on-touchcancel
        on-load
        on-error
        on-abort
        on-animationstart
        on-animationend
        on-animationiteration
        on-transitionend
        on-mousedown
        on-mousemove
        on-mouseup
        on-mouseenter
        on-mouseleave
        on-mouseover
        on-mouseout
        on-pointerdown
        on-pointermove
        on-pointerup
        on-pointerenter
        on-pointerleave
        on-pointerover
        on-pointerout
        on-pointercancel
        on-gotpointercapture
        on-lostpointercapture
        on-loadeddata
        on-loadedmetadata
        on-canplay
        on-canplaythrough
        on-play
        on-playing
        on-pause
        on-ended
        on-timeupdate
        on-volumechange))

    (define (primitive-dom-event-attr-key? attr-key)
      (and (symbol? attr-key)
           (memq attr-key primitive-dom-event-attr-keys)))

    (define (internal-action-attr-key? attr-key)
      (or (eq? attr-key 'on-click-action)
          (eq? attr-key 'on-change-action)))

    (define (procedure-allowed-attr-key? attr-key)
      (or (internal-action-attr-key? attr-key)
          (eq? attr-key 'on-enter-action)
          (primitive-dom-event-attr-key? attr-key)))

    ;; ref-attr-key? : any/c -> boolean?
    ;;   Check whether attr-key is the special primitive ref attribute.
    (define (ref-attr-key? attr-key)
      (eq? attr-key 'ref))

    ;; primitive-event-attr-key->dom-event-name : symbol? -> string?
    ;;   Convert primitive event attr key like 'on-mouseup to DOM event name "mouseup".
    (define (primitive-event-attr-key->dom-event-name attr-key)
      (define s (symbol->string attr-key))
      (substring s 3))

    ;; event-handler-value->callback : symbol? any/c -> any/c
    ;;   Normalize primitive event attr value to a callback procedure or #f.
    (define (event-handler-value->callback attr-key value)
      (cond
        [(or (eq? value #f)
             (procedure? value))
         value]
        [else
         (emit-web-easy-warning!
          (string-append "web-easy: ignored non-procedure event handler "
                         (symbol->string attr-key)))
         #f]))

    ;; event-handlers-from-extra-attrs : list? -> list?
    ;;   Extract generic primitive DOM event callbacks from raw extra attrs.
    (define (event-handlers-from-extra-attrs extra-attrs/raw)
      (let loop ([remaining extra-attrs/raw]
                 [acc '()])
        (cond
          [(null? remaining)
           (reverse acc)]
          [(primitive-dom-event-attr-key? (caar remaining))
           (define callback
             (event-handler-value->callback (caar remaining)
                                            (maybe-observable-value (cdar remaining))))
           (loop (cdr remaining)
                 (if callback
                     (cons (cons (primitive-event-attr-key->dom-event-name (caar remaining))
                                 callback)
                           acc)
                     acc))]
          [else
           (loop (cdr remaining) acc)])))

    ;; ref-observable-from-extra-attrs : list? -> (or/c observable? #f)
    ;;   Return the last ref observable from raw extra attrs, or #f when absent.
    (define (ref-observable-from-extra-attrs extra-attrs/raw)
      (let loop ([remaining extra-attrs/raw]
                 [found #f])
        (cond
          [(null? remaining)
           found]
          [(ref-attr-key? (caar remaining))
           (loop (cdr remaining) (cdar remaining))]
          [else
           (loop (cdr remaining) found)])))

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
            [(ref-attr-key? (caar remaining))
             (loop (cdr remaining) acc)]
            [(primitive-dom-event-attr-key? (caar remaining))
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

    ;; maybe-observable-value : any/c -> any/c
    ;;   Read observable content when v is observable, otherwise return v.
    (define (maybe-observable-value v)
      (if (obs? v) (obs-peek v) v))

    ;; node-ref-mounted-value : view-node? -> any/c
    ;;   Return browser-native node when available, otherwise the backend node itself.
    (define (node-ref-mounted-value n)
      (define native (view-node-native n))
      (if native
          native
          n))

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

    ;; icon-node : string? string? -> view-node?
    ;;   Construct icon span node with data-we-widget/class and text content.
    (define (icon-node widget class-name icon-text)
      (view-node 'span
                (list (cons 'data-we-widget widget)
                      (cons 'class class-name)
                      (cons 'aria-hidden "true"))
                '()
                icon-text
                #f
                #f))

    ;; build-node : view? (-> (-> void?) void?) -> view-node?
    ;;   Build a view-node tree from v and register lifecycle cleanups.
    (define (build-node v register-cleanup!)
      (define kind      (view-kind v))
      (define root-node #f)
      ;; append-view-child! : view-node? view? -> void?
      ;;   Append child view to parent, flattening Fragment children without wrapper nodes.
      (define (append-view-child! parent child)
        (if (and (view? child)
                 (eq? (view-kind child) 'fragment))
            (for-each (lambda (grand-child)
                        (append-view-child! parent grand-child))
                      (view-children child))
            (backend-append-child! parent (build-node child register-cleanup!))))
      ;; set-view-node-attrs! : view-node? list? -> void?
      ;;   Set attrs, applying view-level style hooks only on this view's root node.
      (define (set-view-node-attrs! n attrs)
        (backend-set-view-node-attrs!
         n (normalize-menu-item-attrs/internal
            (if (and root-node (eq? n root-node))
                (merge-root-extra-attrs v attrs)
                attrs))))
      (define node
        (case kind
          [(window)
           (define node       (view-node 'div (list (cons attr/role 'window)
                                                   (cons 'data-we-widget "window")) '() #f #f #f))
           (define style-node (view-node 'style '() '() shared-style-text #f #f))
           (validate-window-base-order! v)
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           (backend-append-child! node style-node)
           node]
          [(vpanel)
           (define node (view-node 'div (list (cons 'data-we-widget "vpanel")
                                             (cons 'class          "we-vpanel")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(hpanel)
           (define node (view-node 'div (list (cons 'data-we-widget "hpanel")
                                             (cons 'class          "we-hpanel")) '() #f #f #f))
           (for-each (lambda (child)
                       (append-view-child! node child))
                     (view-children v))
           node]
          [(fragment)
           (error 'Fragment "cannot be rendered directly; use as child content")]
          [(raw-text)
           (define raw-value (alist-ref (view-props v) 'value 'render))
           (define node (view-node 'text '() '() "" #f #f))
           (define (set-text! value0)
             (set-view-node-text! node (value->text value0)))
           (cond
             [(obs? raw-value)
              (set-text! (obs-peek raw-value))
              (define (value-listener updated-value)
                (set-text! updated-value))
              (obs-observe! raw-value value-listener)
              (register-cleanup! (lambda ()
                                   (obs-unobserve! raw-value value-listener)))]
             [else
              (set-text! raw-value)])
           node]
          [(html-element)
           (define raw-tag   (alist-ref (view-props v) 'tag   'render))
           (define raw-value (alist-ref (view-props v) 'value 'render))
           (define initial-tag
             (let ([v0 (maybe-observable-value raw-tag)])
               (if (symbol? v0) v0 'div)))
           (define node (view-node initial-tag '() '() "" #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
           (define ref-obs (ref-observable-from-extra-attrs extra-attrs/raw))
           (define (refresh-ref!)
             (when ref-obs
               (obs-set! ref-obs (node-ref-mounted-value node))))
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
             (set-view-node-attrs! node (attr-remove-key (view-node-attrs node) 'class)))
          (define (set-tag! tag-value)
             (set-view-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div))
             (refresh-ref!))
          (define (set-text! value0)
             (set-view-node-text! node (value->text value0))
             (refresh-ref!))
          (define (set-string-only-text! value0)
            (set-view-node-text! node
                                (html-string-only-text-value->string value0))
            (refresh-ref!))
          (define (emit-string-only-text-update-ignored! tag value0)
            (define msg
              (string-append "web-easy: ignored non-string observable text update for "
                             (symbol->string tag)))
            (emit-web-easy-warning! msg)
            (js-log msg))
          (define (current-text-tag)
            (view-node-tag node))
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
             (define initial-text-tag (current-text-tag))
             (if (html-string-only-text-tag? initial-text-tag)
                 (let ([initial-value (obs-peek raw-value)])
                   (if (or (string? initial-value)
                           (eq? initial-value #f))
                       (set-string-only-text! initial-value)
                       (emit-string-only-text-update-ignored! initial-text-tag initial-value)))
                 (set-text! (obs-peek raw-value)))
             (define (listener updated)
               (define text-tag (current-text-tag))
               (if (html-string-only-text-tag? text-tag)
                   (if (or (string? updated)
                           (eq? updated #f))
                       (set-string-only-text! updated)
                       (emit-string-only-text-update-ignored! text-tag updated))
                   (set-text! updated)))
              (obs-observe! raw-value listener)
              (register-cleanup! (lambda () (obs-unobserve! raw-value listener)))]
             [else
              (set-text! raw-value)])
           (define (refresh-root-event-handlers!)
             (set-view-node-event-handlers! node
                                           (event-handlers-from-extra-attrs extra-attrs/raw)))
           (define (refresh-root-callbacks!)
             (set-view-node-on-click! node (callback-from-action-attr 'on-click-action))
             (set-view-node-on-change! node (callback-from-action-attr 'on-change-action))
             (refresh-root-event-handlers!))
           (refresh-root-callbacks!)
           (refresh-ref!)
           (when ref-obs
             (register-cleanup! (lambda ()
                                  (obs-set! ref-obs #f))))
           (for-each
            (lambda (entry)
              (when (and (pair? entry)
                         (symbol? (car entry))
                         (obs? (cdr entry))
                         (not (ref-attr-key? (car entry))))
                (define attr-obs (cdr entry))
                (define (attr-listener _updated)
                  (if (valid-observable-attr-update? (car entry) _updated)
                      (let ()
                        (refresh-root-attrs!)
                        (refresh-root-callbacks!))
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
           (define node (view-node initial-tag '() '() #f #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
           (define ref-obs (ref-observable-from-extra-attrs extra-attrs/raw))
           (define (refresh-ref!)
             (when ref-obs
               (obs-set! ref-obs (node-ref-mounted-value node))))
           (define (callback-from-action-attr attr-key)
             (define p (assq attr-key extra-attrs/raw))
             (if p
                 (let ([v0 (maybe-observable-value (cdr p))])
                   (if (procedure? v0) v0 #f))
                 #f))
           (define (refresh-root-callbacks!)
             (set-view-node-on-click! node (callback-from-action-attr 'on-click-action))
             (set-view-node-on-change! node (callback-from-action-attr 'on-change-action))
             (refresh-root-event-handlers!))
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
             (set-view-node-attrs! node (attr-remove-key (view-node-attrs node) 'class)))
          (define (set-tag! tag-value)
             (set-view-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div))
             (refresh-ref!))
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
           (define (refresh-root-event-handlers!)
             (set-view-node-event-handlers! node
                                           (event-handlers-from-extra-attrs extra-attrs/raw)))
           (refresh-root-callbacks!)
           (refresh-ref!)
           (when ref-obs
             (register-cleanup! (lambda ()
                                  (obs-set! ref-obs #f))))
           (for-each
            (lambda (entry)
              (when (and (pair? entry)
                         (symbol? (car entry))
                         (obs? (cdr entry))
                         (not (ref-attr-key? (car entry))))
                (define attr-obs (cdr entry))
                (define (attr-listener updated)
                  (if (valid-observable-attr-update? (car entry) updated)
                      (let ()
                        (refresh-root-attrs!)
                        (refresh-root-callbacks!))
                      (void)))
                (obs-observe! attr-obs attr-listener)
                (register-cleanup! (lambda () (obs-unobserve! attr-obs attr-listener)))))
            extra-attrs/raw)
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
           (define node (view-node initial-tag '() '() #f #f #f))
           (define extra-attrs/raw (props-extra-attrs (view-props v)))
           (define ref-obs (ref-observable-from-extra-attrs extra-attrs/raw))
           (define (refresh-ref!)
             (when ref-obs
               (obs-set! ref-obs (node-ref-mounted-value node))))
           (define (callback-from-action-attr attr-key)
             (define p (assq attr-key extra-attrs/raw))
             (if p
                 (let ([v0 (maybe-observable-value (cdr p))])
                   (if (procedure? v0) v0 #f))
                 #f))
           (define (dom-node-attr-ref n key [default #f])
             (define p (assq key (view-node-attrs n)))
             (if p
                 (cdr p)
                 default))
           (define (find-node-by-widget root widget-name)
             (if (equal? (dom-node-attr-ref root 'data-we-widget #f)
                         widget-name)
                 root
                 (let loop ([rest (view-node-children root)])
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
                   (cons 'view-node-children view-node-children)
                   (cons 'view-node-on-click view-node-on-click)
                   (cons 'set-view-node-on-click! set-view-node-on-click!)
                   (cons 'backend-replace-children! backend-replace-children!)
                   (cons 'build-node
                         (lambda (child)
                           (build-node child register-cleanup!)))
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
             (set-view-node-attrs! node (attr-remove-key (view-node-attrs node) 'class)))
           (define (set-tag! tag-value)
             (set-view-node-tag! node
                                (if (symbol? tag-value)
                                    tag-value
                                    'div))
             (refresh-ref!))
           (define (refresh-root-event-handlers!)
             (set-view-node-event-handlers! node
                                           (event-handlers-from-extra-attrs extra-attrs/raw)))
           (define (refresh-root-callbacks!)
             (set-view-node-on-click! node (callback-from-action-attr 'on-click-action))
             (set-view-node-on-change! node (callback-from-action-attr 'on-change-action))
             (refresh-root-event-handlers!))
           (refresh-root-callbacks!)
           (refresh-ref!)
           (when ref-obs
             (register-cleanup! (lambda ()
                                  (obs-set! ref-obs #f))))
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
                         (obs? (cdr entry))
                         (not (ref-attr-key? (car entry))))
                (define attr-obs (cdr entry))
                (define (attr-listener updated)
                  (if (valid-observable-attr-update? (car entry) updated)
                      (let ()
                        (refresh-root-attrs!)
                        (refresh-root-callbacks!))
                      (void)))
                (obs-observe! attr-obs attr-listener)
                (register-cleanup! (lambda () (obs-unobserve! attr-obs attr-listener)))))
            extra-attrs/raw)
           node]
          [else
           (raise-arguments-error 'render
                                  "unknown view kind"
                                  "kind"
                                  kind)]))
      (set! root-node node)
      (set-view-node-attrs! node (view-node-attrs node))
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
