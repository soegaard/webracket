---
title: "xterm"
category: API-modules
layout: docs
---


# Module: "xterm"

**`license`** MIT

This contains the type declarations for the xterm.js library. Note that
some interfaces differ between this file and the actual implementation in
src/, that's because this file declares the *public* API which is intended
to be stable and consumed by external programs.

## Index

### Classes

* [Terminal]({% link _docs/api/terminal/classes/terminal.md %})

### Interfaces

* [IBuffer]({% link _docs/api/terminal/interfaces/ibuffer.md %})
* [IBufferCell]({% link _docs/api/terminal/interfaces/ibuffercell.md %})
* [IBufferCellPosition]({% link _docs/api/terminal/interfaces/ibuffercellposition.md %})
* [IBufferElementProvider]({% link _docs/api/terminal/interfaces/ibufferelementprovider.md %})
* [IBufferLine]({% link _docs/api/terminal/interfaces/ibufferline.md %})
* [IBufferNamespace]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %})
* [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %})
* [IDecoration]({% link _docs/api/terminal/interfaces/idecoration.md %})
* [IDecorationOptions]({% link _docs/api/terminal/interfaces/idecorationoptions.md %})
* [IDecorationOverviewRulerOptions]({% link _docs/api/terminal/interfaces/idecorationoverviewruleroptions.md %})
* [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})
* [IDisposableWithEvent]({% link _docs/api/terminal/interfaces/idisposablewithevent.md %})
* [IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})
* [IFunctionIdentifier]({% link _docs/api/terminal/interfaces/ifunctionidentifier.md %})
* [ILink]({% link _docs/api/terminal/interfaces/ilink.md %})
* [ILinkDecorations]({% link _docs/api/terminal/interfaces/ilinkdecorations.md %})
* [ILinkHandler]({% link _docs/api/terminal/interfaces/ilinkhandler.md %})
* [ILinkProvider]({% link _docs/api/terminal/interfaces/ilinkprovider.md %})
* [ILocalizableStrings]({% link _docs/api/terminal/interfaces/ilocalizablestrings.md %})
* [ILogger]({% link _docs/api/terminal/interfaces/ilogger.md %})
* [IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})
* [IModes]({% link _docs/api/terminal/interfaces/imodes.md %})
* [IParser]({% link _docs/api/terminal/interfaces/iparser.md %})
* [ITerminalAddon]({% link _docs/api/terminal/interfaces/iterminaladdon.md %})
* [ITerminalInitOnlyOptions]({% link _docs/api/terminal/interfaces/iterminalinitonlyoptions.md %})
* [ITerminalOptions]({% link _docs/api/terminal/interfaces/iterminaloptions.md %})
* [ITheme]({% link _docs/api/terminal/interfaces/itheme.md %})
* [IUnicodeHandling]({% link _docs/api/terminal/interfaces/iunicodehandling.md %})
* [IUnicodeVersionProvider]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %})
* [IViewportRange]({% link _docs/api/terminal/interfaces/iviewportrange.md %})
* [IViewportRangePosition]({% link _docs/api/terminal/interfaces/iviewportrangeposition.md %})
* [IWindowOptions]({% link _docs/api/terminal/interfaces/iwindowoptions.md %})
* [IWindowsPty]({% link _docs/api/terminal/interfaces/iwindowspty.md %})

### Type aliases

* [FontWeight]({% link _docs/api/terminal/modules/xterm.md %}#fontweight)
* [LogLevel]({% link _docs/api/terminal/modules/xterm.md %}#loglevel)

## Type aliases

###  FontWeight

Ƭ **FontWeight**: *"normal" | "bold" | "100" | "200" | "300" | "400" | "500" | "600" | "700" | "800" | "900" | number*

*Defined in [xterm.d.ts:16](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L16)*

A string or number representing text font weight.

___

###  LogLevel

Ƭ **LogLevel**: *"trace" | "debug" | "info" | "warn" | "error" | "off"*

*Defined in [xterm.d.ts:21](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L21)*

A string representing log level.
