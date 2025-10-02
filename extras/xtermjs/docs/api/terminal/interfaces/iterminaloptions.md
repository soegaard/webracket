---
title: ITerminalOptions
category: API-interfaces
layout: docs
---


# Interface: ITerminalOptions

An object containing options for the terminal.

## Hierarchy

* **ITerminalOptions**

## Index

### Properties

* [allowProposedApi]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-allowproposedapi)
* [allowTransparency]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-allowtransparency)
* [altClickMovesCursor]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-altclickmovescursor)
* [convertEol]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-converteol)
* [cursorBlink]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-cursorblink)
* [cursorInactiveStyle]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-cursorinactivestyle)
* [cursorStyle]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-cursorstyle)
* [cursorWidth]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-cursorwidth)
* [customGlyphs]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-customglyphs)
* [disableStdin]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-disablestdin)
* [documentOverride]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-documentoverride)
* [drawBoldTextInBrightColors]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-drawboldtextinbrightcolors)
* [fastScrollModifier]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fastscrollmodifier)
* [fastScrollSensitivity]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fastscrollsensitivity)
* [fontFamily]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fontfamily)
* [fontSize]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fontsize)
* [fontWeight]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fontweight)
* [fontWeightBold]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-fontweightbold)
* [ignoreBracketedPasteMode]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-ignorebracketedpastemode)
* [letterSpacing]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-letterspacing)
* [lineHeight]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-lineheight)
* [linkHandler]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-linkhandler)
* [logLevel]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-loglevel)
* [logger]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-logger)
* [macOptionClickForcesSelection]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-macoptionclickforcesselection)
* [macOptionIsMeta]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-macoptionismeta)
* [minimumContrastRatio]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-minimumcontrastratio)
* [overviewRulerWidth]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-overviewrulerwidth)
* [rescaleOverlappingGlyphs]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-rescaleoverlappingglyphs)
* [rightClickSelectsWord]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-rightclickselectsword)
* [screenReaderMode]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-screenreadermode)
* [scrollOnUserInput]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-scrollonuserinput)
* [scrollSensitivity]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-scrollsensitivity)
* [scrollback]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-scrollback)
* [smoothScrollDuration]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-smoothscrollduration)
* [tabStopWidth]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-tabstopwidth)
* [theme]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-theme)
* [windowOptions]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-windowoptions)
* [windowsMode]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-windowsmode)
* [windowsPty]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-windowspty)
* [wordSeparator]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-wordseparator)

## Properties

### `Optional` allowProposedApi

• **allowProposedApi**? : *boolean*

*Defined in [xterm.d.ts:32](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L32)*

Whether to allow the use of proposed API. When false, any usage of APIs
marked as experimental/proposed will throw an error. The default is
false.

___

### `Optional` allowTransparency

• **allowTransparency**? : *boolean*

*Defined in [xterm.d.ts:40](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L40)*

Whether background should support non-opaque color. It must be set before
executing the `Terminal.open()` method and can't be changed later without
executing it again. Note that enabling this can negatively impact
performance.

___

### `Optional` altClickMovesCursor

• **altClickMovesCursor**? : *boolean*

*Defined in [xterm.d.ts:46](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L46)*

If enabled, alt + click will move the prompt cursor to position
underneath the mouse. The default is true.

___

### `Optional` convertEol

• **convertEol**? : *boolean*

*Defined in [xterm.d.ts:56](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L56)*

When enabled the cursor will be set to the beginning of the next line
with every new line. This is equivalent to sending '\r\n' for each '\n'.
Normally the termios settings of the underlying PTY deals with the
translation of '\n' to '\r\n' and this setting should not be used. If you
deal with data from a non-PTY related source, this settings might be
useful.

___

### `Optional` cursorBlink

• **cursorBlink**? : *boolean*

*Defined in [xterm.d.ts:61](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L61)*

Whether the cursor blinks.

___

### `Optional` cursorInactiveStyle

• **cursorInactiveStyle**? : *"outline" | "block" | "bar" | "underline" | "none"*

*Defined in [xterm.d.ts:76](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L76)*

The style of the cursor when the terminal is not focused.

___

### `Optional` cursorStyle

• **cursorStyle**? : *"block" | "underline" | "bar"*

*Defined in [xterm.d.ts:66](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L66)*

The style of the cursor when the terminal is focused.

___

### `Optional` cursorWidth

• **cursorWidth**? : *number*

*Defined in [xterm.d.ts:71](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L71)*

The width of the cursor in CSS pixels when `cursorStyle` is set to 'bar'.

___

### `Optional` customGlyphs

• **customGlyphs**? : *boolean*

*Defined in [xterm.d.ts:85](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L85)*

Whether to draw custom glyphs for block element and box drawing
characters instead of using the font. This should typically result in
better rendering with continuous lines, even when line height and letter
spacing is used. Note that this doesn't work with the DOM renderer which
renders all characters using the font. The default is true.

___

### `Optional` disableStdin

• **disableStdin**? : *boolean*

*Defined in [xterm.d.ts:90](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L90)*

Whether input should be disabled.

___

### `Optional` documentOverride

• **documentOverride**? : *any | null*

*Defined in [xterm.d.ts:101](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L101)*

A {@link Document} to use instead of the one that xterm.js was attached
to. The purpose of this is to improve support in multi-window
applications where HTML elements may be references across multiple
windows which can cause problems with `instanceof`.

The type is `any` because using `Document` can cause TS to have
performance/compiler problems.

___

### `Optional` drawBoldTextInBrightColors

• **drawBoldTextInBrightColors**? : *boolean*

*Defined in [xterm.d.ts:106](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L106)*

Whether to draw bold text in bright colors. The default is true.

___

### `Optional` fastScrollModifier

• **fastScrollModifier**? : *"none" | "alt" | "ctrl" | "shift"*

*Defined in [xterm.d.ts:111](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L111)*

The modifier key hold to multiply scroll speed.

___

### `Optional` fastScrollSensitivity

• **fastScrollSensitivity**? : *number*

*Defined in [xterm.d.ts:116](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L116)*

The scroll speed multiplier used for fast scrolling.

___

### `Optional` fontFamily

• **fontFamily**? : *string*

*Defined in [xterm.d.ts:126](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L126)*

The font family used to render text.

___

### `Optional` fontSize

• **fontSize**? : *number*

*Defined in [xterm.d.ts:121](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L121)*

The font size used to render text.

___

### `Optional` fontWeight

• **fontWeight**? : *[FontWeight]({% link _docs/api/terminal/modules/xterm.md %}#fontweight)*

*Defined in [xterm.d.ts:131](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L131)*

The font weight used to render non-bold text.

___

### `Optional` fontWeightBold

• **fontWeightBold**? : *[FontWeight]({% link _docs/api/terminal/modules/xterm.md %}#fontweight)*

*Defined in [xterm.d.ts:136](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L136)*

The font weight used to render bold text.

___

### `Optional` ignoreBracketedPasteMode

• **ignoreBracketedPasteMode**? : *boolean*

*Defined in [xterm.d.ts:143](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L143)*

Whether to ignore the bracketed paste mode. When true, this will always
paste without the `\x1b[200~` and `\x1b[201~` sequences, even when the
shell enables bracketed mode.

___

### `Optional` letterSpacing

• **letterSpacing**? : *number*

*Defined in [xterm.d.ts:148](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L148)*

The spacing in whole pixels between characters.

___

### `Optional` lineHeight

• **lineHeight**? : *number*

*Defined in [xterm.d.ts:153](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L153)*

The line height used to render text.

___

### `Optional` linkHandler

• **linkHandler**? : *[ILinkHandler]({% link _docs/api/terminal/interfaces/ilinkhandler.md %}) | null*

*Defined in [xterm.d.ts:166](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L166)*

The handler for OSC 8 hyperlinks. Links will use the `confirm` browser
API with a strongly worded warning if no link handler is set.

When setting this, consider the security of users opening these links,
at a minimum there should be a tooltip or a prompt when hovering or
activating the link respectively. An example of what might be possible is
a terminal app writing link in the form `javascript:...` that runs some
javascript, a safe approach to prevent that is to validate the link
starts with http(s)://.

___

### `Optional` logLevel

• **logLevel**? : *[LogLevel]({% link _docs/api/terminal/modules/xterm.md %}#loglevel)*

*Defined in [xterm.d.ts:179](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L179)*

What log level to use, this will log for all levels below and including
what is set:

1. trace
2. debug
3. info (default)
4. warn
5. error
6. off

___

### `Optional` logger

• **logger**? : *[ILogger]({% link _docs/api/terminal/interfaces/ilogger.md %}) | null*

*Defined in [xterm.d.ts:184](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L184)*

A logger to use instead of `console`.

___

### `Optional` macOptionClickForcesSelection

• **macOptionClickForcesSelection**? : *boolean*

*Defined in [xterm.d.ts:198](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L198)*

Whether holding a modifier key will force normal selection behavior,
regardless of whether the terminal is in mouse events mode. This will
also prevent mouse events from being emitted by the terminal. For
example, this allows you to use xterm.js' regular selection inside tmux
with mouse mode enabled.

___

### `Optional` macOptionIsMeta

• **macOptionIsMeta**? : *boolean*

*Defined in [xterm.d.ts:189](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L189)*

Whether to treat option as the meta key.

___

### `Optional` minimumContrastRatio

• **minimumContrastRatio**? : *number*

*Defined in [xterm.d.ts:210](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L210)*

The minimum contrast ratio for text in the terminal, setting this will
change the foreground color dynamically depending on whether the contrast
ratio is met. Example values:

- 1: The default, do nothing.
- 4.5: Minimum for WCAG AA compliance.
- 7: Minimum for WCAG AAA compliance.
- 21: White on black or black on white.

___

### `Optional` overviewRulerWidth

• **overviewRulerWidth**? : *number*

*Defined in [xterm.d.ts:329](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L329)*

The width, in pixels, of the canvas for the overview ruler. The overview
ruler will be hidden when not set.

___

### `Optional` rescaleOverlappingGlyphs

• **rescaleOverlappingGlyphs**? : *boolean*

*Defined in [xterm.d.ts:227](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L227)*

Whether to rescale glyphs horizontally that are a single cell wide but
have glyphs that would overlap following cell(s). This typically happens
for ambiguous width characters (eg. the roman numeral characters U+2160+)
which aren't featured in monospace fonts. This is an important feature
for achieving GB18030 compliance.

The following glyphs will never be rescaled:

- Emoji glyphs
- Powerline glyphs
- Nerd font glyphs

Note that this doesn't work with the DOM renderer. The default is false.

___

### `Optional` rightClickSelectsWord

• **rightClickSelectsWord**? : *boolean*

*Defined in [xterm.d.ts:233](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L233)*

Whether to select the word under the cursor on right click, this is
standard behavior in a lot of macOS applications.

___

### `Optional` screenReaderMode

• **screenReaderMode**? : *boolean*

*Defined in [xterm.d.ts:240](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L240)*

Whether screen reader support is enabled. When on this will expose
supporting elements in the DOM to support NVDA on Windows and VoiceOver
on macOS.

___

### `Optional` scrollOnUserInput

• **scrollOnUserInput**? : *boolean*

*Defined in [xterm.d.ts:253](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L253)*

Whether to scroll to the bottom whenever there is some user input. The
default is true.

___

### `Optional` scrollSensitivity

• **scrollSensitivity**? : *number*

*Defined in [xterm.d.ts:258](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L258)*

The scrolling speed multiplier used for adjusting normal scrolling speed.

___

### `Optional` scrollback

• **scrollback**? : *number*

*Defined in [xterm.d.ts:247](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L247)*

The amount of scrollback in the terminal. Scrollback is the amount of
rows that are retained when lines are scrolled beyond the initial
viewport. Defaults to 1000.

___

### `Optional` smoothScrollDuration

• **smoothScrollDuration**? : *number*

*Defined in [xterm.d.ts:264](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L264)*

The duration to smoothly scroll between the origin and the target in
milliseconds. Set to 0 to disable smooth scrolling and scroll instantly.

___

### `Optional` tabStopWidth

• **tabStopWidth**? : *number*

*Defined in [xterm.d.ts:269](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L269)*

The size of tab stops in the terminal.

___

### `Optional` theme

• **theme**? : *[ITheme]({% link _docs/api/terminal/interfaces/itheme.md %})*

*Defined in [xterm.d.ts:274](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L274)*

The color theme of the terminal.

___

### `Optional` windowOptions

• **windowOptions**? : *[IWindowOptions]({% link _docs/api/terminal/interfaces/iwindowoptions.md %})*

*Defined in [xterm.d.ts:323](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L323)*

Enable various window manipulation and report features.
All features are disabled by default for security reasons.

___

### `Optional` windowsMode

• **windowsMode**? : *boolean*

*Defined in [xterm.d.ts:293](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L293)*

Whether "Windows mode" is enabled. Because Windows backends winpty and
conpty operate by doing line wrapping on their side, xterm.js does not
have access to wrapped lines. When Windows mode is enabled the following
changes will be in effect:

- Reflow is disabled.
- Lines are assumed to be wrapped if the last character of the line is
  not whitespace.

When using conpty on Windows 11 version >= 21376, it is recommended to
disable this because native text wrapping sequences are output correctly
thanks to https://github.com/microsoft/terminal/issues/405

**`deprecated`** Use [windowsPty]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-windowspty). This value will be ignored if
windowsPty is set.

___

### `Optional` windowsPty

• **windowsPty**? : *[IWindowsPty]({% link _docs/api/terminal/interfaces/iwindowspty.md %})*

*Defined in [xterm.d.ts:311](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L311)*

Compatibility information when the pty is known to be hosted on Windows.
Setting this will turn on certain heuristics/workarounds depending on the
values:

- `if (backend !== undefined || buildNumber !== undefined)`
  - When increasing the rows in the terminal, the amount increased into
    the scrollback. This is done because ConPTY does not behave like
    expect scrollback to come back into the viewport, instead it makes
    empty rows at of the viewport. Not having this behavior can result in
    missing data as the rows get replaced.
- `if !(backend === 'conpty' && buildNumber >= 21376)`
  - Reflow is disabled
  - Lines are assumed to be wrapped if the last character of the line is
    not whitespace.

___

### `Optional` wordSeparator

• **wordSeparator**? : *string*

*Defined in [xterm.d.ts:317](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L317)*

A string containing all characters that are considered word separated by
the double click to select work logic.
