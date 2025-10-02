---
title: Terminal
category: API-classes
layout: docs
---


# Class: Terminal

The class that represents an xterm.js terminal.

## Hierarchy

* **Terminal**

## Implements

* [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})

## Index

### Constructors

* [constructor]({% link _docs/api/terminal/classes/terminal.md %}#constructor)

### Properties

* [buffer]({% link _docs/api/terminal/classes/terminal.md %}#readonly-buffer)
* [cols]({% link _docs/api/terminal/classes/terminal.md %}#readonly-cols)
* [element]({% link _docs/api/terminal/classes/terminal.md %}#readonly-element)
* [markers]({% link _docs/api/terminal/classes/terminal.md %}#readonly-markers)
* [modes]({% link _docs/api/terminal/classes/terminal.md %}#readonly-modes)
* [onBell]({% link _docs/api/terminal/classes/terminal.md %}#onbell)
* [onBinary]({% link _docs/api/terminal/classes/terminal.md %}#onbinary)
* [onCursorMove]({% link _docs/api/terminal/classes/terminal.md %}#oncursormove)
* [onData]({% link _docs/api/terminal/classes/terminal.md %}#ondata)
* [onKey]({% link _docs/api/terminal/classes/terminal.md %}#onkey)
* [onLineFeed]({% link _docs/api/terminal/classes/terminal.md %}#onlinefeed)
* [onRender]({% link _docs/api/terminal/classes/terminal.md %}#onrender)
* [onResize]({% link _docs/api/terminal/classes/terminal.md %}#onresize)
* [onScroll]({% link _docs/api/terminal/classes/terminal.md %}#onscroll)
* [onSelectionChange]({% link _docs/api/terminal/classes/terminal.md %}#onselectionchange)
* [onTitleChange]({% link _docs/api/terminal/classes/terminal.md %}#ontitlechange)
* [onWriteParsed]({% link _docs/api/terminal/classes/terminal.md %}#onwriteparsed)
* [options]({% link _docs/api/terminal/classes/terminal.md %}#options)
* [parser]({% link _docs/api/terminal/classes/terminal.md %}#readonly-parser)
* [rows]({% link _docs/api/terminal/classes/terminal.md %}#readonly-rows)
* [textarea]({% link _docs/api/terminal/classes/terminal.md %}#readonly-textarea)
* [unicode]({% link _docs/api/terminal/classes/terminal.md %}#readonly-unicode)
* [strings]({% link _docs/api/terminal/classes/terminal.md %}#static-strings)

### Methods

* [attachCustomKeyEventHandler]({% link _docs/api/terminal/classes/terminal.md %}#attachcustomkeyeventhandler)
* [attachCustomWheelEventHandler]({% link _docs/api/terminal/classes/terminal.md %}#attachcustomwheeleventhandler)
* [blur]({% link _docs/api/terminal/classes/terminal.md %}#blur)
* [clear]({% link _docs/api/terminal/classes/terminal.md %}#clear)
* [clearSelection]({% link _docs/api/terminal/classes/terminal.md %}#clearselection)
* [clearTextureAtlas]({% link _docs/api/terminal/classes/terminal.md %}#cleartextureatlas)
* [deregisterCharacterJoiner]({% link _docs/api/terminal/classes/terminal.md %}#deregistercharacterjoiner)
* [dispose]({% link _docs/api/terminal/classes/terminal.md %}#dispose)
* [focus]({% link _docs/api/terminal/classes/terminal.md %}#focus)
* [getSelection]({% link _docs/api/terminal/classes/terminal.md %}#getselection)
* [getSelectionPosition]({% link _docs/api/terminal/classes/terminal.md %}#getselectionposition)
* [hasSelection]({% link _docs/api/terminal/classes/terminal.md %}#hasselection)
* [input]({% link _docs/api/terminal/classes/terminal.md %}#input)
* [loadAddon]({% link _docs/api/terminal/classes/terminal.md %}#loadaddon)
* [open]({% link _docs/api/terminal/classes/terminal.md %}#open)
* [paste]({% link _docs/api/terminal/classes/terminal.md %}#paste)
* [refresh]({% link _docs/api/terminal/classes/terminal.md %}#refresh)
* [registerCharacterJoiner]({% link _docs/api/terminal/classes/terminal.md %}#registercharacterjoiner)
* [registerDecoration]({% link _docs/api/terminal/classes/terminal.md %}#registerdecoration)
* [registerLinkProvider]({% link _docs/api/terminal/classes/terminal.md %}#registerlinkprovider)
* [registerMarker]({% link _docs/api/terminal/classes/terminal.md %}#registermarker)
* [reset]({% link _docs/api/terminal/classes/terminal.md %}#reset)
* [resize]({% link _docs/api/terminal/classes/terminal.md %}#resize)
* [scrollLines]({% link _docs/api/terminal/classes/terminal.md %}#scrolllines)
* [scrollPages]({% link _docs/api/terminal/classes/terminal.md %}#scrollpages)
* [scrollToBottom]({% link _docs/api/terminal/classes/terminal.md %}#scrolltobottom)
* [scrollToLine]({% link _docs/api/terminal/classes/terminal.md %}#scrolltoline)
* [scrollToTop]({% link _docs/api/terminal/classes/terminal.md %}#scrolltotop)
* [select]({% link _docs/api/terminal/classes/terminal.md %}#select)
* [selectAll]({% link _docs/api/terminal/classes/terminal.md %}#selectall)
* [selectLines]({% link _docs/api/terminal/classes/terminal.md %}#selectlines)
* [write]({% link _docs/api/terminal/classes/terminal.md %}#write)
* [writeln]({% link _docs/api/terminal/classes/terminal.md %}#writeln)

## Constructors

###  constructor

\+ **new Terminal**(`options?`: [ITerminalOptions]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}) & [ITerminalInitOnlyOptions]({% link _docs/api/terminal/interfaces/iterminalinitonlyoptions.md %})): *[Terminal]({% link _docs/api/terminal/classes/terminal.md %})*

*Defined in [xterm.d.ts:872](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L872)*

Creates a new `Terminal` object.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`options?` | [ITerminalOptions]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}) & [ITerminalInitOnlyOptions]({% link _docs/api/terminal/interfaces/iterminalinitonlyoptions.md %}) | An object containing a set of options.  |

**Returns:** *[Terminal]({% link _docs/api/terminal/classes/terminal.md %})*

## Properties

### `Readonly` buffer

• **buffer**: *[IBufferNamespace]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %})*

*Defined in [xterm.d.ts:809](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L809)*

Access to the terminal's normal and alt buffer.

___

### `Readonly` cols

• **cols**: *number*

*Defined in [xterm.d.ts:804](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L804)*

The number of columns in the terminal's viewport. Use
`ITerminalOptions.cols` to set this in the constructor and
`Terminal.resize` for when the terminal exists.

___

### `Readonly` element

• **element**: *HTMLElement | undefined*

*Defined in [xterm.d.ts:785](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L785)*

The element containing the terminal.

___

### `Readonly` markers

• **markers**: *ReadonlyArray‹[IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})›*

*Defined in [xterm.d.ts:815](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L815)*

(EXPERIMENTAL) Get all markers registered against the buffer. If the alt
buffer is active this will always return [].

___

### `Readonly` modes

• **modes**: *[IModes]({% link _docs/api/terminal/interfaces/imodes.md %})*

*Defined in [xterm.d.ts:831](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L831)*

Gets the terminal modes as set by SM/DECSET.

___

###  onBell

• **onBell**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:885](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L885)*

Adds an event listener for when the bell is triggered.

**`returns`** an `IDisposable` to stop listening.

___

###  onBinary

• **onBinary**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹string›*

*Defined in [xterm.d.ts:896](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L896)*

Adds an event listener for when a binary event fires. This is used to
enable non UTF-8 conformant binary messages to be sent to the backend.
Currently this is only used for a certain type of mouse reports that
happen to be not UTF-8 compatible.
The event value is a JS string, pass it to the underlying pty as
binary data, e.g. `pty.write(Buffer.from(data, 'binary'))`.

**`returns`** an `IDisposable` to stop listening.

___

###  onCursorMove

• **onCursorMove**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:902](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L902)*

Adds an event listener for the cursor moves.

**`returns`** an `IDisposable` to stop listening.

___

###  onData

• **onData**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹string›*

*Defined in [xterm.d.ts:911](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L911)*

Adds an event listener for when a data event fires. This happens for
example when the user types or pastes into the terminal. The event value
is whatever `string` results, in a typical setup, this should be passed
on to the backing pty.

**`returns`** an `IDisposable` to stop listening.

___

###  onKey

• **onKey**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹object›*

*Defined in [xterm.d.ts:919](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L919)*

Adds an event listener for when a key is pressed. The event value
contains the string that will be sent in the data event as well as the
DOM event that triggered it.

**`returns`** an `IDisposable` to stop listening.

___

###  onLineFeed

• **onLineFeed**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:925](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L925)*

Adds an event listener for when a line feed is added.

**`returns`** an `IDisposable` to stop listening.

___

###  onRender

• **onRender**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹object›*

*Defined in [xterm.d.ts:933](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L933)*

Adds an event listener for when rows are rendered. The event value
contains the start row and end rows of the rendered area (ranges from `0`
to `Terminal.rows - 1`).

**`returns`** an `IDisposable` to stop listening.

___

###  onResize

• **onResize**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹object›*

*Defined in [xterm.d.ts:951](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L951)*

Adds an event listener for when the terminal is resized. The event value
contains the new size.

**`returns`** an `IDisposable` to stop listening.

___

###  onScroll

• **onScroll**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹number›*

*Defined in [xterm.d.ts:958](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L958)*

Adds an event listener for when a scroll occurs. The event value is the
new position of the viewport.

**`returns`** an `IDisposable` to stop listening.

___

###  onSelectionChange

• **onSelectionChange**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:964](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L964)*

Adds an event listener for when a selection change occurs.

**`returns`** an `IDisposable` to stop listening.

___

###  onTitleChange

• **onTitleChange**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹string›*

*Defined in [xterm.d.ts:971](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L971)*

Adds an event listener for when an OSC 0 or OSC 2 title change occurs.
The event value is the new title.

**`returns`** an `IDisposable` to stop listening.

___

###  onWriteParsed

• **onWriteParsed**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:944](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L944)*

Adds an event listener for when data has been parsed by the terminal,
after [write]({% link _docs/api/terminal/classes/terminal.md %}#write) is called. This event is useful to listen for any
changes in the buffer.

This fires at most once per frame, after data parsing completes. Note
that this can fire when there are still writes pending if there is a lot
of data.

___

###  options

• **options**: *[ITerminalOptions]({% link _docs/api/terminal/interfaces/iterminaloptions.md %})*

*Defined in [xterm.d.ts:867](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L867)*

Gets or sets the terminal options. This supports setting multiple
options.

**`example`** Get a single option
```ts
console.log(terminal.options.fontSize);
```

**`example`** Set a single option:
```ts
terminal.options.fontSize = 12;
```
Note that for options that are object, a new object must be used in order
to take effect as a reference comparison will be done:
```ts
const newValue = terminal.options.theme;
newValue.background = '#000000';

// This won't work
terminal.options.theme = newValue;

// This will work
terminal.options.theme = { ...newValue };
```

**`example`** Set multiple options
```ts
terminal.options = {
  fontSize: 12,
  fontFamily: 'Courier New'
};
```

___

### `Readonly` parser

• **parser**: *[IParser]({% link _docs/api/terminal/interfaces/iparser.md %})*

*Defined in [xterm.d.ts:820](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L820)*

Get the parser interface to register custom escape sequence handlers.

___

### `Readonly` rows

• **rows**: *number*

*Defined in [xterm.d.ts:797](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L797)*

The number of rows in the terminal's viewport. Use
`ITerminalOptions.rows` to set this in the constructor and
`Terminal.resize` for when the terminal exists.

___

### `Readonly` textarea

• **textarea**: *HTMLTextAreaElement | undefined*

*Defined in [xterm.d.ts:790](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L790)*

The textarea that accepts input for the terminal.

___

### `Readonly` unicode

• **unicode**: *[IUnicodeHandling]({% link _docs/api/terminal/interfaces/iunicodehandling.md %})*

*Defined in [xterm.d.ts:826](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L826)*

(EXPERIMENTAL) Get the Unicode handling interface
to register and switch Unicode version.

___

### `Static` strings

▪ **strings**: *[ILocalizableStrings]({% link _docs/api/terminal/interfaces/ilocalizablestrings.md %})*

*Defined in [xterm.d.ts:872](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L872)*

Natural language strings that can be localized.

## Methods

###  attachCustomKeyEventHandler

▸ **attachCustomKeyEventHandler**(`customKeyEventHandler`: function): *void*

*Defined in [xterm.d.ts:1040](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1040)*

Attaches a custom key event handler which is run before keys are
processed, giving consumers of xterm.js ultimate control as to what keys
should be processed by the terminal and what keys should not.

**`example`** A custom keymap that overrides the backspace key
```ts
const keymap = [
  { "key": "Backspace", "shiftKey": false, "mapCode": 8 },
  { "key": "Backspace", "shiftKey": true, "mapCode": 127 }
];
term.attachCustomKeyEventHandler(ev => {
  if (ev.type === 'keydown') {
    for (let i in keymap) {
      if (keymap[i].key == ev.key && keymap[i].shiftKey == ev.shiftKey) {
        socket.send(String.fromCharCode(keymap[i].mapCode));
        return false;
      }
    }
  }
});
```

**Parameters:**

▪ **customKeyEventHandler**: *function*

The custom KeyboardEvent handler to attach.
This is a function that takes a KeyboardEvent, allowing consumers to stop
propagation and/or prevent the default action. The function returns
whether the event should be processed by xterm.js.

▸ (`event`: KeyboardEvent): *boolean*

**Parameters:**

Name | Type |
------ | ------ |
`event` | KeyboardEvent |

**Returns:** *void*

___

###  attachCustomWheelEventHandler

▸ **attachCustomWheelEventHandler**(`customWheelEventHandler`: function): *void*

*Defined in [xterm.d.ts:1062](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1062)*

Attaches a custom wheel event handler which is run before keys are
processed, giving consumers of xterm.js control over whether to proceed
or cancel terminal wheel events.

**`example`** A handler that prevents all wheel events while ctrl is held from
being processed.
```ts
term.attachCustomWheelEventHandler(ev => {
  if (ev.ctrlKey) {
    return false;
  }
  return true;
});
```

**Parameters:**

▪ **customWheelEventHandler**: *function*

The custom WheelEvent handler to attach.
This is a function that takes a WheelEvent, allowing consumers to stop
propagation and/or prevent the default action. The function returns
whether the event should be processed by xterm.js.

▸ (`event`: WheelEvent): *boolean*

**Parameters:**

Name | Type |
------ | ------ |
`event` | WheelEvent |

**Returns:** *void*

___

###  blur

▸ **blur**(): *void*

*Defined in [xterm.d.ts:976](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L976)*

Unfocus the terminal.

**Returns:** *void*

___

###  clear

▸ **clear**(): *void*

*Defined in [xterm.d.ts:1206](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1206)*

Clear the entire buffer, making the prompt line the new first line.

**Returns:** *void*

___

###  clearSelection

▸ **clearSelection**(): *void*

*Defined in [xterm.d.ts:1146](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1146)*

Clears the current terminal selection.

**Returns:** *void*

___

###  clearTextureAtlas

▸ **clearTextureAtlas**(): *void*

*Defined in [xterm.d.ts:1249](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1249)*

Clears the texture atlas of the canvas renderer if it's active. Doing
this will force a redraw of all glyphs which can workaround issues
causing the texture to become corrupt, for example Chromium/Nvidia has an
issue where the texture gets messed up when resuming the OS from sleep.

**Returns:** *void*

___

###  deregisterCharacterJoiner

▸ **deregisterCharacterJoiner**(`joinerId`: number): *void*

*Defined in [xterm.d.ts:1108](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1108)*

(EXPERIMENTAL) Deregisters the character joiner if one was registered.
NOTE: character joiners are only used by the canvas renderer.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`joinerId` | number | The character joiner's ID (returned after register)  |

**Returns:** *void*

___

###  dispose

▸ **dispose**(): *void*

*Implementation of [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})*

*Defined in [xterm.d.ts:1173](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1173)*

**Returns:** *void*

___

###  focus

▸ **focus**(): *void*

*Defined in [xterm.d.ts:981](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L981)*

Focus the terminal.

**Returns:** *void*

___

###  getSelection

▸ **getSelection**(): *string*

*Defined in [xterm.d.ts:1136](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1136)*

Gets the terminal's current selection, this is useful for implementing
copy behavior outside of xterm.js.

**Returns:** *string*

___

###  getSelectionPosition

▸ **getSelectionPosition**(): *[IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %}) | undefined*

*Defined in [xterm.d.ts:1141](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1141)*

Gets the selection position or undefined if there is no selection.

**Returns:** *[IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %}) | undefined*

___

###  hasSelection

▸ **hasSelection**(): *boolean*

*Defined in [xterm.d.ts:1130](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1130)*

Gets whether the terminal has an active selection.

**Returns:** *boolean*

___

###  input

▸ **input**(`data`: string, `wasUserInput?`: boolean): *void*

*Defined in [xterm.d.ts:993](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L993)*

Input data to application side. The data is treated the same way input
typed into the terminal would (ie. the [onData]({% link _docs/api/terminal/classes/terminal.md %}#ondata) event will fire).

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`data` | string | The data to forward to the application. |
`wasUserInput?` | boolean | Whether the input is genuine user input. This is true by default and triggers additionalbehavior like focus or selection clearing. Set this to false if the data sent should not be treated like user input would, for example passing an escape sequence to the application.  |

**Returns:** *void*

___

###  loadAddon

▸ **loadAddon**(`addon`: [ITerminalAddon]({% link _docs/api/terminal/interfaces/iterminaladdon.md %})): *void*

*Defined in [xterm.d.ts:1260](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1260)*

Loads an addon into this instance of xterm.js.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`addon` | [ITerminalAddon]({% link _docs/api/terminal/interfaces/iterminaladdon.md %}) | The addon to load.  |

**Returns:** *void*

___

###  open

▸ **open**(`parent`: HTMLElement): *void*

*Defined in [xterm.d.ts:1011](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1011)*

Opens the terminal within an element. This should also be called if the
xterm.js element ever changes browser window.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`parent` | HTMLElement | The element to create the terminal within. This element must be visible (have dimensions) when `open` is called as several DOM- based measurements need to be performed when this function is called.  |

**Returns:** *void*

___

###  paste

▸ **paste**(`data`: string): *void*

*Defined in [xterm.d.ts:1233](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1233)*

Writes text to the terminal, performing the necessary transformations for
pasted text.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`data` | string | The text to write to the terminal.  |

**Returns:** *void*

___

###  refresh

▸ **refresh**(`start`: number, `end`: number): *void*

*Defined in [xterm.d.ts:1241](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1241)*

Tells the renderer to refresh terminal content between two rows
(inclusive) at the next opportunity.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`start` | number | The row to start from (between 0 and this.rows - 1). |
`end` | number | The row to end at (between start and this.rows - 1).  |

**Returns:** *void*

___

###  registerCharacterJoiner

▸ **registerCharacterJoiner**(`handler`: function): *number*

*Defined in [xterm.d.ts:1101](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1101)*

(EXPERIMENTAL) Registers a character joiner, allowing custom sequences of
characters to be rendered as a single unit. This is useful in particular
for rendering ligatures and graphemes, among other things.

Each registered character joiner is called with a string of text
representing a portion of a line in the terminal that can be rendered as
a single unit. The joiner must return a sorted array, where each entry is
itself an array of length two, containing the start (inclusive) and end
(exclusive) index of a substring of the input that should be rendered as
a single unit. When multiple joiners are provided, the results of each
are collected. If there are any overlapping substrings between them, they
are combined into one larger unit that is drawn together.

All character joiners that are registered get called every time a line is
rendered in the terminal, so it is essential for the handler function to
run as quickly as possible to avoid slowdowns when rendering. Similarly,
joiners should strive to return the smallest possible substrings to
render together, since they aren't drawn as optimally as individual
characters.

NOTE: character joiners are only used by the canvas renderer.

**Parameters:**

▪ **handler**: *function*

The function that determines character joins. It is called
with a string of text that is eligible for joining and returns an array
where each entry is an array containing the start (inclusive) and end
(exclusive) indexes of ranges that should be rendered as a single unit.

▸ (`text`: string): *[][]*

**Parameters:**

Name | Type |
------ | ------ |
`text` | string |

**Returns:** *number*

The ID of the new joiner, this can be used to deregister

___

###  registerDecoration

▸ **registerDecoration**(`decorationOptions`: [IDecorationOptions]({% link _docs/api/terminal/interfaces/idecorationoptions.md %})): *[IDecoration]({% link _docs/api/terminal/interfaces/idecoration.md %}) | undefined*

*Defined in [xterm.d.ts:1125](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1125)*

(EXPERIMENTAL) Adds a decoration to the terminal using

**`throws`** when options include a negative x offset.

**Parameters:**

Name | Type |
------ | ------ |
`decorationOptions` | [IDecorationOptions]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}) |

**Returns:** *[IDecoration]({% link _docs/api/terminal/interfaces/idecoration.md %}) | undefined*

___

###  registerLinkProvider

▸ **registerLinkProvider**(`linkProvider`: [ILinkProvider]({% link _docs/api/terminal/interfaces/ilinkprovider.md %})): *[IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})*

*Defined in [xterm.d.ts:1070](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1070)*

Registers a link provider, allowing a custom parser to be used to match
and handle links. Multiple link providers can be used, they will be asked
in the order in which they are registered.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`linkProvider` | [ILinkProvider]({% link _docs/api/terminal/interfaces/ilinkprovider.md %}) | The link provider to use to detect links.  |

**Returns:** *[IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})*

___

###  registerMarker

▸ **registerMarker**(`cursorYOffset?`: number): *[IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})*

*Defined in [xterm.d.ts:1115](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1115)*

Adds a marker to the normal buffer and returns it.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`cursorYOffset?` | number | The y position offset of the marker from the cursor. |

**Returns:** *[IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})*

The new marker or undefined.

___

###  reset

▸ **reset**(): *void*

*Defined in [xterm.d.ts:1254](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1254)*

Perform a full reset (RIS, aka '\x1bc').

**Returns:** *void*

___

###  resize

▸ **resize**(`columns`: number, `rows`: number): *void*

*Defined in [xterm.d.ts:1002](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1002)*

Resizes the terminal. It's best practice to debounce calls to resize,
this will help ensure that the pty can respond to the resize event
before another one occurs.

**Parameters:**

Name | Type |
------ | ------ |
`columns` | number |
`rows` | number |

**Returns:** *void*

___

###  scrollLines

▸ **scrollLines**(`amount`: number): *void*

*Defined in [xterm.d.ts:1179](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1179)*

Scroll the display of the terminal

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`amount` | number | The number of lines to scroll down (negative scroll up).  |

**Returns:** *void*

___

###  scrollPages

▸ **scrollPages**(`pageCount`: number): *void*

*Defined in [xterm.d.ts:1185](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1185)*

Scroll the display of the terminal by a number of pages.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`pageCount` | number | The number of pages to scroll (negative scrolls up).  |

**Returns:** *void*

___

###  scrollToBottom

▸ **scrollToBottom**(): *void*

*Defined in [xterm.d.ts:1195](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1195)*

Scrolls the display of the terminal to the bottom.

**Returns:** *void*

___

###  scrollToLine

▸ **scrollToLine**(`line`: number): *void*

*Defined in [xterm.d.ts:1201](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1201)*

Scrolls to a line within the buffer.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`line` | number | The 0-based line index to scroll to.  |

**Returns:** *void*

___

###  scrollToTop

▸ **scrollToTop**(): *void*

*Defined in [xterm.d.ts:1190](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1190)*

Scrolls the display of the terminal to the top.

**Returns:** *void*

___

###  select

▸ **select**(`column`: number, `row`: number, `length`: number): *void*

*Defined in [xterm.d.ts:1154](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1154)*

Selects text within the terminal.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`column` | number | The column the selection starts at. |
`row` | number | The row the selection starts at. |
`length` | number | The length of the selection.  |

**Returns:** *void*

___

###  selectAll

▸ **selectAll**(): *void*

*Defined in [xterm.d.ts:1159](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1159)*

Selects all text within the terminal.

**Returns:** *void*

___

###  selectLines

▸ **selectLines**(`start`: number, `end`: number): *void*

*Defined in [xterm.d.ts:1166](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1166)*

Selects text in the buffer between 2 lines.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`start` | number | The 0-based line index to select from (inclusive). |
`end` | number | The 0-based line index to select to (inclusive).  |

**Returns:** *void*

___

###  write

▸ **write**(`data`: string | Uint8Array, `callback?`: function): *void*

*Defined in [xterm.d.ts:1216](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1216)*

Write data to the terminal.

**Parameters:**

▪ **data**: *string | Uint8Array*

The data to write to the terminal. This can either be raw
bytes given as Uint8Array from the pty or a string. Raw bytes will always
be treated as UTF-8 encoded, string data as UTF-16.

▪`Optional`  **callback**: *function*

Optional callback that fires when the data was processed
by the parser.

▸ (): *void*

**Returns:** *void*

___

###  writeln

▸ **writeln**(`data`: string | Uint8Array, `callback?`: function): *void*

*Defined in [xterm.d.ts:1226](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1226)*

Writes data to the terminal, followed by a break line character (\n).

**Parameters:**

▪ **data**: *string | Uint8Array*

The data to write to the terminal. This can either be raw
bytes given as Uint8Array from the pty or a string. Raw bytes will always
be treated as UTF-8 encoded, string data as UTF-16.

▪`Optional`  **callback**: *function*

Optional callback that fires when the data was processed
by the parser.

▸ (): *void*

**Returns:** *void*
