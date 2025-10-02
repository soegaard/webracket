---
title: IDecorationOptions
category: API-interfaces
layout: docs
---


# Interface: IDecorationOptions

## Hierarchy

* **IDecorationOptions**

## Index

### Properties

* [anchor]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-anchor)
* [backgroundColor]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-backgroundcolor)
* [foregroundColor]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-foregroundcolor)
* [height]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-height)
* [layer]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-layer)
* [marker]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#readonly-marker)
* [overviewRulerOptions]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-overviewruleroptions)
* [width]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-width)
* [x]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-x)

## Properties

### `Optional` `Readonly` anchor

• **anchor**? : *"right" | "left"*

*Defined in [xterm.d.ts:553](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L553)*

___

### `Optional` `Readonly` backgroundColor

• **backgroundColor**? : *string*

*Defined in [xterm.d.ts:576](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L576)*

The background color of the cell(s). When 2 decorations both set the
foreground color the last registered decoration will be used. Only the
`#RRGGBB` format is supported.

___

### `Optional` `Readonly` foregroundColor

• **foregroundColor**? : *string*

*Defined in [xterm.d.ts:583](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L583)*

The foreground color of the cell(s). When 2 decorations both set the
foreground color the last registered decoration will be used. Only the
`#RRGGBB` format is supported.

___

### `Optional` `Readonly` height

• **height**? : *number*

*Defined in [xterm.d.ts:569](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L569)*

The height of the decoration in cells, defaults to 1.

___

### `Optional` `Readonly` layer

• **layer**? : *"bottom" | "top"*

*Defined in [xterm.d.ts:593](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L593)*

What layer to render the decoration at when [backgroundColor]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-backgroundcolor) or
[foregroundColor]({% link _docs/api/terminal/interfaces/idecorationoptions.md %}#optional-readonly-foregroundcolor) are used. `'bottom'` will render under the
selection, `'top`' will render above the selection\*.

*\* The selection will render on top regardless of layer on the canvas
renderer due to how it renders selection separately.*

___

### `Readonly` marker

• **marker**: *[IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})*

*Defined in [xterm.d.ts:547](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L547)*

The line in the terminal where
the decoration will be displayed

___

### `Optional` overviewRulerOptions

• **overviewRulerOptions**? : *[IDecorationOverviewRulerOptions]({% link _docs/api/terminal/interfaces/idecorationoverviewruleroptions.md %})*

*Defined in [xterm.d.ts:602](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L602)*

When defined, renders the decoration in the overview ruler to the right
of the terminal. [ITerminalOptions.overviewRulerWidth]({% link _docs/api/terminal/interfaces/iterminaloptions.md %}#optional-overviewrulerwidth) must be set
in order to see the overview ruler.

**`param`** The color of the decoration.

**`param`** The position of the decoration.

___

### `Optional` `Readonly` width

• **width**? : *number*

*Defined in [xterm.d.ts:564](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L564)*

The width of the decoration in cells, defaults to 1.

___

### `Optional` `Readonly` x

• **x**? : *number*

*Defined in [xterm.d.ts:558](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L558)*

The x position offset relative to the anchor
