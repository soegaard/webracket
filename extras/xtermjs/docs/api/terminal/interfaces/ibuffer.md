---
title: IBuffer
category: API-interfaces
layout: docs
---


# Interface: IBuffer

Represents a terminal buffer.

## Hierarchy

* **IBuffer**

## Index

### Properties

* [baseY]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-basey)
* [cursorX]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-cursorx)
* [cursorY]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-cursory)
* [length]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-length)
* [type]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-type)
* [viewportY]({% link _docs/api/terminal/interfaces/ibuffer.md %}#readonly-viewporty)

### Methods

* [getLine]({% link _docs/api/terminal/interfaces/ibuffer.md %}#getline)
* [getNullCell]({% link _docs/api/terminal/interfaces/ibuffer.md %}#getnullcell)

## Properties

### `Readonly` baseY

• **baseY**: *number*

*Defined in [xterm.d.ts:1489](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1489)*

The line within the buffer where the top of the bottom page is (when
fully scrolled down).

___

### `Readonly` cursorX

• **cursorX**: *number*

*Defined in [xterm.d.ts:1478](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1478)*

The x position of the cursor. This ranges between `0` (left side) and
`Terminal.cols` (after last cell of the row).

___

### `Readonly` cursorY

• **cursorY**: *number*

*Defined in [xterm.d.ts:1472](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1472)*

The y position of the cursor. This ranges between `0` (when the
cursor is at baseY) and `Terminal.rows - 1` (when the cursor is on the
last row).

___

### `Readonly` length

• **length**: *number*

*Defined in [xterm.d.ts:1494](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1494)*

The amount of lines in the buffer.

___

### `Readonly` type

• **type**: *"normal" | "alternate"*

*Defined in [xterm.d.ts:1465](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1465)*

The type of the buffer.

___

### `Readonly` viewportY

• **viewportY**: *number*

*Defined in [xterm.d.ts:1483](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1483)*

The line within the buffer where the top of the viewport is.

## Methods

###  getLine

▸ **getLine**(`y`: number): *[IBufferLine]({% link _docs/api/terminal/interfaces/ibufferline.md %}) | undefined*

*Defined in [xterm.d.ts:1506](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1506)*

Gets a line from the buffer, or undefined if the line index does not
exist.

Note that the result of this function should be used immediately after
calling as when the terminal updates it could lead to unexpected
behavior.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`y` | number | The line index to get.  |

**Returns:** *[IBufferLine]({% link _docs/api/terminal/interfaces/ibufferline.md %}) | undefined*

___

###  getNullCell

▸ **getNullCell**(): *[IBufferCell]({% link _docs/api/terminal/interfaces/ibuffercell.md %})*

*Defined in [xterm.d.ts:1513](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1513)*

Creates an empty cell object suitable as a cell reference in
`line.getCell(x, cell)`. Use this to avoid costly recreation of
cell objects when dealing with tons of cells.

**Returns:** *[IBufferCell]({% link _docs/api/terminal/interfaces/ibuffercell.md %})*
