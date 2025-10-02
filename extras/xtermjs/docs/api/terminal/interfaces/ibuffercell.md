---
title: IBufferCell
category: API-interfaces
layout: docs
---


# Interface: IBufferCell

Represents a single cell in the terminal's buffer.

## Hierarchy

* **IBufferCell**

## Index

### Methods

* [getBgColor]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getbgcolor)
* [getBgColorMode]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getbgcolormode)
* [getChars]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getchars)
* [getCode]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getcode)
* [getFgColor]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getfgcolor)
* [getFgColorMode]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getfgcolormode)
* [getWidth]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#getwidth)
* [isAttributeDefault]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isattributedefault)
* [isBgDefault]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isbgdefault)
* [isBgPalette]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isbgpalette)
* [isBgRGB]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isbgrgb)
* [isBlink]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isblink)
* [isBold]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isbold)
* [isDim]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isdim)
* [isFgDefault]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isfgdefault)
* [isFgPalette]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isfgpalette)
* [isFgRGB]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isfgrgb)
* [isInverse]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isinverse)
* [isInvisible]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isinvisible)
* [isItalic]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isitalic)
* [isOverline]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isoverline)
* [isStrikethrough]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isstrikethrough)
* [isUnderline]({% link _docs/api/terminal/interfaces/ibuffercell.md %}#isunderline)

## Methods

###  getBgColor

▸ **getBgColor**(): *number*

*Defined in [xterm.d.ts:1661](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1661)*

Gets a cell's background color number, this differs depending on what the
color mode of the cell is:

- Default: This should be 0, representing the default background color
  (CSI 49 m).
- Palette: This is a number from 0 to 255 of ANSI colors
  (CSI 4(0-7) m, CSI 10(0-7) m, CSI 48 ; 5 ; 0-255 m).
- RGB: A hex value representing a 'true color': 0xRRGGBB
  (CSI 4 8 ; 2 ; Pi ; Pr ; Pg ; Pb)

**Returns:** *number*

___

###  getBgColorMode

▸ **getBgColorMode**(): *number*

*Defined in [xterm.d.ts:1635](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1635)*

Gets the number representation of the background color mode, this can be
used to perform quick comparisons of 2 cells to see if they're the same.
Use `isBgRGB`, `isBgPalette` and `isBgDefault` to check what color mode
a cell is.

**Returns:** *number*

___

###  getChars

▸ **getChars**(): *string*

*Defined in [xterm.d.ts:1613](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1613)*

The character(s) within the cell. Examples of what this can contain:

- A normal width character
- A wide character (eg. CJK)
- An emoji

**Returns:** *string*

___

###  getCode

▸ **getCode**(): *number*

*Defined in [xterm.d.ts:1619](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1619)*

Gets the UTF32 codepoint of single characters, if content is a combined
string it returns the codepoint of the last character in the string.

**Returns:** *number*

___

###  getFgColor

▸ **getFgColor**(): *number*

*Defined in [xterm.d.ts:1648](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1648)*

Gets a cell's foreground color number, this differs depending on what the
color mode of the cell is:

- Default: This should be 0, representing the default foreground color
  (CSI 39 m).
- Palette: This is a number from 0 to 255 of ANSI colors (CSI 3(0-7) m,
  CSI 9(0-7) m, CSI 38 ; 5 ; 0-255 m).
- RGB: A hex value representing a 'true color': 0xRRGGBB.
  (CSI 3 8 ; 2 ; Pi ; Pr ; Pg ; Pb)

**Returns:** *number*

___

###  getFgColorMode

▸ **getFgColorMode**(): *number*

*Defined in [xterm.d.ts:1627](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1627)*

Gets the number representation of the foreground color mode, this can be
used to perform quick comparisons of 2 cells to see if they're the same.
Use `isFgRGB`, `isFgPalette` and `isFgDefault` to check what color mode
a cell is.

**Returns:** *number*

___

###  getWidth

▸ **getWidth**(): *number*

*Defined in [xterm.d.ts:1604](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1604)*

The width of the character. Some examples:

- `1` for most cells.
- `2` for wide character like CJK glyphs.
- `0` for cells immediately following cells with a width of `2`.

**Returns:** *number*

___

###  isAttributeDefault

▸ **isAttributeDefault**(): *boolean*

*Defined in [xterm.d.ts:1696](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1696)*

Whether the cell has the default attribute (no color or style).

**Returns:** *boolean*

___

###  isBgDefault

▸ **isBgDefault**(): *boolean*

*Defined in [xterm.d.ts:1693](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1693)*

Whether the cell is using the default background color mode.

**Returns:** *boolean*

___

###  isBgPalette

▸ **isBgPalette**(): *boolean*

*Defined in [xterm.d.ts:1689](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1689)*

Whether the cell is using the palette background color mode.

**Returns:** *boolean*

___

###  isBgRGB

▸ **isBgRGB**(): *boolean*

*Defined in [xterm.d.ts:1685](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1685)*

Whether the cell is using the RGB background color mode.

**Returns:** *boolean*

___

###  isBlink

▸ **isBlink**(): *number*

*Defined in [xterm.d.ts:1672](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1672)*

Whether the cell has the blink attribute (CSI 5 m).

**Returns:** *number*

___

###  isBold

▸ **isBold**(): *number*

*Defined in [xterm.d.ts:1664](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1664)*

Whether the cell has the bold attribute (CSI 1 m).

**Returns:** *number*

___

###  isDim

▸ **isDim**(): *number*

*Defined in [xterm.d.ts:1668](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1668)*

Whether the cell has the dim attribute (CSI 2 m).

**Returns:** *number*

___

###  isFgDefault

▸ **isFgDefault**(): *boolean*

*Defined in [xterm.d.ts:1691](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1691)*

Whether the cell is using the default foreground color mode.

**Returns:** *boolean*

___

###  isFgPalette

▸ **isFgPalette**(): *boolean*

*Defined in [xterm.d.ts:1687](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1687)*

Whether the cell is using the palette foreground color mode.

**Returns:** *boolean*

___

###  isFgRGB

▸ **isFgRGB**(): *boolean*

*Defined in [xterm.d.ts:1683](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1683)*

Whether the cell is using the RGB foreground color mode.

**Returns:** *boolean*

___

###  isInverse

▸ **isInverse**(): *number*

*Defined in [xterm.d.ts:1674](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1674)*

Whether the cell has the inverse attribute (CSI 7 m).

**Returns:** *number*

___

###  isInvisible

▸ **isInvisible**(): *number*

*Defined in [xterm.d.ts:1676](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1676)*

Whether the cell has the invisible attribute (CSI 8 m).

**Returns:** *number*

___

###  isItalic

▸ **isItalic**(): *number*

*Defined in [xterm.d.ts:1666](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1666)*

Whether the cell has the italic attribute (CSI 3 m).

**Returns:** *number*

___

###  isOverline

▸ **isOverline**(): *number*

*Defined in [xterm.d.ts:1680](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1680)*

Whether the cell has the overline attribute (CSI 53 m).

**Returns:** *number*

___

###  isStrikethrough

▸ **isStrikethrough**(): *number*

*Defined in [xterm.d.ts:1678](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1678)*

Whether the cell has the strikethrough attribute (CSI 9 m).

**Returns:** *number*

___

###  isUnderline

▸ **isUnderline**(): *number*

*Defined in [xterm.d.ts:1670](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1670)*

Whether the cell has the underline attribute (CSI 4 m).

**Returns:** *number*
