---
title: IUnicodeVersionProvider
category: API-interfaces
layout: docs
---


# Interface: IUnicodeVersionProvider

(EXPERIMENTAL) Unicode version provider.
Used to register custom Unicode versions with `Terminal.unicode.register`.

## Hierarchy

* **IUnicodeVersionProvider**

## Index

### Properties

* [version]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %}#readonly-version)

### Methods

* [charProperties]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %}#charproperties)
* [wcwidth]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %}#wcwidth)

## Properties

### `Readonly` version

• **version**: *string*

*Defined in [xterm.d.ts:1833](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1833)*

String indicating the Unicode version provided.

## Methods

###  charProperties

▸ **charProperties**(`codepoint`: number, `preceding`: number): *number*

*Defined in [xterm.d.ts:1839](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1839)*

**Parameters:**

Name | Type |
------ | ------ |
`codepoint` | number |
`preceding` | number |

**Returns:** *number*

___

###  wcwidth

▸ **wcwidth**(`codepoint`: number): *0 | 1 | 2*

*Defined in [xterm.d.ts:1838](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1838)*

Unicode version dependent wcwidth implementation.

**Parameters:**

Name | Type |
------ | ------ |
`codepoint` | number |

**Returns:** *0 | 1 | 2*
