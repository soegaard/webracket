---
title: IUnicodeHandling
category: API-interfaces
layout: docs
---


# Interface: IUnicodeHandling

(EXPERIMENTAL) Unicode handling interface.

## Hierarchy

* **IUnicodeHandling**

## Index

### Properties

* [activeVersion]({% link _docs/api/terminal/interfaces/iunicodehandling.md %}#activeversion)
* [versions]({% link _docs/api/terminal/interfaces/iunicodehandling.md %}#readonly-versions)

### Methods

* [register]({% link _docs/api/terminal/interfaces/iunicodehandling.md %}#register)

## Properties

###  activeVersion

• **activeVersion**: *string*

*Defined in [xterm.d.ts:1859](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1859)*

Getter/setter for active Unicode version.

___

### `Readonly` versions

• **versions**: *ReadonlyArray‹string›*

*Defined in [xterm.d.ts:1854](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1854)*

Registered Unicode versions.

## Methods

###  register

▸ **register**(`provider`: [IUnicodeVersionProvider]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %})): *void*

*Defined in [xterm.d.ts:1849](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1849)*

Register a custom Unicode version provider.

**Parameters:**

Name | Type |
------ | ------ |
`provider` | [IUnicodeVersionProvider]({% link _docs/api/terminal/interfaces/iunicodeversionprovider.md %}) |

**Returns:** *void*
