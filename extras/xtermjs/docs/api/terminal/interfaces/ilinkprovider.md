---
title: ILinkProvider
category: API-interfaces
layout: docs
---


# Interface: ILinkProvider

A custom link provider.

## Hierarchy

* **ILinkProvider**

## Index

### Methods

* [provideLinks]({% link _docs/api/terminal/interfaces/ilinkprovider.md %}#providelinks)

## Methods

###  provideLinks

▸ **provideLinks**(`bufferLineNumber`: number, `callback`: function): *void*

*Defined in [xterm.d.ts:1359](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1359)*

Provides a link a buffer position

**Parameters:**

▪ **bufferLineNumber**: *number*

The y position of the buffer to check for links
within.

▪ **callback**: *function*

The callback to be fired when ready with the resulting
link(s) for the line or `undefined`.

▸ (`links`: [ILink]({% link _docs/api/terminal/interfaces/ilink.md %})[] | undefined): *void*

**Parameters:**

Name | Type |
------ | ------ |
`links` | [ILink]({% link _docs/api/terminal/interfaces/ilink.md %})[] &#124; undefined |

**Returns:** *void*
