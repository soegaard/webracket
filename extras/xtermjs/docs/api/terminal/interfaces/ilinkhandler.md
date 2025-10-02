---
title: ILinkHandler
category: API-interfaces
layout: docs
---


# Interface: ILinkHandler

A link handler for OSC 8 hyperlinks.

## Hierarchy

* **ILinkHandler**

## Index

### Properties

* [allowNonHttpProtocols]({% link _docs/api/terminal/interfaces/ilinkhandler.md %}#optional-allownonhttpprotocols)

### Methods

* [activate]({% link _docs/api/terminal/interfaces/ilinkhandler.md %}#activate)
* [hover]({% link _docs/api/terminal/interfaces/ilinkhandler.md %}#optional-hover)
* [leave]({% link _docs/api/terminal/interfaces/ilinkhandler.md %}#optional-leave)

## Properties

### `Optional` allowNonHttpProtocols

• **allowNonHttpProtocols**? : *boolean*

*Defined in [xterm.d.ts:1345](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1345)*

Whether to receive non-HTTP URLs from LinkProvider. When false, any
usage of non-HTTP URLs will be ignored. Enabling this option without
proper protection in `activate` function may cause security issues such
as XSS.

## Methods

###  activate

▸ **activate**(`event`: MouseEvent, `text`: string, `range`: [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %})): *void*

*Defined in [xterm.d.ts:1318](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1318)*

Calls when the link is activated.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`event` | MouseEvent | The mouse event triggering the callback. |
`text` | string | The text of the link. |
`range` | [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %}) | The buffer range of the link.  |

**Returns:** *void*

___

### `Optional` hover

▸ **hover**(`event`: MouseEvent, `text`: string, `range`: [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %})): *void*

*Defined in [xterm.d.ts:1329](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1329)*

Called when the mouse hovers the link. To use this to create a DOM-based
hover tooltip, create the hover element within `Terminal.element` and
add the `xterm-hover` class to it, that will cause mouse events to not
fall through and activate other links.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`event` | MouseEvent | The mouse event triggering the callback. |
`text` | string | The text of the link. |
`range` | [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %}) | The buffer range of the link.  |

**Returns:** *void*

___

### `Optional` leave

▸ **leave**(`event`: MouseEvent, `text`: string, `range`: [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %})): *void*

*Defined in [xterm.d.ts:1337](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1337)*

Called when the mouse leaves the link.

**Parameters:**

Name | Type | Description |
------ | ------ | ------ |
`event` | MouseEvent | The mouse event triggering the callback. |
`text` | string | The text of the link. |
`range` | [IBufferRange]({% link _docs/api/terminal/interfaces/ibufferrange.md %}) | The buffer range of the link.  |

**Returns:** *void*
