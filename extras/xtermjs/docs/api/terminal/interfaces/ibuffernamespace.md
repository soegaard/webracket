---
title: IBufferNamespace
category: API-interfaces
layout: docs
---


# Interface: IBufferNamespace

Represents the terminal's set of buffers.

## Hierarchy

* **IBufferNamespace**

## Index

### Properties

* [active]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %}#readonly-active)
* [alternate]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %}#readonly-alternate)
* [normal]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %}#readonly-normal)
* [onBufferChange]({% link _docs/api/terminal/interfaces/ibuffernamespace.md %}#onbufferchange)

## Properties

### `Readonly` active

• **active**: *[IBuffer]({% link _docs/api/terminal/interfaces/ibuffer.md %})*

*Defined in [xterm.d.ts:1531](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1531)*

The active buffer, this will either be the normal or alternate buffers.

___

### `Readonly` alternate

• **alternate**: *[IBuffer]({% link _docs/api/terminal/interfaces/ibuffer.md %})*

*Defined in [xterm.d.ts:1542](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1542)*

The alternate buffer, this becomes the active buffer when an application
enters this mode via DECSET (`CSI ? 4 7 h`)

___

### `Readonly` normal

• **normal**: *[IBuffer]({% link _docs/api/terminal/interfaces/ibuffer.md %})*

*Defined in [xterm.d.ts:1536](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1536)*

The normal buffer.

___

###  onBufferChange

• **onBufferChange**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹[IBuffer]({% link _docs/api/terminal/interfaces/ibuffer.md %})›*

*Defined in [xterm.d.ts:1548](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1548)*

Adds an event listener for when the active buffer changes.

**`returns`** an `IDisposable` to stop listening.
