---
title: IDisposableWithEvent
category: API-interfaces
layout: docs
---


# Interface: IDisposableWithEvent

Represents a disposable that tracks is disposed state.

## Hierarchy

* [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %})

  ↳ **IDisposableWithEvent**

  ↳ [IMarker]({% link _docs/api/terminal/interfaces/imarker.md %})

  ↳ [IDecoration]({% link _docs/api/terminal/interfaces/idecoration.md %})

## Index

### Properties

* [isDisposed]({% link _docs/api/terminal/interfaces/idisposablewithevent.md %}#readonly-isdisposed)
* [onDispose]({% link _docs/api/terminal/interfaces/idisposablewithevent.md %}#ondispose)

### Methods

* [dispose]({% link _docs/api/terminal/interfaces/idisposablewithevent.md %}#dispose)

## Properties

### `Readonly` isDisposed

• **isDisposed**: *boolean*

*Defined in [xterm.d.ts:495](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L495)*

Whether this is disposed.

___

###  onDispose

• **onDispose**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Defined in [xterm.d.ts:490](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L490)*

Event listener to get notified when this gets disposed.

## Methods

###  dispose

▸ **dispose**(): *void*

*Inherited from [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %}).[dispose]({% link _docs/api/terminal/interfaces/idisposable.md %}#dispose)*

*Defined in [xterm.d.ts:454](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L454)*

**Returns:** *void*
