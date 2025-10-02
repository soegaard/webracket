---
title: IMarker
category: API-interfaces
layout: docs
---


# Interface: IMarker

Represents a specific line in the terminal that is tracked when scrollback
is trimmed and lines are added or removed. This is a single line that may
be part of a larger wrapped line.

## Hierarchy

  ↳ [IDisposableWithEvent]({% link _docs/api/terminal/interfaces/idisposablewithevent.md %})

  ↳ **IMarker**

## Index

### Properties

* [id]({% link _docs/api/terminal/interfaces/imarker.md %}#readonly-id)
* [isDisposed]({% link _docs/api/terminal/interfaces/imarker.md %}#readonly-isdisposed)
* [line]({% link _docs/api/terminal/interfaces/imarker.md %}#readonly-line)
* [onDispose]({% link _docs/api/terminal/interfaces/imarker.md %}#ondispose)

### Methods

* [dispose]({% link _docs/api/terminal/interfaces/imarker.md %}#dispose)

## Properties

### `Readonly` id

• **id**: *number*

*Defined in [xterm.d.ts:474](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L474)*

A unique identifier for this marker.

___

### `Readonly` isDisposed

• **isDisposed**: *boolean*

*Inherited from [IMarker]({% link _docs/api/terminal/interfaces/imarker.md %}).[isDisposed]({% link _docs/api/terminal/interfaces/imarker.md %}#readonly-isdisposed)*

*Defined in [xterm.d.ts:495](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L495)*

Whether this is disposed.

___

### `Readonly` line

• **line**: *number*

*Defined in [xterm.d.ts:480](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L480)*

The actual line index in the buffer at this point in time. This is set to
-1 if the marker has been disposed.

___

###  onDispose

• **onDispose**: *[IEvent]({% link _docs/api/terminal/interfaces/ievent.md %})‹void›*

*Inherited from [IMarker]({% link _docs/api/terminal/interfaces/imarker.md %}).[onDispose]({% link _docs/api/terminal/interfaces/imarker.md %}#ondispose)*

*Defined in [xterm.d.ts:490](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L490)*

Event listener to get notified when this gets disposed.

## Methods

###  dispose

▸ **dispose**(): *void*

*Inherited from [IDisposable]({% link _docs/api/terminal/interfaces/idisposable.md %}).[dispose]({% link _docs/api/terminal/interfaces/idisposable.md %}#dispose)*

*Defined in [xterm.d.ts:454](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L454)*

**Returns:** *void*
