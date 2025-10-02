---
title: IWindowsPty
category: API-interfaces
layout: docs
---


# Interface: IWindowsPty

Pty information for Windows.

## Hierarchy

* **IWindowsPty**

## Index

### Properties

* [backend]({% link _docs/api/terminal/interfaces/iwindowspty.md %}#optional-backend)
* [buildNumber]({% link _docs/api/terminal/interfaces/iwindowspty.md %}#optional-buildnumber)

## Properties

### `Optional` backend

• **backend**? : *"conpty" | "winpty"*

*Defined in [xterm.d.ts:412](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L412)*

What pty emulation backend is being used.

___

### `Optional` buildNumber

• **buildNumber**? : *number*

*Defined in [xterm.d.ts:416](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L416)*

The Windows build version (eg. 19045)
