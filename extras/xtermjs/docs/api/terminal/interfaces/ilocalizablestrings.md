---
title: ILocalizableStrings
category: API-interfaces
layout: docs
---


# Interface: ILocalizableStrings

The set of localizable strings.

## Hierarchy

* **ILocalizableStrings**

## Index

### Properties

* [promptLabel]({% link _docs/api/terminal/interfaces/ilocalizablestrings.md %}#promptlabel)
* [tooMuchOutput]({% link _docs/api/terminal/interfaces/ilocalizablestrings.md %}#toomuchoutput)

## Properties

###  promptLabel

• **promptLabel**: *string*

*Defined in [xterm.d.ts:612](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L612)*

The aria label for the underlying input textarea for the terminal.

___

###  tooMuchOutput

• **tooMuchOutput**: *string*

*Defined in [xterm.d.ts:618](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L618)*

Announcement for when line reading is suppressed due to too many lines
being printed to the terminal when `screenReaderMode` is enabled.
