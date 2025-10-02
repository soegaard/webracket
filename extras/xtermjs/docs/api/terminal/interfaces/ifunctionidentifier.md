---
title: IFunctionIdentifier
category: API-interfaces
layout: docs
---


# Interface: IFunctionIdentifier

Data type to register a CSI, DCS or ESC callback in the parser
in the form:
   ESC I..I F
   CSI Prefix P..P I..I F
   DCS Prefix P..P I..I F data_bytes ST

with these rules/restrictions:
- prefix can only be used with CSI and DCS
- only one leading prefix byte is recognized by the parser
  before any other parameter bytes (P..P)
- intermediate bytes are recognized up to 2

For custom sequences make sure to read ECMA-48 and the resources at
vt100.net to not clash with existing sequences or reserved address space.
General recommendations:
- use private address space (see ECMA-48)
- use max one intermediate byte (technically not limited by the spec,
  in practice there are no sequences with more than one intermediate byte,
  thus parsers might get confused with more intermediates)
- test against other common emulators to check whether they escape/ignore
  the sequence correctly

Notes: OSC command registration is handled differently (see addOscHandler)
       APC, PM or SOS is currently not supported.

## Hierarchy

* **IFunctionIdentifier**

## Index

### Properties

* [final]({% link _docs/api/terminal/interfaces/ifunctionidentifier.md %}#final)
* [intermediates]({% link _docs/api/terminal/interfaces/ifunctionidentifier.md %}#optional-intermediates)
* [prefix]({% link _docs/api/terminal/interfaces/ifunctionidentifier.md %}#optional-prefix)

## Properties

###  final

• **final**: *string*

*Defined in [xterm.d.ts:1740](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1740)*

Final byte, must be in range \x40 .. \x7e for CSI and DCS,
\x30 .. \x7e for ESC.

___

### `Optional` intermediates

• **intermediates**? : *string*

*Defined in [xterm.d.ts:1735](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1735)*

Optional intermediate bytes, must be in range \x20 .. \x2f.
Usable in CSI, DCS and ESC.

___

### `Optional` prefix

• **prefix**? : *string*

*Defined in [xterm.d.ts:1730](https://github.com/xtermjs/xterm.js/blob/5.5.0/typings/xterm.d.ts#L1730)*

Optional prefix byte, must be in range \x3c .. \x3f.
Usable in CSI and DCS.
