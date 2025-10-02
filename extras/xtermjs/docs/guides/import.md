---
title: Importing
category: Guides
---

The preferred way to import xterm.js and its addons is the [`import` statement](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import) of ES6 Modules.

> **⚠️  Attention!** Do not forget to import the [CSS stylesheet](https://github.com/xtermjs/xterm.js/blob/master/css/xterm.css) of xterm.js into your app to get it working appropriately.

## Example

```javascript

import { Terminal } from '@xterm/xterm';

const term = new Terminal();

term.open(document.getElementById('xterm-container'));
```
