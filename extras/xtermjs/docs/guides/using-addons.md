---
title: Using addons
category: Guides
---

Addons are JavaScript modules that use the xterm.js API to extend functionality of the terminal.

There are a handful available in the main repository in the [`addons`](https://github.com/xtermjs/xterm.js/tree/master/addons/) directory, but you can even write your own by leveraging on the [xterm.js public API](/docs/).

To use an xterm.js addon, you have to:

1. [Import it](/docs/guides/import/) into your code
2. Pass it as argument to the static method [`Terminal.loadAddon`](/docs/api/terminal/classes/terminal/#loadaddon)

## Usage example

```ts
import { Terminal } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';

const term = new Terminal();
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);

// Open the terminal in #terminal-container
term.open(document.getElementById('terminal-container'));

// Make the terminal's size and geometry fit the size of #terminal-container
fitAddon.fit();
```

## Creating an addon

Creating an addon is quite simple, you just need to export some object that has an `activate` and `dispose` method. The following addon logs any `onData` events coming from the terminal which fire when the user types:

```ts
import { Terminal, IDisposable } from '@xterm/xterm';

class DataLoggerAddon {
  private _disposables: IDisposable[] = [];

  activate(terminal: Terminal): void {
    this._disposables.push(terminal.onData(d => console.log(d)));
  }

  dispose(): void {
    this._disposables.forEach(d => d.dispose());
    this._disposables.length = 0;
  }
}
```

Use `Terminal.loadAddon` to start logging the data events:

```ts
const terminal = new Terminal();
terminal.loadAddon(new ExampleAddon());
```
