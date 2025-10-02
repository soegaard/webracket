---
title: Downloading
category: Guides
---

The recommended way to get xterm.js is via a package manager like [npm](https://npmjs.org/package/xterm) or [Yarn](https://yarnpkg.com/en/).

You can either download official releases of xterm.js or arbitrary commits, which is not encouraged for production usage. All the production ready releases of xterm.js are available at [https://github.com/xtermjs/xterm.js/releases](https://github.com/xtermjs/xterm.js/releases).

## Npm and Yarn

The best way to download xterm.js for use in your project is with [`npm`](http://npmjs.org/) or [`yarn`](https://yarnpkg.com/en/).

```bash
# Install with npm
npm install @xterm/xterm

# Or alternatively, install with Yarn
yarn add @xterm/xterm
```

## GitHub Releases

All xterm.js releases are available as tar.gz and zip archives at [https://github.com/xtermjs/xterm.js/releases](https://github.com/xtermjs/xterm.js/releases), to download them manually.

## Clone the Git repository
You can clone the xterm.js repository and checkout any arbitrary commit you would like to use.

**We do not suggest doing this** unless you are certain about your actions, as only official tagged releases of xterm.js are considered production-ready.

Also, make sure to **manually install dependencies and build xterm.js** after cloning.

```bash
git clone https://github.com/xtermjs/xterm.js
cd xterm.js
npm install
npm run build
```

## 📣 ⛔️ No more Bower support

Xterm.js stopped supporting Bower since version 3.0.0. You should use [`npm`](http://npmjs.org/) or [`yarn`](https://yarnpkg.com/en/) instead.

## Importing in your application

Next, take a look at the different ways of [importing xterm.js](/docs/guides/import) into your project.
