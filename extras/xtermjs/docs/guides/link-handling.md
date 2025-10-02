---
title: Link Handling
category: Guides
---

Clickable links may appear in the terminal output two ways:

* Emitted using an explicit escape sequence (OSC 8).
* Implicitly using something that looks a URL in the output, recognized using pattern matching. This requires the `web-links` addon.

## Setup

This is one way to handle both kinds of links.

```js
function activateLink(event, uri) {
  doHandleLink(); // example: open link in new browser window
}
let linkHandler = {
  activate: (event, text, range) => { activateLink(event, text); } ,
  hover: (event, text, range) => { /* nothing, by default */},
  leave: (event, text, range) => { /* nothing, by default */},
  allowNonHttpProtocols: true
};

/* Detect links because of URL patterns. */
webLinksAddon = new WebLinksAddon(activateLink, linkHandler);
/* Handle explicit links using USC 8 escape sequences. */
xterm.options.linkHandler = linkHandler;
```

## Require modifier key

Terminal emulators that handle clicking on a link usually require a modifier to be pressed, to avoid unintentional window-opening. Commonly the `Ctrl` modifier (or on macOS the `Cmd` modifier) must be pressed.

```js
function linkRequiresModifier() { return true; }
function isMac() {
  return typeof navigator != "undefined" ? /Mac/.test(navigator.platform)
      : typeof os != "undefined" ? os.platform() == "darwin" : false;
}

// Replace activateLink above
function activateLink(event, uri) {
  if ((isMac() ? event.metaKey : event.ctrlKey)
    || ! linkRequiresModifier()) {
      doHandleLink(); // example: open link in new browser window
  }
}
```

## Display URL on hovering

It might be helpful to show the full URL when hovering over a link, especially for links created by OSC 8 (which might not show the actual URL). This is safety feature commonly implemented in web browsers and mail readers.

```js
let _linkPopup;
function removeLinkPopup = (event, text, range) {
  if (_linkPopup) {
     _linkPopup.remove();
     _linkPopup = undefined;
  }
}
function showLinkPopup(event, text, range) {
  removeLinkPopup(event, text, range);
  let popup = document.createElement('div');
  popup.classList.add('xterm-link-popup');
  popup.style.position = 'absolute';
  popup.style.top = (event.clientY + 25) + 'px';
  popup.style.right = '0.5em';
  popup.innerText = text;
  if (linkRequiresModifier()) {
    popup.appendChild(document.createElement('br'));
    const e2 = document.createElement('i');
    e2.innerText = `(${isMac() ? 'Cmd' : 'Ctrl'}+Click to open link)`;
    popup.appendChild(e2);
  }
  const topElement = event.target.parentNode;
  topElement.appendChild(popup);
  const popupHeight = popup.offsetHeight;
  if (event.clientY + 25 + popupHeight > topNode.clientHeight) {
    let y = event.clientY - 25 - popupHeight;
    popup.style.top = (y < 0 ? 0 : y) + 'px';
  }
  _linkPopup = popup;
};

linkHandler.hover = showLinkPopup;
linkHandler.leave = removeLinkPopup;
```

Possible CSS styling for the hover popup:

```css
div.xterm-link-popup {
  font-size: small;
  line-break: normal; /* auto doesn't work for WebKit */
  padding: 4px;
  min-width: 15em;
  max-width: 80%;
  border: thin solid;
  border-radius: 6px;
  background: #6c4c4c;
  border-color: #150262;
}
```
