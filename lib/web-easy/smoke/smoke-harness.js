// Shared helpers for iframe-based smoke/contract pages.
(function () {
  function setStatus(text, cls) {
    var status = document.getElementById("status");
    status.textContent = text;
    status.className = cls;
  }

  function sleep(ms) {
    return new Promise(function (resolve) {
      setTimeout(resolve, ms);
    });
  }

  function frameDocFrom(frame) {
    return frame.contentDocument || (frame.contentWindow && frame.contentWindow.document);
  }

  function bodyText(frameDoc) {
    return frameDoc && frameDoc.body ? (frameDoc.body.textContent || "") : "";
  }

  function assertTrue(v, label) {
    if (!v) {
      throw new Error(label + ": expected true, got false");
    }
  }

  function assertEqual(got, want, label) {
    if (got !== want) {
      throw new Error(label + ": expected " + JSON.stringify(want) + ", got " + JSON.stringify(got));
    }
  }

  function dispatchKey(el, key, options) {
    var doc = el && el.ownerDocument;
    var view = doc && doc.defaultView;
    var KeyEvent = view && view.KeyboardEvent ? view.KeyboardEvent : KeyboardEvent;
    var init = Object.assign({ key: key, bubbles: true }, options || {});
    el.dispatchEvent(new KeyEvent("keydown", init));
  }

  function hasForbiddenAttrTokens(doc, pageLabel) {
    var offenders = [];
    var nodes = doc.querySelectorAll("*");
    var pattern = /#<value>|#<undefined>|#<void>|#<object>|#<procedure>/;
    for (var i = 0; i < nodes.length; i += 1) {
      var node = nodes[i];
      for (var j = 0; j < node.attributes.length; j += 1) {
        var attr = node.attributes[j];
        if (pattern.test(attr.value)) {
          offenders.push((pageLabel || "page") + ": " + node.tagName.toLowerCase() + "[" + attr.name + "]=" + JSON.stringify(attr.value));
        }
      }
    }
    return offenders;
  }

  function hasInlineWidgetStyles(doc, pageLabel) {
    var offenders = [];
    var nodes = doc.querySelectorAll("[data-we-widget]");
    for (var i = 0; i < nodes.length; i += 1) {
      var node = nodes[i];
      if (node.hasAttribute("style")) {
        offenders.push((pageLabel || "page") + ": " + node.tagName.toLowerCase() + "[data-we-widget=" + JSON.stringify(node.getAttribute("data-we-widget")) + "] has inline style");
      }
    }
    return offenders;
  }

  function assertNoForbiddenAttrTokens(doc, pageLabel) {
    var offenders = hasForbiddenAttrTokens(doc, pageLabel);
    if (offenders.length > 0) {
      var sample = offenders.slice(0, 4).join("; ");
      throw new Error("Found forbidden attr token(s): " + sample);
    }
  }

  function assertNoInlineWidgetStyles(doc, pageLabel) {
    var offenders = hasInlineWidgetStyles(doc, pageLabel);
    if (offenders.length > 0) {
      var sample = offenders.slice(0, 4).join("; ");
      throw new Error("Found inline style on widget node(s): " + sample);
    }
  }

  async function waitForCondition(label, timeoutMs, pred) {
    var start = Date.now();
    while (Date.now() - start < timeoutMs) {
      if (pred()) {
        return;
      }
      await sleep(25);
    }
    throw new Error("Timed out waiting for " + label);
  }

  async function loadCase(frame, src, readyFn, timeoutMs) {
    await new Promise(function (resolve, reject) {
      var timer = setTimeout(function () {
        reject(new Error("Timed out loading iframe: " + src));
      }, timeoutMs);
      frame.addEventListener("load", function () {
        clearTimeout(timer);
        resolve();
      }, { once: true });
      frame.src = src;
    });

    var start = Date.now();
    while (Date.now() - start < timeoutMs) {
      var frameDoc = frameDocFrom(frame);
      if (frameDoc && readyFn(frameDoc)) {
        return frameDoc;
      }
      await sleep(25);
    }
    throw new Error("Timed out waiting for ready state: " + src);
  }

  window.SmokeHarness = {
    setStatus: setStatus,
    sleep: sleep,
    frameDocFrom: frameDocFrom,
    bodyText: bodyText,
    assertTrue: assertTrue,
    assertEqual: assertEqual,
    dispatchKey: dispatchKey,
    waitForCondition: waitForCondition,
    loadCase: loadCase,
    assertNoForbiddenAttrTokens: assertNoForbiddenAttrTokens,
    assertNoInlineWidgetStyles: assertNoInlineWidgetStyles
  };
})();
