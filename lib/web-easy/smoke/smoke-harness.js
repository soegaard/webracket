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

  function dispatchKey(el, key) {
    el.dispatchEvent(new KeyboardEvent("keydown", { key: key, bubbles: true }));
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
    loadCase: loadCase
  };
})();
