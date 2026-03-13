(function () {
  function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  function frameDocFrom(frame) {
    return frame.contentDocument || (frame.contentWindow && frame.contentWindow.document);
  }

  function normalizedCssColor(v) {
    return (v || "").replace(/\s+/g, "").toLowerCase();
  }

  function assertTrue(v, label) {
    if (!v) {
      throw new Error(label + ": expected true, got false");
    }
  }

  function ensureThemeLayer(frameDoc, options) {
    const themeClass = options && options.themeClass ? options.themeClass : "we-theme-solar2";
    const coreHref = options && options.coreHref ? options.coreHref : "../web-easy-core.css";
    const themeHref = options && options.themeHref ? options.themeHref : "../theme-contract-vars.css";

    frameDoc.documentElement.classList.add(themeClass);

    let coreLink = frameDoc.getElementById("we-theme-core-css");
    if (!coreLink) {
      coreLink = frameDoc.createElement("link");
      coreLink.id = "we-theme-core-css";
      coreLink.rel = "stylesheet";
      frameDoc.head.appendChild(coreLink);
    }
    coreLink.href = coreHref;

    let themeLink = frameDoc.getElementById("we-theme-external-css");
    if (!themeLink) {
      themeLink = frameDoc.createElement("link");
      themeLink.id = "we-theme-external-css";
      themeLink.rel = "stylesheet";
      frameDoc.head.appendChild(themeLink);
    }
    themeLink.href = themeHref;
  }

  window.themeContractHelper = {
    assertTrue: assertTrue,
    ensureThemeLayer: ensureThemeLayer,
    frameDocFrom: frameDocFrom,
    normalizedCssColor: normalizedCssColor,
    sleep: sleep
  };
})();
