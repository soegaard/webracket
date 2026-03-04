// Shared guard helpers for smoke dashboard pages.

window.smokeDashboardGuard = (() => {
  const inlineStyleAllowlist = new Set([
    "vpanel",
    "hpanel",
    "group",
    "if-view",
    "cond-view",
    "case-view"
  ]);
  const forbiddenAttrTokens = [
    "#<value>",
    "#<void>",
    "#<procedure>",
    "#<eof>",
    "[object Object]",
    "undefined",
    "NaN",
    "Infinity"
  ];

  function tokens() {
    return forbiddenAttrTokens.slice();
  }

  function findForbiddenAttrTokensInDoc(doc, originLabel) {
    const offenders = [];
    const nodes = Array.from(doc.querySelectorAll("*"));
    for (const el of nodes) {
      for (const attr of Array.from(el.attributes || [])) {
        const v = attr.value || "";
        const hit = forbiddenAttrTokens.find((tok) => v.includes(tok));
        if (hit) {
          offenders.push(
            originLabel + " " + el.tagName.toLowerCase() +
            "[" + attr.name + "=" + JSON.stringify(v) + ", token=" + JSON.stringify(hit) + "]"
          );
        }
      }
    }
    return offenders;
  }

  function findForbiddenAttrTokensRecursively(doc, originLabel) {
    let offenders = findForbiddenAttrTokensInDoc(doc, originLabel);
    const iframes = Array.from(doc.querySelectorAll("iframe"));
    for (const frame of iframes) {
      const childDoc = frame.contentDocument || (frame.contentWindow && frame.contentWindow.document);
      if (childDoc) {
        const src = frame.getAttribute("src") || "<inline>";
        offenders = offenders.concat(
          findForbiddenAttrTokensRecursively(childDoc, originLabel + " -> iframe(" + src + ")")
        );
      }
    }
    return offenders;
  }

  function findInlineStyleAttrsOnWidgetsInDoc(doc, originLabel) {
    const offenders = [];
    const nodes = Array.from(doc.querySelectorAll("[data-we-widget][style]"));
    for (const el of nodes) {
      const widget = el.getAttribute("data-we-widget") || "";
      if (inlineStyleAllowlist.has(widget)) {
        continue;
      }
      const styleValue = el.getAttribute("style");
      if (styleValue && styleValue.trim() !== "") {
        offenders.push(
          originLabel + " " + el.tagName.toLowerCase() +
          "[data-we-widget=" + JSON.stringify(widget) +
          ", style=" + JSON.stringify(styleValue) + "]"
        );
      }
    }
    return offenders;
  }

  function findInlineStyleAttrsOnWidgetsRecursively(doc, originLabel) {
    let offenders = findInlineStyleAttrsOnWidgetsInDoc(doc, originLabel);
    const iframes = Array.from(doc.querySelectorAll("iframe"));
    for (const frame of iframes) {
      const childDoc = frame.contentDocument || (frame.contentWindow && frame.contentWindow.document);
      if (childDoc) {
        const src = frame.getAttribute("src") || "<inline>";
        offenders = offenders.concat(
          findInlineStyleAttrsOnWidgetsRecursively(childDoc, originLabel + " -> iframe(" + src + ")")
        );
      }
    }
    return offenders;
  }

  return {
    tokens,
    findForbiddenAttrTokensInDoc,
    findForbiddenAttrTokensRecursively,
    findInlineStyleAttrsOnWidgetsInDoc,
    findInlineStyleAttrsOnWidgetsRecursively
  };
})();
