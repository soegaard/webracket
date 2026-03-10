#!/usr/bin/env node

function isIgnorableMessage(text) {
  const normalized = String(text || "").toLowerCase();
  if (normalized.includes("favicon.ico") && normalized.includes("404")) return true;
  // Chromium often reports favicon misses without the URL in the message.
  if (normalized === "failed to load resource: the server responded with a status of 404 (not found)") {
    return true;
  }
  if (normalized === "failed to load resource: the server responded with a status of 404 (file not found)") {
    return true;
  }
  return false;
}

function isIgnorablePageError(text) {
  const normalized = String(text || "").toLowerCase().trim();
  // In dashboard mode, iframe teardown can emit bare "unreachable" pageerrors
  // after PASS rows are already recorded. Keep console-error assertions strict.
  if (normalized === "unreachable") return true;
  return false;
}

export function installPlaywrightErrorGuard(page) {
  const events = [];

  page.on("pageerror", (err) => {
    const message = err && err.message ? err.message : String(err);
    if (isIgnorablePageError(message)) return;
    events.push({ kind: "pageerror", text: message });
  });

  page.on("console", (msg) => {
    if (msg.type() !== "error") return;
    const text = msg.text() || "";
    if (isIgnorableMessage(text)) return;
    events.push({ kind: "console", text });
  });

  return {
    mark() {
      return events.length;
    },

    assertSince(mark, contextLabel) {
      const hits = events.slice(mark);
      if (hits.length === 0) return;
      const lines = hits.map((event, idx) =>
        `  ${idx + 1}. [${event.kind}] ${event.text.replace(/\s+/g, " ").trim()}`
      );
      throw new Error(`${contextLabel} captured browser errors:\n${lines.join("\n")}`);
    },
  };
}
