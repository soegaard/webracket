#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
PAGE="$ROOT_DIR/web-site/public/web-easy-examples.html"
INDEX_PAGE="$ROOT_DIR/web-site/public/web-easy-examples/index.html"

{
  cat <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Web-Easy Examples</title>
  <style>
    :root { color-scheme: dark; --bg: #0c0d1a; --panel: rgba(20, 22, 40, 0.92); --text: #e6e8f2; --muted: #aeb4cc; --accent: #8f9dff; }
    html, body { margin: 0; min-height: 100%; background: radial-gradient(circle at top, rgba(101, 79, 240, 0.25), transparent 55%), var(--bg); color: var(--text); font-family: Inter, "Fira Code", system-ui, sans-serif; }
    body { line-height: 1.6; }
    main { max-width: 920px; margin: 0 auto; padding: 4rem 1.5rem 5rem; }
    .panel { background: var(--panel); border: 1px solid rgba(255,255,255,0.08); border-radius: 24px; padding: 2rem; box-shadow: 0 30px 80px rgba(0,0,0,0.35); }
    .section { margin-top: 2rem; }
    h1 { margin: 0 0 0.5rem; font-size: clamp(2.5rem, 5vw, 4rem); line-height: 1.05; }
    h2 { margin: 0 0 0.5rem; font-size: clamp(1.4rem, 2.5vw, 2rem); line-height: 1.1; }
    p { margin: 0 0 1rem; color: var(--muted); }
    .section-grid { display: grid; grid-template-columns: minmax(0, 1fr); gap: 1rem; margin-top: 1rem; }
    .seven-gui-layout { display: grid; grid-template-columns: minmax(0, 1.4fr) minmax(260px, 0.8fr); gap: 1.5rem; align-items: start; }
    .fact-box { background: rgba(143, 157, 255, 0.10); border: 1px solid rgba(143, 157, 255, 0.25); border-radius: 18px; padding: 1rem 1.1rem; color: var(--text); }
    .fact-box h3 { margin: 0 0 0.35rem; font-size: 1rem; }
    .fact-box p { margin: 0; }
    .group { margin-top: 1rem; }
    .group h3 { margin: 0 0 0.35rem; font-size: 1.1rem; }
    ul { margin: 0; padding-left: 1.2rem; }
    li { margin: 0 0 0.55rem; break-inside: avoid; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    @media (max-width: 700px) { .seven-gui-layout { grid-template-columns: 1fr; } main { padding-inline: 1rem; } .panel { padding: 1.25rem; } }
  </style>
</head>
<body>
  <main>
    <section class="panel">
      <h1>Web-Easy Examples</h1>
      <p>Compiled example pages from <code>lib/web-easy/examples/</code>.</p>
      <section class="section">
        <h2>Handwritten examples</h2>
        <p>Small starter apps written by hand to show the basic Web Easy workflow.</p>
        <div class="section-grid">
          <ul>
EOF
  left_examples=(
    hello-world
    a-single-counter
    multiple-counters
    dynamic-counters
    add-two-numbers
    todo-lists
  )
  query_examples=(
    query-helpers
    query-counter
    query-toggle
    query-delegate-list
    query-once
    query-trigger
  )
  seven_gui_examples=(
    7gui-counter
    7gui-temperature-converter
    7gui-flight-booker
    7gui-timer
    7gui-crud
    7gui-circle
    7gui-circle-extended
  )
  machine_written_examples=(
    audio-oscillator
    audio-buffer-playback
    audio-analyser
    audio-effects
    websocket
  )

  emit_example_items() {
    local example_name label
    for example_name in "$@"; do
      case "$example_name" in
        7gui-*)
          label="7 GUI $(printf '%s' "${example_name#7gui-}" | perl -pe 's/-/ /g; s/(^| )([a-z])/$1\U$2/g')"
          ;;
        websocket)
          label="WebSocket"
          ;;
        *)
          label="$(printf '%s' "$example_name" | perl -pe 's/-/ /g; s/(^| )([a-z])/$1\U$2/g')"
          ;;
      esac
      printf '          <li><a href="web-easy-examples/%s/%s.html">%s</a></li>\n' "$example_name" "$example_name" "$label"
    done
  }

  emit_example_items "${left_examples[@]}"
  cat <<'EOF'
          </ul>
        </div>
      </section>
      <section class="section">
        <h2>Query Examples</h2>
        <p>Examples that show the <code>query</code> helpers, from <code>.text!</code>, <code>.val!</code>, and <code>.data!</code> to <code>.on</code>, <code>.off</code>, <code>.once</code>, <code>.trigger</code>, and <code>.on-delegate</code> for selector-based event wiring.</p>
        <div class="section-grid">
          <ul>
EOF
  emit_example_items "${query_examples[@]}"
  cat <<'EOF'
          </ul>
        </div>
      </section>
      <section class="section">
        <h2>7 GUI Examples</h2>
        <p>The classic 7 GUIs benchmark is a teaching set of seven small interface problems used to compare UI toolkits and architecture styles. It covers counter updates, temperature conversion, flight booking, timers, CRUD editing, and circle drawing.</p>
        <div class="seven-gui-layout">
          <div class="section-grid">
            <ul>
EOF
  emit_example_items "${seven_gui_examples[@]}"
  cat <<'EOF'
            </ul>
          </div>
          <aside class="fact-box">
            <h3>Fact box</h3>
            <p>7 GUI is a small benchmark suite that helps compare how different UI libraries handle common interaction patterns. The seven tasks are intentionally simple, but they reveal how much ceremony a toolkit needs for state, validation, layout, and updates.</p>
          </aside>
        </div>
      </section>
      <section class="section">
        <h2>Machine-written examples</h2>
        <p>Examples generated or assembled around browser APIs such as Audio and WebSocket.</p>
        <div class="section-grid">
          <div class="group">
            <h3>Audio</h3>
            <ul>
EOF
  emit_example_items audio-oscillator audio-buffer-playback audio-analyser audio-effects
  cat <<'EOF'
            </ul>
          </div>
          <div class="group">
            <h3>WebSocket</h3>
            <ul>
EOF
  emit_example_items websocket
  cat <<'EOF'
            </ul>
          </div>
        </div>
      </section>
    </section>
  </main>
</body>
</html>
EOF
} > "$PAGE"

mkdir -p "$(dirname "$INDEX_PAGE")"
cat <<'EOF' > "$INDEX_PAGE"
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="0; url=../web-easy-examples.html">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Web-Easy Examples</title>
</head>
<body>
  <p>Redirecting to <a href="../web-easy-examples.html">Web-Easy Examples</a>…</p>
</body>
</html>
EOF
