#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
PAGE="$ROOT_DIR/web-site/public/web-easy-examples.html"

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
    h1 { margin: 0 0 0.5rem; font-size: clamp(2.5rem, 5vw, 4rem); line-height: 1.05; }
    p { margin: 0 0 1rem; color: var(--muted); }
    .example-columns { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 2rem; margin-top: 1.5rem; }
    ul { margin: 0; padding-left: 1.2rem; }
    li { margin: 0 0 0.55rem; break-inside: avoid; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    @media (max-width: 700px) { .example-columns { grid-template-columns: 1fr; } main { padding-inline: 1rem; } .panel { padding: 1.25rem; } }
  </style>
</head>
<body>
  <main>
    <section class="panel">
      <h1>Web-Easy Examples</h1>
      <p>Compiled example pages from <code>lib/web-easy/examples/</code>.</p>
      <div class="example-columns">
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
  right_examples=(
    7gui-counter
    7gui-temperature-converter
    7gui-flight-booker
    7gui-timer
    7gui-crud
    7gui-circle
    7gui-circle-extended
  )

  emit_example_items() {
    local example_name label
    for example_name in "$@"; do
      label="$(printf '%s' "$example_name" | perl -pe 's/-/ /g; s/(^| )([a-z])/$1\U$2/g')"
      printf '          <li><a href="web-easy-examples/%s/%s.html">%s</a></li>\n' "$example_name" "$example_name" "$label"
    done
  }

  emit_example_items "${left_examples[@]}"
  cat <<'EOF'
        </ul>
        <ul>
EOF
  emit_example_items "${right_examples[@]}"
  cat <<'EOF'
        </ul>
      </div>
    </section>
  </main>
</body>
</html>
EOF
} > "$PAGE"
