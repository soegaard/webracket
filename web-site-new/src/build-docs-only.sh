#!/bin/bash
set -euo pipefail

echo "-- Generating FFI doc pages --"
racket generate-ffi-doc-pages.rkt
racket generate-ffi-doc-pages-structured.rkt

echo "-- Compiling website-docs-only.rkt --"
racket ../../webracket.rkt --browser --ffi dom --ffi standard --ffi js website-docs-only.rkt

perl -0777 -i -pe 's@<html>@<html lang="en">@s' website-docs-only.html
perl -0777 -i -pe 's~<head>.*?</head>~<head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1"><style>:root{--rk-bg:#f6f5f1;--rk-surface:#ffffff;--rk-text:#1f2328;--rk-muted:#5a6470;--rk-link:#114b7a;--rk-link-hover:#8b1a1a;--rk-accent:#8b1a1a;--rk-border:#d6d6d1;--rk-code-bg:#f1efe9;--rk-shadow:0 10px 28px rgba(0,0,0,0.08);}*{box-sizing:border-box;}html,body{margin:0;min-height:100%;background:linear-gradient(180deg,#f4f3ef 0%,#f8f8f6 32%,#ffffff 100%);color:var(--rk-text);}body{line-height:1.62;font-family:\"Lucida Grande\",\"Lucida Sans Unicode\",\"Trebuchet MS\",Verdana,sans-serif;-webkit-font-smoothing:antialiased;max-width:1280px;padding:0 1.2rem 2.2rem;margin:0 auto;}main,article,section,nav,aside,header,footer{max-width:100%;}h1,h2,h3,h4,h5,h6{font-family:Georgia,\"Times New Roman\",Times,serif;line-height:1.24;color:#212429;margin-top:1.1em;margin-bottom:.45em;}h1{font-size:2rem;border-bottom:2px solid #b61e1e;padding-bottom:.25rem;}h2{font-size:1.52rem;border-bottom:1px solid #ddd7cd;padding-bottom:.2rem;}h3{font-size:1.25rem;}p,li,dt,dd{color:var(--rk-text);}a{color:var(--rk-link);text-decoration:none;border-bottom:1px solid rgba(17,75,122,.25);transition:color .15s ease,border-color .15s ease;}a:hover,a:focus-visible{color:var(--rk-link-hover);border-color:rgba(139,26,26,.5);}code,kbd,samp,pre{font-family:\"Fira Code\",\"Consolas\",\"Menlo\",monospace;}code,kbd,samp{background:var(--rk-code-bg);border:1px solid #dfdbcf;border-radius:4px;padding:.1rem .32rem;font-size:.94em;}pre{background:var(--rk-code-bg);border:1px solid #dfdbcf;border-left:4px solid #b61e1e;border-radius:8px;padding:.85rem 1rem;overflow:auto;}table{width:100%;border-collapse:collapse;background:var(--rk-surface);border:1px solid var(--rk-border);border-radius:8px;overflow:hidden;}th,td{padding:.52rem .62rem;border-bottom:1px solid #e6e3dc;text-align:left;}th{background:#f2eee4;color:#3a3d44;font-weight:700;}tr:nth-child(even) td{background:#fbfaf7;}hr{border:0;border-top:1px solid #d8d4ca;margin:1.5rem 0;}button,.button,input[type=\"button\"],input[type=\"submit\"]{font:inherit;color:#fff;background:#8b1a1a;border:1px solid #751414;border-radius:7px;padding:.42rem .75rem;cursor:pointer;}button:hover,.button:hover,input[type=\"button\"]:hover,input[type=\"submit\"]:hover{background:#701414;}input,select,textarea{font:inherit;color:var(--rk-text);background:#fff;border:1px solid #cfc9bc;border-radius:6px;padding:.42rem .55rem;}blockquote{margin:1rem 0;padding:.75rem 1rem;border-left:4px solid #be9d5a;background:#f9f6ef;color:#38414a;}img,svg,canvas{max-width:100%;height:auto;}#app,.app,.container,.content,.docs-content,.docs-main,.docs-layout,.hero,.card,.panel,.toc-card,.example-card{background:var(--rk-surface);border:1px solid var(--rk-border);border-radius:10px;box-shadow:var(--rk-shadow);}header,.site-header,.topbar,.navbar,.nav,.docs-header{background:linear-gradient(180deg,#f9f7f2,#f1eee5);border-bottom:1px solid #d8d2c6;}nav a,.nav a,.menu a{font-weight:600;border-bottom:none;padding:.2rem .15rem;}nav a:hover,.nav a:hover,.menu a:hover{color:#8b1a1a;}footer,.site-footer{color:var(--rk-muted);border-top:1px solid #d9d5cb;padding-top:1rem;margin-top:2rem;}::selection{background:#f7d8a8;color:#2a1a10;}@media (max-width:900px){body{padding:0 .85rem 1.4rem;}h1{font-size:1.7rem;}h2{font-size:1.34rem;}table{font-size:.95rem;display:block;overflow-x:auto;}}</style></head>~s' website-docs-only.html

echo "-- Copying Docs Assets --"
mkdir -p ../public/assets/vendor/jsxgraph
mkdir -p ../public/css/fonts
cp website-docs-only.wasm ../public
cp css/app.css ../public/css/app.css
cp css/styles.css ../public/css/styles.css
cp css/fonts/fonts.css ../public/css/fonts/fonts.css
cp website-docs-only.html ../public/documentation.html
cp website-docs-only.html ../public/documentation-compiler-overview.html
cp website-docs-only.html ../public/documentation-js-ffi.html
cp website-docs-only.html ../public/documentation-ffi-standard.html
cp website-docs-only.html ../public/documentation-ffi-dom.html
cp website-docs-only.html ../public/documentation-ffi-js.html
cp website-docs-only.html ../public/documentation-ffi-math.html
cp website-docs-only.html ../public/documentation-ffi-jsxgraph.html
cp website-docs-only.html ../public/documentation-ffi-xtermjs.html
cp website-docs-only.html ../public/documentation-extended-example-jsxgraph-board-points.html
cp website-docs-only.html ../public/documentation-extended-example-jsxgraph-geometry-constructors.html
cp ../../extras/jsxgraph/docs/static/jsxgraphcore.js ../public/assets/vendor/jsxgraph/jsxgraphcore.js
cp ../../extras/jsxgraph/docs/static/jsxgraph.css ../public/assets/vendor/jsxgraph/jsxgraph.css

echo "-- Done --"
if [[ "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff
fi
