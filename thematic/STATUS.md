# Thematic Status

## Summary

`thematic` is now a working Bootstrap-to-`web-easy` theme generator with a repeatable review loop.

Current status:

- source lives in [`thematic/main.rkt`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/main.rkt)
- launcher lives in [`tools/thematic`](/Users/soegaard/Dropbox/GitHub/webracket/tools/thematic)
- generated reference outputs live in [`thematic/generated`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/generated)
- smoke preview pages live in [`lib/web-easy/smoke/test-browser-theme-thematic.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic.html) and [`lib/web-easy/smoke/test-browser-theme-thematic-compare.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic-compare.html)

## What Has Been Implemented

- Parses Bootstrap CSS with `(require lexers/css)`.
- Extracts custom properties for one selector context.
- Matches selectors inside comma-separated selector lists.
- Supports `:root`, `[data-bs-theme=light]`, and `[data-bs-theme=dark]`.
- Normalizes Bootstrap variables into an internal theme model.
- Preserves and prefers Bootstrap companion tokens when present:
  - `--bs-*-text-emphasis`
  - `--bs-*-bg-subtle`
  - `--bs-*-border-subtle`
  - `--bs-secondary-color`
  - `--bs-tertiary-color`
  - `--bs-secondary-bg`
  - `--bs-tertiary-bg`
- Derives missing `--we-*` semantic and neutral surface tokens.
- Infers light-vs-dark behavior from the selected body background.
- Emits a full starter stylesheet using the shared `web-easy` scaffold.
- Preserves top-level CSS `@import` rules and emits them above the generated stylesheet.
- Resolves simple Bootstrap custom-property indirections such as:
  - `--bs-body-font-family: var(--bs-font-sans-serif)`
- Supports `--accent-boost` as an opt-in mode for stronger low-contrast accent companions.
- Includes a regeneration script:
  - [`thematic/regenerate.sh`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/regenerate.sh)
- Includes current real-theme fixtures and generated outputs for:
  - Superhero
  - Sketchy
  - Lux
  - Sandstone
  - Lux Boost
  - Sandstone Boost

## Preview And Validation Workflow

- Cross-theme token summary exists in [`thematic/CROSS_THEME_REPORT.md`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/CROSS_THEME_REPORT.md).
- Main preview page:
  - [`lib/web-easy/smoke/test-browser-theme-thematic.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic.html)
- Side-by-side strict vs boost preview:
  - [`lib/web-easy/smoke/test-browser-theme-thematic-compare.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic-compare.html)
- Main preview supports `?theme=` in the URL.
- Compare preview supports `?pair=lux` and `?pair=sandstone`.

## Typography Progress

Font support is in better shape now than earlier in the work:

- generated stylesheets preserve Google Fonts imports from the source CSS
- generated font-family tokens now resolve to real font stacks
- theme-specific font families are now visible in generated output for:
  - Superhero -> `Lato`
  - Sketchy -> `Neucha`
  - Lux -> `Nunito Sans`
  - Sandstone -> `Roboto`

Typography has also been pushed deeper into the shared scaffold:

- base font is used more explicitly for ordinary controls and content rows
- heading font is used more explicitly for title-like chrome
- tabs were specifically fixed to use the theme heading font

## Loose Ends

- The typography pass should still be visually reviewed, especially after the broader scaffold updates.
- `--accent-boost` exists, but its exact strength is still a product decision rather than a finalized policy.
- Some hard-coded non-font literals still exist in the shared starter themes outside the token block. The biggest leak was cleaned up, but the scaffold is not yet perfectly tokenized.
- The preview pages exist, but there is not yet a formal documented review rubric for deciding whether a generated theme is "good enough".
- There is no explicit docs page for `thematic` usage beyond lightweight notes and code comments.

## What Still Needs To Be Implemented

- Documentation for `thematic` itself:
  - purpose
  - CLI usage
  - strict mode vs `--accent-boost`
  - font import behavior
  - selector behavior
- A clearer contract for generated output:
  - which `--we-*` tokens are guaranteed
  - which values come from Bootstrap directly
  - which values are derived
- Better test coverage for:
  - `@import` preservation edge cases
  - nested/odd `var(...)` fallback patterns
  - more real Bootswatch fixtures
- A decision about whether boosted outputs should be documented as recommended for some themes or kept as purely optional.

## What Needs Improvement

- The scaffold still tends to neutralize some light themes, especially themes whose Bootstrap companion tokens are intentionally subtle.
- `Lux` and `Sandstone` remain the best stress cases for "faithful but visually restrained" output.
- Typography probably still needs refinement in a few component classes after the recent pass.
- The preview shell itself is intentionally plain; that keeps focus on widget chrome, but it can also hide how much personality a theme really has.
- The current generator is CSS-first only. That is the right default, but there may eventually be value in a separate SCSS-aware path.

## Ideas For Future Improvements

- Add more opt-in modes rather than silently changing strict behavior.
  - `--accent-boost` is the current example.
- Consider a future typography-specific enhancement mode instead of hard-coding stronger typography assumptions.
- Add an explicit "fidelity vs personality" note to the docs so users understand the difference between strict output and boosted output.
- Add a small machine-readable validation summary for generated themes, for example:
  - resolved font family
  - primary token set
  - light/dark inference
  - whether explicit Bootstrap companion tokens were used
- Add more compare pages, for example:
  - strict vs boost for a single theme
  - multiple themes with only one component family visible
- Consider a token inspection page that lists computed CSS custom properties in-browser for a selected theme.

## Visual Inspection Checklist

When resuming, inspect these first on the main preview page:

- tabs
- menus
- navbar labels and brands
- buttons
- card headers and footers
- table headers
- badges
- dialog/modal/popover titles

Use these pages:

- [`lib/web-easy/smoke/test-browser-theme-thematic.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic.html)
- [`lib/web-easy/smoke/test-browser-theme-thematic-compare.html`](/Users/soegaard/Dropbox/GitHub/webracket/lib/web-easy/smoke/test-browser-theme-thematic-compare.html)

Visual questions to answer:

- Do the four main themes feel distinct in typography as well as color?
- Does `Sketchy` still feel hand-drawn enough?
- Does `Superhero` still feel dense and dark without washed-out accents?
- Does `Lux` stay elegant rather than dull?
- Does `Sandstone` stay warm and editorial rather than generic white?
- Are boosted variants actually better, or just louder?
- Which components still look too neutral across themes?
- Which components look over-themed after the typography pass?

## Recommended Next Steps

1. Do a visual audit with the two preview pages.
2. Decide whether the current typography pass should stay as-is.
3. Decide whether `--accent-boost` should remain optional-only or become the recommended mode for some Bootswatch themes.
4. Write dedicated `thematic` documentation once the visual policy decisions feel stable.
