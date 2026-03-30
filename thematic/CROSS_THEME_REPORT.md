# Thematic Cross-Theme Report

This report captures a small validation pass for `tools/thematic` against
several real Bootswatch themes.

## Inputs

- [`thematic/original/superhero/bootstrap.css`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/original/superhero/bootstrap.css)
- [`thematic/original/sketchy/bootstrap.css`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/original/sketchy/bootstrap.css)
- [`thematic/original/lux/bootstrap.css`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/original/lux/bootstrap.css)
- [`thematic/original/sandstone/bootstrap.css`](/Users/soegaard/Dropbox/GitHub/webracket/thematic/original/sandstone/bootstrap.css)

## Validation Command

```sh
for theme in superhero sketchy lux sandstone; do
  racket tools/thematic \
    --input thematic/original/$theme/bootstrap.css \
    --output /tmp/$theme-theme.css \
    --selector ':root'
done
```

## Token Summary

| Theme | `--we-bg` | `--we-fg` | `--we-surface-raised` | `--we-surface-subtle` | `--we-popup-bg` | `--we-primary` | `--we-primary-emphasis` | `--we-fg-muted` | `--we-radius-md` | `--we-font-family-base` |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Superhero | `#0f2537` | `#ebebeb` | `#4e5d6c` | `#2b3e4f` | `#4e5d6c` | `#df6919` | `#592a0a` | `rgba(235, 235, 235, 0.750)` | `0` | `var(--bs-font-sans-serif)` |
| Sketchy | `#fff` | `#212529` | `#f8f9fa` | `#fcfcfd` | `#f8f9fa` | `#333` | `#141414` | `rgba(33, 37, 41, 0.750)` | `25px` | `var(--bs-font-sans-serif)` |
| Lux | `#fff` | `#55595c` | `#f8f9fa` | `#fafafa` | `#f8f9fa` | `#1a1a1a` | `#0a0a0a` | `rgba(85, 89, 92, 0.750)` | `0.375rem` | `var(--bs-font-sans-serif)` |
| Sandstone | `#fff` | `#3e3f3a` | `#f8f9fa` | `#fdfcfa` | `#f8f9fa` | `#325d88` | `#142536` | `rgba(62, 63, 58, 0.750)` | `0.375rem` | `var(--bs-font-sans-serif)` |

## Notes

- Superhero is the strongest regression test for neutral-surface handling.
  Its generated output now keeps dark raised/popup surfaces instead of
  collapsing to bright light neutrals.
- Sketchy confirms that large authored radii survive extraction and emission.
- Lux and Sandstone confirm that light themes still produce restrained, light
  raised surfaces after the neutral-surface heuristic changes.
- All four themes preserved their main theme personality through the generated
  `--we-primary` and `--we-primary-emphasis` tokens.
