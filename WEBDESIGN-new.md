# Web Design Rules (New Site)

This document defines the design rules for the **new** WebRacket website, based on the current implementation of:

- Front page (`index.html`)
- Compiler overview page (`documentation-compiler-overview.html`)

## 1. Core Intent

- Use a clean, documentation-first layout with strong visual hierarchy.
- Keep pages readable on light backgrounds.
- Prefer consistency of section/card patterns over one-off styling.
- Make content blocks scan-friendly: short intros, clear headings, grouped cards, and explicit callouts.

## 2. Page Structure

- Use one centered page container (`.page`) with constrained width.
- Use this flow:
  1. Navbar shell
  2. Hero block
  3. Repeating section blocks
  4. Footer
- Build content with shared helpers (`section-block`, `card-grid`, `callout`) instead of custom per-page markup.

## 3. Hero Rules

- Hero is a content region, not a framed card.
- In the new design:
  - No visible hero border.
  - No decorative tag pills in hero panels.
  - Keep headline + short lead only.
- Hero should establish topic and purpose quickly; details belong in following sections.

## 4. Section Rules

- Every major content chunk is a `section-block`.
- Section heading appears once, followed by optional lead text.
- Section interior should use one of:
  - Plain prose
  - Card grid
  - Callout + prose
  - Code block(s)
- Avoid nesting “panel inside panel” when section content can live directly in the section body.

## 5. Card Rules

- Cards are for parallel items (coverage categories, pipeline stages, examples).
- Keep card text concise; avoid long paragraphs.
- Use one card type per row/group where possible.
- For front page and compiler overview:
  - Use uniform card spacing and alignment.
  - Keep titles short and descriptive.

## 6. Compiler Overview-Specific Rules

- Page order:
  1. Introduction
  2. Compiler Pipeline
  3. Direct style vs CPS
  4. Frontend
  5. Middle End
  6. Backend
- Pipeline is presented as cards with explicit stage labels.
- Explanatory prose should be paragraph-based, not one sentence per line.
- References should be grouped in dedicated “References” subsections.
- Source pointers should be grouped in “Source” subsections.

## 7. Notes and Callouts

- Use `callout` component for notes, not ad-hoc boxes.
- New site note style:
  - Light blue background (subtle)
  - No hand icon
  - Clear title + compact body
- Use notes for context, constraints, and terminology; not for core flow content.

## 8. Code and Inline Identifier Rules

- Block code:
  - Simple background
  - No per-line decoration/borders
  - Monospace readable at normal zoom
- Inline identifiers (`code` in prose):
  - Plain text-like style (no pill/background/border)
  - Monospace font only
- Example snippets should match executable examples as closely as possible.

## 9. New/Old Site Switch Rules

- Use segmented control style (`New | Old`), outside main nav links.
- Active side is not clickable.
- Inactive side is clearly clickable.
- New site includes a short note that redesign is in progress.

## 10. Interaction and Accessibility

- Preserve keyboard navigation and visible focus where needed.
- Avoid large container focus outlines that appear during page scrolling.
- Keep button/link affordances unambiguous.
- Provide explicit loading/failure status for JS-driven demos (e.g., JSXGraph board setup).

## 11. Copy Rules

- Prefer short, direct sentences.
- Avoid duplicate explanations between hero and first section.
- Keep terminology consistent:
  - “Frontend”, “Middle End”, “Backend”
  - “Compiler Pipeline”
  - “Direct style”

## 12. Implementation Rules

- Apply shared style changes in `web-site-new/src/web-site.rkt`.
- Keep page content definitions in `web-site-new/src/documentation.rkt` and shared site/page builders.
- If a change affects both old and new sites, update both explicitly.
- If a change is new-site-only, keep scope to `web-site-new` and related mirror flow.

## 13. Regression Checklist (New Site)

- Front page hero has **no** border and no hero pills.
- Compiler overview sections render with one title divider line.
- Inline identifiers are plain (no pill style) across all pages.
- Toolchain content is integrated into section body (no mismatched floating panel).
- JSXGraph extended examples show board status and initialize correctly.
- New/Old switch alignment and active/inactive affordance are clear.
