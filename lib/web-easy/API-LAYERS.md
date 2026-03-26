# Web-easy API Layers

This note captures the intended split between the low-level primitive API and the higher-level component library API.

Current pilot extensions in this direction:

- `Button`, `A`, `Label`, and `H1` .. `H6` now preserve their historical single text-like form while also accepting child views.
- `P`, `Span`, `Strong`, `Em`, `Code`, `Pre`, `Small`, `B`, `I`, `U`, `S`, `Mark`, `Sub`, `Sup`, `Kbd`, `Samp`, `Var`, `Q`, `Cite`, `Dfn`, `Abbr`, `Time`, `Data`, `Del`, and `Ins` now preserve their historical single text-like form while also accepting child views.
- `Option` and `Legend` now preserve their historical child-content usage while also accepting a single text-like form.

## Core API

These exports should behave like the core HTML building blocks that other component libraries can layer on top of.

### Text-bearing primitives

- `Text`
- `Span`
- `P`
- `A`
- `Button`
- `Label`
- `Title`
- `Code`
- `Strong`
- `Em`
- `Small`
- `B`
- `I`
- `U`
- `S`
- `Mark`
- `Sub`
- `Sup`
- `Kbd`
- `Samp`
- `Var`
- `Q`
- `Cite`
- `Dfn`
- `Abbr`
- `Time`
- `Data`
- `Del`
- `Ins`

### Container primitives

- `Div`
- `Section`
- `Article`
- `Nav`
- `Main`
- `Header`
- `Footer`
- `Aside`
- `Form`
- `Ul`
- `Ol`
- `Li`
- `Dl`
- `Dt`
- `Dd`
- `Table`
- `Tr`
- `Th`
- `Td`
- `Figure`
- `Figcaption`
- `Hgroup`
- `Address`
- `Blockquote`
- `Ruby`
- `Rt`
- `Rp`
- `Bdi`
- `Bdo`
- `Fieldset`
- `Legend`
- `Datalist`
- `Optgroup`
- `Colgroup`
- `Map`
- `Slot`

### Void and leaf primitives

- `Br`
- `Wbr`
- `Hr`
- `Img`
- `Source`
- `Track`
- `Col`
- `Area`
- `Script`
- `Link`
- `Meta`
- `Base`
- `Style`

### Form and control primitives

- `Input`
- `Textarea`
- `Select`
- `Option`
- `Canvas`
- `Iframe`
- `Embed`
- `Object`
- `Progress`
- `Meter`
- `Output`
- `Details`
- `Dialog`
- `Summary`

### Low-level helpers

- `html-element`
- `html-element-children`
- `observable-element-children`
- `text-content/c`
- attribute and normalization helpers

## Core rules

- Mirror HTML semantics closely.
- Accept observables wherever the HTML value can change.
- Accept child views whenever the HTML element can contain children.
- Expose global attributes and element-specific attributes directly.
- Expose generic primitive DOM event keywords directly on primitive elements.
- Avoid library policy, styling conventions, and widget behavior.

Current Phase 1 primitive DOM event support covers bubbling mouse and pointer
events such as `#:on-click`, `#:on-mousedown`, `#:on-mouseup`,
`#:on-pointerdown`, `#:on-pointermove`, and `#:on-pointerup`.
Callbacks receive the raw browser event object.

## Widening Audit

The main text-bearing primitive family has now been widened to `#:content-mode text-or-children`.

Remaining uppercase `html-element` primitives are mostly true leaf/special elements, for example:

- `Br`, `Wbr`, `Hr`
- `Img`, `Source`, `Track`
- `Input`, `Embed`, `Area`, `Col`
- `Link`, `Meta`, `Base`

Still worth evaluating separately:

- `Title`
- `Style`
- `Textarea`

These remaining cases need a more deliberate decision because their HTML content model is not just ordinary phrasing content.

Current policy direction:

- `Title` and `Style` should remain text-only primitives.
- They should accept plain strings and observables, but observable updates that become non-string and non-`#f` should be ignored and logged as console errors.
- `Textarea` should remain a value/text primitive rather than a child-view container.

## Library API

These exports can build on the core primitives and provide the higher-level ergonomic layer.

### Typography

- `heading`
- `h1` .. `h6`
- `display-heading`
- `display-1` .. `display-6`
- `heading-with-subtitle`
- `display-heading-with-subtitle`
- `lead`
- `blockquote`

### Buttons and actions

- `button`
- `close-button`
- `button-group`
- `toggle-button-group`
- `button-toolbar`
- `toolbar`
- `toolbar-group`

### Form widgets

- `input`
- `textarea`
- `checkbox`
- `choice`
- `radios`
- `slider`

### Feedback and status

- `alert`
- `alert-rich`
- `toast`
- `badge`
- `spinner`
- `progress`
- `placeholder`

### Navigation and composition

- `pagination`
- `breadcrumb`
- `list-group`
- `tab-panel`
- `dropdown`
- `menu-bar`
- `menu`
- `menu-item`
- `navigation-bar`
- `top-bar`

### Data and layout

- `table`
- `list-view`
- `card`
- `accordion`
- `carousel`
- `scrollspy`
- `collapse`
- `spacer`
- `divider`
- `group`
- `container`
- `grid`
- `stack`
- `inline`
- `Fragment`

### Overlays

- `dialog`
- `modal`
- `offcanvas`
- `tooltip`
- `popover`

## Library rules

- Add defaults and conventions that make the primitives easier to use.
- Normalize convenient calling forms.
- Compose multiple primitives when needed.
- Add widget-specific classes and event bridging only at this layer.
- Keep the behavior policy here, not in the core primitives.

## Internal-only helpers

These should remain implementation details rather than public surface area.

- `apply-root-decorators`
- `apply-extra-attrs/internal`
- `normalize-*` helpers
- `observable-element-children`
- renderer bridge helpers
- `define/component` machinery if it stays internal

## Suggested export policy

### Public now

- Keep the core primitives public and stable.
- Keep the higher-level component exports public for backward compatibility.
- Keep the low-level helpers public only when they are genuinely part of the supported building surface.

### Future submodules

If we want a cleaner split later, a good shape would be:

- `web-easy/core`
  - primitives and low-level helpers
- `web-easy/components`
  - `button`, `input`, `checkbox`, `dialog`, `dropdown`, and friends

That lets users build their own component libraries on top of the core primitives without inheriting the higher-level conventions.

## Documentation split

- Document core primitives as the base HTML-like API.
- Document component wrappers separately as convenience APIs.
- Call out which constructors are core primitives and which are library composition layers.
- Keep source comments and Scribble signatures aligned so the split stays obvious for readers.
