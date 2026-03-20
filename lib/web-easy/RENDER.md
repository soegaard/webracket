
The current implementation has an `observable-view` abstraction. When the
observable changes, it calls the view-producing function again, builds a fresh
view subtree, and replaces the old DOM subtree with the new one.

This is the current behavior in broad terms:

- `observable-view` watches an observable value
- when the value changes, it calls `make-view`
- it then rebuilds the child DOM with something like:
  `replace-with-single-child! parent (build-node child-view ...)`
- in the browser backend, that eventually becomes a full DOM child replacement

So if I use `observable-view` to rebuild an `input` just to change its `style`
attribute, the renderer creates a new DOM `<input>` node and replaces the old
one.

That causes the usual problem:
- focus is lost
- cursor/selection is lost
- typing feels broken if validity/style changes while editing

This differs from React’s approach. In React, a state change re-runs render, but
the reconciler usually keeps the same DOM `<input>` node if the element type and
position are unchanged, and only patches changed props such as `style`.

I want to move this framework toward that React-style behavior.

Relevant local files:
- `view.rkt`
- `renderer.rkt`
- `observable.rkt`
- `backend-browser.rkt`

Please do the following:

1. Read the code structure and explain precisely how `observable-view` currently
   works, including where subtree replacement happens.

2. Sketch a design for a React-like reconciliation approach suitable for this
   framework.

3. Focus on the specific case where:
   - an `input` remains an `input`
   - only attributes like `style`, `disabled`, `value`, or event handlers change
   - the DOM node should be preserved rather than recreated

4. Propose a minimal, incremental refactor rather than a total rewrite.

5. Show which functions/data structures should change, and how.

6. If useful, introduce the idea of:
   - a stable rendered-node record
   - a `patch-node!` function
   - attribute diffing
   - child reconciliation for simple cases
   - preserving DOM nodes when view “shape” is the same

7. Explain how `observable-view` should behave under the new model:
   - it may still call `make-view` again
   - but instead of always replacing the subtree, it should patch the existing
     rendered subtree when possible

8. Pay special attention to browser inputs:
   - preserve focus
   - preserve cursor/selection when possible
   - avoid clobbering user edits unnecessarily
   - be careful when updating the DOM `value`

9. Please give:
   - a conceptual design
   - a concrete patch plan
   - pseudocode or small code sketches in Racket

10. Do not just describe React at a high level. Adapt the solution to this code
    base and its existing abstractions.

A good solution will likely:
- distinguish view description from rendered node instance
- compare old and new views
- reuse the existing DOM node when the tag/type matches
- patch attributes/listeners instead of recreating the element
- recurse into children only as needed

Please be concrete and pragmatic.

-----

My browser renderer for a Racket UI framework currently handles
`observable-view` by rebuilding and replacing the entire child subtree whenever
the observable changes. That means an `input` gets recreated even if only its
style changes, which loses focus and cursor position.

I want a React-style approach instead:
- re-run the view function
- compare old and new view descriptions
- preserve the existing DOM node when the element type is the same
- patch attributes/listeners/children incrementally

Please inspect these files:
- `view.rkt`
- `renderer.rkt`
- `observable.rkt`
- `backend-browser.rkt`

Then:
1. explain the current replacement behavior,
2. propose a minimal reconciliation design,
3. sketch how to implement `patch-node!`,
4. show how `observable-view` should patch rather than replace when possible,
5. pay special attention to preserving `<input>` focus, selection, and value.

Please give a concrete implementation plan in Racket-oriented pseudocode.


-----

Optimize for the smallest change that fixes input preservation first, even if
the first version of reconciliation handles only a subset of view types.
