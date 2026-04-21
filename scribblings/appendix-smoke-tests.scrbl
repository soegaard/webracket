#lang scribble/manual

@(require scribble/manual
          (except-in scribble-tools racketblock racketblock0)
          "webracket-scribble-utils.rkt")

@title[#:tag "smoke-tests"]{Appendix: Smoke Tests}

Smoke tests are the small integration checks we use to make sure the
browser-facing pieces of WebRacket still work together after a change.
They are meant to be run in two places:

@itemlist[
 @item{locally, while developing, so you can catch regressions early}
 @item{on GitHub Actions, so the same checks protect the branch in CI}
]

The smoke suite usually has two layers:

@itemlist[
 @item{compile the examples and contracts}
 @item{serve the generated pages and run the headless browser checks}
]

A @em{parity test} is a smoke test that checks that two related views of
the same behavior stay in sync. In WebRacket, parity tests are used to
compare two versions of a browser example or component so that a new
change does not accidentally make the two versions diverge.

For example, the parity counter page checks that the rendered counter
and its update behavior still match the expected quickstart-style
counter interaction after a change.

Browser-driven headless checks run the same smoke pages in an automated
browser with no visible window. They start the local smoke server, open
the generated pages, and click through or inspect the UI to make sure
the browser behavior still works end to end. These are the checks that
CI runs on GitHub Actions, and they are also useful locally when you
want to verify the browser path without manually opening each page.
Running them requires Playwright in addition to the usual browser build
tools.

@section{Web-easy Smoke Tests}

The @tt{web-easy} smoke suite lives in @tt{lib/web-easy/smoke}. From
the repository root, the most common commands are:

@shellblock{
cd lib/web-easy/smoke
./smoke.sh check
}

That command compiles all smoke examples and contracts.
To clean and rebuild the generated artifacts first, use:

@shellblock{
cd lib/web-easy/smoke
./smoke.sh rebuild
}

If you only want the parity examples, run:

@shellblock{
cd lib/web-easy/smoke
./smoke.sh parity-check
}

For the browser-driven headless checks, use:

@shellblock{
cd lib/web-easy/smoke
./check-smoke-headless.sh
./check-parity-headless.sh
}

The theme contract pages have their own headless check:

@shellblock{
cd lib/web-easy/smoke
./check-theme-contracts-headless.sh
}

To browse the generated smoke pages manually, start the local server:

@shellblock{
cd lib/web-easy/smoke
./smoke.sh open
}

The server prints URLs for the smoke dashboard, parity dashboard, and
theme-contract dashboard. Open those pages in a browser to inspect the
generated artifacts interactively.
