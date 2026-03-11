SHELL := /bin/bash

SMOKE_DIR := lib/web-easy/smoke
SINGLE_COMPILE ?= run-browser-parity-profile-compile.sh
SINGLE_PAGE ?= test-browser-parity-profile.html

.PHONY: help smoke-ci smoke-headless-lite smoke-style smoke-theme smoke-theme-core smoke-theme-visual smoke-verify smoke-quick smoke-release smoke-one smoke-list smoke-commands smoke-compare-navbars smoke-compare-buttons smoke-compare-all smoke-solar-parity

help:
	@echo "Available targets:"
	@echo "  help                  Show this help."
	@echo "  smoke-ci              Run local CI smoke entrypoint (preferred)."
	@echo "  smoke-headless-lite   Run contract+smoke+parity+guard without compile (preferred)."
	@echo "  smoke-style           Run style-hook contracts only (smoke+parity)."
	@echo "  smoke-theme           Run theme-only dashboard headless."
	@echo "  smoke-theme-core      Run focused theme-core lane (compile+contracts+solar polish)."
	@echo "  smoke-theme-visual    Run theme visual-diff lane headless."
	@echo "  smoke-verify          Run local headless verify preflight."
	@echo "  smoke-quick           Run smoke-verify + smoke-headless-lite."
	@echo "  smoke-release         Run smoke-commands + smoke-quick + smoke-ci."
	@echo "  smoke-list            Print canonical smoke/headless commands."
	@echo "  smoke-commands        Regenerate smoke/COMMANDS.tsv."
	@echo "  smoke-one             Run one headless smoke page (set SINGLE_COMPILE, SINGLE_PAGE)."
	@echo "  smoke-compare-navbars Run Bootswatch-vs-solar2 navbar compare sweep."
	@echo "  smoke-compare-buttons Run Bootswatch-vs-solar2 button compare sweep."
	@echo "  smoke-compare-all     Run both compare sweeps."
	@echo "  smoke-solar-parity    Run full Solar2 parity sweep (accordion/navbar/forms/tables/list/progress)."

smoke-ci:
	cd $(SMOKE_DIR) && ./headless.sh ci

smoke-headless-lite:
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./headless.sh contract
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./headless.sh smoke
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./headless.sh parity
	cd $(SMOKE_DIR) && ./headless.sh guard

smoke-style:
	cd $(SMOKE_DIR) && ./headless.sh style

smoke-theme:
	cd $(SMOKE_DIR) && ./headless.sh theme

smoke-theme-core:
	cd $(SMOKE_DIR) && ./run-browser-theme-showcase-compile.sh
	cd $(SMOKE_DIR) && ./run-browser-solar-showcase-compile.sh
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-theme-external-css-contract.html
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-theme-core-link-contract.html
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-theme-starter-contract.html
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./check-single-headless.sh run-browser-theme-showcase-compile.sh test-browser-theme-showcase-contract.html
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./check-solar-polish.sh

smoke-theme-visual:
	cd $(SMOKE_DIR) && ./headless.sh theme-visual

smoke-verify:
	cd $(SMOKE_DIR) && ./headless.sh verify

smoke-quick: smoke-verify smoke-headless-lite

smoke-release: smoke-commands smoke-quick smoke-ci

smoke-list:
	@echo "Canonical headless modes:"
	@cd $(SMOKE_DIR) && ./headless.sh list
	@echo "Direct style script:"
	@echo "./check-style-headless.sh"
	@echo
	@echo "Canonical Make targets:"
	@echo "smoke-ci"
	@echo "smoke-headless-lite"
	@echo "smoke-style"
	@echo "smoke-theme"
	@echo "smoke-theme-core"
	@echo "smoke-theme-visual"
	@echo "smoke-verify"
	@echo "smoke-quick"
	@echo "smoke-release"
	@echo "smoke-commands"
	@echo "smoke-one"
	@echo "smoke-compare-navbars"
	@echo "smoke-compare-buttons"
	@echo "smoke-compare-all"
	@echo "smoke-solar-parity"

smoke-commands:
	cd $(SMOKE_DIR) && ./gen-commands.sh

smoke-one:
	cd $(SMOKE_DIR) && ./headless.sh single $(SINGLE_COMPILE) $(SINGLE_PAGE)

smoke-compare-navbars:
	cd $(SMOKE_DIR) && for v in primary dark light subtle; do node ./compare-navbars.cjs $$v; done

smoke-compare-buttons:
	cd $(SMOKE_DIR) && for r in solid disabled outline sizes; do node ./compare-buttons.cjs $$r; done

smoke-compare-all: smoke-compare-buttons smoke-compare-navbars

smoke-solar-parity:
	cd $(SMOKE_DIR) && ./check-solar-parity-sweep.sh
