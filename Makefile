SHELL := /bin/bash

SMOKE_DIR := lib/web-easy/smoke
SINGLE_COMPILE ?= run-browser-parity-profile-compile.sh
SINGLE_PAGE ?= test-browser-parity-profile.html

.PHONY: help smoke-ci smoke-ci-lite smoke-headless-lite smoke-verify smoke-quick smoke-release smoke-smoke smoke-parity smoke-dashboards smoke-one smoke-list smoke-commands

help:
	@echo "Available targets:"
	@echo "  help                  Show this help."
	@echo "  smoke-ci              Run local CI smoke entrypoint (preferred)."
	@echo "  smoke-ci-lite         Run local CI-lite headless gate without compile."
	@echo "  smoke-headless-lite   Run contract+smoke+parity+guard without compile."
	@echo "  smoke-verify          Run local headless verify preflight."
	@echo "  smoke-quick           Run smoke-verify + smoke-ci-lite."
	@echo "  smoke-release         Run smoke-commands + smoke-quick + smoke-ci."
	@echo "  smoke-smoke           Run full smoke dashboard headless."
	@echo "  smoke-parity          Run parity-only headless dashboard."
	@echo "  smoke-dashboards      Run contract+smoke dashboards headless."
	@echo "  smoke-list            Print canonical smoke/headless commands."
	@echo "  smoke-commands        Regenerate smoke/COMMANDS.tsv."
	@echo "  smoke-one             Run one headless smoke page (set SINGLE_COMPILE, SINGLE_PAGE)."

smoke-ci:
	cd $(SMOKE_DIR) && ./headless.sh ci

smoke-ci-lite:
	cd $(SMOKE_DIR) && ./headless.sh ci-lite

smoke-headless-lite:
	cd $(SMOKE_DIR) && ./headless.sh ci-lite

smoke-verify:
	cd $(SMOKE_DIR) && ./headless.sh verify

smoke-quick: smoke-verify smoke-ci-lite

smoke-release: smoke-commands smoke-quick smoke-ci

smoke-smoke:
	cd $(SMOKE_DIR) && ./headless.sh smoke

smoke-parity:
	cd $(SMOKE_DIR) && ./headless.sh parity

smoke-dashboards:
	cd $(SMOKE_DIR) && ./headless.sh dashboards

smoke-list:
	@echo "Canonical headless modes:"
	@cd $(SMOKE_DIR) && ./headless.sh list
	@echo
	@echo "Canonical Make targets:"
	@echo "smoke-ci"
	@echo "smoke-ci-lite"
	@echo "smoke-headless-lite"
	@echo "smoke-verify"
	@echo "smoke-quick"
	@echo "smoke-release"
	@echo "smoke-smoke"
	@echo "smoke-parity"
	@echo "smoke-dashboards"
	@echo "smoke-commands"
	@echo "smoke-one"

smoke-commands:
	cd $(SMOKE_DIR) && ./gen-commands.sh

smoke-one:
	cd $(SMOKE_DIR) && ./headless.sh single $(SINGLE_COMPILE) $(SINGLE_PAGE)
