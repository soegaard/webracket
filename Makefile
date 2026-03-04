SHELL := /bin/bash

SMOKE_DIR := lib/web-easy/smoke
SINGLE_COMPILE ?= run-browser-parity-profile-compile.sh
SINGLE_PAGE ?= test-browser-parity-profile.html

.PHONY: help smoke-ci smoke-ci-lite smoke-smoke smoke-parity smoke-dashboards smoke-one smoke-list

help:
	@echo "Available targets:"
	@echo "  help                  Show this help."
	@echo "  smoke-ci              Run local CI smoke entrypoint."
	@echo "  smoke-ci-lite         Run local CI headless gate without compile."
	@echo "  smoke-smoke           Run full smoke dashboard headless."
	@echo "  smoke-parity          Run parity-only headless dashboard."
	@echo "  smoke-dashboards      Run contract+smoke dashboards headless."
	@echo "  smoke-list            Print canonical smoke/headless commands."
	@echo "  smoke-one             Run one headless smoke page (set SINGLE_COMPILE, SINGLE_PAGE)."

smoke-ci:
	cd $(SMOKE_DIR) && ./headless.sh ci

smoke-ci-lite:
	cd $(SMOKE_DIR) && SMOKE_SKIP_COMPILE=1 ./headless.sh ci

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
	@echo "smoke-smoke"
	@echo "smoke-parity"
	@echo "smoke-dashboards"
	@echo "smoke-one"

smoke-one:
	cd $(SMOKE_DIR) && ./headless.sh single $(SINGLE_COMPILE) $(SINGLE_PAGE)
