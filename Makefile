SHELL := /bin/bash

SMOKE_DIR := lib/web-easy/smoke
SINGLE_COMPILE ?= run-browser-parity-profile-compile.sh
SINGLE_PAGE ?= test-browser-parity-profile.html

.PHONY: help smoke-ci smoke-contract smoke-all-contract smoke-headless smoke-parity-headless smoke-one

help:
	@echo "Available targets:"
	@echo "  help                  Show this help."
	@echo "  smoke-contract        Run contract-only headless dashboard."
	@echo "  smoke-all-contract    Run contract+smoke dashboards headless."
	@echo "  smoke-headless        Run full headless smoke flow."
	@echo "  smoke-parity-headless Run parity-only headless dashboard."
	@echo "  smoke-ci              Run local CI smoke entrypoint (ci-fast)."
	@echo "  smoke-one             Run one headless smoke page (set SINGLE_COMPILE, SINGLE_PAGE)."

smoke-ci:
	cd $(SMOKE_DIR) && ./smoke.sh ci-fast

smoke-contract:
	cd $(SMOKE_DIR) && ./headless.sh contract

smoke-all-contract:
	cd $(SMOKE_DIR) && ./headless.sh dashboards

smoke-headless:
	cd $(SMOKE_DIR) && ./headless.sh smoke

smoke-parity-headless:
	cd $(SMOKE_DIR) && ./headless.sh parity

smoke-one:
	cd $(SMOKE_DIR) && ./headless.sh single $(SINGLE_COMPILE) $(SINGLE_PAGE)
