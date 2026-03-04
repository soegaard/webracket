SHELL := /bin/bash

SMOKE_DIR := lib/web-easy/smoke
SINGLE_COMPILE ?= run-browser-parity-profile-compile.sh
SINGLE_PAGE ?= test-browser-parity-profile.html

.PHONY: smoke-ci smoke-contract smoke-all-contract smoke-headless smoke-parity-headless smoke-one

smoke-ci:
	cd $(SMOKE_DIR) && ./smoke.sh ci

smoke-contract:
	cd $(SMOKE_DIR) && ./smoke.sh contract

smoke-all-contract:
	cd $(SMOKE_DIR) && ./check-all.sh --headless --contract-first

smoke-headless:
	cd $(SMOKE_DIR) && ./smoke.sh headless

smoke-parity-headless:
	cd $(SMOKE_DIR) && ./smoke.sh parity-headless

smoke-one:
	cd $(SMOKE_DIR) && ./check-single-headless.sh $(SINGLE_COMPILE) $(SINGLE_PAGE)
