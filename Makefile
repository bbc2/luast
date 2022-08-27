.PHONY: lint
lint:
	dune build @check

.PHONY: check-format
check-format:
	dune build @fmt

.PHONY: check-test
check-test:
	dune build @runtest

.PHONY: format
format:
	dune build @fmt --auto-promote

.PHONY: check
check: lint check-test check-format
