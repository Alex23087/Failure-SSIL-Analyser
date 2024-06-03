.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: test-rerun
test-rerun:
	dune test --force

.PHONY: doc
doc:
	dune build @doc

.PHONY: docopen
docopen: doc
	open _build/default/_doc/_html/index.html

setup:
	opam update
	opam upgrade
	opam install dune ppx_deriving odoc

.PHONY: build test test-rerun doc docopen setup