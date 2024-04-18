build:
	dune build

test:
	dune test

test-rerun:
	dune test --force

doc:
	dune build @doc

docopen: doc
	open _build/default/_doc/_html/index.html

setup:
	opam update
	opam upgrade
	opam install dune ppx_deriving odoc

.PHONY: build test test-rerun doc docopen setup