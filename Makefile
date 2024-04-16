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