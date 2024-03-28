build:
	dune build

test:
	dune test

doc:
	dune build @doc

docopen: doc
	open _build/default/_doc/_html/index.html