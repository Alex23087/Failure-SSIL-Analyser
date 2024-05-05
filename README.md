# LIS-Project

### Dependencies
- [OPAM](https://opam.ocaml.org/) - OCaml Package Manager
- [Dune](https://dune.build/) - build system

### Installing the dependencies
- `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`
- `opam init`
- `opam update`
- `opam install dune ppx_deriving odoc menhirLib`

### Building and running
- `make build`
- `dune exec lisproject`

### Testing
- `make test`
- `make test-rerun` To force running all the tests (even cached ones)

### Documentation
- `make doc` To only build the documentation
- `make docopen` To build and then open the documentation in browser
- The built documentation can be found at `_build/default/_doc/_html/index.html`