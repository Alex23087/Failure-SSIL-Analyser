# LIS-Project

### Dependencies

- [OPAM](https://opam.ocaml.org/) - OCaml Package Manager

- [Dune](https://dune.build/) - build system

### Installing the dependencies

- `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`
- `opam init`
- `opam update`
- `opam install dune ppx_deriving odoc`

### Building and running

- `dune build`

- `dune exec lisproject`

### Testing
- `dune test`

### Documentation
- `dune build @doc`