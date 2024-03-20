# LIS-Project

### Dependencies

- [OPAM](https://opam.ocaml.org/) - OCaml Package Manager

- [Dune](https://dune.build/) - build system

### Installing the dependencies

- `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`

- `opam install dune ppx_deriving`

### Building and running

- `dune build`

- `dune exec lisproject`

### Testing

- `dune test`


### Additional dependencies for developers
- Language Server for VSCode

  `https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform`

  `opam install ocaml-lsp-server`