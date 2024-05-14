(lang dune 3.14)

(name lisproject)

(generate_opam_files true)

(source
 (uri https://github.com/Alex23087/LIS-Project))

(authors
  "Yuri Andriaccio"
  "Alessandro Scala"
  "Andrea Simone Costa"
  "Elvis Rossi"
  "Marco Antonio Corallo"
  "Samuele Bonini"
  "Andrea Castagna"
  "Fabio Federico"
  "Matteo Simone"
)

(using menhir 3.0)

(license GPL-3.0-or-later)

(package
 (name lisproject)
 (depends ocaml dune)
)
