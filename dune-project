(lang dune 3.19)

(name calyx)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name calyx)
 (synopsis "A compiler written in OCaml")
 (description "A small compiler project using parser combinators and OCaml")
 (depends
  ocaml
  dune
  angstrom
  sedlex
  ppx_deriving
  ppx_sexp_conv
  sexplib
  base
  stdio
  fmt
  cmdliner
  (alcotest :with-test)
  (qcheck :with-test))
 (tags
  (compiler parser "parser-combinator" ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
