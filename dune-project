(lang dune 3.19)

(name calyx)

(generate_opam_files true)

(source
 (github faycarsons/calyx))

(authors "Fay Carsons <faycarsons23@gmail.com>")

(using menhir 3.0)

(package
 (name calyx)
 (depends
  ocaml
  dune
  menhir
  angstrom
  ppx_deriving
  ppx_sexp_conv
  core
  stdio
  fmt
  cmdliner
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)))
