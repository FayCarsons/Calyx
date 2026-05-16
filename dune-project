(lang dune 3.19)

(name calyx)

(generate_opam_files true)

(source
 (github faycarsons/calyx))

(authors "Fay Carsons <faycarsons23@gmail.com>")

(using menhir 3.0)

(package
 (name calyx)
 (synopsis "Calyx compiler: core AST, parser, pipeline and CLI")
 (depends
  ocaml
  dune
  menhir
  ppx_deriving
  ppx_sexp_conv
  core
  core_unix
  stdio
  fmt
  cmdliner
  qcheck
  pprint
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)))

(package
 (name calyx-typecheck)
 (synopsis "Calyx bidirectional type checker and constraint solver")
 (depends ocaml dune core calyx))

(package
 (name calyx-codegen)
 (synopsis "Calyx WGSL and JavaScript code generation")
 (depends ocaml dune core calyx calyx-typecheck))

(package
 (name calyx-trace)
 (synopsis "Calyx inference tracing and interactive trace viewer")
 (depends ocaml dune core core_unix calyx nottui notty))

(package
 (name calyx-lsp)
 (synopsis "Calyx language server")
 (depends ocaml dune calyx linol eio))
