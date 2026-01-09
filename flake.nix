{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = with ocamlPackages; [
            zstd
            ocaml
            dune_3
            findlib
            ocaml-lsp
            ocamlformat
            odoc
            utop
            merlin
            fzf

            nodejs-slim_latest

            # Parser combinator libraries
            angstrom

            # Compiler-related libraries
            menhir
            menhirLib
            sedlex
            ppx_deriving
            ppx_sexp_conv
            sexplib
            base
            core
            stdio

            # Command line parsing
            cmdliner
          ];
        };
      }
    );
}
