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
        ocamlPackages = pkgs.ocamlPackages.overrideScope (
          self: super: {
            ocaml = (super.ocaml.override { flambdaSupport = true; }).overrideAttrs (old: {
              # Force different output path to avoid cache collisions
              pname = "${old.pname}-flambda";
            });
          }
        );
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

            # OCaml libs
            menhir
            menhirLib
            ppx_deriving
            ppx_sexp_conv
            ppx_sexp_value
            qcheck
            sexplib
            base
            core
            cmdliner
          ];
        };

        packages.default = ocamlPackages.buildDunePackage {
          pname = "calyx";
          version = "0.1.0";
          src = ./.;
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

            # OCaml libs
            menhir
            menhirLib
            ppx_deriving
            ppx_sexp_conv
            ppx_sexp_value
            qcheck
            sexplib
            base
            core
            cmdliner
          ];
        };
      }
    );
}
