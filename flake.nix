{
  description = "OCaml Hello World with Opam";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            opam
            ocaml
            dune_3
            ocamlPackages.findlib
            ocamlPackages.yojson
            curl
            nushell
          ];

          shellHook = ''
            export OCAMLPATH="${pkgs.ocamlPackages.yojson}/lib/ocaml/${pkgs.ocaml.version}/site-lib:$OCAMLPATH"
            echo "OCaml development environment"
            echo "OCaml version: $(ocaml -version)"
            echo ""
            echo "Run 'dune build' to compile"
            echo "Run 'dune exec sharpcoder' to run"
          '';
        };
      });
}
