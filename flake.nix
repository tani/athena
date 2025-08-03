{
  description = "Athena - A comprehensive Prolog engine implemented in Scheme";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            (pkgs.sbcl.withPackages(ps: with ps; [ fiveam ]) // { inherit (pkgs.sbcl) meta; })
            (pkgs.abcl.withPackages(ps: with ps; [ fiveam ]) // { inherit (pkgs.abcl) meta; })
            (pkgs.clisp.withPackages(ps: with ps; [ fiveam ]) // { inherit (pkgs.clisp) meta; })
            (pkgs.ecl.withPackages(ps: with ps; [ fiveam ]) // { inherit (pkgs.ecl) meta; })
          ];
          buildInputs = with pkgs; [
            racket-minimal
            gauche
            guile
            chibi
            sagittarius-scheme
            chez
            chez-srfi
            gambit
            chicken
            chickenPackages.chickenEggs.r7rs
            chickenPackages.chickenEggs.srfi-1
            chickenPackages.chickenEggs.srfi-64
            chickenPackages.chickenEggs.srfi-132
            schemat
            moreutils
            findutils
            lefthook
          ];
          CHEZSCHEMELIBDIRS = "${pkgs.chez-srfi}/lib/csv10.2-site/";
          shellHook = ''
            lefthook install
            raco pkg install --user --auto srfi-lib
            export CL_SOURCE_REGISTRY="$PWD"
          '';
        };
      });
    };
}
