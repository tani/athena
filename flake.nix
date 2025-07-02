{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { pkgs, system, ... }:
        let
          chickenEggs = pkgs.chickenPackages_5.chickenEggs;
          nodePackages = import ./node {
            inherit pkgs;
            nodejs = pkgs.nodejs; # nodejsのバージョンを明示的に指定
          };
        in {
          devShells.default = pkgs.mkShell {
            packages = (with pkgs; [
              rlwrap nodejs
              gauche chicken sagittarius-scheme chibi guile chez chez-srfi gambit
            ]) ++ (with chickenEggs; [
              srfi-1 srfi-132 srfi-64 srfi-41 r7rs
            ]) ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux (with pkgs; [
              racket
            ]);
            CHEZSCHEMELIBDIRS="${pkgs.chez-srfi}/lib/csv10.2-site/";
            shellHook = ''
              echo "Scheme dev shell loaded"
            '';
          };
        };
    };
}
