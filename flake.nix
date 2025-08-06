{
  description = "Athena - A comprehensive Prolog engine implemented in Scheme and Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ crane, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { config, pkgs, lib, system, ... }:
        let
          # Import modularized configurations
          scheme = import "${./.}/scheme.nix" { inherit pkgs crane; };
          common-lisp = import "${./.}/common-lisp.nix" { inherit pkgs lib system config; };
        in {
          # Overlays
          inherit (common-lisp) overlayAttrs packages;

          # Development shell
          devShells.default = pkgs.mkShell {
            inherit (common-lisp) LD_LIBRARY_PATH;
            packages = with pkgs; [
              rlwrap
              moreutils
              findutils
              lefthook
            ] ++ common-lisp.dev-packages
              ++ scheme.dev-packages;
            shellHook = ''
              lefthook install
              raco pkg install --user --auto srfi-lib
              export CHEZSCHEMELIBDIRS="${pkgs.chez-srfi}/lib/csv10.2-site/"
              export CL_SOURCE_REGISTRY="$PWD/common-lisp:$PWD"
            '';
          };

          # Apps
          apps = common-lisp.apps // scheme.tests;
        };
    };
}
