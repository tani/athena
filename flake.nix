{
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
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { pkgs, ... }:
        let
          craneLib = crane.mkLib pkgs;
          schemat-src = pkgs.fetchCrate {
              pname = "schemat";
              version = "0.4.2";
              hash = "sha256-C5H/lykodY99UZAbFjH1aKP1B+dPP/+IDVidEKisKL4=";
          };
          schemat = craneLib.buildPackage {
            src = schemat-src;
          };
          rlwrapCmd = { cmd, pkg }: pkgs.writeShellScriptBin "${cmd}x" ''
            ${pkgs.rlwrap}/bin/rlwrap -n ${pkg}/bin/${cmd} "$@"
          '';
          rlwraps = {
            gauche = rlwrapCmd { pkg = pkgs.gauche; cmd = "gosh"; };
            chicken = rlwrapCmd { pkg = pkgs.chicken; cmd = "csi"; };
            sagittarius-scheme = rlwrapCmd { pkg = pkgs.sagittarius-scheme; cmd = "sash"; };
            chibi = rlwrapCmd { pkg = pkgs.chibi; cmd = "chibi-scheme"; };
            guile = rlwrapCmd { pkg = pkgs.guile; cmd = "guile"; };
            chez = rlwrapCmd { pkg = pkgs.chez; cmd = "scheme"; };
            gambit = rlwrapCmd { pkg = pkgs.gambit; cmd = "gsi"; };
            racket = rlwrapCmd { pkg = pkgs.racket; cmd = "racket"; };
          };
        in {
          devShells.default = pkgs.mkShell {
            packages = (with pkgs; [
              gauche chicken sagittarius-scheme chibi guile gambit chez chez-srfi schemat moreutils
            ]) ++ (with rlwraps; [
              gauche chicken sagittarius-scheme chibi guile gambit chez
            ]) ++ (with pkgs.chickenPackages_5.chickenEggs; [
              srfi-1 srfi-132 srfi-64 srfi-41 r7rs
            ]) ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux (with pkgs; [
              racket rlwraps.racket
            ]);
            CHEZSCHEMELIBDIRS="${pkgs.chez-srfi}/lib/csv10.2-site/";
            shellHook = ''
              echo "Scheme dev shell loaded"
            '';
          };
        };
    };
}
