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
          makeRlwrap = name: target: pkgs.writeShellScriptBin name ''
            exec ${pkgs.rlwrap}/bin/rlwrap -c ${target} "$@"
          '';

          rlwrapPkgs = {
            gauche = makeRlwrap "gosh" "${pkgs.gauche}/bin/gosh";
            gambit = makeRlwrap "gsi" "${pkgs.gambit}/bin/gsi";
            chicken = makeRlwrap "csi" "${pkgs.chicken}/bin/csi";
            sagittarius-scheme = makeRlwrap "sagittarius" "${pkgs.sagittarius-scheme}/bin/sagittarius";
            chibi = makeRlwrap "chibi-scheme" "${pkgs.chibi}/bin/chibi-scheme";
            guile = makeRlwrap "guile" "${pkgs.guile}/bin/guile";
          };

          chickenEggs = pkgs.chickenPackages_5.chickenEggs;
        in {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              rlwrap
              gauche gambit chicken sagittarius-scheme chibi guile
            ] ++ (with rlwrapPkgs; [
              gauche gambit chicken sagittarius-scheme chibi guile
            ]) ++ (with chickenEggs; [
              srfi-1 srfi-132 srfi-64 awful r7rs
            ]);

            shellHook = ''
              echo "Scheme dev shell loaded"
            '';
          };
        };
    };
}
