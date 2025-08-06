{
  description = "Athena - A comprehensive Prolog engine implemented in Scheme and Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { config, pkgs, lib, system, ... }:
        let
          # Project configuration
          pname = "prolog";
          clSrc = ./common-lisp;

          # Common Lisp dependencies
          lispLibs = lisp: with lisp.pkgs; [ fiveam ];
          nativeLibs = with pkgs; [ ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;

          # Supported implementations
          lispImpls = [ "abcl" "sbcl" "ecl" "ccl" "mkcl" "clisp" "cmucl_binary" "clasp-common-lisp" ];

          # Helper functions
          isAvailable = impl: let
            basePkgs = import nixpkgs { inherit system; overlays = []; };
            lisp = basePkgs.${impl};
          in (builtins.tryEval lisp).success
             && (builtins.elem system lisp.meta.platforms)
             && (!lisp.meta.broken);

          availableLispImpls = builtins.filter isAvailable lispImpls;

          # Extract system and version info from ASDF file
          extractASDFInfo = {
            systems = let
              asd = builtins.readFile "${clSrc}/${pname}.asd";
              res = builtins.split ''defsystem[[:space:]]*([^[:space:]]*)'' asd;
              odd = n: lib.trivial.mod n 2 == 1;
              sys1 = lib.lists.flatten (lib.lists.ifilter0 (i: v: odd i) res);
              sys2 = builtins.map (s: builtins.replaceStrings [''"'' "#:" ":"] ["" "" ""] s) sys1;
            in sys2;

            version = let
              asd = builtins.readFile "${clSrc}/${pname}.asd";
              res = builtins.split '':version[[:space:]]*([^[:space:]]*)'' asd;
              odd = n: lib.trivial.mod n 2 == 1;
              ver1 = lib.lists.flatten (lib.lists.ifilter0 (i: v: odd i) res);
              ver2 = builtins.map (s: builtins.replaceStrings [''"''] [""] s) ver1;
            in builtins.head ver2;
          };

          asdInfo = extractASDFInfo;
          inherit (asdInfo) systems version;

          # Scheme test app builder
          mkSchemeTest = { name, cmd }: {
            type = "app";
            program = pkgs.writeShellScriptBin "test-${name}" cmd;
          };

          schemeTests = {
            test-racket = mkSchemeTest {
              name = "racket";
              cmd = "${pkgs.racket-minimal}/bin/racket scheme/test/test.rkt";
            };

            test-gauche = mkSchemeTest {
              name = "gauche";
              cmd = "${pkgs.gauche}/bin/gosh -r 7 -I scheme/src scheme/test/test.7.scm";
            };

            test-chicken = mkSchemeTest {
              name = "chicken";
              cmd = "${pkgs.chicken}/bin/csi -require-extension r7rs -include-path scheme/src -eval '(include \"scheme/src/prolog.sld\")' -script scheme/test/test.7.scm";
            };

            test-guile = mkSchemeTest {
              name = "guile";
              cmd = "${pkgs.guile}/bin/guile --fresh-auto-compile -x .sld -L scheme/src scheme/test/test.7.scm";
            };

            test-chibi = mkSchemeTest {
              name = "chibi";
              cmd = "${pkgs.chibi}/bin/chibi-scheme -I scheme/src scheme/test/test.7.scm";
            };

            test-sagittarius = mkSchemeTest {
              name = "sagittarius";
              cmd = "${pkgs.sagittarius-scheme}/bin/sagittarius --clean-cache --disable-cache --loadsuffix=.sld --standard=7 --loadpath=scheme/src scheme/test/test.7.scm";
            };

            test-chez = mkSchemeTest {
              name = "chez";
              cmd = ''
                CHEZSCHEMELIBDIRS="${pkgs.chez-srfi}/lib/csv10.2-site/"
                ${pkgs.chez}/bin/scheme --libdirs $CHEZSCHEMELIBDIRS:$PWD/scheme/src --script scheme/test/test.6.scm
              '';
            };

            test-gambit = mkSchemeTest {
              name = "gambit";
              cmd = "${pkgs.gambit}/bin/gsi -:r7rs scheme/src/ scheme/test/test.7.scm";
            };
          };

          # Common Lisp package builders
          packageBuilders = {
            unbundled = { lisp, mainProgram, evalFlag, extraArgs }: rec {
              mainLib = lisp.buildASDFSystem {
                inherit pname version systems nativeLibs;
                src = clSrc;
                lispLibs = lispLibs lisp;
              };
              lisp' = lisp.withPackages (ps: [ mainLib ]) // {
                inherit (lisp) meta;
              };
              testExe = pkgs.writeShellScriptBin "${pname}-test" ''
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                ${lisp'}/bin/${mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                  (progn
                    (asdf:test-system :${pname})
                    (quit))
                EOF
                )" -- "$@"
              '';
            };

            bundled = { lisp, mainProgram, evalFlag, extraArgs }: rec {
              mainLib = lisp.buildASDFSystem {
                inherit pname version systems nativeLibs;
                src = clSrc;
                lispLibs = lispLibs lisp;
              };
              lisp' = lisp.withPackages (ps: [ mainLib ]) // {
                inherit (lisp) meta;
              };
              testExe = pkgs.writeShellScriptBin "${pname}-test" ''
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                ${lisp'}/bin/${mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
                  (progn
                    (asdf:test-system :${pname})
                    (quit))
                EOF
                )" -- "$@"
              '';
            };
          };

          # Implementation recipes
          recipe = {
            sbcl = packageBuilders.bundled {
              lisp = pkgs.sbcl;
              mainProgram = "sbcl";
              evalFlag = "--eval";
              extraArgs = "--noinform --disable-debugger";
            };
            ccl = packageBuilders.bundled {
              lisp = pkgs.ccl;
              mainProgram = "ccl";
              evalFlag = "--eval";
              extraArgs = "--quiet";
            };
            clisp = packageBuilders.unbundled {
              lisp = pkgs.clisp;
              mainProgram = "clisp";
              evalFlag = "-x";
              extraArgs = "--quiet";
            };
            ecl = packageBuilders.unbundled {
              lisp = pkgs.ecl;
              mainProgram = "ecl";
              evalFlag = "--eval";
              extraArgs = "";
            };
            cmucl_binary = packageBuilders.unbundled {
              lisp = pkgs.cmucl_binary;
              mainProgram = "lisp";
              evalFlag = "-eval";
              extraArgs = "-quiet";
            };
            abcl = packageBuilders.unbundled {
              lisp = pkgs.abcl;
              mainProgram = "abcl";
              evalFlag = "--eval";
              extraArgs = "--noinform";
            };
            clasp-common-lisp = packageBuilders.unbundled {
              lisp = pkgs.clasp-common-lisp;
              mainProgram = "clasp";
              evalFlag = "--eval";
              extraArgs = "--noinform";
            };
            mkcl = packageBuilders.unbundled {
              lisp = pkgs.mkcl;
              mainProgram = "mkcl";
              evalFlag = "-eval";
              extraArgs = "--quiet";
            };
          };

          # App and package generators
          mkApps = impl: [
            {
              name = "test-" + impl;
              value = {
                type = "app";
                program = recipe.${impl}.testExe;
              };
            }
          ];

          mkPackages = impl: [
            {
              name = "lib-" + impl;
              value = recipe.${impl}.mainLib;
            }
          ];

          mkDevPackage = impl:
            pkgs.${impl}.withPackages (ps: lispLibs pkgs.${impl});

          mkOverlays = impl: [
            {
              name = impl;
              value = pkgs.${impl}.withOverrides
                (self: super: { ${pname} = config.packages."lib-${impl}"; });
            }
          ];

          # Coverage support for SBCL
          coverage-sbcl = let
            lisp = pkgs.sbcl.withPackages (ps: lispLibs pkgs.sbcl) // {
              inherit (pkgs.sbcl) meta;
            };
            mainProgram = "sbcl";
            program = pkgs.writeShellScriptBin "${pname}-coverage" ''
              export CL_SOURCE_REGISTRY=$PWD
              ${lisp}/bin/${mainProgram} --noinform --disable-debugger <<EOF
                (require "asdf")
                (require :sb-cover)
                (declaim (optimize sb-cover:store-coverage-data))
                (asdf:compile-system :${pname} :force t)
                (declaim (optimize (sb-cover:store-coverage-data 0)))
                (asdf:test-system :${pname})
                (sb-cover:report "coverage/")
              EOF
            '';
          in {
            type = "app";
            inherit program;
          };
        in {
          # Overlays
          overlayAttrs = builtins.listToAttrs
            (builtins.concatMap mkOverlays availableLispImpls);

          # Development shell
          devShells.default = pkgs.mkShell {
            inherit LD_LIBRARY_PATH;
            packages = with pkgs; [
              # Scheme implementations
              rlwrap
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
              # Tools
              schemat
              moreutils
              findutils
              lefthook
            ] ++ (builtins.map mkDevPackage availableLispImpls);
            CHEZSCHEMELIBDIRS = "${pkgs.chez-srfi}/lib/csv10.2-site/";
            shellHook = ''
              lefthook install
              raco pkg install --user --auto srfi-lib
              export CL_SOURCE_REGISTRY="$PWD/common-lisp:$PWD"
            '';
          };

          # Packages
          packages = builtins.listToAttrs
            (builtins.concatMap mkPackages availableLispImpls);

          # Apps
          apps = builtins.listToAttrs (builtins.concatMap mkApps availableLispImpls)
            // lib.optionals (isAvailable "sbcl") { coverage-sbcl = coverage-sbcl; }
            // schemeTests;
        };
    };
}
