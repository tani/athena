{ pkgs, crane }:
let
  # Scheme test app builder
  mkSchemeTest = { name, cmd }: {
    type = "app";
    program = pkgs.writeShellScriptBin "test-${name}" ''
      # Create temporary file for test output
      TMPLOG=$(mktemp)
      trap "rm -f $TMPLOG" EXIT

      # Run test and capture output
      ${cmd} 2>&1 | tee "$TMPLOG"

      # Check for unexpected failures
      if grep -q "unexpected failures" "$TMPLOG"; then
        echo "ERROR: Tests failed with unexpected failures"
        exit 1
      fi
    '';
  };
  craneLib = crane.mkLib pkgs;
  schemat-src = pkgs.fetchCrate {
      pname = "schemat";
      version = "0.4.2";
      hash = "sha256-C5H/lykodY99UZAbFjH1aKP1B+dPP/+IDVidEKisKL4=";
  };
  schemat = craneLib.buildPackage {
    src = schemat-src;
  };
in
{
  dev-packages = with pkgs; [
    schemat
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
  ];
  tests = {
    test-racket = mkSchemeTest {
      name = "racket";
      cmd = ''
        ${pkgs.racket-minimal}/bin/raco pkg install --user --auto srfi-lib
        ${pkgs.racket-minimal}/bin/racket scheme/test/test.rkt
      '';
    };

    test-gauche = mkSchemeTest {
      name = "gauche";
      cmd = "${pkgs.gauche}/bin/gosh -r 7 -I scheme/src scheme/test/test.7.scm";
    };

    test-chicken = mkSchemeTest {
      name = "chicken";
      cmd = let
        chickenEnv = pkgs.buildEnv {
          name = "chicken-test-env";
          paths = with pkgs; [
            chicken
            chickenPackages.chickenEggs.r7rs
            chickenPackages.chickenEggs.srfi-1
            chickenPackages.chickenEggs.srfi-13
            chickenPackages.chickenEggs.srfi-14
            chickenPackages.chickenEggs.srfi-64
            chickenPackages.chickenEggs.srfi-132
          ];
        };
      in ''
        export CHICKEN_REPOSITORY_PATH=${chickenEnv}/lib/chicken/11
        ${chickenEnv}/bin/csi -require-extension r7rs -include-path scheme/src -eval '(include "scheme/src/prolog.sld")' -script scheme/test/test.7.scm
      '';
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
}
