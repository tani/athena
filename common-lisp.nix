{ pkgs, lib, system, config }:
let
  # Project configuration
  pname = "prolog";
  clSrc = ./common-lisp;

  # Common Lisp dependencies
  lispLibs = lisp: with lisp.pkgs; [ fiveam ];

  # Supported implementations
  lispImpls = [ "abcl" "sbcl" "ecl" "ccl" "mkcl" "clisp" "cmucl_binary" "clasp-common-lisp" ];

  # Helper functions
  isAvailable = impl: let
    basePkgs = import pkgs.path { inherit system; overlays = []; };
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

  # Common Lisp package builders
  packageBuilders = {
    unbundled = { lisp, mainProgram, evalFlag, extraArgs }: rec {
      mainLib = lisp.buildASDFSystem {
        inherit pname version systems;
        src = clSrc;
        lispLibs = lispLibs lisp;
      };
      lisp' = lisp.withPackages (ps: [ mainLib ]) // {
        inherit (lisp) meta;
      };
      testExe = pkgs.writeShellScriptBin "${pname}-test" ''
        # Create temporary file for test output
        TMPLOG=$(mktemp)
        trap "rm -f $TMPLOG" EXIT
        
        ${lisp'}/bin/${mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
          (progn
            (asdf:test-system :${pname})
            (quit))
        EOF
        )" -- "$@" 2>&1 | tee "$TMPLOG"
        
        # Check for test failures
        if grep -q "Some tests failed!" "$TMPLOG"; then
          echo "ERROR: Some tests failed!"
          exit 1
        fi
      '';
    };

    bundled = { lisp, mainProgram, evalFlag, extraArgs }: rec {
      mainLib = lisp.buildASDFSystem {
        inherit pname version systems;
        src = clSrc;
        lispLibs = lispLibs lisp;
      };
      lisp' = lisp.withPackages (ps: [ mainLib ]) // {
        inherit (lisp) meta;
      };
      testExe = pkgs.writeShellScriptBin "${pname}-test" ''
        # Create temporary file for test output
        TMPLOG=$(mktemp)
        trap "rm -f $TMPLOG" EXIT
        
        ${lisp'}/bin/${mainProgram} ${extraArgs} ${evalFlag} '(require "asdf")' ${evalFlag} "$(cat <<EOF
          (progn
            (asdf:test-system :${pname})
            (quit))
        EOF
        )" -- "$@" 2>&1 | tee "$TMPLOG"
        
        # Check for test failures
        if grep -q "Failure Details:" "$TMPLOG"; then
          echo "ERROR: Some tests failed!"
          exit 1
        fi
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
      export CL_SOURCE_REGISTRY="$PWD/common-lisp:$PWD"
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
in
{
  dev-packages = builtins.map mkDevPackage availableLispImpls;
  packages = builtins.listToAttrs (builtins.concatMap mkPackages availableLispImpls);
  apps = builtins.listToAttrs (builtins.concatMap mkApps availableLispImpls) // { inherit coverage-sbcl; };
  overlayAttrs = builtins.listToAttrs (builtins.concatMap mkOverlays availableLispImpls);
}
