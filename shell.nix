{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, graphviz, optparse-applicative, process
      , stdenv
      }:
      mkDerivation {
        pname = "brew-graph";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base graphviz optparse-applicative process
        ];
        executableHaskellDepends = [ base optparse-applicative process ];
        homepage = "https://github.com/yurrriq/brew-graph";
        description = "Visualize Homebrew dependencies";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
