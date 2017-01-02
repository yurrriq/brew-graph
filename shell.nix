{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, haphviz, mtl
      , optparse-applicative, process, stdenv, text
      }:
      mkDerivation {
        pname = "brew-graph";
        version = "0.1.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers haphviz mtl optparse-applicative text
        ];
        executableHaskellDepends = [ base process ];
        homepage = "https://github.com/yurrriq/brew-graph";
        description = "Visualize Homebrew dependencies";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {
    haphviz = pkgs.haskell.lib.dontCheck haskellPackages.haphviz;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
