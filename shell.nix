let
  pkgs = import <nixpkgs> {};
  ghc  = pkgs.ghc;
in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "brew-graph-env";
  buildInputs = with pkgs.haskellPackages; [
    graphviz
    optparse-applicative
  ];
}
