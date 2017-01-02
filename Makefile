shell.nix: brew-graph.cabal
	nix-shell -p haskellPackages.cabal2nix \
	--run "cabal2nix --shell . >$@ && git apply dontCheck_haphviz.patch"
