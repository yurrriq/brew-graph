shell.nix: dontCheck_haphviz.patch brew-graph.cabal
	cabal2nix --shell . >$@ && git apply $<
