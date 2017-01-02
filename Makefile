root_dir := $(shell stack path | awk '/^local-install-root/ { print $$2 }')

.PHONY: all build clean clobber install

all: build

build:
	stack build

clean:
	stack clean

clobber:
	stack clean --full

install:
	stack install

shell.nix: dontCheck_haphviz.patch brew-graph.cabal
	cabal2nix --shell . >$@ && git apply $<

installed.png: ${root_dir}/bin/brew-graph
	stack exec brew-graph -- --installed | dot -Tpng -o $@
