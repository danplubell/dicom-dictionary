.PHONY: all build clean configure install repl run 

all: install configure build

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests


install:
	cabal sandbox init --sandbox  /Users/cerdep/Documents/haskell-workspace/dicom/.cabal-sandbox
	cabal install --enable-benchmarks --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:dicom-dictionary

run:
	cabal run --jobs dicom-dictionary

