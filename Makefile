default: build

EXE		= dist/build/Ghide/Ghide

# Building

clean:
	cabal clean

configure:
	cabal configure

build: configure
	cabal build --ghc-options="-Wall -Werror"

haddock: configure
	cabal haddock

install: build haddock
	cabal install --user

run: build
	$(EXE)

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test

