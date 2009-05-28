default: build

run: build
	dist/build/truth/truth

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

test: install
	cd Test && make clean && make

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test
