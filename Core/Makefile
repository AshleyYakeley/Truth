default: complete

# Building

clean:
	cabal clean

configure:
	cabal configure

build: configure
	cabal build --ghc-options=-Werror

haddock: configure
	cabal haddock

install: build
	cabal install --user

complete: install haddock

sdist: configure
	cabal sdist

test: install
	cd Test && make clean && make

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install complete test sdist
