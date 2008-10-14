default: build

EXE		= dist/build/Ghide/Ghide

# Building

clean:
	runhaskell Setup.hs clean

configure:
	runhaskell Setup.hs configure --user

build: configure
	runhaskell Setup.hs build

haddock: configure
	runhaskell Setup.hs haddock

install: build haddock
	sudo runhaskell Setup.hs install

run: build
	$(EXE)

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test

