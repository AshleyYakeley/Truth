default: build

test:
	cd Core && make test
	cd Linux && make test
	cd GTK && make install
	cd Cabal && make install

build:
	cd Core && make install
	cd Linux && make install
	cd GTK && make install
	cd Cabal && make install

clean:
	cd Core && make clean
	cd Linux && make clean
	cd GTK && make clean
	cd Cabal && make clean
