default: build

test:
	cd Changes && make test
	cd Linux && make test
	cd GTK && make install
	cd Cabal && make install

build:
	cd Changes && make install
	cd Linux && make install
	cd GTK && make install
	cd Cabal && make install

clean:
	cd Changes && make clean
	cd Linux && make clean
	cd GTK && make clean
	cd Cabal && make clean
