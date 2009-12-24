default: complete

test:
	cd Shapes && make install
	cd Core && make test
	cd Linux && make test
	cd GTK && make install
	cd Cabal && make install

complete:
	cd Shapes && make complete
	cd Core && make complete
	cd Linux && make complete
	cd GTK && make complete
	cd Cabal && make complete

clean:
	cd Shapes && make clean
	cd Core && make clean
	cd Linux && make clean
	cd GTK && make clean
	cd Cabal && make clean
