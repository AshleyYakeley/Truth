build:
	cd Changes && make test
	cd Linux && make test
	cd GTK && make install

clean:
	cd Changes && make clean
	cd Linux && make clean
	cd GTK && make clean
