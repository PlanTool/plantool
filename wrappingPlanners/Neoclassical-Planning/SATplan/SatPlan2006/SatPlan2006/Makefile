#### This is the top-level Makefile --> recursively calls other makes
#### The next variable, BIN, should be edited to the directory where
####  you would like to install the binaries/scripts generated.

BIN = $(HOME)/bin


satplan:
	cd include; make;

install: satplan
	cp include/bin/* $(BIN)
	cp include/satplan $(BIN)
	cp include/bb $(BIN)
	cp include/SolverRevisions.txt $(BIN)

clean:
	cd include; make clean


