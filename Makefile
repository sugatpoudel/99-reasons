RE=rebuild

all: main

main: lists.native arithmetic.native
	$(RE) arithmetic.native lists.native

run: arithmetic.native
	./arithmetic.native

lists.native: lists.re
	$(RE) lists.native

arithmetic.native: arithmetic.re
	$(RE) arithmetic.native

clean:
	rm -rf _build *.native