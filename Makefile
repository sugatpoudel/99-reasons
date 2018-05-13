RE=rebuild

run: tests.native
	./tests.native

tests.native: tests.re lists.re arithmetic.re
	$(RE) tests.native

clean:
	rm -rf _build *.native