RE=rebuild

lists.native: lists.re
	$(RE) lists.native

run: lists.native
	./lists.native

clean:
	rm -rf _build lists.native