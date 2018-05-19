OO=ocamlopt
PP=refmt --print binary
COMPILE=$(OO) -pp "$(PP)"
SOURCES=lists.cmx arithmetic.cmx logic.cmx tests.cmx

out: $(SOURCES)
	$(COMPILE) -o out $(SOURCES)

%.cmx: %.re
	$(COMPILE) -c -impl $<

.PHONY: run
run: out
	./out

.PHONY: clean
clean:
	rm -rf _build *.native *.cm* out *.o