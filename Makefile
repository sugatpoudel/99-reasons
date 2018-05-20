OO=ocamlopt
PP=refmt --print binary
COMPILE=$(OO) -pp "$(PP)"
SOURCES=lists.cmx arithmetic.cmx logic.cmx tests.cmx
EXE=out

$(EXE): $(SOURCES)
	$(COMPILE) -o $(EXE) $(SOURCES)

%.cmx: %.re
	$(COMPILE) -c -impl $<

.PHONY: run
run: $(EXE)
	./$(EXE)

.PHONY: clean
clean:
	rm -rf _build
	rm -f *.o *.cm* $(EXE)