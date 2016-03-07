OCAMLBUILD=ocamlbuild
OCAMLBUILD_OPTS=-use-menhir -use-ocamlfind

TARGET=calc

.PHONY: all test clean

all: 
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(TARGET).native

doc:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(DOC_DIR)/index.html

clean:
	rm -f $(TARGET).native *~ \#*
	$(OCAMLBUILD) -clean
