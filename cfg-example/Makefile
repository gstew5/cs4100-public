OCAMLBUILD=ocamlbuild
OCAMLBUILD_OPTS=-use-ocamlfind

TARGET=cfg

.PHONY: all test clean

all: 
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(TARGET).byte

doc:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(DOC_DIR)/index.html

clean:
	rm -f $(TARGET).native *~ \#*
	$(OCAMLBUILD) -clean
