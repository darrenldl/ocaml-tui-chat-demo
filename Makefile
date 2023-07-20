SRCFILES = lib/*.ml lib/*.mli bin/*.ml bin/*.mli

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: release-static
release-static :
	OCAMLPARAM='_,ccopt=-static' dune build --release bin/chat.exe
	mkdir -p statically-linked
	cp _build/default/bin/chat.exe statically-linked/chat

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean

.PHONY: readme-pdf
readme-pdf:
	pandoc README.md -o README.pdf
