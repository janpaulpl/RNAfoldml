.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc
