build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec tests/main.exe

clean:
	dune clean
	rm -f RNAfoldml.zip

doc:
	dune build @doc

zip:
	rm -f RNAfoldml.zip
	zip -r RNAfoldml.zip . -x@exclude.lst
