build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec --instrument-with bisect_ppx tests/main.exe

clean:
	dune clean
	rm -f RNAfoldml.zip

docs:
	dune build @doc

zip:
	dune clean
	rm -f RNAfoldml.zip
	zip -r RNAfoldml.zip . -x@exclude.lst

cloc:
	dune clean
	rm -f RNAfoldml.zip
	cloc --by-file --include-lang=OCaml .

bisect:
	bisect-ppx-report html