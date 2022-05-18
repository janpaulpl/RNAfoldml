build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	rm -rf _coverage bisect*.coverage
	-dune exec --instrument-with bisect_ppx --force tests/main.exe
	bisect-ppx-report html
	rm bisect*.coverage

clean:
	rm -rf _coverage bisect*.coverage
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
